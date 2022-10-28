rm(list=ls())

library(tidyverse)
library(curl)
library(RcppRoll)
library(readxl)
library(paletteer)
library(extrafont)
library(ragg)
library(scales)
library(ggrepel)
library(ggridges)
library(ungroup)
library(lubridate)

theme_custom <- function() {
  theme_classic() %+replace%
    theme(plot.title.position="plot", plot.caption.position="plot",
          strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)),
          plot.title=element_text(face="bold", size=rel(1.5), hjust=0,
                                  margin=margin(0,0,5.5,0)),
          text=element_text(family="Lato"))
}

#Have to download these two metrics separately for some reason?
temp <- tempfile()
sourcevax <- "https://api.coronavirus.data.gov.uk/v2/data?areaType=nation&areaCode=E92000001&metric=vaccinationsAgeDemographics&format=csv"
temp <- curl_download(url=sourcevax, destfile=temp, quiet=FALSE, mode="wb")

vaxdata <- read.csv(temp) %>% 
  select(date, age, newPeopleVaccinatedFirstDoseByVaccinationDate,
         newPeopleVaccinatedSecondDoseByVaccinationDate,
         newPeopleVaccinatedThirdInjectionByVaccinationDate,
         newPeopleVaccinatedSpring22ByVaccinationDate,
         newPeopleVaccinatedAutumn22ByVaccinationDate) %>% 
  set_names("date", "age", "dose1", "dose2", "dose3", "spring22", "autumn22") %>% 
  mutate(date=as.Date(date)) %>% 
  #Some fuckery to crudely align the age bands, assuming equal coverage *within* each
  #age band in the vaccination data
  pivot_wider(names_from=age, values_from=c(dose1, dose2, dose3, spring22, autumn22), 
              names_sep="__") %>% 
  mutate(across(starts_with("spring22"), ~replace_na(., 0)),
         across(starts_with("autumn22"), ~replace_na(., 0)),
         dose1__10_14=dose1__12_15*0.75, dose2__10_14=dose2__12_15*0.75, 
         dose3__10_14=dose3__12_15*0.75, spring22__10_14=spring22__05_11*2/7+spring22__12_15*0.75,
         autumn22__10_14=autumn22__05_11*2/7+autumn22__12_15*0.75,
         dose1__15_19=dose1__12_15*0.25+dose1__16_17+dose1__18_24*2/7,
         dose2__15_19=dose2__12_15*0.25+dose2__16_17+dose2__18_24*2/7,
         dose3__15_19=dose3__12_15*0.25+dose3__16_17+dose3__18_24*2/7,
         spring22__15_19=spring22__12_15*0.25+spring22__16_17+spring22__18_24*2/7,
         autumn22__15_19=autumn22__12_15*0.25+autumn22__16_17+autumn22__18_24*2/7,
         dose1__20_24=dose1__18_24*5/7, dose2__20_24=dose2__18_24*5/7,
         dose3__20_24=dose3__18_24*5/7, spring22__20_24=spring22__18_24*5/7,
         autumn22__20_24=autumn22__18_24*5/7) %>% 
  pivot_longer(cols=c(2:ncol(.)), names_to=c("dose", "age"), names_sep="__") %>% 
  filter(!age %in% c("12_15", "16_17", "18_24")) %>% 
  spread(dose, value)

#Case data
sourcecases <- "https://api.coronavirus.data.gov.uk/v2/data?areaType=nation&areaCode=E92000001&metric=newCasesBySpecimenDateAgeDemographics&format=csv"
temp <- curl_download(url=sourcecases, destfile=temp, quiet=FALSE, mode="wb")

casedata <- read.csv(temp) %>% 
  filter(!age %in% c("unassigned", "00_59", "60+")) %>% 
  select(date, age, cases) %>% 
  mutate(date=as.Date(date))

#Deaths data
sourcedeaths <- "https://api.coronavirus.data.gov.uk/v2/data?areaType=nation&areaCode=E92000001&metric=newDeaths28DaysByDeathDateAgeDemographics&format=csv"
temp <- curl_download(url=sourcedeaths, destfile=temp, quiet=FALSE, mode="wb")

deathsdata <- read.csv(temp) %>% 
  filter(!age %in% c("00_59", "60+")) %>% 
  select(date, age, deaths) %>% 
  mutate(date=as.Date(date))

#Combine (avoiding missing date/age combinations because of faff reasons)  
data <- data.frame(date=rep(seq.Date(from=min(min(deathsdata$date), min(casedata$date), 
                                          min(vaxdata$date)),
                                 to=max(max(deathsdata$date), max(casedata$date), 
                                        max(vaxdata$date)), "days"), 
                       each=length(unique(casedata$age)))) %>% 
  mutate(age=rep(unique(casedata$age), 
                 times=length(seq.Date(from=min(min(deathsdata$date), min(casedata$date), 
                                                                      min(vaxdata$date)),
                                       to=max(max(deathsdata$date), max(casedata$date), 
                                                                    max(vaxdata$date)), 
                                       "days")))) %>% 
  merge(casedata, all=TRUE) %>% 
  merge(vaxdata, all=TRUE) %>% 
  merge(deathsdata, all=TRUE) %>% 
  mutate(dose1=if_else(is.na(dose1), 0, dose1),
         dose2=if_else(is.na(dose2), 0, dose2),
         dose3=if_else(is.na(dose3), 0, dose3),
         spring22=replace_na(spring22, 0),
         autumn22=replace_na(autumn22, 0)) %>% 
  #Calculate rolling avg cases and cumulative vax doses
  group_by(age) %>% 
  arrange(date) %>% 
  mutate(cases_roll=roll_mean(cases, 7, align="center", fill=NA),
         deaths_roll=roll_mean(deaths, 7, align="center", fill=NA),
         cum_dose1=cumsum(dose1), cum_dose2=cumsum(dose2),
         cum_dose3=cumsum(dose3), cumspring22=cumsum(spring22),
         cumautumn22=cumsum(autumn22)) %>% 
  ungroup()

#Bring in populations for rates
url <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fpopulationandmigration%2fpopulationestimates%2fdatasets%2fpopulationestimatesforukenglandandwalesscotlandandnorthernireland%2fmid2020/ukpopestimatesmid2020on2021geography.xls"

temp <- tempfile()
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

ONSpop <- read_excel(temp, sheet="MYE2 - Persons", range="E8:CQ12") %>% 
  slice_tail() %>% 
  gather(age, pop, c(1:ncol(.))) %>% 
  mutate(age=as.numeric(substr(age, 1, 2)),
         age=case_when(
           age<5 ~ "00_04",
           age<10 ~ "05_09",
           age<15 ~ "10_14",
           age<20 ~ "15_19",
           age<25 ~ "20_24",
           age<30 ~ "25_29",
           age<35 ~ "30_34",
           age<40 ~ "35_39",
           age<45 ~ "40_44",
           age<50 ~ "45_49",
           age<55 ~ "50_54",
           age<60 ~ "55_59",
           age<65 ~ "60_64",
           age<70 ~ "65_69",
           age<75 ~ "70_74",
           age<80 ~ "75_79",
           age<85 ~ "80_84",
           age<90 ~ "85_89",
           TRUE ~ "90+")) %>% 
  group_by(age) %>% 
  summarise(pop=sum(pop)) %>% 
  ungroup()

finaldata <- data %>% 
  merge(ONSpop, all=TRUE) %>% 
  mutate(caserate_roll=cases_roll*100000/pop,
         deathrate_roll=deaths_roll*100000/pop,
         dose1prop=cum_dose1/pop,
         dose2prop=cum_dose2/pop,
         dose3prop=cum_dose3/pop, spring22prop=cumspring22/pop,
         autumn22prop=cumautumn22/pop) %>% 
  #Pick out Dec20/Jan21 peak
  group_by(age) %>% 
  mutate(casepeak=max(caserate_roll[date>as.Date("2020-10-01") & 
                                      date<as.Date("2021-02-01")]),
         deathpeak=max(deathrate_roll[date>as.Date("2020-10-01") & 
                                      date<as.Date("2021-02-01")])) %>% 
  ungroup() %>% 
  mutate(caseproppeak=caserate_roll/casepeak,
         deathproppeak=deathrate_roll/deathpeak)

agg_tiff("Outputs/COVIDBoostersxAgevsCases.tiff", units="in", width=9, height=7, res=500)
ggplot()+
  geom_hline(yintercept=1, linetype=2, colour="Grey60")+
  geom_line(data=finaldata %>% filter(date>as.Date("2021-09-10") & 
                                        !age %in% c("00_04", "05_09","10_14")), 
            aes(x=date, y=caseproppeak, group=age, colour=dose3prop))+
  geom_text_repel(data=finaldata %>% filter(date==max(date[!is.na(caseproppeak)]) & 
                                              !age %in% c("00_04", "05_09","10_14")),
    aes(x=max(date[!is.na(caseproppeak)]), y=caseproppeak, label = age, colour=dose3prop),
    family = "Lato", direction = "y", xlim = c(as.Date("2021-12-10"), NA),
    hjust = 0, segment.size = .7, segment.alpha = .5, segment.linetype = "dotted",
    box.padding = .3, segment.curvature = -0.1, segment.ncp = 3, segment.angle = 20) +
  scale_x_date(name="")+
  scale_y_continuous(name="COVID cases as a proportion of their Dec 20/Jan21 peak",
                     labels=label_percent(accuracy=1), limits=c(0,NA),
                     breaks=c(0,0.25,0.5,0.75,1,1.25))+
  scale_colour_paletteer_c("ggthemes::Red-Green Diverging", name="Booster coverage",
                           limits=c(0,1), labels=label_percent(accuracy=1))+
  theme_custom()+
  theme(legend.position="top")+
  guides(colour = guide_colorbar(title.position = 'top', title.hjust = .5,
                               barwidth = unit(20, 'lines'), barheight = unit(.5, 'lines')))+
  labs(title="Boosters have been doing a great job against Delta transmission",
       subtitle="Rolling 7-day rate of new COVID cases as a proportion of the peak last winter by age group, coloured by booster/3rd dose coverage\nfor all age groups 15+",
       caption="Data from coronavirus.data.gov.uk and ONS | Inspired by @jburnmurdoch | Plot by @VictimOfMaths")

dev.off()

agg_tiff("Outputs/COVIDBoostersxAgevsCasesDark.tiff", units="in", width=9, height=7, res=500)
ggplot()+
  geom_hline(yintercept=1, linetype=2, colour="Grey40")+
  geom_line(data=finaldata %>% filter(date>as.Date("2021-09-10") & 
                                        !age %in% c("00_04", "05_09","10_14")), 
            aes(x=date, y=caseproppeak, group=age, colour=dose3prop))+
  geom_text_repel(data=finaldata %>% filter(date==max(date[!is.na(caseproppeak)]) & 
                                              !age %in% c("00_04", "05_09","10_14")),
                  aes(x=max(date[!is.na(caseproppeak)]), y=caseproppeak, label = age, colour=dose3prop),
                  family = "Lato", direction = "y", xlim = c(as.Date("2021-12-10"), NA),
                  hjust = 0, segment.size = .7, segment.alpha = .5, segment.linetype = "dotted",
                  box.padding = .3, segment.curvature = -0.1, segment.ncp = 3, segment.angle = 20) +
  scale_x_date(name="")+
  scale_y_continuous(name="COVID cases as a proportion of their Dec 20/Jan21 peak",
                     labels=label_percent(accuracy=1), limits=c(0,NA),
                     breaks=c(0,0.25,0.5,0.75,1,1.25))+
  scale_colour_paletteer_c("viridis::magma", name="Booster coverage",
                           limits=c(0,1), labels=label_percent(accuracy=1))+
  theme_custom()+
  theme(legend.position="top", plot.background=element_rect(fill="Grey20", 
                                                            colour="Grey20"),
        panel.background=element_rect(fill="Grey20", 
                                      colour="Grey20"),
        legend.background=element_rect(fill="Grey20", 
                                       colour="Grey20"),
        text=element_text(colour="cornsilk"),
        axis.text=element_text(colour="cornsilk"))+
  guides(colour = guide_colorbar(title.position = 'top', title.hjust = .5,
                                 barwidth = unit(20, 'lines'), 
                                 barheight = unit(.5, 'lines')))+
  labs(title="Boosters have been doing a great job against Delta transmission",
       subtitle="Rolling 7-day rate of new COVID cases as a proportion of the peak last winter by age group, coloured by booster/3rd dose coverage\nfor all age groups 15+",
       caption="Data from coronavirus.data.gov.uk and ONS | Inspired by @jburnmurdoch | Plot by @VictimOfMaths")

dev.off()

agg_tiff("Outputs/COVIDBoostersxAgevsCasesFacet.tiff", units="in", width=9, height=7, res=500)
ggplot()+
  geom_hline(yintercept=1, linetype=2, colour="Grey60")+
  geom_line(data=finaldata %>% filter(date>as.Date("2021-09-10") & 
                                        !age %in% c("00_04", "05_09","10_14")), 
            aes(x=date, y=caseproppeak, group=age, colour=dose3prop))+
  
  scale_x_date(name="")+
  scale_y_continuous(name="COVID cases as a proportion of their Dec 20/Jan21 peak",
                     labels=label_percent(accuracy=1), limits=c(0,NA),
                     breaks=c(0,0.25,0.5,0.75,1,1.25))+
  scale_colour_paletteer_c("ggthemes::Red-Green Diverging", name="Booster coverage",
                           limits=c(0,1), labels=label_percent(accuracy=1))+
  facet_wrap(~age)+
  theme_custom()+
  theme(legend.position="top")+
  guides(colour = guide_colorbar(title.position = 'top', title.hjust = .5,
                                 barwidth = unit(20, 'lines'), barheight = unit(.5, 'lines')))+
  labs(title="Boosters have been doing a great job against Delta transmission",
       subtitle="Rolling 7-day rate of new COVID cases as a proportion of the peak last winter by age group, coloured by booster/3rd dose coverage\nfor all age groups 15+",
       caption="Data from coronavirus.data.gov.uk and ONS | Inspired by @jburnmurdoch | Plot by @VictimOfMaths")

dev.off()

agg_tiff("Outputs/COVIDBoostersxAgevsDeaths.tiff", units="in", width=9, height=7, res=500)
ggplot()+
  #geom_hline(yintercept=1, linetype=2, colour="Grey60")+
  geom_line(data=finaldata %>% filter(date>as.Date("2021-09-10") & 
                                        !age %in% c("00_04", "05_09","10_14", "15_19", 
                                                    "20_24", "25_29", "30_34", "35_39")), 
            aes(x=date, y=deathproppeak, group=age, colour=dose3prop))+
  geom_text_repel(data=finaldata %>% filter(date==max(date[!is.na(deathproppeak)]) & 
                                              !age %in% c("00_04", "05_09","10_14", "15_19", 
                                                          "20_24", "25_29", "30_34", "35_39")),
                  aes(x=max(date[!is.na(deathproppeak)]), y=deathproppeak, label = age, colour=dose3prop),
                  family = "Lato", direction = "y", xlim = c(as.Date("2021-12-10"), NA),
                  hjust = 0, segment.size = .7, segment.alpha = .5, segment.linetype = "dotted",
                  box.padding = .3, segment.curvature = -0.1, segment.ncp = 3, segment.angle = 20) +
  scale_x_date(name="")+
  scale_y_continuous(name="COVID deaths as a proportion of their Dec 20/Jan21 peak",
                     labels=label_percent(accuracy=1), limits=c(0,NA))+
  scale_colour_paletteer_c("ggthemes::Red-Green Diverging", name="Booster coverage",
                           limits=c(0,1), labels=label_percent(accuracy=1))+
  theme_custom()+
  theme(legend.position="top")+
  guides(colour = guide_colorbar(title.position = 'top', title.hjust = .5,
                                 barwidth = unit(20, 'lines'), barheight = unit(.5, 'lines')))+
  labs(title="Boosters have been effective at limiting deaths due to Delta",
       subtitle="Rolling 7-day rate of deaths within 28 days of a positive COVID test as a proportion of the peak last winter by age group,\ncoloured by booster/3rd dose coverage for all age groups 40+",
       caption="Data from coronavirus.data.gov.uk and ONS | Inspired by @jburnmurdoch | Plot by @VictimOfMaths")

dev.off()

##########################
#Ridgeplots of vaccination age profiles by week
#Heavily inspired by this wonderful plot 
#https://twitter.com/MathiasLeroy_/status/1470785658537062407
#from Mathias Leroy

ridgedata <- finaldata %>% 
  filter(!age %in% c("50+", "75+", "05_11")) %>% 
  select(date, age, dose1, dose2, dose3, spring22, autumn22) %>% 
  filter(date>as.Date("2020-12-01")) %>% 
  gather(dose, no, c(dose1, dose2, dose3, spring22, autumn22)) %>% 
  mutate(contage=case_when(
    age=="00_04" ~ 2, age=="05_09" ~ 7, age=="10_14" ~ 12, age=="15_19" ~ 17,
    age=="20_24" ~ 22, age=="25_29" ~ 27, age=="30_34" ~ 32, age=="35_39" ~ 37,
    age=="40_44" ~ 42, age=="45_49" ~ 47, age=="50_54" ~ 52, age=="55_59" ~ 57,
    age=="60_64" ~ 62, age=="65_69" ~ 67, age=="70_74" ~ 72, age=="75_79" ~ 77,
    age=="80_84" ~ 82, age=="85_89" ~ 87, age=="90+" ~ 92),
    week=case_when(
      year(date)==2020 ~ week(date), 
      year(date)==2021 ~ week(date)+53,
      TRUE ~ week(date)+105)) %>% 
  group_by(contage, week, dose) %>% 
  summarise(no=sum(no)) %>% 
  ungroup() %>% 
  mutate(dose=factor(dose, levels=c("dose1", "dose2", "dose3", "spring22", "autumn22")))


agg_tiff("Outputs/COVIDVaxRidgesxAgexDose.tiff", units="in", width=8, height=8, res=500)
ggplot(ridgedata, aes(x=contage, y=fct_rev(as.factor(week)), height=no, fill=dose))+
  geom_density_ridges(stat="identity", scale=10, alpha=0.3, colour=NA)+
  scale_x_continuous(name="Age", limits=c(0,95))+
  scale_y_discrete(breaks=c(49, 54, 58, 62, 67, 71, 75, 79, 84, 88, 93, 97, 101, 106, 110, 114,
                            118, 123, 127, 131, 136, 140),
                   labels=c("Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep",
                            "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", 
                            "Aug", "Sep"), name="")+
  scale_fill_paletteer_d("lisa::AndyWarhol_2", name="Dose", 
                         labels=c("1st dose", "2nd dose", "3rd dose/booster",
                                  "Spring booster", "Autumn booster"))+
  theme_custom()+
  theme(axis.line.y=element_blank())+
  labs(title="Vaccine rollout down the age groups",
       subtitle="Age distribution of COVID vaccines delivered each week in England by dose",
       caption="Data from coronavirus.data.gov.uk | Inspired by @MathiasLeroy_ | Plot by @VictimOfMaths")

dev.off()

#Alternative version interpolating the age groups to give (hopefully) smoother
smoothdata <- finaldata %>% 
  filter(!age %in% c("50+", "75+", "05_11")) %>% 
  select(date, age, dose1, dose2, dose3, spring22, autumn22) %>% 
  filter(date>as.Date("2020-12-01")) %>% 
  gather(dose, no, c(dose1, dose2, dose3, spring22, autumn22)) %>% 
  mutate(contage=case_when(
    age=="00_04" ~ 0, age=="05_09" ~ 5, age=="10_14" ~ 10, age=="15_19" ~ 15,
    age=="20_24" ~ 20, age=="25_29" ~ 25, age=="30_34" ~ 30, age=="35_39" ~ 35,
    age=="40_44" ~ 40, age=="45_49" ~ 45, age=="50_54" ~ 50, age=="55_59" ~ 55,
    age=="60_64" ~ 60, age=="65_69" ~ 65, age=="70_74" ~ 70, age=="75_79" ~ 75,
    age=="80_84" ~ 80, age=="85_89" ~ 85, age=="90+" ~ 90),
    week=case_when(
      year(date)==2020 ~ week(date), 
      year(date)==2021 ~ week(date)+53,
      TRUE ~ week(date)+105)) %>% 
  #filter(dose=="dose1" & week=="55") %>% 
  group_by(age, contage, week, dose) %>% 
  summarise(no=sum(no)) %>% 
  ungroup() %>% 
  merge(ONSpop)%>% 
  mutate(dose=factor(dose, levels=c("dose1", "dose2", "dose3", "spring22", "autumn22")))

smootheddata <- data.frame(dose=character(), week=integer(), age=integer(),
                            smoothedno=double())

#First smooth out first 3 doses
for(i in c("dose1", "dose2", "dose3")){
  for(j in min(smoothdata$week):max(smoothdata$week)){
    working <- smoothdata %>% 
      filter(dose==i & week==j) %>% 
      mutate(no=if_else(no==0, 0.001, no))
    x <- working$contage
    y <- working$no
    offset <- working$pop
    nlast <- 21
    
    smoothed <- pclm(x, y, nlast)
    
    outputs <- as.data.frame(smoothed$fitted) %>% 
      mutate(dose=i, week=j, age=0:110) %>% 
      rename("smoothedno"="smoothed$fitted")
    
    smootheddata=smootheddata %>% bind_rows(outputs)
  }
}

#Add in spring boosters (only available to over 75s)
for(j in c(117:140)){
  working <- smoothdata %>% 
    filter(dose=="spring22" & week==j & contage>=75) %>% 
    mutate(no=if_else(no==0, 0.001, no))
  x <- working$contage
  y <- working$no
  offset <- working$pop
  nlast <- 21
  
  smoothed <- pclm(x, y, nlast)
  
  outputs <- as.data.frame(smoothed$fitted) %>% 
    mutate(dose="spring22", week=j, age=75:110) %>% 
    rename("smoothedno"="smoothed$fitted")
  
  smootheddata=smootheddata %>% bind_rows(outputs)
}

#Add in autumn boosters (only available to over 50s)
for(j in c(140:max(smoothdata$week))){
  working <- smoothdata %>% 
    filter(dose=="autumn22" & week==j & contage>=50) %>% 
    mutate(no=if_else(no==0, 0.001, no))
  x <- working$contage
  y <- working$no
  offset <- working$pop
  nlast <- 21
  
  smoothed <- pclm(x, y, nlast)
  
  outputs <- as.data.frame(smoothed$fitted) %>% 
    mutate(dose="autumn22", week=j, age=50:110) %>% 
    rename("smoothedno"="smoothed$fitted")
  
  smootheddata=smootheddata %>% bind_rows(outputs)
}

smootheddata <- smootheddata %>% 
  mutate(dose=factor(dose, levels=c("dose1", "dose2", "dose3", "spring22", "autumn22")))

agg_tiff("Outputs/COVIDVaxRidgesxAgexDoseSmoothed.tiff", units="in", width=8, height=8, res=500)
ggplot(smootheddata, aes(x=age, y=fct_rev(as.factor(week)), height=smoothedno, fill=dose,
                         colour=dose))+
  geom_density_ridges(stat="identity", scale=10, alpha=0.4, rel_min_height=0.01)+
  geom_segment(aes(x = 5, y = 35, xend = 5, yend = 25),
               arrow = arrow(length = unit(0.3, "cm")), colour="Grey40")+
  scale_x_continuous(name="Age", limits=c(0,105))+
  scale_y_discrete(breaks=c(49, 54, 58, 62, 67, 71, 75, 79, 84, 88, 93, 97, 101, 106, 110, 114,
                            118, 123, 127, 131, 136, 140, 144),
                   labels=c("Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep",
                            "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", 
                            "Aug", "Sep", "Oct"), name="")+
  scale_fill_paletteer_d("fishualize::Scarus_tricolor", name="Dose", 
                         labels=c("1st dose", "2nd dose", "3rd dose/booster",
                                  "Spring booster", "Autumn booster"))+
  scale_colour_paletteer_d("fishualize::Scarus_tricolor", guide="none")+
  annotate("text", x=2, y=30, angle=90, label="More recent", family="Lato", colour="Grey40")+
  annotate("text", x=0, y=38, label="2022", family="Lato", colour="Grey40", size=rel(3))+
  annotate("text", x=0, y=90, label="2021", family="Lato", colour="Grey40", size=rel(3))+
  theme_custom()+
  theme(axis.line.y=element_blank())+
  labs(title="Vaccine rollout down the age groups",
       subtitle="Age distribution of COVID vaccines delivered each week in England by dose",
       caption="Data from coronavirus.data.gov.uk | Inspired by @MathiasLeroy_ | Plot by @VictimOfMaths")

dev.off()

agg_tiff("Outputs/COVIDVaxRidgesxAgexDoseSmoothedv2.tiff", units="in", width=8, height=8, res=500)
ggplot(smootheddata, aes(x=age, y=fct_rev(as.factor(week)), height=smoothedno, fill=dose))+
  geom_density_ridges(stat="identity", scale=10, alpha=0.4, rel_min_height=0.01, colour=NA)+
  geom_segment(aes(x = 5, y = 35, xend = 5, yend = 25),
               arrow = arrow(length = unit(0.3, "cm")), colour="Grey40")+
  scale_x_continuous(name="Age", limits=c(0,105))+
  scale_y_discrete(breaks=c(49, 54, 58, 62, 67, 71, 75, 79, 84, 88, 93, 97, 101, 106, 110, 114,
                            118, 123, 127, 131, 136, 140, 144),
                   labels=c("Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep",
                            "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", 
                            "Aug", "Sep", "Oct"), name="")+
  scale_fill_paletteer_d("fishualize::Scarus_tricolor", name="Dose", 
                         labels=c("1st dose", "2nd dose", "3rd dose/booster",
                                  "Spring booster", "Autumn booster"))+
  annotate("text", x=2, y=30, angle=90, label="More recent", family="Lato", colour="Grey40")+
  annotate("text", x=0, y=43, label="2022", family="Lato", colour="Grey40", size=rel(3))+
  annotate("text", x=0, y=95, label="2021", family="Lato", colour="Grey40", size=rel(3))+
  theme_custom()+
  theme(axis.line.y=element_blank())+
  labs(title="Vaccine rollout down the age groups",
       subtitle="Age distribution of COVID vaccines delivered each week in England by dose",
       caption="Data from coronavirus.data.gov.uk | Inspired by @MathiasLeroy_ | Plot by @VictimOfMaths")

dev.off()

agg_tiff("Outputs/COVIDVaxRidgesxAgexDoseSmoothedv3.tiff", units="in", width=8, height=8, res=500)
ggplot(smootheddata, aes(x=age, y=as.factor(week), height=smoothedno, fill=dose))+
  geom_density_ridges(stat="identity", scale=10, alpha=0.4, colour=NA)+
  geom_segment(aes(x = 5, y = 25, xend = 5, yend = 35),
               arrow = arrow(length = unit(0.3, "cm")), colour="Grey40")+
  scale_x_continuous(name="Age", limits=c(0,105))+
  scale_y_discrete(breaks=c(49, 54, 58, 62, 67, 71, 75, 79, 84, 88, 93, 97, 101),
                   labels=c("Dec\n2020", "Jan\n2021", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep",
                            "Oct", "Nov", "Dec"), name="")+
  scale_fill_paletteer_d("lisa::AndyWarhol_2", name="Dose", 
                         labels=c("1st dose", "2nd dose", "3rd dose/booster"))+
  annotate("text", x=3, y=30, label="More recent", family="Lato", colour="Grey40")+
  theme_custom()+
  theme(axis.line.y=element_blank())+
  labs(title="Vaccine rollout down the age groups",
       subtitle="Age distribution of COVID vaccines delivered each week in England by dose",
       caption="Data from coronavirus.data.gov.uk | Inspired by @MathiasLeroy_ | Plot by @VictimOfMaths")+
  coord_flip()

dev.off()
