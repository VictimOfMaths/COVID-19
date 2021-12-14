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
         newPeopleVaccinatedThirdInjectionByVaccinationDate) %>% 
  set_names("date", "age", "dose1", "dose2", "dose3") %>% 
  mutate(date=as.Date(date)) %>% 
  #Some fuckery to crudely align the age bands, assuming equal coverage *within* each
  #age band in the vaccination data
  pivot_wider(names_from=age, values_from=c(dose1, dose2, dose3), names_sep="__") %>% 
  mutate(dose1__10_14=dose1__12_15*0.75, dose2__10_14=dose2__12_15*0.75, 
         dose3__10_14=dose3__12_15*0.75, 
         dose1__15_19=dose1__12_15*0.25+dose1__16_17+dose1__18_24*2/7,
         dose2__15_19=dose2__12_15*0.25+dose2__16_17+dose2__18_24*2/7,
         dose3__15_19=dose3__12_15*0.25+dose3__16_17+dose3__18_24*2/7,
         dose1__20_24=dose1__18_24*5/7, dose2__20_24=dose2__18_24*5/7,
         dose3__20_24=dose3__18_24*5/7) %>% 
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

#Combine  
data <- merge(casedata, vaxdata, all=TRUE) %>% 
  merge(deathsdata, all=TRUE) %>% 
  mutate(dose1=if_else(is.na(dose1), 0, dose1),
         dose2=if_else(is.na(dose2), 0, dose2),
         dose3=if_else(is.na(dose3), 0, dose3)) %>% 
  #Calculate rolling avg cases and cumulative vax doses
  group_by(age) %>% 
  arrange(date) %>% 
  mutate(cases_roll=roll_mean(cases, 7, align="center", fill=NA),
         deaths_roll=roll_mean(deaths, 7, align="center", fill=NA),
         cum_dose1=cumsum(dose1), cum_dose2=cumsum(dose2),
         cum_dose3=cumsum(dose3)) %>% 
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
         dose3prop=cum_dose3/pop) %>% 
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
  select(date, age, dose1, dose2, dose3) %>% 
  filter(date>as.Date("2020-12-01")) %>% 
  gather(dose, no, c(dose1, dose2, dose3)) %>% 
  mutate(contage=case_when(
    age=="00_04" ~ 2, age=="05_09" ~ 7, age=="10_14" ~ 12, age=="15_19" ~ 17,
    age=="20_24" ~ 22, age=="25_29" ~ 27, age=="30_34" ~ 32, age=="35_39" ~ 37,
    age=="40_44" ~ 42, age=="45_49" ~ 47, age=="50_54" ~ 52, age=="55_59" ~ 57,
    age=="60_64" ~ 62, age=="65_69" ~ 67, age=="70_74" ~ 72, age=="75_79" ~ 77,
    age=="80_84" ~ 82, age=="85_89" ~ 87, age=="90+" ~ 92),
    week=if_else(year(date)==2020, week(date), week(date)+53)) %>% 
  group_by(contage, week, dose) %>% 
  summarise(no=sum(no)) %>% 
  ungroup()


agg_tiff("Outputs/COVIDVaxRidgesxAgexDose.tiff", units="in", width=8, height=8, res=500)
ggplot(ridgedata, aes(x=contage, y=fct_rev(as.factor(week)), height=no, fill=dose))+
  geom_density_ridges(stat="identity", scale=10, alpha=0.3, colour=NA)+
  scale_x_continuous(name="Age", limits=c(0,95))+
  scale_y_discrete(breaks=c(49, 54, 58, 62, 67, 71, 75, 79, 84, 88, 93, 97, 101),
                   labels=c("Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep",
                            "Oct", "Nov", "Dec"), name="")+
  scale_fill_paletteer_d("lisa::AndyWarhol_2", name="Dose", 
                         labels=c("1st dose", "2nd dose", "3rd dose/booster"))+
  theme_custom()+
  theme(axis.line.y=element_blank())+
  labs(title="Vaccine rollout down the age groups",
       subtitle="Age distribution of COVID vaccines delivered each week in England by dose",
       caption="Data from coronavirus.data.gov.uk | Inspired by @MathiasLeroy_ | Plot by @VictimOfMaths")

dev.off()
