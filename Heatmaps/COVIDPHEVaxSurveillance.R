rm(list=ls())

library(tidyverse)
library(readxl)
library(lubridate)
library(extrafont)
library(ragg)

theme_custom <- function() {
  theme_classic() %+replace%
    theme(plot.title.position="plot", plot.caption.position="plot",
          strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)),
          plot.title=element_text(face="bold", size=rel(1.5), hjust=0,
                                  margin=margin(0,0,5.5,0)),
          text=element_text(family="Lato"))
}

#All case data from the latest PHE vaccine surveillance report
#https://www.gov.uk/government/publications/covid-19-vaccine-surveillance-report

#Cases
cases <- data.frame(
  age=c("Under 18", "18-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+"),
  Unlinked=c(15901, 19529, 12452, 8930, 6868, 3657, 2034, 1124),
  Unvaccinated=c(141676, 53187, 33986, 15106, 7552, 2650, 910, 545),
  Fully_vaccinated=c(757, 32533, 43004, 67349, 67652, 38119, 22270, 10087)
) %>% 
  mutate(metric="cases")

deaths28 <- data.frame(
  age=c("Under 18", "18-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+"),
  Unlinked=c(0,1,2,3,3,7,2,7),
  Unvaccinated=c(3,13,31,54,100,115,129,155),
  Fully_vaccinated=c(0,3,8,27,71,194,428,928)
) %>% 
  mutate(metric="deaths")

admissions <-  data.frame(
  age=c("Under 18", "18-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+"),
  Unlinked=c(25,14,16,14,10,7,3,1),
  Unvaccinated=c(404,387,516,497,421,328,194,144),
  Fully_vaccinated=c(0,80,118,220,406,571,873,965)
) %>% 
  mutate(metric="admissions")

rawdata <- bind_rows(cases, admissions, deaths28)

#Bring in vaccinated and NIMS/ONS population data - use figures from week 32 (to allow for 2 weeks post 2nd jab by
#~mid-point of 4 week analysis window)
url <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/08/COVID-19-weekly-announced-vaccinations-12-August-2021.xlsx"
temp <- tempfile()
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

vaxed1st <- read_excel(temp, sheet="NHS Region", range="D12:Q13") %>% 
  mutate(`18-29`=`18-24`+`25-29`, `30-39`=`30-34`+`35-39`, `40-49`=`40-44`+`45-49`,
         `50-59`=`50-54`+`55-59`, `60-69`=`60-64`+`65-69`, `70-79`=`70-74`+`75-79`) %>% 
  select(`Under 18`, `18-29`, `30-39`, `40-49`, `50-59`, `60-69`, `70-79`, `80+`) %>% 
  gather(age, vax1pop)

vaxed2nd <- read_excel(temp, sheet="NHS Region", range="U12:AH13") %>% 
  mutate(`18-29`=`18-24`+`25-29`, `30-39`=`30-34`+`35-39`, `40-49`=`40-44`+`45-49`,
         `50-59`=`50-54`+`55-59`, `60-69`=`60-64`+`65-69`, `70-79`=`70-74`+`75-79`) %>% 
  select(`Under 18`, `18-29`, `30-39`, `40-49`, `50-59`, `60-69`, `70-79`, `80+`) %>% 
  gather(age, vax2pop)

NIMSpop <- read_excel(temp, sheet="Population estimates (NIMS)", range="F13:S14") %>% 
  mutate(`18-29`=`18-24`+`25-29`, `30-39`=`30-34`+`35-39`, `40-49`=`40-44`+`45-49`,
         `50-59`=`50-54`+`55-59`, `60-69`=`60-64`+`65-69`, `70-79`=`70-74`+`75-79`) %>% 
  select(`Under 18`, `18-29`, `30-39`, `40-49`, `50-59`, `60-69`, `70-79`, `80+`) %>% 
  gather(age, pop_NIMS)

ONSpop <- read_excel(temp, sheet="Population estimates (ONS 2020)", range="B16:D29", col_names=FALSE) %>% 
  select(-2) %>% 
  set_names(c("age", "pop_ONS")) %>% 
  spread(age, pop_ONS) %>% 
  mutate(`18-29`=`18-24`+`25-29`, `30-39`=`30-34`+`35-39`, `40-49`=`40-44`+`45-49`,
         `50-59`=`50-54`+`55-59`, `60-69`=`60-64`+`65-69`, `70-79`=`70-74`+`75-79`) %>% 
  select(`Under 18`, `18-29`, `30-39`, `40-49`, `50-59`, `60-69`, `70-79`, `80+`) %>% 
  gather(age, pop_ONS)

#Alternative approach to calculating vaxed populations from dashboard
url <- "https://api.coronavirus.data.gov.uk/v2/data?areaType=nation&areaCode=E92000001&metric=vaccinationsAgeDemographics&format=csv"
temp <- tempfile()
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

dashboarddata <- read.csv(temp) %>% 
  mutate(age=case_when(
    age=="16_17" ~ "16-17",
    age %in% c("18_24", "25_29") ~ "18-29",
    age %in% c("30_34", "35_39") ~ "30-39",
    age %in% c("40_44", "45_49") ~ "40-49",
    age %in% c("50_54", "55_59") ~ "50-59",
    age %in% c("60_64", "65_69") ~ "60-69",
    age %in% c("70_74", "75_79") ~ "70-79",
    age %in% c("80_84", "85_89", "90+") ~ "80+",
    TRUE ~ age)) %>% 
  group_by(age, date) %>% 
  summarise(cumPeopleVaccinatedSecondDoseByVaccinationDate=sum(cumPeopleVaccinatedSecondDoseByVaccinationDate),
            cumPeopleVaccinatedFirstDoseByVaccinationDate=sum(cumPeopleVaccinatedFirstDoseByVaccinationDate)) %>% 
  ungroup() %>% 
  #lag second doses by 2 weeks
  group_by(age) %>% 
  arrange(date) %>% 
  mutate(dose2=lag(cumPeopleVaccinatedSecondDoseByVaccinationDate, 14)) %>% 
  #filter out weeks 32-35 to match PHE data
  filter(date>=as.Date("2021-08-09") & date<=as.Date("2021-09-05")) %>% 
  #calculate average populations across the study period
  summarise(singledose=mean(cumPeopleVaccinatedFirstDoseByVaccinationDate),
            doubledose=mean(dose2))

#Because of reasons, PHE's NIMS figures in the dashboard are not (in spite of the field name) by vaccination date,
#The data reflects only the NIMS pop estimates on the day that you download the data.
#So, here's a big old faff with the archive data to get the actual figures
callarchive <- function(funcdate) {
  filepath <- paste0("https://api.coronavirus.data.gov.uk/v2/data?areaType=nation&areaCode=E92000001&metric=vaccinationsAgeDemographics&format=csv&release=",
                     as.character(as.Date(funcdate)+days(1)))
  temp <- curl_download(url=filepath, destfile=temp, quiet=FALSE, mode="wb")
  file <- read.csv(temp) %>% 
    select(date, age, VaccineRegisterPopulationByVaccinationDate) %>% 
    filter(date==as.Date(funcdate))
  return(file)
}

#Without doubt there is a better way to do this, probably involving lists
archivepops <- bind_rows(callarchive("2021-07-26"), callarchive("2021-07-27"),
                         callarchive("2021-07-28"), callarchive("2021-07-29"),
                         callarchive("2021-07-30"), callarchive("2021-07-31"),
                         callarchive("2021-08-01"), callarchive("2021-08-02"),
                         callarchive("2021-08-03"), callarchive("2021-08-04"),
                         callarchive("2021-08-05"), callarchive("2021-08-06"),
                         callarchive("2021-08-07"), callarchive("2021-08-08"),
                         callarchive("2021-08-09"), callarchive("2021-08-10"),
                         callarchive("2021-08-11"), callarchive("2021-08-12"),
                         callarchive("2021-08-13"), callarchive("2021-08-14"),
                         callarchive("2021-08-15"), callarchive("2021-08-16"),
                         callarchive("2021-08-17"), callarchive("2021-08-18"),
                         callarchive("2021-08-19"), callarchive("2021-08-20"),
                         callarchive("2021-08-21"), callarchive("2021-08-22")) %>% 
  mutate(age=case_when(
    age=="16_17" ~ "16-17",
    age %in% c("18_24", "25_29") ~ "18-29",
    age %in% c("30_34", "35_39") ~ "30-39",
    age %in% c("40_44", "45_49") ~ "40-49",
    age %in% c("50_54", "55_59") ~ "50-59",
    age %in% c("60_64", "65_69") ~ "60-69",
    age %in% c("70_74", "75_79") ~ "70-79",
    age %in% c("80_84", "85_89", "90+") ~ "80+",
    TRUE ~ age)) %>% 
  group_by(age, date) %>% 
  summarise(VaccineRegisterPopulationByVaccinationDate=sum(VaccineRegisterPopulationByVaccinationDate)) %>% 
  ungroup() %>% 
  #lag second doses by 2 weeks
  group_by(age) %>% 
  arrange(date) %>% 
  mutate(dose2pop=lag(VaccineRegisterPopulationByVaccinationDate, 14)) %>% 
  #filter out weeks 32-35 to match PHE data
  filter(date>=as.Date("2021-08-09") & date<=as.Date("2021-09-05")) %>% 
  #calculate average populations across the study period
  summarise(singledoseNIMSpop=mean(VaccineRegisterPopulationByVaccinationDate),
            doubledoseNIMSpop=mean(dose2pop))

data <- merge(rawdata, vaxed1st) %>% 
  merge(vaxed2nd) %>% 
  merge(NIMSpop) %>% 
  merge(ONSpop) %>% 
  merge(dashboarddata, all=TRUE) %>% 
  merge(archivepops, all=TRUE) %>% 
  mutate(singledose=if_else(age=="Under 18", vax1pop, singledose),
         doubledose=if_else(age=="Under 18", vax2pop, doubledose),
         unvaxpop_ONS=pop_ONS-singledose, unvaxpop_NIMS=pop_NIMS-singledose,
         age=factor(age, levels=c("Under 18", "18-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")))

#Graphic of difference in estimates of unvaxed pop
agg_tiff("Outputs/EngPopUnvaxEstimates.tiff", units="in", width=9, height=6, res=800)
data %>% filter(metric=="cases") %>%
  mutate(popdiff_abs=unvaxpop_ONS-unvaxpop_NIMS,
         popdiff_rel=popdiff_abs/unvaxpop_NIMS, 
         labpos=if_else(popdiff_abs<0, 1.1, -0.1)) %>% 
  ggplot()+
  geom_col(aes(x=popdiff_abs, y=age), fill="aquamarine4")+
  geom_vline(aes(xintercept=0), colour="Grey60")+
  geom_text(aes(x=popdiff_abs, y=age, label=paste0(if_else(popdiff_abs>0, "+", ""),
                                                   round(popdiff_rel*100, 0), "%"),
            hjust=labpos), size=rel(3.6))+
  scale_x_continuous(limits=c(-1800000, 1800000), breaks=c(-1500000, -1000000, -500000, 0, 500000,
                                                           1000000, 1500000),
                     labels=c("-1.5m", "-1m", "-0.5m", "0", "+0.5m", "+1m", "+1.5m"),
                     name="Difference between using ONS and NIMS denominators")+
  scale_y_discrete(name="Age group")+
  annotate("text", x=-1000000, y=8, label="ONS estimates\nlower than NIMS", 
           colour="Grey60", size=rel(5), family="Lato")+  
  annotate("text", x=1000000, y=8, label="ONS estimates\nhigher than NIMS", 
           colour="Grey60", size=rel(5), family="Lato")+  
  theme_custom()+
  labs(title="Estimating the number of unvaccinated people is hard",
       subtitle="Difference between estimates of the number of people in England who have not yet received two COVID vaccine doses\nbased on NIMS and ONS population estimates. Bars represent the absolute differences, labels the relative difference.",
       caption="Data from NHS England | Plot by @VictimOfMaths")
dev.off()  
  
plotdata <- data %>% 
  rowwise() %>% 
  mutate(unvaxrate_NIMS=Unvaccinated*100000/unvaxpop_NIMS,
         unvaxrate_ONS=Unvaccinated*100000/unvaxpop_ONS,
         vaxrate=Fully_vaccinated*100000/doubledose, minunvaxrate=min(unvaxrate_NIMS, unvaxrate_ONS),
         maxunvaxrate=max(unvaxrate_NIMS, unvaxrate_ONS)) %>% 
  gather(measure, rate, c(16:20)) %>% 
  select(age, measure, rate, metric) 

plotdata2 <- data %>% 
  rowwise() %>% 
  mutate(unvaxrate_NIMS=Unvaccinated*100000/unvaxpop_NIMS,
         unvaxrate_ONS=Unvaccinated*100000/unvaxpop_ONS) %>% 
  gather(measure, rate, c("unvaxrate_NIMS", "unvaxrate_ONS"))

#Updated version of the plot in the PHE report (Figure 2)
#Cases from PHE report, fully vaccinated counts from dashboard (accounting for changes over time),
#ONS population figures lifted from NHS England spreadsheet,
#NIMS populations for u18s taken from NHS England spreadsheet,
#NIMS populations for adults taken from dashboard (accountinf for changes over time).

agg_tiff("Outputs/COVIDCaseRatesxAgexVax.tiff", units="in", width=9, height=6, res=800)
ggplot()+
  geom_col(data=plotdata %>% filter(metric=="cases" & measure %in% c("vaxrate", "maxunvaxrate")), 
           aes(x=rate, y=age, fill=measure), position="dodge")+
  geom_col(data=plotdata %>% filter(metric=="cases" & measure %in% c("vaxrate", "minunvaxrate")), 
           aes(x=rate, y=age, fill=measure), position="dodge")+
  geom_point(data=plotdata2 %>% filter(metric=="cases"),
             aes(x=rate, y=age, colour=measure), position=position_nudge(y=-0.225), size=5.8, shape="|")+
  scale_x_continuous(name="Confirmed cases per 100,000 people")+
  scale_y_discrete(name="Age group")+
  scale_fill_manual(values=c("Grey70", "Grey30", "Red"), breaks=c("vaxrate", "minunvaxrate"),
                    labels=c("Fully vaccinated", "Unvaccinated"), name="")+
  scale_colour_manual(values=c("Grey90", "Black"), labels=c("Based on NIMS", "Based on ONS"),
                      name="")+
  theme_custom()+
  theme(legend.position="top", plot.subtitle=element_markdown())+
  labs(title="Choice of population data has a huge impact on estimates of vaccine effectiveness",
       subtitle="Rates of confirmed COVID cases between 8th August and 5th September 2021 by age and vaccination status.<br> <span style='color:Grey70;'>Light grey</span> bars represent the difference in estimated case rates in unvaccinated groups between ONS and NIMS estimates.",
       caption="Data from PHE & NHS England | Plot by @VictimOfMaths")
dev.off()
