rm(list=ls())

library(tidyverse)
library(curl)
library(RcppRoll)
library(readxl)
library(paletteer)
library(extrafont)
library(ragg)
library(scales)
library(ggtext)
library(gtools)

options(scipen=999999)

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
  mutate(date=as.Date(date)) 

#Bring in populations 
url <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fpopulationandmigration%2fpopulationestimates%2fdatasets%2fpopulationestimatesforukenglandandwalesscotlandandnorthernireland%2fmid2020/ukpopestimatesmid2020on2021geography.xls"

temp <- tempfile()
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

ONSpop <- read_excel(temp, sheet="MYE2 - Persons", range="E8:CQ12") %>% 
  slice_tail() %>% 
  gather(age, pop, c(1:ncol(.))) %>% 
  mutate(age=as.numeric(substr(age, 1, 2)),
         age=case_when(
           age<12 ~ "00_11",
           age<16 ~ "12_15",
           age<18 ~ "16_17",
           age<25 ~ "18_24",
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

data <- merge(vaxdata, ONSpop, all=TRUE) %>% 
  mutate(dose1=if_else(is.na(dose1), 0, dose1),
         dose2=if_else(is.na(dose2), 0, dose2),
         dose3=if_else(is.na(dose3), 0, dose3),
         date=if_else(is.na(date), max(date, na.rm=TRUE), date)) %>% 
  group_by(age) %>% 
  summarise(dose1=sum(dose1), dose2=sum(dose2), dose3=sum(dose3), pop=unique(pop)) %>% 
  ungroup() %>% 
  mutate(dose2only=dose2-dose3,
         dose1only=dose1-dose2,
         unvaccinated=pop-dose1,
         minage=as.numeric(substr(age, 1, 2)),
         maxage=as.numeric(substr(age, 4, 5))+1,
         maxage=if_else(is.na(maxage), 100, maxage),
         age=gsub("_", "-", age),
         unvaccinated=if_else(unvaccinated<0, 0, unvaccinated)) 

agg_png("Outputs/COVIDVaxPyramidWithBoostersEngProp.png", units="in", res=800, width=9, height=7)
data %>% 
  gather(dose, no, c(dose1only, dose2only, dose3, unvaccinated)) %>% 
  mutate(dose=factor(dose, levels=c("dose3", "dose2only","dose1only","unvaccinated"))) %>% 
ggplot(aes(x=no, y=age, fill=dose))+
  geom_col(position="fill")+
  scale_x_continuous(name="Proportion of the population", labels=label_percent(accuracy=1))+
  scale_y_discrete(name="")+
  scale_fill_manual(values=c("#2BAA92FF", "#2F191BFF","#D32934FF","Grey70"), name="",
                    guide=guide_legend(reverse=TRUE), 
                    labels=c("3 doses/booster", "2 doses", "1 dose", "Unvaccinated"))+
  theme_custom()+
  labs(title="Many adults in England still haven't received a COVID booster jab",
       subtitle=paste0("Proportion of the population by vaccination status in England as of ", max(vaxdata$date)),
       caption="Data from coronavirus.data.gov.uk | Population data from ONS | Plot by Colin Angus")

dev.off()

agg_png("Outputs/COVIDVaxPyramidWithBoostersEng.png", units="in", res=800, width=10, height=8)
data %>% mutate(agewidth=maxage-minage,
                unvaccinated=unvaccinated/agewidth,
                dose1only=dose1only/agewidth,
                dose2only=dose2only/agewidth,
                dose3=dose3/agewidth) %>% 
ggplot()+
  geom_rect(aes(xmin=0, xmax=unvaccinated, ymin=minage, ymax=maxage), fill="Grey70")+
  geom_rect(aes(xmin=unvaccinated, xmax=unvaccinated+dose1only, ymin=minage, ymax=maxage), 
            fill="#D32934FF")+
  geom_rect(aes(xmin=unvaccinated+dose1only, xmax=unvaccinated+dose1only+dose2only, 
                ymin=minage, ymax=maxage), 
            fill="#2F191BFF")+
  geom_rect(aes(xmin=unvaccinated+dose1only+dose2only, 
                xmax=unvaccinated+dose1only+dose2only+dose3, 
                ymin=minage, ymax=maxage), 
            fill="#2BAA92FF")+
  scale_x_continuous(name="People at each single year of age")+
  scale_y_continuous(name="Age", breaks=c(0,20,40,60,80,100))+
  theme_custom()+
  theme(plot.subtitle=element_markdown(colour="Grey30"))+
  labs(title="Many adults in England still haven't received a COVID booster jab",
       subtitle="Number of people who have received <span style='color:#D32934FF;'>one</span>, <span style='color:#2F191BFF;'>two</span> or <span style='color:#2BAA92FF;'>three</span> vaccine doses or <span style='color:Grey70;'>remain unvaccinated",
       caption="Data from coronavirus.data.gov.uk | Population data from ONS | Plot by Colin Angus")

dev.off()

###################################################
#Deprivation analysis of vaccination rates
#Start by calculating IMD at MSOA level
#Download IMD data
temp <- tempfile()
source <- "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/833970/File_1_-_IMD2019_Index_of_Multiple_Deprivation.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

IMD <- read_excel(temp, sheet="IMD2019", range="A2:F32845", col_names=FALSE) %>% 
  select(c(1,2,5,6)) %>% 
  set_names("LSOA11CD", "LSOA11NM", "IMDrank", "IMDdecile")

#Download LSOA to MSOA lookup
source <- ("https://opendata.arcgis.com/datasets/fe6c55f0924b4734adf1cf7104a0173e_0.csv")
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

lookup <- read.csv(temp) %>% 
  select(LSOA11CD, MSOA11CD, LAD17CD) %>% 
  unique()

#Merge into IMD data
IMD <- merge(IMD, lookup, by="LSOA11CD")

#Bring in population data for LSOAs
source <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fpopulationandmigration%2fpopulationestimates%2fdatasets%2flowersuperoutputareamidyearpopulationestimates%2fmid2020sape23dt2/sape23dt2mid2020lsoasyoaestimatesunformatted.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

pop <- read_excel(temp, sheet="Mid-2020 Persons", range="A6:G34758", col_names=FALSE) %>% 
  select(-c(2:6)) %>% 
  set_names("LSOA11CD", "pop")

pop_full <- read_excel(temp, sheet="Mid-2020 Persons", range="A6:CT34758", col_names=FALSE) %>% 
  select(-c(2:7)) %>% 
  set_names("LSOA11CD", c(0:90))

#Merge into IMD data
IMD <- merge(IMD, pop)

#Calculate IMD rank at LTLA level as weighted average of LSOA level ranks, weight by population
IMD_MSOA <- IMD %>% 
  group_by(MSOA11CD, LAD17CD) %>% 
  summarise(IMDrank=weighted.mean(IMDrank, pop), pop=sum(pop)) %>% 
  ungroup() %>% 
  mutate(decile=quantcut(IMDrank, q=10, labels=FALSE))

IMD_LTLA <- IMD %>% 
  group_by(LAD17CD)%>% 
  summarise(IMDrank=weighted.mean(IMDrank, pop), pop=sum(pop)) %>% 
  ungroup() %>% 
  mutate(decile=quantcut(IMDrank, q=10, labels=FALSE))

#######################
url <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2022/01/COVID-19-weekly-announced-vaccinations-13-January-2022.xlsx"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

MSOAvax <- read_excel(temp, sheet="MSOA", range="F15:AZ6803", col_names=FALSE) %>% 
  select(c(1, 34:47)) %>% 
  set_names("MSOA11CD", "Under18", "18-24", "25-29", "30-34", "35-39", "40-44", "45-49", 
            "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80+") %>% 
  merge(IMD_MSOA) %>% 
  select(-pop) %>% 
  mutate(Under18=as.numeric(if_else(Under18=="*", "0", Under18))) %>% 
  gather(age, boosted, c(2:15)) %>% 
  merge(pop_full %>% 
          gather(age, pop, c(2:92)) %>% 
          mutate(age=case_when(
            age<18 ~ "Under18", age<25 ~ "18-24", age<30 ~ "25-29", age<35 ~ "30-34",
            age<40 ~ "35-39", age<45 ~ "40-44", age<50 ~ "45-49",
            age<55 ~ "50-54", age<60 ~ "55-59", age<65 ~ "60-64",
            age<70 ~ "65-69", age<75 ~ "70-74", age<80 ~ "75-79", TRUE ~ "80+")) %>% 
          merge(lookup) %>% 
          group_by(MSOA11CD, age) %>% 
          summarise(pop=sum(pop)) %>% 
          ungroup()) %>% 
  group_by(age, decile) %>% 
  summarise(boosted=sum(boosted), pop=sum(pop)) %>% 
  ungroup() %>% 
  mutate(boostpop=boosted/pop,
         age=factor(age, levels=c("Under18", "18-24", "25-29", "30-34", "35-39", "40-44", "45-49", 
                                  "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80+")))

agg_png("Outputs/COVIDBoostersxIMDxAgev2.png", units="in", res=800, width=10, height=8)
  ggplot(MSOAvax, aes(x=boosted/pop, y=as.factor(decile), fill=as.factor(decile)))+
  geom_col(show.legend=FALSE)+
  scale_x_continuous(name="Proportion of population who have received a booster/3rd dose", 
                     labels=label_percent(accuracy=1))+
  scale_y_discrete(name="Deprivation decile", labels=c("1 - Most deprived","2","3","4","5","6","7","8","9",
                                                       "10 - least deprived"))+
  scale_fill_paletteer_d("dichromat::BrowntoBlue_10")+
  facet_wrap(~age)+
  theme_custom()+
  labs(title="Younger populations cannot explain lower boster coverage in deprived areas",
       subtitle="Proportion of the population who have received a COVID booster or 3rd vaccination by deciles of\nthe Index of Multiple Deprivation and age in England",
       caption="Data from coronavirus.data.gov.uk & ONS | Plot by Colin Angus")
dev.off()

agg_png("Outputs/COVIDBoostersxIMDv2.png", units="in", res=800, width=10, height=8)
MSOAvax %>% 
  group_by(decile) %>% 
  summarise(boosted=sum(boosted), pop=sum(pop)) %>% 
  ungroup() %>% 
  mutate(boostprop=boosted/pop) %>% 
  ggplot(aes(x=boostprop, y=as.factor(decile), fill=as.factor(decile)))+
  geom_col(show.legend=FALSE)+
  scale_x_continuous(name="Proportion of population who have received a booster/3rd dose", 
                     labels=label_percent(accuracy=1))+
  scale_y_discrete(name="Deprivation decile", labels=c("1 - Most deprived","2","3","4","5","6","7","8","9",
                                                       "10 - least deprived"))+
  scale_fill_paletteer_d("dichromat::BrowntoBlue_10")+
  theme_custom()+
  theme(plot.title=element_text(size=rel(2)))+
  labs(title="More deprived areas have received fewer boosters",
       subtitle="Proportion of the population who have received a COVID booster or 3rd vaccination by deciles of the Index of Multiple Deprivation and age in England",
       caption="Data from coronavirus.data.gov.uk & ONS | Plot by Colin Angus")

dev.off()
