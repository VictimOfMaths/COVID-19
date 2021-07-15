rm(list=ls())

library(tidyverse)
library(curl)
library(paletteer)
library(scales)
library(extrafont)
library(ragg)
library(readxl)

options(scipen=10000)

theme_custom <- function() {
  theme_classic() %+replace%
    theme(plot.title.position="plot", plot.caption.position="plot",
          strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)),
          plot.title=element_text(face="bold", size=rel(1.5), hjust=0,
                                  margin=margin(0,0,5.5,0)),
          text=element_text(family="Lato"))
}

#Download vaccination data from the dashboard
url <- "https://api.coronavirus.data.gov.uk/v2/data?areaType=nation&areaCode=E92000001&metric=vaccinationsAgeDemographics&format=csv"
temp <- tempfile()
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

data <- read.csv(temp) %>% 
  select(date, age, cumVaccinationFirstDoseUptakeByVaccinationDatePercentage,
         cumVaccinationSecondDoseUptakeByVaccinationDatePercentage,
         VaccineRegisterPopulationByVaccinationDate,
         cumPeopleVaccinatedFirstDoseByVaccinationDate,
         cumPeopleVaccinatedSecondDoseByVaccinationDate) %>% 
  set_names(c("date", "age", "First dose_NIMS", "Second dose_NIMS", "pop_NIMS", 
              "Total1stDoses", "Total2ndDoses")) %>% 
  mutate(age=gsub("_", "-", age))

#Read in ONS population projections for 2021 to use as alternative denominator
url <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationprojections/datasets/tablea24principalprojectionenglandpopulationinagegroups/2018based/enpppsumpop18.xls"

temp <- tempfile()
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

ONSpop_proj <- read_excel(temp, sheet="PERSONS", range="A9:E29", col_names=FALSE) %>% 
  set_names("age", "2018", "2019", "2020", "2021") %>% 
  select(age, `2021`) %>% 
  filter(!age %in% c("0-4", "5-9", "10-14")) %>% 
  #Adjust 15-19 age band to 18-19, assuming equal distribution of population within the band
  mutate(`2021`=if_else(age=="15-19", `2021`*0.4, `2021`),
         #Align age bands with vaccination data
         age=case_when(
           age %in% c("90-94", "95-99", "100 & over") ~ "90+",
           age %in% c("15-19", "20-24") ~  "18-24",
           TRUE ~ age)) %>% 
  group_by(age) %>% 
  summarise(pop_ONSproj=sum(`2021`)*1000) %>% 
  ungroup()

#Add in ONS population estimates (mid-2020 figures)
url <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fpopulationandmigration%2fpopulationestimates%2fdatasets%2fpopulationestimatesforukenglandandwalesscotlandandnorthernireland%2fmid2020/ukpopestimatesmid2020on2021geography.xls"

temp <- tempfile()
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

ONSpop_2020 <- as.data.frame(t(read_excel(temp, sheet="MYE2 - Persons", range="E8:CQ12",
                                          col_names=FALSE))) %>% 
  select(V1, V5) %>% 
  set_names(c("age", "pop_ONS2020")) %>% 
  mutate(age=as.numeric(if_else(age=="90+", "90", age)),
         pop_ONS2020=as.numeric(pop_ONS2020),
         age=case_when(
           age<18 ~ "u18", age<25 ~ "18-24", age<30 ~ "25-29",
           age<35 ~ "30-34", age<40 ~ "35-39", age<45 ~ "40-44",
           age<50 ~ "45-49", age<55 ~ "50-54", age<60 ~ "55-59",
           age<65 ~ "60-64", age<70 ~ "65-69", age<75 ~ "70-74",
           age<80 ~ "75-79", age<85 ~ "80-84", age<90 ~ "85-89",
           TRUE ~ "90+")) %>% 
  group_by(age) %>%
  summarise(pop_ONS2020=sum(pop_ONS2020)) %>% 
  ungroup()

datafull <- data %>% 
  merge(ONSpop_proj) %>% 
  merge(ONSpop_2020) %>% 
  mutate(`First dose_ONSproj`=Total1stDoses*100/pop_ONSproj,
         `Second dose_ONSproj`=Total2ndDoses*100/pop_ONSproj,
         `First dose_ONS2020`=Total1stDoses*100/pop_ONS2020,
         `Second dose_ONS2020`=Total2ndDoses*100/pop_ONS2020)

#Calculate total
datafull <- datafull %>% 
  group_by(date) %>% 
  summarise(pop_NIMS=sum(pop_NIMS), Total1stDoses=sum(Total1stDoses),
            Total2ndDoses=sum(Total2ndDoses), pop_ONSproj=sum(pop_ONSproj),
            pop_ONS2020=sum(pop_ONS2020)) %>% 
  mutate(age="Total", `First dose_NIMS`=Total1stDoses*100/pop_NIMS,
         `Second dose_NIMS`=Total2ndDoses*100/pop_NIMS,
         `First dose_ONSproj`=Total1stDoses*100/pop_ONSproj,
         `Second dose_ONSproj`=Total2ndDoses*100/pop_ONSproj,
         `First dose_ONS2020`=Total1stDoses*100/pop_ONS2020,
         `Second dose_ONS2020`=Total2ndDoses*100/pop_ONS2020) %>% 
  bind_rows(datafull) %>% 
  #Tidy up
  pivot_longer(c(`First dose_NIMS`, `Second dose_NIMS`,
                 `First dose_ONSproj`, `Second dose_ONSproj`,
                 `First dose_ONS2020`, `Second dose_ONS2020`),
               names_to=c("dose", "source"), names_sep="_",
               values_to="prop") %>% 
  mutate(prop=prop/100, date=as.Date(date))

agg_tiff("Outputs/COVIDVaxUptakexAge.tiff", units="in", width=9, height=6, res=800)
ggplot()+
  geom_line(data=datafull %>% filter(age!="Total" & source=="NIMS"), 
            aes(x=date, y=prop, colour=age, group=age))+
  geom_line(data=datafull %>% filter(age=="Total" & source=="NIMS"),
            aes(x=date, y=prop), colour="Black", linetype=3)+
  geom_hline(yintercept=1, colour="Grey80")+
  scale_x_date(name="")+
  scale_y_continuous(name="Proportion of age group vaccinated", 
                     labels=label_percent(accuracy=1))+
  scale_colour_paletteer_d("pals::stepped", guide=guide_legend(reverse=TRUE),
                           name="Age")+
  facet_wrap(~dose)+
  theme_custom()+
  theme(panel.grid.major.y=element_line(colour="Grey90"))+
  labs(title="Vaccine uptake is lower in younger age groups",
       subtitle="Cumulative proportion of English population vaccinated against COVID with one or two doses by age group.\nThe dotted line represents the overall proportion of all adults (aged 18+) vaccinated",
       caption="Data from coronavirus.data.gov.uk | Population estimates from NIMS | Plot by @VictimOfMaths")

dev.off()

agg_tiff("Outputs/COVIDVaxUptakexAgeONSproj.tiff", units="in", width=9, height=6, res=800)
ggplot()+
  geom_line(data=datafull %>% filter(age!="Total" & source=="ONSproj"), 
            aes(x=date, y=prop, colour=age, group=age))+
  geom_line(data=datafull %>% filter(age=="Total" & source=="ONSproj"),
            aes(x=date, y=prop), colour="Black", linetype=3)+
  geom_hline(yintercept=1, colour="Grey80")+
  scale_x_date(name="")+
  scale_y_continuous(name="Proportion of age group vaccinated", 
                     labels=label_percent(accuracy=1))+
  scale_colour_paletteer_d("pals::stepped", guide=guide_legend(reverse=TRUE),
                           name="Age")+
  facet_wrap(~dose)+
  theme_custom()+
  theme(panel.grid.major.y=element_line(colour="Grey90"))+
  labs(title="Vaccine uptake is lower in younger age groups",
       subtitle="Cumulative proportion of English population vaccinated against COVID with one or two doses by age group.\nThe dotted line represents the overall proportion of all adults (aged 18+) vaccinated",
       caption="Data from coronavirus.data.gov.uk | Population data from ONS projections for 2021 | Plot by @VictimOfMaths")

dev.off()

agg_tiff("Outputs/COVIDVaxUptakexAgeONS2020.tiff", units="in", width=9, height=6, res=800)
ggplot()+
  geom_line(data=datafull %>% filter(age!="Total" & source=="ONS2020"), 
            aes(x=date, y=prop, colour=age, group=age))+
  geom_line(data=datafull %>% filter(age=="Total" & source=="ONS2020"),
            aes(x=date, y=prop), colour="Black", linetype=3)+
  geom_hline(yintercept=1, colour="Grey80")+
  scale_x_date(name="")+
  scale_y_continuous(name="Proportion of age group vaccinated", 
                     labels=label_percent(accuracy=1))+
  scale_colour_paletteer_d("pals::stepped", guide=guide_legend(reverse=TRUE),
                           name="Age")+
  facet_wrap(~dose)+
  theme_custom()+
  theme(panel.grid.major.y=element_line(colour="Grey90"))+
  labs(title="Vaccine uptake is lower in younger age groups",
       subtitle="Cumulative proportion of English population vaccinated against COVID with one or two doses by age group.\nThe dotted line represents the overall proportion of all adults (aged 18+) vaccinated",
       caption="Data from coronavirus.data.gov.uk | Population data from ONS 2020 estimates | Plot by @VictimOfMaths")

dev.off()

#Faceted plot showing all 3 estimates
agg_tiff("Outputs/COVIDVaxUptakexAgeComparisons.tiff", units="in", 
         width=8, height=8, res=800)
ggplot(datafull %>% filter(dose=="First dose"), 
       aes(x=date, y=prop, colour=source, group=source))+
  geom_line()+
  geom_hline(yintercept=1, colour="Grey80")+
  scale_x_date(name="")+
  scale_y_continuous(name="Proportion of age group vaccinated", 
                     labels=label_percent(accuracy=1))+
  scale_colour_paletteer_d("rcartocolor::Safe", name="Population\nsource:",
                           labels=c("NIMS", "ONS 2020\nestimates",
                                    "ONS 2021\nprojections"))+
  facet_wrap(~age)+
  theme_custom()+
  theme(legend.position="top")+
  labs(title="NIMS gives a different picture of vaccine uptake to ONS population estimates",
       subtitle="Cumulative proportion of English population vaccinated against COVID with at least one dose,\nusing alternative population estimates.",
       caption="Data from coronavirus.data.gov.uk, NIMS & ONS | Plot by @VictimOfMaths")
dev.off()

#Just the populations
popdata <- datafull %>% 
  filter(date==max(date) & dose=="First dose" & source=="NIMS") %>% 
  select(age, pop_NIMS, pop_ONSproj, pop_ONS2020) %>% 
  gather(source, pop, c(2:4))

agg_tiff("Outputs/EngPopEstimates.tiff", units="in", width=8, height=8, res=800)
ggplot(popdata %>% filter(age!="Total"), aes(x=pop/1000000, y=age, fill=source))+
  geom_col(position="dodge")+
  scale_x_continuous(name="Estimated population (millions)")+
  scale_y_discrete(name="Age")+
  scale_fill_paletteer_d("rcartocolor::Safe", name="Population\nsource:",
                         labels=c("NIMS", "ONS 2020\nestimates",
                                  "ONS 2021\nprojections"))+
  theme_custom()+
  theme(legend.position="top")+
  labs(title="Population estimates for younger age groups vary a lot between sources",
       subtitle="Estimated English populations by age group from the National Immunisation Management Service (NIMS),\nONS mid-year population estimates for 2019 and ONS population projections for 2021 based on 2018 data.",
       caption="data from NIMS & ONS | Plot by @VictimOfMaths")
dev.off()
