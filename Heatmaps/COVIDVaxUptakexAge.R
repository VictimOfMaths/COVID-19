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
url <- "https://api.coronavirus.data.gov.uk/v2/data?areaType=nation&metric=vaccinationsAgeDemographics&format=csv"
temp <- tempfile()
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

data <- read.csv(temp) %>% 
  select(date, age, areaName, cumVaccinationFirstDoseUptakeByVaccinationDatePercentage,
         cumVaccinationSecondDoseUptakeByVaccinationDatePercentage,
         VaccineRegisterPopulationByVaccinationDate,
         cumPeopleVaccinatedFirstDoseByVaccinationDate,
         cumPeopleVaccinatedSecondDoseByVaccinationDate) %>% 
  set_names(c("date", "age", "country", "First dose_NIMS", "Second dose_NIMS", "pop_NIMS", 
              "Total1stDoses", "Total2ndDoses")) %>% 
  mutate(age=gsub("_", "-", age), date=as.Date(date))

#Add in ONS population estimates (mid-2020 figures)
url <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fpopulationandmigration%2fpopulationestimates%2fdatasets%2fpopulationestimatesforukenglandandwalesscotlandandnorthernireland%2fmid2020/ukpopestimatesmid2020on2021geography.xls"

temp <- tempfile()
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

ONSpop_2020 <- as.data.frame(t(read_excel(temp, sheet="MYE2 - Persons", range="E8:CQ384",
                                          col_names=FALSE))) %>% 
  select(V1, V5, V377) %>% 
  set_names(c("age", "England", "Scotland")) %>% 
  gather(country, pop_ONS2020, c(2,3)) %>% 
  mutate(age=as.numeric(if_else(age=="90+", "90", age)),
         pop_ONS2020=as.numeric(pop_ONS2020),
         age=case_when(
           age<12 ~ "u12", age<16 ~ "12-15", age<18 ~ "16-17", 
           age<25 & country=="England" ~ "18-24", age<30 & country=="England" ~ "25-29",
           age<30 ~ "18-29",
           age<35 & country=="England" ~ "30-34", age<40 & country=="England" ~ "35-39", 
           age<40 ~ "30-39",
           age<45 & country=="England" ~ "40-44", age<50 & country=="England" ~ "45-49", 
           age<50 ~ "40-49", age<55 ~ "50-54", age<60 ~ "55-59",
           age<65 ~ "60-64", age<70 ~ "65-69", age<75 ~ "70-74",
           age<80 ~ "75-79", age<85 & country=="England" ~ "80-84", age<90 & country=="England" ~ "85-89",
           country=="England" ~ "90+",
           TRUE ~ "80+")) %>% 
  group_by(age, country) %>%
  summarise(pop_ONS2020=sum(pop_ONS2020)) %>% 
  ungroup()

#Merge populations into data
datafull <- data %>% 
  merge(ONSpop_2020) %>% 
  mutate(`First dose_ONS2020`=Total1stDoses*100/pop_ONS2020,
         `Second dose_ONS2020`=Total2ndDoses*100/pop_ONS2020)

#Separate out dataset for England only and for comparison with Scotland 
#(with broader Scottish age bands)

datafull_e <- datafull %>% 
  filter(country=="England") %>%  
  group_by(date) %>% 
  summarise(pop_NIMS=sum(pop_NIMS), Total1stDoses=sum(Total1stDoses),
            Total2ndDoses=sum(Total2ndDoses),
            pop_ONS2020=sum(pop_ONS2020)) %>% 
  mutate(age="Total", `First dose_NIMS`=Total1stDoses*100/pop_NIMS,
         `Second dose_NIMS`=Total2ndDoses*100/pop_NIMS,
         `First dose_ONS2020`=Total1stDoses*100/pop_ONS2020,
         `Second dose_ONS2020`=Total2ndDoses*100/pop_ONS2020) %>% 
  bind_rows(datafull %>% 
              filter(country=="England")) %>% 
  #Tidy up
  pivot_longer(c(`First dose_NIMS`, `Second dose_NIMS`,
                 `First dose_ONS2020`, `Second dose_ONS2020`),
               names_to=c("dose", "source"), names_sep="_",
               values_to="prop") %>% 
  mutate(prop=prop/100, date=as.Date(date)) %>% 
  ungroup()

datafull_se <- datafull %>% 
  mutate(age=case_when(
    age %in% c("18-24", "25-29") ~ "18-29",
    age %in% c("30-34", "35-39") ~ "30-39",
    age %in% c("40-44", "45-49") ~ "40-49",
    age %in% c("80-84", "85-89", "90+") ~ "80+",
    TRUE ~ age)) %>% 
  group_by(date, country, age) %>% 
  summarise(Total1stDoses=sum(Total1stDoses),
            Total2ndDoses=sum(Total2ndDoses),
            pop_ONS2020=sum(pop_ONS2020)) %>% 
  ungroup() %>% 
  mutate(`First dose`=Total1stDoses*100/pop_ONS2020,
         `Second dose`=Total2ndDoses*100/pop_ONS2020) %>% 
  gather(dose, prop, c(`First dose`, `Second dose`))

agg_tiff("Outputs/COVIDVaxUptakexAge.tiff", units="in", width=9, height=6, res=800)
ggplot()+
  geom_line(data=datafull_e %>% filter(age!="Total" & source=="NIMS"), 
            aes(x=date, y=prop, colour=age, group=age))+
  geom_line(data=datafull_e %>% filter(age=="Total" & source=="NIMS"),
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
       subtitle="Cumulative proportion of English population vaccinated against COVID with one or two doses by age group.\nThe dotted line represents the overall proportion of the population aged 12+ who have been vaccinated",
       caption="Data from coronavirus.data.gov.uk | Population estimates from NIMS | Plot by @VictimOfMaths")

dev.off()

agg_tiff("Outputs/COVIDVaxUptakexAgeONS2020.tiff", units="in", width=9, height=6, res=800)
ggplot()+
  geom_line(data=datafull_e %>% filter(age!="Total" & source=="ONS2020"), 
            aes(x=date, y=prop, colour=age, group=age))+
  geom_line(data=datafull_e %>% filter(age=="Total" & source=="ONS2020"),
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
       subtitle="Cumulative proportion of English population vaccinated against COVID with one or two doses by age group.\nThe dotted line represents the overall proportion of the population aged 12+ who have been vaccinated",
       caption="Data from coronavirus.data.gov.uk | Population data from ONS 2020 estimates | Plot by @VictimOfMaths")

dev.off()

#Faceted plot showing both estimates
agg_tiff("Outputs/COVIDVaxUptakexAgeComparisons.tiff", units="in", 
         width=8, height=8, res=800)
ggplot(datafull_e %>% filter(dose=="First dose"), 
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
popdata <- datafull_e %>% 
  filter(date==max(date) & dose=="First dose" & source=="NIMS") %>% 
  select(age, pop_NIMS, pop_ONS2020) %>% 
  gather(source, pop, c(2:3))

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

#Comparing England & Scotland
agg_tiff("Outputs/COVIDVaxEngvsScotxAge.tiff", units="in", width=9, height=7, res=800)
ggplot(datafull_se %>% mutate(dosecountry=paste(dose, country)), 
       aes(x=date, y=prop/100, colour=country, linetype=dose, group=dosecountry))+
  geom_line()+
  scale_x_date(name="")+
  scale_y_continuous(name="Proportion of the population vaccinated", 
                     labels=label_percent(accuracy=1))+
  scale_colour_manual(values=c("#F44B4B", "#0076BB"), name="Country")+
  scale_linetype_discrete(name="Dose")+
  facet_wrap(~age)+
  theme_custom()+
  labs(title="Scotland is vaccinating under 18s faster than England",
       subtitle="Vaccination coverage in England and Scotland by age",
       caption="Vaccination data from coronavirus.gov.uk | Population estimates from ONS | Plot by @VictimOfMaths")

dev.off()

agg_tiff("Outputs/COVIDVaxEngvsScotxAgeu18.tiff", units="in", width=8, height=6, res=800)
ggplot(datafull_se %>% filter(age %in% c("12-15", "16-17")) %>% 
         mutate(dosecountry=paste(dose, country)), 
       aes(x=date, y=prop/100, colour=country, linetype=dose, group=dosecountry))+
  geom_line()+
  scale_x_date(name="")+
  scale_y_continuous(name="Proportion of the population vaccinated", 
                     labels=label_percent(accuracy=1))+
  scale_colour_manual(values=c("#F44B4B", "#0076BB"), name="Country")+
  scale_linetype_discrete(name="Dose")+
  facet_wrap(~age)+
  theme_custom()+
  labs(title="Scotland is vaccinating under 18s faster than England",
       subtitle="Vaccination coverage in England and Scotland by age",
       caption="Vaccination data from coronavirus.gov.uk | Population estimates from ONS | Plot by @VictimOfMaths")

dev.off()

#########################
url <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/10/COVID-19-daily-announced-vaccinations-29-October-2021.xlsx"
temp <- tempfile()
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

Boosters <- read_excel(temp, sheet="Total Vaccinations by Age", range="B15:F29") %>% 
  select(-2) %>% 
  set_names("age", "1 doses", "2 doses", "3 dose") %>% 
  mutate(`1 dose`=`1 doses`-`2 doses`,
         `2 dose`=`2 doses`-`3 dose`) %>% 
  merge(popdata %>% filter(source=="pop_ONS2020") %>% 
          mutate(age=case_when(
    age %in% c("80-84", "85-89", "90+") ~ "80+",
    age %in% c("12-15", "17-18") ~ "Under 18",
    age=="Total" ~ "Total",
    TRUE ~ age)) %>% 
      group_by(age) %>% 
      summarise(pop=sum(pop))) %>% 
  mutate(`Unvaccinated`=pop-`1 doses`,
         age=if_else(age=="Under 18", "12-17", age)) %>% 
  gather(Doses, Vaccinated, c(Unvaccinated, `1 dose`, `2 dose`, `3 dose`)) %>% 
  mutate(Vaccinated=if_else(Vaccinated<0, 0, Vaccinated),
         Doses=factor(Doses, levels=c("Unvaccinated", "1 dose", "2 dose", "3 dose")))

total <- Boosters %>% group_by(Doses) %>% 
  summarise(Vaccinated=sum(Vaccinated))

agg_tiff("Outputs/COVIDVaxBooster Pyramix.tiff", units="in", width=9, height=8, res=500)
ggplot(Boosters, aes(x=Vaccinated/1000000, y=age, fill=Doses))+
  geom_col()+
  scale_x_continuous(name="Number of people (millions)")+
  scale_y_discrete(name="Age")+
  scale_fill_manual(values=c("#01000E", "#FBA724", "#EE7E24", "#D42D24"), name="")+
  theme_custom()+
  labs(title="13% of people in England aged 12+ have now received a 3rd COVID jab",
       subtitle="Number of vaccine doses delivered by age group in England. 3 doses includes both boosters and 3rd primary doses",
       caption="Vaccination data from NHS England | Population data from ONS | plot by @VictimOfMaths\nNote that for ages 75+, ONS population estimates suggest >100% first dose coverage")
dev.off()
