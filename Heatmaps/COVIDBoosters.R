rm(list=ls())

library(tidyverse)
library(curl)
library(paletteer)
library(scales)
library(extrafont)
library(ragg)
library(lubridate)
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

#Download vaccination data from NHS England website
#Daily data (includes boosters)
dailyurl <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/10/COVID-19-daily-announced-vaccinations-20-October-2021.xlsx"

temp <- tempfile()
temp <- curl_download(url=dailyurl, destfile=temp, quiet=FALSE, mode="wb")

dailydata.region <- read_excel(temp, sheet=1, range="B14:F22", col_names=FALSE) %>% 
  select(1,5) %>% 
  filter(complete.cases(.)) %>% 
  set_names(c("Region", "Booster")) %>% 
  mutate(date=as.Date("2021-10-19"), Region=if_else(Region=="England4", "England", Region))

dailydata.age <- read_excel(temp, sheet=2, range="B15:F28", col_names=FALSE) %>% 
  select(1,5) %>% 
  set_names(c("Age", "Booster"))%>% 
  mutate(date=as.Date("2021-10-19"))

daily19th <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/10/COVID-19-daily-announced-vaccinations-19-October-2021.xlsx"

temp <- curl_download(url=daily19th, destfile=temp, quiet=FALSE, mode="wb")

dailydata.region <- dailydata.region %>% 
  bind_rows(read_excel(temp, sheet=1, range="B14:F22", col_names=FALSE) %>% 
              select(1,5) %>% 
              filter(complete.cases(.)) %>% 
              set_names(c("Region", "Booster")) %>% 
              mutate(date=as.Date("2021-10-18"), Region=if_else(Region=="England4", "England", Region)))

dailydata.age <- dailydata.age %>% 
  bind_rows(read_excel(temp, sheet=2, range="B16:F29", col_names=FALSE) %>% 
              select(1,5) %>% 
              set_names(c("Age", "Booster"))%>% 
              mutate(date=as.Date("2021-10-18")))

daily18th <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/10/COVID-19-daily-announced-vaccinations-18-October-2021.xlsx"

temp <- curl_download(url=daily18th, destfile=temp, quiet=FALSE, mode="wb")

dailydata.region <- dailydata.region %>% 
  bind_rows(read_excel(temp, sheet=1, range="B14:F22", col_names=FALSE) %>% 
              select(1,5) %>% 
              filter(complete.cases(.)) %>% 
              set_names(c("Region", "Booster")) %>% 
              mutate(date=as.Date("2021-10-17"), Region=if_else(Region=="England4", "England", Region)))

dailydata.age <- dailydata.age %>% 
  bind_rows(read_excel(temp, sheet=2, range="B16:F29", col_names=FALSE) %>% 
              select(1,5) %>% 
              set_names(c("Age", "Booster"))%>% 
              mutate(date=as.Date("2021-10-17")))

daily17th <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/10/COVID-19-daily-announced-vaccinations-17-October-2021.xlsx"

temp <- curl_download(url=daily17th, destfile=temp, quiet=FALSE, mode="wb")

dailydata.region <- dailydata.region %>% 
  bind_rows(read_excel(temp, sheet=1, range="B14:F22", col_names=FALSE) %>% 
  select(1,5) %>% 
  filter(complete.cases(.)) %>% 
  set_names(c("Region", "Booster")) %>% 
  mutate(date=as.Date("2021-10-16"), Region=if_else(Region=="England4", "England", Region)))

dailydata.age <- dailydata.age %>% 
  bind_rows(read_excel(temp, sheet=2, range="B16:F29", col_names=FALSE) %>% 
  select(1,5) %>% 
  set_names(c("Age", "Booster"))%>% 
  mutate(date=as.Date("2021-10-16")))

daily16th <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/10/COVID-19-daily-announced-vaccinations-16-October-2021.xlsx"

temp <- curl_download(url=daily16th, destfile=temp, quiet=FALSE, mode="wb")

dailydata.region <- dailydata.region %>% 
  bind_rows(read_excel(temp, sheet=1, range="B14:F22", col_names=FALSE) %>% 
              select(1,5) %>% 
              filter(complete.cases(.)) %>% 
              set_names(c("Region", "Booster")) %>% 
              mutate(date=as.Date("2021-10-15"), Region=if_else(Region=="England4", "England", Region)))

dailydata.age <- dailydata.age %>% 
  bind_rows(read_excel(temp, sheet=2, range="B15:F28", col_names=FALSE) %>% 
              select(1,5) %>% 
              set_names(c("Age", "Booster"))%>% 
              mutate(date=as.Date("2021-10-15")))

daily15th <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/10/COVID-19-daily-announced-vaccinations-15-October-2021.xlsx"

temp <- curl_download(url=daily15th, destfile=temp, quiet=FALSE, mode="wb")

dailydata.region <- dailydata.region %>% 
  bind_rows(read_excel(temp, sheet=1, range="B14:F22", col_names=FALSE) %>% 
              select(1,5) %>% 
              filter(complete.cases(.)) %>% 
              set_names(c("Region", "Booster")) %>% 
              mutate(date=as.Date("2021-10-14"), Region=if_else(Region=="England4", "England", Region)))

dailydata.age <- dailydata.age %>% 
  bind_rows(read_excel(temp, sheet=2, range="B15:F28", col_names=FALSE) %>% 
              select(1,5) %>% 
              set_names(c("Age", "Booster"))%>% 
              mutate(date=as.Date("2021-10-14")))

daily14th <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/10/COVID-19-daily-announced-vaccinations-14-October-2021.xlsx"

temp <- curl_download(url=daily14th, destfile=temp, quiet=FALSE, mode="wb")

dailydata.region <- dailydata.region %>% 
  bind_rows(read_excel(temp, sheet=1, range="B14:F22", col_names=FALSE) %>% 
              select(1,5) %>% 
              filter(complete.cases(.)) %>% 
              set_names(c("Region", "Booster")) %>% 
              mutate(date=as.Date("2021-10-13"), Region=if_else(Region=="England4", "England", Region)))

dailydata.age <- dailydata.age %>% 
  bind_rows(read_excel(temp, sheet=2, range="B16:F29", col_names=FALSE) %>% 
              select(1,5) %>% 
              set_names(c("Age", "Booster"))%>% 
              mutate(date=as.Date("2021-10-13")))

daily13th <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/10/COVID-19-daily-announced-vaccinations-13-October-2021.xlsx"

temp <- curl_download(url=daily13th, destfile=temp, quiet=FALSE, mode="wb")

dailydata.region <- dailydata.region %>% 
  bind_rows(read_excel(temp, sheet=1, range="B14:F22", col_names=FALSE) %>% 
              select(1,5) %>% 
              filter(complete.cases(.)) %>% 
              set_names(c("Region", "Booster")) %>% 
              mutate(date=as.Date("2021-10-12"), Region=if_else(Region=="England4", "England", Region)))

dailydata.age <- dailydata.age %>% 
  bind_rows(read_excel(temp, sheet=2, range="B16:F29", col_names=FALSE) %>% 
              select(1,5) %>% 
              set_names(c("Age", "Booster"))%>% 
              mutate(date=as.Date("2021-10-12")))

daily12th <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/10/COVID-19-daily-announced-vaccinations-12-October-2021.xlsx"

temp <- curl_download(url=daily12th, destfile=temp, quiet=FALSE, mode="wb")

dailydata.region <- dailydata.region %>% 
  bind_rows(read_excel(temp, sheet=1, range="B14:F22", col_names=FALSE) %>% 
              select(1,5) %>% 
              filter(complete.cases(.)) %>% 
              set_names(c("Region", "Booster")) %>% 
              mutate(date=as.Date("2021-10-11"), Region=if_else(Region=="England4", "England", Region)))

dailydata.age <- dailydata.age %>% 
  bind_rows(read_excel(temp, sheet=2, range="B15:F28", col_names=FALSE) %>% 
              select(1,5) %>% 
              set_names(c("Age", "Booster"))%>% 
              mutate(date=as.Date("2021-10-11")))

daily11th <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/10/COVID-19-daily-announced-vaccinations-11-October-2021.xlsx"

temp <- curl_download(url=daily11th, destfile=temp, quiet=FALSE, mode="wb")

dailydata.region <- dailydata.region %>% 
  bind_rows(read_excel(temp, sheet=1, range="B14:F22", col_names=FALSE) %>% 
              select(1,5) %>% 
              filter(complete.cases(.)) %>% 
              set_names(c("Region", "Booster")) %>% 
              mutate(date=as.Date("2021-10-10"), Region=if_else(Region=="England4", "England", Region)))

dailydata.age <- dailydata.age %>% 
  bind_rows(read_excel(temp, sheet=2, range="B16:F29", col_names=FALSE) %>% 
              select(1,5) %>% 
              set_names(c("Age", "Booster"))%>% 
              mutate(date=as.Date("2021-10-10")))

daily10th <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/10/COVID-19-daily-announced-vaccinations-10-October-2021.xlsx"

temp <- curl_download(url=daily10th, destfile=temp, quiet=FALSE, mode="wb")

dailydata.region <- dailydata.region %>% 
  bind_rows(read_excel(temp, sheet=1, range="B14:F22", col_names=FALSE) %>% 
              select(1,5) %>% 
              filter(complete.cases(.)) %>% 
              set_names(c("Region", "Booster")) %>% 
              mutate(date=as.Date("2021-10-09"), Region=if_else(Region=="England4", "England", Region)))

dailydata.age <- dailydata.age %>% 
  bind_rows(read_excel(temp, sheet=2, range="B15:F28", col_names=FALSE) %>% 
              select(1,5) %>% 
              set_names(c("Age", "Booster"))%>% 
              mutate(date=as.Date("2021-10-09")))

daily9th <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/10/COVID-19-daily-announced-vaccinations-09-October-2021.xlsx"

temp <- curl_download(url=daily9th, destfile=temp, quiet=FALSE, mode="wb")

dailydata.region <- dailydata.region %>% 
  bind_rows(read_excel(temp, sheet=1, range="B14:F22", col_names=FALSE) %>% 
              select(1,5) %>% 
              filter(complete.cases(.)) %>% 
              set_names(c("Region", "Booster")) %>% 
              mutate(date=as.Date("2021-10-08"), Region=if_else(Region=="England4", "England", Region)))

dailydata.age <- dailydata.age %>% 
  bind_rows(read_excel(temp, sheet=2, range="B16:F29", col_names=FALSE) %>% 
              select(1,5) %>% 
              set_names(c("Age", "Booster"))%>% 
              mutate(date=as.Date("2021-10-08")))

daily8th <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/10/COVID-19-daily-announced-vaccinations-08-October-2021.xlsx"

temp <- curl_download(url=daily8th, destfile=temp, quiet=FALSE, mode="wb")

dailydata.region <- dailydata.region %>% 
  bind_rows(read_excel(temp, sheet=1, range="B14:F22", col_names=FALSE) %>% 
              select(1,5) %>% 
              filter(complete.cases(.)) %>% 
              set_names(c("Region", "Booster")) %>% 
              mutate(date=as.Date("2021-10-07"), Region=if_else(Region=="England4", "England", Region)))

dailydata.age <- dailydata.age %>% 
  bind_rows(read_excel(temp, sheet=2, range="B15:F28", col_names=FALSE) %>% 
              select(1,5) %>% 
              set_names(c("Age", "Booster"))%>% 
              mutate(date=as.Date("2021-10-07")))

daily7th <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/10/COVID-19-daily-announced-vaccinations-07-October-2021.xlsx"

temp <- curl_download(url=daily7th, destfile=temp, quiet=FALSE, mode="wb")

dailydata.region <- dailydata.region %>% 
  bind_rows(read_excel(temp, sheet=1, range="B14:F22", col_names=FALSE) %>% 
              select(1,5) %>% 
              filter(complete.cases(.)) %>% 
              set_names(c("Region", "Booster")) %>% 
              mutate(date=as.Date("2021-10-06"), Region=if_else(Region=="England4", "England", Region)))

dailydata.age <- dailydata.age %>% 
  bind_rows(read_excel(temp, sheet=2, range="B16:F29", col_names=FALSE) %>% 
              select(1,5) %>% 
              set_names(c("Age", "Booster"))%>% 
              mutate(date=as.Date("2021-10-06")))

daily6th <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/10/COVID-19-daily-announced-vaccinations-06-October-2021.xlsx"

temp <- curl_download(url=daily6th, destfile=temp, quiet=FALSE, mode="wb")

dailydata.region <- dailydata.region %>% 
  bind_rows(read_excel(temp, sheet=1, range="B14:F22", col_names=FALSE) %>% 
              select(1,5) %>% 
              filter(complete.cases(.)) %>% 
              set_names(c("Region", "Booster")) %>% 
              mutate(date=as.Date("2021-10-05"), Region=if_else(Region=="England4", "England", Region)))

dailydata.age <- dailydata.age %>% 
  bind_rows(read_excel(temp, sheet=2, range="B16:F29", col_names=FALSE) %>% 
              select(1,5) %>% 
              set_names(c("Age", "Booster"))%>% 
              mutate(date=as.Date("2021-10-05")))

daily5th <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/10/COVID-19-daily-announced-vaccinations-05-October-2021.xlsx"

temp <- curl_download(url=daily5th, destfile=temp, quiet=FALSE, mode="wb")

dailydata.region <- dailydata.region %>% 
  bind_rows(read_excel(temp, sheet=1, range="B14:F22", col_names=FALSE) %>% 
              select(1,5) %>% 
              filter(complete.cases(.)) %>% 
              set_names(c("Region", "Booster")) %>% 
              mutate(date=as.Date("2021-10-04"), Region=if_else(Region=="England4", "England", Region)))

dailydata.age <- dailydata.age %>% 
  bind_rows(read_excel(temp, sheet=2, range="B16:F29", col_names=FALSE) %>% 
              select(1,5) %>% 
              set_names(c("Age", "Booster"))%>% 
              mutate(date=as.Date("2021-10-04")))

daily4th <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/10/COVID-19-daily-announced-vaccinations-04-October-2021.xlsx"

temp <- curl_download(url=daily4th, destfile=temp, quiet=FALSE, mode="wb")

dailydata.region <- dailydata.region %>% 
  bind_rows(read_excel(temp, sheet=1, range="B14:F22", col_names=FALSE) %>% 
              select(1,5) %>% 
              filter(complete.cases(.)) %>% 
              set_names(c("Region", "Booster")) %>% 
              mutate(date=as.Date("2021-10-03"), Region=if_else(Region=="England4", "England", Region)))

dailydata.age <- dailydata.age %>% 
  bind_rows(read_excel(temp, sheet=2, range="B16:F29", col_names=FALSE) %>% 
              select(1,5) %>% 
              set_names(c("Age", "Booster"))%>% 
              mutate(date=as.Date("2021-10-03")))

daily3rd <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/10/COVID-19-daily-announced-vaccinations-03-October-2021.xlsx"

temp <- curl_download(url=daily3rd, destfile=temp, quiet=FALSE, mode="wb")

dailydata.region <- dailydata.region %>% 
  bind_rows(read_excel(temp, sheet=1, range="B14:F22", col_names=FALSE) %>% 
              select(1,5) %>% 
              filter(complete.cases(.)) %>% 
              set_names(c("Region", "Booster")) %>% 
              mutate(date=as.Date("2021-10-02"), Region=if_else(Region=="England4", "England", Region)))

dailydata.age <- dailydata.age %>% 
  bind_rows(read_excel(temp, sheet=2, range="B15:F28", col_names=FALSE) %>% 
              select(1,5) %>% 
              set_names(c("Age", "Booster"))%>% 
              mutate(date=as.Date("2021-10-02")))

daily2nd <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/10/COVID-19-daily-announced-vaccinations-02-October-2021.xlsx"

temp <- curl_download(url=daily2nd, destfile=temp, quiet=FALSE, mode="wb")

dailydata.region <- dailydata.region %>% 
  bind_rows(read_excel(temp, sheet=1, range="B14:F22", col_names=FALSE) %>% 
              select(1,5) %>% 
              filter(complete.cases(.)) %>% 
              set_names(c("Region", "Booster")) %>% 
              mutate(date=as.Date("2021-10-01"), Region=if_else(Region=="England4", "England", Region)))

dailydata.age <- dailydata.age %>% 
  bind_rows(read_excel(temp, sheet=2, range="B16:F29", col_names=FALSE) %>% 
              select(1,5) %>% 
              set_names(c("Age", "Booster"))%>% 
              mutate(date=as.Date("2021-10-01")))

daily1st <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/10/COVID-19-daily-announced-vaccinations-01-October-2021.xlsx"

temp <- curl_download(url=daily1st, destfile=temp, quiet=FALSE, mode="wb")

dailydata.region <- dailydata.region %>% 
  bind_rows(read_excel(temp, sheet=1, range="B14:F22", col_names=FALSE) %>% 
              select(1,5) %>% 
              filter(complete.cases(.)) %>% 
              set_names(c("Region", "Booster")) %>% 
              mutate(date=as.Date("2021-09-30"), Region=if_else(Region=="England4", "England", Region)))

dailydata.age <- dailydata.age %>% 
  bind_rows(read_excel(temp, sheet=2, range="B16:F29", col_names=FALSE) %>% 
              select(1,5) %>% 
              set_names(c("Age", "Booster"))%>% 
              mutate(date=as.Date("2021-09-30")))

#Calculate daily boosters by age and region
final.region <- dailydata.region %>% 
  group_by(Region) %>% 
  arrange(date) %>% 
  mutate(newBoosters=Booster-lag(Booster, 1)) %>% 
  ungroup()

final.age <- dailydata.age %>% 
  group_by(Age) %>% 
  arrange(date) %>% 
  mutate(newBoosters=Booster-lag(Booster, 1)) %>% 
  ungroup() %>% 
  mutate(Age=factor(Age, levels=c("Under 18", "18-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54",
                                  "55-59", "60-64", "65-69", "70-74", "75-79", "80+")))

ggplot(final.region %>% filter(Region!="England"), aes(x=date, y=newBoosters, colour=Region))+
  geom_line()+
  scale_x_date(name="")+
  scale_y_continuous(limits=c(0,NA), name="Booster jabs delivered per day")+
  scale_colour_paletteer_d("colorblindr::OkabeIto")+
  theme_custom()

ggplot(final.age, aes(x=date, y=newBoosters, colour=Age))+
  geom_line()+
  scale_x_date(name="")+
  scale_y_continuous(limits=c(0,NA), name="Booster jabs delivered per day")+
  scale_colour_paletteer_d("pals::stepped")+
  theme_custom()

#Calculate eligible populations using dashboard data
#National data
naturl <- "https://api.coronavirus.data.gov.uk/v2/data?areaType=nation&areaCode=E92000001&metric=cumPeopleVaccinatedSecondDoseByVaccinationDate&format=csv"
temp <- curl_download(url=naturl, destfile=temp, quiet=FALSE, mode="wb")

natdata <- read.csv(temp) %>% 
  select(4,5) %>% 
  set_names("date", "Eligible") %>% 
  mutate(date=as.Date(date)+days(182), Region="England")
  
#Regional data
regurl <- "https://api.coronavirus.data.gov.uk/v2/data?areaType=region&metric=cumPeopleVaccinatedSecondDoseByVaccinationDate&format=csv"
temp <- curl_download(url=regurl, destfile=temp, quiet=FALSE, mode="wb")

eligible.region <- read.csv(temp) %>% 
  mutate(date=as.Date(date)+days(182)) %>% 
  select(c(2,4, 5)) %>% 
  set_names("Region", "date", "Eligible") %>% 
  mutate(Region=case_when(
    Region %in% c("North East", "Yorkshire and The Humber") ~ "North East and Yorkshire",
    Region %in% c("West Midlands", "East Midlands") ~ "Midlands",
    TRUE ~ Region)) %>% 
  bind_rows(natdata) %>% 
  group_by(Region, date) %>% 
  summarise(Eligible=sum(Eligible)) %>% 
  ungroup() %>% 
  mutate(date=as.Date(date)) %>% 
  merge(final.region) %>% 
  group_by(Region) %>% 
  arrange(date) %>% 
  mutate(newEligible=Eligible-lag(Eligible, 1)) %>% 
  ungroup() %>% 
  mutate(BoosterProp=Booster/Eligible)

#Age banded data
ageurl <- "https://api.coronavirus.data.gov.uk/v2/data?areaType=nation&areaCode=E92000001&metric=vaccinationsAgeDemographics&format=csv"
temp <- curl_download(url=ageurl, destfile=temp, quiet=FALSE, mode="wb")

eligible.age <- read.csv(temp) %>% 
  mutate(date==as.Date(date)+days(182)) %>% 
  select(age, date, cumPeopleVaccinatedSecondDoseByVaccinationDate) %>% 
  set_names("Age", "date", "Eligible") %>% 
  mutate(Age=case_when(
    Age %in% c("12_15", "16_17") ~ "Under 18",
    Age %in% c("80_84", "85_89", "90+") ~ "80+",
    TRUE ~ gsub("_", "-", Age))) %>% 
  group_by(Age, date) %>% 
  summarise(Eligible=sum(Eligible)) %>% 
  ungroup() %>% 
  mutate(date=as.Date(date)) %>% 
  merge(final.age) %>% 
  group_by(Age) %>% 
  arrange(date) %>% 
  mutate(newEligible=Eligible-lag(Eligible, 1)) %>% 
  ungroup() %>% 
  mutate(BoosterProp=Booster/Eligible)

ggplot(eligile.region %>% filter(Region!="England"), aes(x=date, y=BoosterProp, colour=Region))+
  geom_line()+
  scale_x_date(name="")+
  scale_y_continuous(limits=c(0,NA), name="Booster jabs delivered per day")+
  scale_colour_paletteer_d("colorblindr::OkabeIto")+
  theme_custom()

ggplot(eligile.region %>% filter(Region=="England"))+
  geom_col(aes(x=date, y=newBoosters), fill="Blue")+
  geom_col(aes(x=date, y=-newEligible), fill="Red")+
  theme_custom()

ggplot(final.age, aes(x=date, y=newBoosters, colour=Age))+
  geom_line()+
  scale_x_date(name="")+
  scale_y_continuous(limits=c(0,NA), name="Booster jabs delivered per day")+
  scale_colour_paletteer_d("pals::stepped")+
  theme_custom()
















