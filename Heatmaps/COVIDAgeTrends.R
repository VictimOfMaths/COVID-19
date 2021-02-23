rm(list=ls())

library(tidyverse)
library(broom)
library(curl)
library(readxl)
library(lubridate)
library(RcppRoll)
library(ragg)

#Read in Case data
temp <- tempfile()
source <- "https://api.coronavirus.data.gov.uk/v2/data?areaType=nation&areaCode=E92000001&metric=newCasesBySpecimenDateAgeDemographics&format=csv"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

data1 <- read.csv(temp) %>% 
  filter(!age %in% c("60+", "00_59")) %>%
  select(date, age, cases) %>% 
  mutate(metric="Cases",
         age=case_when(
           age %in% c("00_04", "05_09", "10_14") ~ "0-14",
           age %in% c("15_19", "20_24") ~ "15-24",
           age %in% c("25_29", "30_34", "35_39", "40_44") ~ "25-44",
           age %in% c("45_49", "50_54", "55_59", "60_64") ~ "45-64",
           age %in% c("65_69", "70_74", "75_79") ~ "65-79",
           age %in% c("80_84", "85-89", "90+") ~ "80+",
           TRUE ~ "Unknown"),
         date=as.Date(date)) %>% 
  group_by(date, age, metric) %>% 
  summarise(count=sum(cases))

#Read in 28 day deaths data
temp <- tempfile()
source <- "https://api.coronavirus.data.gov.uk/v2/data?areaType=nation&areaCode=E92000001&metric=newDeaths28DaysByDeathDateAgeDemographics&format=csv"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

data2 <- read.csv(temp) %>% 
  filter(!age %in% c("60+", "00_59")) %>%
  select(date, age, deaths) %>% 
  mutate(metric="Deaths",
         age=case_when(
           age %in% c("00_04", "05_09", "10_14") ~ "0-14",
           age %in% c("15_19", "20_24") ~ "15-24",
           age %in% c("25_29", "30_34", "35_39", "40_44") ~ "25-44",
           age %in% c("45_49", "50_54", "55_59", "60_64") ~ "45-64",
           age %in% c("65_69", "70_74", "75_79") ~ "65-79",
           age %in% c("80_84", "85-89", "90+") ~ "80+",
           TRUE ~ "Unknown"),
         date=as.Date(date)) %>% 
  group_by(date, age, metric) %>% 
  summarise(count=sum(deaths))

#Read in Admissions data
#Taken from https://www.england.nhs.uk/statistics/statistical-work-areas/covid-19-hospital-activity/
#Updated each Thursday (I think)
admrange <- "DR"

temp <- tempfile()
source <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/02/Covid-Publication-11-02-2021-Supplementary-Data.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

data3 <- as.data.frame(t(read_excel(temp, range=paste0("D16:", admrange, "23"), 
                                    col_names=FALSE))) %>% 
  mutate(date=seq.Date(from=as.Date("2020-10-12"), length=nrow(.), by="day")) %>% 
  gather(age, count, c(1:8)) %>% 
  mutate(age=case_when(
    age=="V1" ~ "0-5",
    age=="V2" ~ "6-17",
    age=="V3" ~ "18-54",
    age=="V4" ~ "55-64",
    age=="V5" ~ "65-74",
    age=="V6" ~ "75-84",
    age=="V7" ~ "85+",
    TRUE ~ "Unknown"),
    metric="Admissions")

#Read in Hospital deaths data
#Taken from https://www.england.nhs.uk/statistics/statistical-work-areas/covid-19-daily-deaths/
#Updated daily
deathrange <- "MT"

temp <- tempfile()
source <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/02/COVID-19-total-announced-deaths-23-February-2021.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

data4 <- as.data.frame(t(read_excel(temp, sheet="Tab3 Deaths by age", 
                                    range=paste0("E19:", deathrange, "24"), 
                                    col_names=FALSE))) %>% 
  mutate(date=seq.Date(from=as.Date("2020-03-01"), length=nrow(.), by="day")) %>% 
  gather(age, count, c(1:6)) %>% 
  mutate(age=case_when(
    age=="V1" ~ "0-19",
    age=="V2" ~ "20-39",
    age=="V3" ~ "40-59",
    age=="V4" ~ "60-79",
    age=="V5" ~ "80+",
    age=="V6" ~ "Unknown"),
    metric="Hospital Deaths")
  
#Combine them all together
data <- bind_rows(data1, data2, data3, data4)

#Take rolling 7-day averages
data <- data %>% 
  group_by(age, metric) %>% 
  arrange(date) %>% 
  mutate(count_roll=roll_mean(count, 7, align="center", fill=NA),
         age=factor(age, levels=c("0-5", "0-14", "0-19", "6-17", "15-24", "18-54", "20-39",
                                  "25-44", "40-59", "45-64", "55-64", "60-79", "65-74",
                                  "65-79", "75-84", "80+", "85+", "Unknown")))

#Fit linear models
FitFrom <- as.Date("2021-01-21")
FitTo <- as.Date("2021-01-27")

models <- data %>% 
  filter(date>=FitFrom & date<=FitTo) %>% 
  mutate(daysfrom=as.numeric(difftime(date, FitFrom, units = "days"))) %>% 
  group_by(metric, age) %>% 
  do(tidy(lm(log(count_roll+0.5)~daysfrom, .))) %>% 
  select(1:4) %>% 
  spread(term, estimate) %>% 
  rename(intercept=`(Intercept)`, slope=daysfrom)

#Merge into case data
plot.data <- data %>%
  filter(date>=FitFrom) %>% 
  merge(models) %>% 
  mutate(daysfrom=as.numeric(difftime(date, FitFrom, units = "days")),
         baseline=exp(intercept+slope*daysfrom))

#Plot of cases
agg_tiff("Outputs/COVIDAgeEffects1Cases.tiff", units="in", width=10, height=7, res=500)
ggplot(subset(plot.data, metric=="Cases" & age!="Unknown"))+
  geom_line(aes(x=date, y=baseline, colour=age), show.legend=FALSE)+
  geom_ribbon(aes(x=date, ymin=count_roll, ymax=baseline, fill=age), alpha=0.3, show.legend=FALSE)+
  geom_point(aes(x=date, y=count_roll, colour=age), show.legend = FALSE)+
  geom_point(aes(x=date, y=count), colour="Black", alpha=0.08)+
  scale_x_date(name="")+
  scale_y_continuous(trans="log", name="Daily new cases (log scale)", 
                     labels=number_format(accuracy=1))+
  scale_colour_paletteer_d("colorblindr::OkabeIto")+
  scale_fill_paletteer_d("colorblindr::OkabeIto")+
  facet_wrap(~age, scales="free_y")+
  theme_classic()+
  theme(strip.background=element_blank(),
        strip.text=element_text(face="bold"),
        plot.title=element_text(face="bold", size=rel(1.2)))+
  labs(title="Age-specific trends in new COVID-19 cases",
       subtitle=paste0("Rolling 7-day average of new COVID-19 cases by specimen date. Grey dots reflect daily data.\nTrend line fitted between ", FitFrom, " to ", FitTo),
       caption="Data from coronavirus.data.gov.uk | Plot by @VictimOfMaths")

dev.off()

#Plot of deaths within 28 days
agg_tiff("Outputs/COVIDAgeEffects2Deaths.tiff", units="in", width=10, height=7, res=500)
ggplot(subset(plot.data, metric=="Deaths" & age!="Unknown"))+
  geom_line(aes(x=date, y=baseline, colour=age), show.legend=FALSE)+
  geom_ribbon(aes(x=date, ymin=count_roll, ymax=baseline, fill=age), alpha=0.3, show.legend=FALSE)+
  geom_point(aes(x=date, y=count_roll, colour=age), show.legend = FALSE)+
  geom_point(aes(x=date, y=count), colour="Black", alpha=0.08)+
  scale_x_date(name="")+
  scale_y_continuous(trans="log", name="Daily deaths (log scale)", 
                     labels=number_format(accuracy=1))+
  scale_colour_paletteer_d("colorblindr::OkabeIto")+
  scale_fill_paletteer_d("colorblindr::OkabeIto")+
  facet_wrap(~age, scales="free_y")+
  theme_classic()+
  theme(strip.background=element_blank(),
        strip.text=element_text(face="bold"),
        plot.title=element_text(face="bold", size=rel(1.2)))+
  labs(title="Age-specific trends in COVID-19 deaths",
       subtitle=paste0("Rolling 7-day average counts of deaths within 28 days of a positive COVID-19 test by date of death. Grey dots reflect daily data.\nTrend line fitted between ", FitFrom, " to ", FitTo),
       caption="Data from coronavirus.data.gov.uk | Plot by @VictimOfMaths")
dev.off()

#Plot of admissions
agg_tiff("Outputs/COVIDAgeEffects3Admissions.tiff", units="in", width=10, height=7, res=500)
ggplot(subset(plot.data, metric=="Admissions" & age!="Unknown"))+
  geom_line(aes(x=date, y=baseline, colour=age), show.legend=FALSE)+
  geom_ribbon(aes(x=date, ymin=count_roll, ymax=baseline, fill=age), alpha=0.3, show.legend=FALSE)+
  geom_point(aes(x=date, y=count_roll, colour=age), show.legend = FALSE)+
  geom_point(aes(x=date, y=count), colour="Black", alpha=0.08)+
  scale_x_date(name="")+
  scale_y_continuous(trans="log", name="Daily new admissions (log scale)", 
                     labels=number_format(accuracy=1))+
  scale_colour_paletteer_d("colorblindr::OkabeIto")+
  scale_fill_paletteer_d("colorblindr::OkabeIto")+
  facet_wrap(~age, scales="free_y")+
  theme_classic()+
  theme(strip.background=element_blank(),
        strip.text=element_text(face="bold"),
        plot.title=element_text(face="bold", size=rel(1.2)))+
  labs(title="Age-specific trends in new COVID-19 admissions",
       subtitle=paste0("Rolling 7-day average of new hospital admissions with a positive COVID-19 test by admission date. Grey dots reflect daily data.\nTrend line fitted between ", FitFrom, " to ", FitTo),
       caption="Data from NHS England | Plot by @VictimOfMaths")
dev.off()

#Plot of hospital deaths
agg_tiff("Outputs/COVIDAgeEffects4HopsDeaths.tiff", units="in", width=10, height=7, res=500)
ggplot(subset(plot.data, metric=="Hospital Deaths" & age!="Unknown"))+
  geom_line(aes(x=date, y=baseline, colour=age), show.legend=FALSE)+
  geom_ribbon(aes(x=date, ymin=count_roll, ymax=baseline, fill=age), alpha=0.3, show.legend=FALSE)+
  geom_point(aes(x=date, y=count_roll, colour=age), show.legend = FALSE)+
  geom_point(aes(x=date, y=count), colour="Black", alpha=0.08)+
  scale_x_date(name="")+
  scale_y_continuous(trans="log", name="Daily new cases (log scale)", 
                     labels=number_format(accuracy=1))+
  scale_colour_paletteer_d("colorblindr::OkabeIto")+
  scale_fill_paletteer_d("colorblindr::OkabeIto")+
  facet_wrap(~age, scales="free_y")+
  theme_classic()+
  theme(strip.background=element_blank(),
        strip.text=element_text(face="bold"),
        plot.title=element_text(face="bold", size=rel(1.2)))+
  labs(title="Age-specific trends in COVID-19 deaths in hospitals",
       subtitle=paste0("Rolling 7-day average of COVID-19 deaths in hospitals by date of death. Grey dots reflect daily data.\nTrend line fitted between ", FitFrom, " to ", FitTo),
       caption="Data from NHS England | Plot by @VictimOfMaths")
dev.off()
