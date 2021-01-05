rm(list=ls())

library(tidyverse)
library(curl)
library(paletteer)
library(RcppRoll)

#Hospital admissions data available from https://www.england.nhs.uk/statistics/statistical-work-areas/covid-19-hospital-activity/
#First look at longer time series of regional data updated daily
dailyurl <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/01/COVID-19-daily-admissions-and-beds-20210104.xlsx"
#Increment by one each day
dailyrange <- "FA"

dailydata <- tempfile()
dailydata <- curl_download(url=dailyurl, destfile=dailydata, quiet=FALSE, mode="wb")

#Total admissions
daily1 <- read_excel(dailydata, range=paste0("B15:", dailyrange, "21"), col_names=FALSE) %>% 
  gather(date, count, c(2:ncol(.))) %>% 
  mutate(metric="Admissions",
         date=as.Date("2020-08-01")+days(as.numeric(substr(date, 4,7))-2)) %>% 
  rename(region=`...1`)
  
#Total admissions
daily2 <- read_excel(dailydata, range=paste0("B91:", dailyrange, "97"), col_names=FALSE) %>% 
  gather(date, count, c(2:ncol(.))) %>% 
  mutate(metric="Occupancy",
         date=as.Date("2020-08-01")+days(as.numeric(substr(date, 4,7))-2)) %>% 
  rename(region=`...1`)

#Total admissions
daily3 <- read_excel(dailydata, range=paste0("B106:", dailyrange, "112"), col_names=FALSE) %>% 
  gather(date, count, c(2:ncol(.))) %>% 
  mutate(metric="Occupancy of MV beds",
         date=as.Date("2020-08-01")+days(as.numeric(substr(date, 4,7))-2)) %>% 
  rename(region=`...1`)

#Merge and convert to rates
dailydata <- bind_rows(daily1, daily2, daily3) %>% 
  mutate(pop=case_when(
    region=="East of England" ~ 6236072,
    region=="London" ~ 8961989,
    region=="Midlands" ~ 5934037+4835928,
    region=="North East and Yorkshire" ~ 2669941+5502967,
    region=="North West" ~ 7341196,
    region=="South East" ~ 9180135,
    region=="South West" ~ 5624696),
    rate=count*100000/pop) %>% 
  group_by(region, metric) %>% 
  mutate(rollrate=roll_mean(rate, 7, align="center", fill=NA))

#Extract max date
maxdailydate=max(dailydata$date)

#Line charts
tiff("Outputs/COVIDNHSMetricsxReg.tiff", units="in", width=12, height=6, res=500)
ggplot(dailydata)+
  geom_line(aes(x=date, y=rollrate, colour=region))+
  scale_x_date(name="")+
  scale_y_continuous(name="Rate per 100,000 population")+
  scale_colour_paletteer_d("colorblindr::OkabeIto", name="NHS Region")+
  facet_wrap(~metric, scales="free_y")+
  theme_classic()+
  theme(strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)),
        plot.title=element_text(face="bold", size=rel(1.2)))+
  labs(title="The rise in COVID-19 hospital numbers in London and East/South East England is shocking",
       subtitle=paste0("Rolling 7-day averages of new hospital admissions, total bed occupancy and Mechanical Ventilation beds\nfor patients with a positive COVID-19 diagnosis. Data up to ", maxdailydate, "."),
       caption="Data from NHS England | Plot by @VictimOfMaths")
dev.off()