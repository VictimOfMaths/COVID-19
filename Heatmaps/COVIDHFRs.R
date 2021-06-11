rm(list=ls())

library(tidyverse)
library(curl)
library(readxl)
library(scales)
library(lubridate)
library(RcppRoll)
library(extrafont)
library(paletteer)
library(ragg)

#Read in admissions data at regional level
admurl <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/06/COVID-19-daily-admissions-and-beds-20210610.xlsx"
admrange <- "BM"
deathurl <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/06/COVID-19-total-announced-deaths-11-June-2021.xlsx"
deathrange <- "QX"

theme_custom <- function() {
  theme_classic() %+replace%
    theme(plot.title.position="plot", plot.caption.position="plot",
          strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)),
          plot.title=element_text(face="bold", size=rel(1.5), hjust=0,
                                  margin=margin(0,0,5.5,0)),
          text=element_text(family="Lato"))
}


temp <- tempfile()
temp <- curl_download(url=admurl, destfile=temp, quiet=FALSE, mode="wb")

admissions.new <- read_excel(temp, range=paste0("B14:", admrange, "21"), 
                             col_names=FALSE) %>% 
  rename(region=`...1`) %>% 
  gather(date, admissions, c(2:ncol(.))) %>% 
  mutate(date=as.Date("2021-04-07")+days(as.numeric(substr(date, 4,7))-2))

beds.new <- read_excel(temp, range=paste0("B90:", admrange, "97"), 
                       col_names=FALSE)%>% 
  rename(region=`...1`)%>% 
  gather(date, beds, c(2:ncol(.))) %>% 
  mutate(date=as.Date("2021-04-07")+days(as.numeric(substr(date, 4,7))-2))


#Read in older data
admurl.old <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/04/COVID-19-daily-admissions-and-beds-20210406-1.xlsx"

temp <- tempfile()
temp <- curl_download(url=admurl.old, destfile=temp, quiet=FALSE, mode="wb")

admissions.old <- read_excel(temp, range="B14:IQ21", col_names=FALSE)%>% 
  rename(region=`...1`)%>% 
  gather(date, admissions, c(2:ncol(.))) %>% 
  mutate(date=as.Date("2020-08-01")+days(as.numeric(substr(date, 4,7))-2))


beds.old <- read_excel(temp, range="B90:IQ97", col_names=FALSE)%>% 
  rename(region=`...1`)%>% 
  gather(date, beds, c(2:ncol(.))) %>% 
  mutate(date=as.Date("2020-08-01")+days(as.numeric(substr(date, 4,7))-2))


admissions <- bind_rows(admissions.old, admissions.new) %>% 
  arrange(region, date) 

#Bring in deaths
temp <- tempfile()
temp <- curl_download(url=deathurl, destfile=temp, quiet=FALSE, mode="wb")

deaths <- read_excel(temp, sheet="Tab1 Deaths by region",
                     range=paste0("B17:", deathrange, "25"), col_names=FALSE) %>% 
  rename(region=`...1`) %>% 
  gather(date, deaths, c(2:ncol(.))) %>% 
  mutate(date=as.Date("2020-03-01")+days(as.numeric(substr(date, 4, 7))-2),
         region=case_when(
           region=="East Of England" ~ "East of England",
           region=="North East And Yorkshire" ~ "North East and Yorkshire",
           region=="England" ~ "ENGLAND",
           TRUE ~ region))

beds <- bind_rows(beds.old, beds.new) %>% 
  mutate(date=date-days(1)) %>% 
  arrange(region, date) %>% 
  group_by(region) %>% 
  mutate(bedchange=beds-lag(beds, 1)) %>% 
  ungroup() %>% 
  merge(admissions) %>% 
  mutate(exits=admissions-bedchange) %>% 
  merge(deaths, all.x=TRUE) %>% 
  mutate(deaths=if_else(is.na(deaths), 0, deaths),
         discharges=exits-deaths,
         adm_roll=roll_mean(admissions, n=7, align="center", fill=NA),
         deaths_roll=roll_mean(deaths, n=7, align="center", fill=NA),
         discharges_roll=roll_mean(discharges, n=7, align="center", fill=NA),
         deathstoadm=deaths_roll/adm_roll,
         distoadm=discharges_roll/adm_roll,
         region=if_else(region=="ENGLAND", "England", region))

agg_tiff("Outputs/COVIDNHSDeathsvsAdm.tiff", units="in", width=8, height=6, res=800)
ggplot(beds %>% filter(region=="England"), 
       aes(x=date, y=deathstoadm))+
  geom_line(colour="#D62828")+
  scale_x_date(name="")+
  scale_y_continuous(name="Number of deaths per new admission", limits=c(0,NA))+
  theme_custom()+
  labs(title="The ratio of hospital deaths to admissions with COVID is still low",
       subtitle="Rolling 7-day average of in-hospital COVID deaths divided by the number of new admissions and newly identified\ncases in hospital",
       caption="Data from NHS England | Plot by @VictimOfMaths")
dev.off()

agg_tiff("Outputs/COVIDNHSDeathsvsAdmxReg.tiff", units="in", width=10, height=6, res=800)
ggplot(beds %>% filter(region!="England"), 
       aes(x=date, y=deathstoadm, colour=region))+
  geom_line()+
  scale_x_date(name="")+
  scale_y_continuous(name="Number of deaths per new admission")+
  scale_colour_paletteer_d("colorblindr::OkabeIto", name="")+
  theme_custom()+
  labs(title="The ratio of hospital deaths to admissions with COVID is still low",
       subtitle="Rolling 7-day average of in-hospital COVID deaths divided by the number of new admissions and newly identified\ncases in hospital",
       caption="Data from NHS England | Plot by @VictimOfMaths")
dev.off()

