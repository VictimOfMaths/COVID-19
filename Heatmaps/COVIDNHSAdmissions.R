rm(list=ls())

library(tidyverse)
library(curl)
library(readxl)
library(lubridate)
library(paletteer)
library(RcppRoll)
library(geofacet)
library(ggtext)
library(snakecase)
library(forcats)
library(ragg)
library(extrafont)

#Hospital admissions data available from https://www.england.nhs.uk/statistics/statistical-work-areas/covid-19-hospital-activity/
#Longer time series of regional data updated daily
dailyurl <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/06/COVID-19-daily-admissions-and-beds-20210601.xlsx"
#Shorter time series of trust-level data updated weekly on a Thursday afternoon
weeklyurl <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/05/Weekly-covid-admissions-and-beds-publication-210527.xlsx"
#Increment by one each day
dailyrange <- "BD"
dailyoccrange <- "BF"
#Increment by seven each week
weeklyrange <- "AY"

dailydata <- tempfile()
dailydata <- curl_download(url=dailyurl, destfile=dailydata, quiet=FALSE, mode="wb")

dailydata.old <- tempfile()
dailyurl.old <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/04/COVID-19-daily-admissions-and-beds-20210406-1.xlsx"
dailydata.old <- curl_download(url=dailyurl.old, destfile=dailydata.old, quiet=FALSE, mode="wb")

#Total admissions
daily1 <- read_excel(dailydata, range=paste0("B15:", dailyrange, "21"), col_names=FALSE) %>% 
  gather(date, count, c(2:ncol(.))) %>% 
  mutate(metric="Admissions",
         date=as.Date("2021-04-07")+days(as.numeric(substr(date, 4,7))-2)) %>% 
  rename(region=`...1`)

daily1.old <- read_excel(dailydata.old, range="B15:IQ21", col_names=FALSE) %>% 
  gather(date, count, c(2:ncol(.))) %>% 
  mutate(metric="Admissions",
         date=as.Date("2020-08-01")+days(as.numeric(substr(date, 4,7))-2)) %>% 
           rename(region=`...1`)
  
#Total occupancy
daily2 <- read_excel(dailydata, range=paste0("B91:", dailyoccrange, "97"), col_names=FALSE) %>% 
  gather(date, count, c(2:ncol(.))) %>% 
  mutate(metric="Occupancy",
         date=as.Date("2021-04-07")+days(as.numeric(substr(date, 4,7))-2)) %>% 
  rename(region=`...1`)

daily2.old <- read_excel(dailydata.old, range="B91:IQ97", col_names=FALSE) %>% 
  gather(date, count, c(2:ncol(.))) %>% 
  mutate(metric="Occupancy",
         date=as.Date("2020-08-01")+days(as.numeric(substr(date, 4,7))-2)) %>% 
  rename(region=`...1`)

#Total MV occupancy
daily3 <- read_excel(dailydata, range=paste0("B106:", dailyoccrange, "112"), col_names=FALSE) %>% 
  gather(date, count, c(2:ncol(.))) %>% 
  mutate(metric="Occupancy of MV beds",
         date=as.Date("2021-04-07")+days(as.numeric(substr(date, 4,7))-2)) %>% 
  rename(region=`...1`)

daily3.old <- read_excel(dailydata.old, range="B106:IQ112", col_names=FALSE) %>% 
  gather(date, count, c(2:ncol(.))) %>% 
  mutate(metric="Occupancy of MV beds",
         date=as.Date("2020-08-01")+days(as.numeric(substr(date, 4,7))-2)) %>% 
  rename(region=`...1`)

#Merge and convert to rates
dailydata <- bind_rows(daily1.old, daily1, daily2.old, daily2, daily3.old, daily3) %>% 
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
agg_tiff("Outputs/COVIDNHSMetricsxReg.tiff", units="in", width=12, height=6, res=500)
ggplot(dailydata)+
  geom_line(aes(x=date, y=rollrate, colour=region))+
  scale_x_date(name="")+
  scale_y_continuous(name="Rate per 100,000 population")+
  scale_colour_paletteer_d("colorblindr::OkabeIto", name="NHS Region")+
  facet_wrap(~metric, scales="free_y")+
  theme_classic()+
  theme(strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)),
        plot.title=element_text(face="bold", size=rel(1.2)),
        text=element_text(family="Lato"))+
  labs(title="The number of COVID-19 patients in English hospitals is still low...",
       subtitle=paste0("Rolling 7-day averages of new hospital admissions, total bed occupancy and Mechanical Ventilation beds\nfor patients with a positive COVID-19 diagnosis. Data up to ", maxdailydate, "."),
       caption="Data from NHS England | Plot by @VictimOfMaths")
dev.off()

#Recent version
agg_tiff("Outputs/COVIDNHSMetricsxRegRecent.tiff", units="in", width=12, height=6, res=500)
ggplot(dailydata %>% filter(date>as.Date("2021-04-01")))+
  geom_line(aes(x=date, y=rollrate, colour=region))+
  scale_x_date(name="")+
  scale_y_continuous(name="Rate per 100,000 population", limits=c(0,NA))+
  scale_colour_paletteer_d("colorblindr::OkabeIto", name="NHS Region")+
  facet_wrap(~metric, scales="free_y")+
  theme_classic()+
  theme(strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)),
        plot.title=element_text(face="bold", size=rel(1.2)),
        text=element_text(family="Lato"))+
  labs(title="COVID hospital admissions are rising in the North West and East of England, but stable elsewhere. So far.",
       subtitle=paste0("Rolling 7-day averages of new hospital admissions, total bed occupancy and Mechanical Ventilation beds\nfor patients with a positive COVID-19 diagnosis. Data up to ", maxdailydate, "."),
       caption="Data from NHS England | Plot by @VictimOfMaths")
dev.off()

agg_tiff("Outputs/COVIDNHSMetricsxRegLog.tiff", units="in", width=12, height=6, res=500)
ggplot(subset(dailydata, date>as.Date("2021-01-01")))+
  geom_line(aes(x=date, y=rollrate, colour=region))+
  scale_x_date(name="")+
  scale_y_continuous(name="Rate per 100,000 population (log scale)", trans="log",
                     labels=label_number(accuracy=1))+
  scale_colour_paletteer_d("colorblindr::OkabeIto", name="NHS Region")+
  facet_wrap(~metric, scales="free_y")+
  theme_classic()+
  theme(strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)),
        plot.title=element_text(face="bold", size=rel(1.2)),
        text=element_text(family="Roboto"))+
  labs(title="All measures of COVID-19 hospital usage are still falling across all regions",
       subtitle=paste0("Rolling 7-day averages of new hospital admissions, total bed occupancy and Mechanical Ventilation beds\nfor patients with a positive COVID-19 diagnosis. Data up to ", maxdailydate, "."),
       caption="Data from NHS England | Plot by @VictimOfMaths")
dev.off()

#Admissions only
agg_tiff("Outputs/COVIDNHSAdmissionsxReg.tiff", units="in", width=9, height=6, res=500)
ggplot(subset(dailydata, metric=="Admissions"))+
  geom_line(aes(x=date, y=rollrate, colour=region))+
  scale_x_date(name="")+
  scale_y_continuous(name="Rate per 100,000 population")+
  scale_colour_paletteer_d("colorblindr::OkabeIto", name="NHS Region")+
  theme_classic()+
  theme(strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)),
        plot.title=element_text(face="bold", size=rel(1.2)),
        text=element_text(family="Roboto"))+
  labs(title="New COVID-19 hospital admissions figures are now at very low levels",
       subtitle=paste0("Rolling 7-day averages of new hospital admissions for patients with a positive COVID-19 diagnosis.\nData up to ", maxdailydate, "."),
       caption="Data from NHS England | Plot by @VictimOfMaths")
dev.off()

#Now look at trust-level weekly data
weeklydata <- tempfile()
weeklydata <- curl_download(url=weeklyurl, destfile=weeklydata, quiet=FALSE, mode="wb")

weeklydata.old <- tempfile()
weeklyurl.old <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/04/Weekly-covid-admissions-and-beds-publication-210429-up-to-210406.xlsx"
weeklydata.old <- curl_download(url=weeklyurl.old, destfile=weeklydata.old, quiet=FALSE, mode="wb")

weeklyCOVID <- read_excel(weeklydata, sheet="Adult G&A Beds Occupied COVID", 
                          range=paste0("B16:", weeklyrange, "167"), col_names=FALSE)[-c(2),] %>% 
  gather(date, count, c(4:ncol(.))) %>% 
  mutate(type="COVID",
         date=as.Date("2021-04-07")+days(as.numeric(substr(date, 4,7))-4)) %>% 
  rename(region=`...1`, code=`...2`, trust=`...3`)

weeklyCOVID.old <- read_excel(weeklydata.old, sheet="Adult G&A Beds Occupied COVID", 
                          range="B16:EO167", col_names=FALSE)[-c(2),] %>% 
  gather(date, count, c(4:ncol(.))) %>% 
  mutate(type="COVID",
         date=as.Date("2020-11-17")+days(as.numeric(substr(date, 4,7))-4)) %>% 
  rename(region=`...1`, code=`...2`, trust=`...3`)

weeklyOther <- read_excel(weeklydata, sheet="Adult G&A Bed Occupied NonCOVID", 
                          range=paste0("B16:", weeklyrange, "167"), col_names=FALSE)[-c(2),] %>% 
  gather(date, count, c(4:ncol(.))) %>% 
  mutate(type="non-COVID",
         date=as.Date("2021-04-07")+days(as.numeric(substr(date, 4,7))-4)) %>% 
  rename(region=`...1`, code=`...2`, trust=`...3`)

weeklyOther.old <- read_excel(weeklydata.old, sheet="Adult G&A Bed Occupied NonCOVID", 
                          range="B16:EO167", col_names=FALSE)[-c(2),] %>% 
  gather(date, count, c(4:ncol(.))) %>% 
  mutate(type="non-COVID",
         date=as.Date("2020-11-17")+days(as.numeric(substr(date, 4,7))-4)) %>% 
  rename(region=`...1`, code=`...2`, trust=`...3`)

weeklyEmpty <- read_excel(weeklydata, sheet="Adult G&A Beds Unoccupied", 
                          range=paste0("B16:", weeklyrange, "167"), col_names=FALSE)[-c(2),] %>% 
  gather(date, count, c(4:ncol(.))) %>% 
  mutate(type="Unoccupied",
         date=as.Date("2021-04-07")+days(as.numeric(substr(date, 4,7))-4)) %>% 
  rename(region=`...1`, code=`...2`, trust=`...3`)

weeklyEmpty.old <- read_excel(weeklydata.old, sheet="Adult G&A Beds Unoccupied", 
                          range="B16:EO167", col_names=FALSE)[-c(2),] %>% 
  gather(date, count, c(4:ncol(.))) %>% 
  mutate(type="Unoccupied",
         date=as.Date("2020-11-17")+days(as.numeric(substr(date, 4,7))-4)) %>% 
  rename(region=`...1`, code=`...2`, trust=`...3`)

weeklydata <- bind_rows(weeklyCOVID, weeklyCOVID.old, weeklyOther, weeklyOther.old,
                        weeklyEmpty, weeklyEmpty.old) %>% 
  mutate(region=case_when(
    trust=="ENGLAND" ~ "Nation", 
    trust %in% c("East of England", "London", "Midlands", "North East and Yorkshire",
                 "North West", "South East", "South West") ~ "Region",
    TRUE ~ region)) %>% 
  group_by(trust, date) %>% 
  mutate(capacity=sum(count)) %>% 
  ungroup() %>% 
  mutate(proportion=count/capacity)

#Extract max date
maxweeklydate=max(weeklydata$date)

#Carve out into separate regional/national and trust-level datasets
natdata <- weeklydata %>% filter(region %in% c("Nation", "Region"))
trustdata <- weeklydata %>% 
  filter(!region %in% c("Nation", "Region") & capacity>=100) %>% 
  mutate(trust=str_replace(trust, " NHS TRUST", ""),
         trust=str_replace(trust, "NHS FOUNDATION TRUST", ""),
         trust=to_any_case(trust, case="title"),
         trust=str_replace(trust, "King s", "King's"),
         trust=str_replace(trust, "Guy s", "Guy's"),
         trust=str_replace(trust, "George s", "George's"),
         trust=str_replace(trust, "Women s", "Women's"),
         trust=str_replace(trust, "Children s", "Children's"),
         trust=str_replace(trust, "Peter s", "Peter's")) %>% 
  group_by(trust) %>% 
  mutate(maxcap=max(count[type=="COVID"])) %>% 
  ungroup() %>% 
  mutate(trust=fct_reorder(trust, -maxcap))

#Convert national/region data to rates
natdata <- natdata %>% 
  mutate(pop=case_when(
    trust=="East of England" ~ 6236072,
    trust=="London" ~ 8961989,
    trust=="Midlands" ~ 5934037+4835928,
    trust=="North East and Yorkshire" ~ 2669941+5502967,
    trust=="North West" ~ 7341196,
    trust=="South East" ~ 9180135,
    trust=="South West" ~ 5624696,
    trust=="ENGLAND" ~ 56286961),
    rate=count*100000/pop)

#Single national plot
agg_tiff("Outputs/COVIDNHSBedOccupancy.tiff", units="in", width=8, height=6, res=500)
ggplot(subset(natdata, trust=="ENGLAND"))+
  geom_area(aes(x=date, y=rate, fill=type), show.legend=FALSE)+
  scale_x_date(name="")+
  scale_y_continuous(name="Beds per 100,000 population")+
  scale_fill_manual(values=c("#FD625E", "#374649", "#00B8AA"), name="Occupied by", 
                    labels=c("Patient with COVID-19", "Other patient", "Unoccupied"))+
  theme_classic()+
  theme(strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)),
        plot.title=element_text(face="bold", size=rel(1.2)), plot.subtitle=element_markdown(),
        text=element_text(family="Lato"))+
  labs(title="The number of people in hospital with a positive COVID-19 test is still falling",
       subtitle=paste0("<span style='color:Grey60;'>Bed occupancy rate in England for <span style='color:#FD625E;'>COVID-19 patients</span>, <span style='color:#374649;'>non-COVID patients</span> and <span style='color:#00B8AA;'>unoccupied beds</span>.<br>Data up to ", maxweeklydate, " ."),
       caption="Data from NHS England | Plot by @VictimOfMaths")
dev.off()

#Set up geofacet grid of NHS regions
mygrid <- data.frame(name=c("North West", "North East and Yorkshire", 
                            "Midlands","East of England",
                            "South West", "London", "South East"),
                     row=c(1,1,2,2,3,3,3), col=c(2,3,2,3,1,2,3),
                     code=c(1:7))

#Faceted regional plot
agg_tiff("Outputs/COVIDNHSBedOccupancyxReg.tiff", units="in", width=8, height=8, res=500)
ggplot(subset(natdata, trust!="ENGLAND"))+
  geom_area(aes(x=date, y=rate, fill=type), show.legend=FALSE)+
  scale_x_date(name="")+
  scale_y_continuous(name="Beds per 100,000 population")+
  scale_fill_manual(values=c("#FD625E", "#374649", "#00B8AA"), name="Occupied by", 
                         labels=c("Patient with COVID-19", "Other patient", "Unoccupied"))+
  facet_geo(~trust, grid=mygrid)+
  theme_classic()+
  theme(strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)),
        plot.title=element_text(face="bold", size=rel(1.2)), plot.subtitle=element_markdown(),
        text=element_text(family="Lato"))+
  labs(title="COVID-19 bed occupancy is still falling in all regions of England",
       subtitle=paste0("<span style='color:Grey60;'>Bed occupancy rate by NHS region for <span style='color:#FD625E;'>COVID-19 patients</span>, <span style='color:#374649;'>non-COVID patients</span> and <span style='color:#00B8AA;'>unoccupied beds</span>.<br>Data up to ", maxweeklydate, " ."),
       caption="Data from NHS England | Plot by @VictimOfMaths")
dev.off()

#Get into trust-level data
agg_tiff("Outputs/COVIDNHSBedOccupancyLondon.tiff", units="in", width=13, height=8, res=500)
trustdata %>% 
  filter(region=="London") %>% 
  ggplot()+
  geom_area(aes(x=date, y=count, fill=type), show.legend=FALSE)+
  scale_x_date(name="")+
  scale_y_continuous(name="Number of beds")+
  scale_fill_manual(values=c("#FD625E", "#374649", "#00B8AA"), name="Occupied by", 
                    labels=c("Patient with COVID-19", "Other patient", "Unoccupied"))+
  facet_wrap(~trust)+
  theme_classic()+
  theme(strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(0.6)),
        plot.title=element_text(face="bold", size=rel(1.2)), plot.subtitle=element_markdown(),
        text=element_text(family="Lato"))+
  labs(title="COVID-19 bed occupancy has fallen across London",
     subtitle=paste0("<span style='color:Grey60;'>Bed occupancy by NHS trust for <span style='color:#FD625E;'>COVID-19 patients</span>, <span style='color:#374649;'>non-COVID patients</span> and <span style='color:#00B8AA;'>unoccupied beds</span>.<br>Data up to ", maxweeklydate, " . Excluding trusts with fewer than 100 beds."),
     caption="Data from NHS England | Plot by @VictimOfMaths")
dev.off()

agg_tiff("Outputs/COVIDNHSBedOccupancySouthEast.tiff", units="in", width=13, height=8, res=500)
trustdata %>% 
  filter(region=="South East") %>% 
  ggplot()+
  geom_area(aes(x=date, y=count, fill=type), show.legend=FALSE)+
  scale_x_date(name="")+
  scale_y_continuous(name="Number of beds")+
  scale_fill_manual(values=c("#FD625E", "#374649", "#00B8AA"), name="Occupied by", 
                    labels=c("Patient with COVID-19", "Other patient", "Unoccupied"))+
  facet_wrap(~trust)+
  theme_classic()+
  theme(strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(0.6)),
        plot.title=element_text(face="bold", size=rel(1.2)), plot.subtitle=element_markdown(),
        text=element_text(family="Lato"))+
  labs(title="The number of COVID-19 patients in hospital is falling across the South East",
       subtitle=paste0("<span style='color:Grey60;'>Bed occupancy by NHS trust for <span style='color:#FD625E;'>COVID-19 patients</span>, <span style='color:#374649;'>non-COVID patients</span> and <span style='color:#00B8AA;'>unoccupied beds</span>.<br>Data up to ", maxweeklydate, " . Excluding trusts with fewer than 100 beds."),
       caption="Data from NHS England | Plot by @VictimOfMaths")
dev.off()

agg_tiff("Outputs/COVIDNHSBedOccupancySouthWest.tiff", units="in", width=10, height=6, res=500)
trustdata %>% 
  filter(region=="South West") %>% 
  ggplot()+
  geom_area(aes(x=date, y=count, fill=type), show.legend=FALSE)+
  scale_x_date(name="")+
  scale_y_continuous(name="Number of beds")+
  scale_fill_manual(values=c("#FD625E", "#374649", "#00B8AA"), name="Occupied by", 
                    labels=c("Patient with COVID-19", "Other patient", "Unoccupied"))+
  facet_wrap(~trust)+
  theme_classic()+
  theme(strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(0.6)),
        plot.title=element_text(face="bold", size=rel(1.2)), plot.subtitle=element_markdown(),
        text=element_text(family="Lato"))+
  labs(title="COVID-19 bed occupancy is falling across the South West",
       subtitle=paste0("<span style='color:Grey60;'>Bed occupancy by NHS trust for <span style='color:#FD625E;'>COVID-19 patients</span>, <span style='color:#374649;'>non-COVID patients</span> and <span style='color:#00B8AA;'>unoccupied beds</span>.<br>Data up to ", maxweeklydate, " . Excluding trusts with fewer than 100 beds."),
       caption="Data from NHS England | Plot by @VictimOfMaths")
dev.off()

agg_tiff("Outputs/COVIDNHSBedOccupancyMidlands.tiff", units="in", width=14, height=9, res=500)
trustdata %>% 
  filter(region=="Midlands") %>% 
  ggplot()+
  geom_area(aes(x=date, y=count, fill=type), show.legend=FALSE)+
  scale_x_date(name="")+
  scale_y_continuous(name="Number of beds")+
  scale_fill_manual(values=c("#FD625E", "#374649", "#00B8AA"), name="Occupied by", 
                    labels=c("Patient with COVID-19", "Other patient", "Unoccupied"))+
  facet_wrap(~trust)+
  theme_classic()+
  theme(strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(0.6)),
        plot.title=element_text(face="bold", size=rel(1.2)), plot.subtitle=element_markdown(),
        text=element_text(family="Lato"))+
  labs(title="COVID-19 bed occupancy in the Midlands is falling across all NHS trusts",
       subtitle=paste0("<span style='color:Grey60;'>Bed occupancy by NHS trust for <span style='color:#FD625E;'>COVID-19 patients</span>, <span style='color:#374649;'>non-COVID patients</span> and <span style='color:#00B8AA;'>unoccupied beds</span>.<br>Data up to ", maxweeklydate, " . Excluding trusts with fewer than 100 beds."),
       caption="Data from NHS England | Plot by @VictimOfMaths")
dev.off()

agg_tiff("Outputs/COVIDNHSBedOccupancyEast.tiff", units="in", width=10, height=6, res=500)
trustdata %>% 
  filter(region=="East of England") %>% 
  ggplot()+
  geom_area(aes(x=date, y=count, fill=type), show.legend=FALSE)+
  scale_x_date(name="")+
  scale_y_continuous(name="Number of beds")+
  scale_fill_manual(values=c("#FD625E", "#374649", "#00B8AA"), name="Occupied by", 
                    labels=c("Patient with COVID-19", "Other patient", "Unoccupied"))+
  facet_wrap(~trust)+
  theme_classic()+
  theme(strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(0.6)),
        plot.title=element_text(face="bold", size=rel(1.2)), plot.subtitle=element_markdown(),
        text=element_text(family="Lato"))+
  labs(title="The number of COVID-19 patients is falling in hospitals in the East of England",
       subtitle=paste0("<span style='color:Grey60;'>Bed occupancy by NHS trust for <span style='color:#FD625E;'>COVID-19 patients</span>, <span style='color:#374649;'>non-COVID patients</span> and <span style='color:#00B8AA;'>unoccupied beds</span>.<br>Data up to ", maxweeklydate, " . Excluding trusts with fewer than 100 beds."),
       caption="Data from NHS England | Plot by @VictimOfMaths")
dev.off()

agg_tiff("Outputs/COVIDNHSBedOccupancyNorthWest.tiff", units="in", width=13, height=8, res=500)
trustdata %>% 
  filter(region=="North West") %>% 
  ggplot()+
  geom_area(aes(x=date, y=count, fill=type), show.legend=FALSE)+
  scale_x_date(name="")+
  scale_y_continuous(name="Number of beds")+
  scale_fill_manual(values=c("#FD625E", "#374649", "#00B8AA"), name="Occupied by", 
                    labels=c("Patient with COVID-19", "Other patient", "Unoccupied"))+
  facet_wrap(~trust)+
  theme_classic()+
  theme(strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(0.6)),
        plot.title=element_text(face="bold", size=rel(1.2)), plot.subtitle=element_markdown(),
        text=element_text(family="Lato"))+
  labs(title="The number of COVID-19 patients in hospital is falling across the North West",
       subtitle=paste0("<span style='color:Grey60;'>Bed occupancy by NHS trust for <span style='color:#FD625E;'>COVID-19 patients</span>, <span style='color:#374649;'>non-COVID patients</span> and <span style='color:#00B8AA;'>unoccupied beds</span>.<br>Data up to ", maxweeklydate, " . Excluding trusts with fewer than 100 beds."),
       caption="Data from NHS England | Plot by @VictimOfMaths")
dev.off()

agg_tiff("Outputs/COVIDNHSBedOccupancyNEYorks.tiff", units="in", width=13, height=8, res=500)
trustdata %>% 
  filter(region=="North East and Yorkshire") %>% 
  ggplot()+
  geom_area(aes(x=date, y=count, fill=type), show.legend=FALSE)+
  scale_x_date(name="")+
  scale_y_continuous(name="Number of beds")+
  scale_fill_manual(values=c("#FD625E", "#374649", "#00B8AA"), name="Occupied by", 
                    labels=c("Patient with COVID-19", "Other patient", "Unoccupied"))+
  facet_wrap(~trust)+
  theme_classic()+
  theme(strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(0.6)),
        plot.title=element_text(face="bold", size=rel(1.2)), plot.subtitle=element_markdown(),
        text=element_text(family="Lato"))+
  labs(title="COVID-19 patient numbers are falling across North East and Yorkshire",
       subtitle=paste0("<span style='color:Grey60;'>Bed occupancy by NHS trust for <span style='color:#FD625E;'>COVID-19 patients</span>, <span style='color:#374649;'>non-COVID patients</span> and <span style='color:#00B8AA;'>unoccupied beds</span>.<br>Data up to ", maxweeklydate, " . Excluding trusts with fewer than 100 beds."),
       caption="Data from NHS England | Plot by @VictimOfMaths")
dev.off()

