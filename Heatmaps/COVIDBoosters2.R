rm(list=ls())

library(tidyverse)
library(curl)
library(paletteer)
library(scales)
library(extrafont)
library(ragg)
library(lubridate)
library(readxl)
library(ggtext)

options(scipen=10000)

theme_custom <- function() {
  theme_classic() %+replace%
    theme(plot.title.position="plot", plot.caption.position="plot",
          strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)),
          plot.title=element_text(face="bold", size=rel(1.5), hjust=0,
                                  margin=margin(0,0,5.5,0)),
          text=element_text(family="Lato"))
}

#Total English doses

#Download vaccination data from NHS England website
#Daily data (includes boosters)
#Start with most recent data
temp <- tempfile()

latest <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/11/COVID-19-daily-announced-vaccinations-15-November-2021.xlsx"

temp <- curl_download(url=latest, destfile=temp, quiet=FALSE, mode="wb")

dailydata <- read_excel(temp, sheet=1, range="B13:F14", col_names=FALSE) %>% 
  select(1,5) %>% 
  set_names(c("Region", "Booster")) %>% 
  mutate(date=as.Date("2021-11-14"), Region=if_else(Region=="England4", "England", Region)) %>% 
  filter(substr(Region, 1, 7)=="England") %>% 
  mutate(Booster=as.numeric(Booster))

#Shout out to @jackd1801 for suggesting this would be *much* easier as a for loop.
#October
for(i in c("01", "02", "03", "04", "05", "06", "07", "08", "09", as.character(10:31))){
  
  url <- paste0("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/10/COVID-19-daily-announced-vaccinations-", i, "-October-2021.xlsx")
  
  temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")
  
  dailydata <- dailydata %>% 
    bind_rows(read_excel(temp, sheet=1, range="B13:F14", col_names=FALSE) %>% 
                select(1,5) %>% 
                set_names(c("Region", "Booster")) %>% 
                mutate(date=as.Date("2021-10-01")+days(as.numeric(i)-2), 
                       Region=if_else(Region=="England4", "England", Region))%>% 
    filter(substr(Region, 1, 7)=="England")%>% 
      mutate(Booster=as.numeric(Booster)))
}

#November
for(i in c("01", "02", "03", "04", "05", "06", "07", "08", "09", as.character(10:14))){
  
  url <- paste0("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/11/COVID-19-daily-announced-vaccinations-", i, "-November-2021.xlsx")
  
  temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")
  
  dailydata <- dailydata %>% 
    bind_rows(read_excel(temp, sheet=1, range="B13:F14", col_names=FALSE) %>% 
                select(1,5) %>% 
                set_names(c("Region", "Booster")) %>% 
                mutate(date=as.Date("2021-11-01")+days(as.numeric(i)-2), 
                       Region=if_else(Region=="England4", "England", Region))%>% 
                filter(substr(Region, 1, 7)=="England")%>% 
                mutate(Booster=as.numeric(Booster)))
}

#Calculate 'run rate' for last 14 days
runrate.e <- as.numeric(dailydata %>% filter(date>=max(date)-days(14)) %>% 
  summarise((value=max(Booster)-min(Booster))/14))

#Bring in eligible population
#Commented out lines are for switching to alternative assumptions about eligibility based on age
#Engurl <- "https://api.coronavirus.data.gov.uk/v2/data?areaType=nation&areaCode=E92000001&metric=vaccinationsAgeDemographics&format=csv"
Engurl <- "https://api.coronavirus.data.gov.uk/v2/data?areaType=nation&areaCode=E92000001&metric=cumPeopleVaccinatedSecondDoseByVaccinationDate&format=csv"

temp <- curl_download(url=Engurl, destfile=temp, quiet=FALSE, mode="wb")

Engdata <- read.csv(temp) %>% 
  select(4,5) %>% 
  set_names("date", "Eligible") %>% 
  mutate(date=as.Date(date)+days(182)) %>% 
  #select(4,5,11) %>% 
  #set_names("date", "age", "Eligible") %>% 
  #mutate(date=as.Date(date)+days(182),
  #       Eligible=if_else(age %in% c("12_15", "16_17", "18_24", "25_29", "30_34", "35_39",
  #                                   "40_44", "45_49"), 0, Eligible)) %>% 
  #group_by(date) %>% 
  #summarise(Eligible=sum(Eligible)) %>% 
  #ungroup() %>% 
  merge(dailydata, by="date", all.x=TRUE) %>% 
  mutate(EligibleUnvax=Eligible-Booster,
         Boosterprop=Booster/Eligible,
         Forecast=if_else(date>max(date[!is.na(Booster)]), 
                          Booster[date==max(date[!is.na(Booster)])]+
                            runrate.e*as.numeric(interval(max(date[!is.na(Booster)]), date), "days"),
                          NA_real_),
         Forecastprop=Forecast/Eligible)

Engprop <- as.numeric(Engdata %>% filter(date==max(date[!is.na(Booster)])) %>% 
                         summarise(value=Booster/Eligible))

agg_tiff("Outputs/COVIDBoostersEng.tiff", units="in", width=9, height=6, res=500)
ggplot(Engdata %>% filter(date>=as.Date("2021-09-21")))+
  geom_line(aes(x=date, y=Eligible), colour="#CC3300")+
  geom_line(aes(x=date, y=Booster), colour="#006666")+
  scale_x_date(name="")+
  scale_y_continuous(name="Number of people", limits=c(0,NA))+
  theme_custom()+
  theme(plot.subtitle=element_markdown())+
  labs(title=paste0("England has delivered booster jabs to ",round(Engprop*100, 0),"% of eligible people"),
       subtitle="Total number of <span style='color:#CC3300;'>people eligible</span> and <span style='color:#006666;'>having received</span> a COVID booster jab in England since bookings were opened on 21st September",
       caption="Data from NHS England & coronavirus.data.gov.uk | Plot by @VictimOfMaths")
dev.off()

#Total Scottish doses
Scotdoseurl <- "https://www.opendata.nhs.scot/dataset/6dbdd466-45e3-4348-9ee3-1eac72b5a592/resource/42f17a3c-a4db-4965-ba68-3dffe6bca13a/download/daily_vacc_scot_20211029.csv"
temp <- tempfile()
temp <- curl_download(url=Scotdoseurl, destfile=temp, quiet=FALSE, mode="wb")

ScotDoses <- read.csv(temp) %>% 
  filter(Product=="Total" & AgeBand=="12 years and over") %>% 
  mutate(date=as.Date(as.character(Date), format="%Y%m%d"))

ggplot(ScotDoses, aes(x=date, y=CumulativeNumberVaccinated, colour=Dose))+
  geom_line()

#Calculate 'run rate' for last 14 days
runrate.s <- as.numeric(ScotDoses %>% filter(Dose=="Dose 3 and Booster" & date>=max(date)-days(14)) %>% 
                          summarise((value=max(CumulativeNumberVaccinated)-min(CumulativeNumberVaccinated))/14))

Scotdata <- ScotDoses %>% 
  filter(Dose=="Dose 2") %>% 
  mutate(date=date+days(182)) %>% 
  select(date, CumulativeNumberVaccinated) %>% 
  rename("Eligible"="CumulativeNumberVaccinated") %>% 
  merge(ScotDoses %>% filter(Dose=="Dose 3 and Booster"), by="date", all.x=TRUE) %>% 
  filter(date>=max(date[Eligible==0])) %>% 
  mutate(EligibleUnvax=Eligible-CumulativeNumberVaccinated,
         Boosterprop=CumulativeNumberVaccinated/Eligible,      
         Forecast=if_else(date>max(date[!is.na(CumulativeNumberVaccinated)]), 
                          CumulativeNumberVaccinated[date==max(date[!is.na(CumulativeNumberVaccinated)])]+ 
                            runrate.s*as.numeric(interval(max(date[!is.na(CumulativeNumberVaccinated)]), date), "days"),
                          NA_real_),
         Forecastprop=Forecast/Eligible)

Scotprop <- as.numeric(Scotdata %>% filter(date==max(date[!is.na(CumulativeNumberVaccinated)])) %>% 
                         summarise(value=CumulativeNumberVaccinated/Eligible))

agg_tiff("Outputs/COVIDBoostersScot.tiff", units="in", width=9, height=6, res=500)
ggplot(Scotdata %>% filter(date>=as.Date("2021-09-21")))+
  geom_line(aes(x=date, y=Eligible), colour="#CC3300")+
  geom_line(aes(x=date, y=CumulativeNumberVaccinated), colour="#006666")+
  scale_x_date(name="")+
  scale_y_continuous(name="Number of people", limits=c(0,NA))+
  theme_custom()+
  theme(plot.subtitle=element_markdown())+
  labs(title=paste0("Scotland has delivered booster jabs to ",round(Scotprop*100, 0),"% of eligible people"),
       subtitle="Total number of <span style='color:#CC3300;'>people eligible</span> and <span style='color:#006666;'>having received</span> a COVID booster jab in Scotland since bookings were opened on 21st September",
       caption="Data from Public Health Scotland | Plot by @VictimOfMaths")
dev.off()

#Comparison of both countries
agg_tiff("Outputs/COVIDBoostersEngScot.tiff", units="in", width=9, height=6, res=500)
ggplot()+
  geom_line(data=Engdata, aes(x=date, y=Boosterprop), colour="Red")+
  geom_line(data=Scotdata, aes(x=date, y=Boosterprop), colour="RoyalBlue")+
  scale_x_date(name="", limits=c(as.Date("2021-09-21"), as.Date("2021-11-15")))+
  scale_y_continuous(name="Proportion of eligible population who have received a booster", 
                     limits=c(0,1),
               labels=label_percent(accuracy=1))+
  theme_custom()+
  theme(plot.title=element_markdown())+
  labs(title="<span style='color:RoyalBlue;'>Scotland's</span> COVID booster coverage has overtaken <span style='color:Red;'>England</span>",
       subtitle="Proportion of people who received their 2nd COVID jab at least 6 months ago who have received a booster since bookings\nwere opened on 21st September. Scottish data is only available for more recent days.\nBooster data includes a small number of people with weakened immune systems who have been offered a 3rd primary dose.",
       caption="Data from Public Health Scotland, NHS England & coronavirus.data.gov.uk | Plot by @VictimOfMaths")
dev.off()

combined <- Engdata %>% 
  select(date, Eligible, Booster, Boosterprop, Forecast, Forecastprop) %>% 
  mutate(country="England") %>% 
  bind_rows(Scotdata %>% 
              select(date, Eligible, CumulativeNumberVaccinated, Boosterprop, Forecast, Forecastprop) %>% 
              rename("Booster"="CumulativeNumberVaccinated") %>% 
              mutate(country="Scotland")) %>% 
  filter(date>=as.Date("2021-09-21") & date<as.Date("2022-01-01")) 

agg_tiff("Outputs/COVIDBoostersEngScotForecasts.tiff", units="in", width=9, height=6, res=500)
ggplot(combined, aes(x=date, colour=country))+
  geom_line(aes(y=Boosterprop), show.legend=FALSE)+
  geom_line(aes(y=Forecastprop), linetype=2, show.legend=FALSE)+
  scale_x_date(name="")+
  scale_y_continuous(name="Proportion of eligible population who have received a booster", limits=c(0,NA),
                     labels=label_percent(accuracy=1))+
  scale_colour_manual(values=c("Red", "RoyalBlue"))+
  theme_custom()+
  theme(plot.title=element_markdown())+
  labs(title="<span style='color:Red;'>England</span> needs to speed up its booster rollout to keep up with <span style='color:RoyalBlue;'>Scotland</span>",
       subtitle="Proportion of people who received their 2nd COVID jab at least 6 months ago who have received a booster since bookings\nwere opened on 21st September (solid lines) and forecasts based on recent vaccination rates and the number of people\ndue to become eligible (dashed lines). Scottish data is only available for more recent days",
       caption="Data from Public Health Scotland, NHS England & coronavirus.data.gov.uk | Plot by @VictimOfMaths")

dev.off()

agg_tiff("Outputs/COVIDBoostersEngScotForecasts2.tiff", units="in", width=10, height=6, res=500)
ggplot(combined %>% mutate(Eligible=if_else(is.na(Booster) & date<as.Date("2021-10-15"), 0, as.numeric(Eligible))), 
       aes(x=date))+
  geom_col(aes(y=Eligible), fill="#D72000")+
  geom_col(aes(y=Booster), fill="#172869")+
  geom_col(aes(y=Forecast), fill="#088BBE")+
  scale_x_date(name="")+
  scale_y_continuous(name="Individuals")+
  scale_fill_manual(values=c("Red", "RoyalBlue"))+
  facet_wrap(~country, scales="free_y")+
  theme_custom()+
  theme(plot.subtitle=element_markdown())+
  labs(title="Both England and Scotland have accelerated their booster programmes",
       subtitle="Total number of <span style='color:#D72000;'>people eligible for a COVID booster</span> and the number of those who have <span style='color:#172869;'>already been vaccinated</span>, or <span style='color:#088BBE;'>will be in coming months</span><br>at current vaccination rates",
       caption="Data from Public Health Scotland, NHS England & coronavirus.data.gov.uk | Plot by @VictimOfMaths")
dev.off()
