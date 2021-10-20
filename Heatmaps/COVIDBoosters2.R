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
temp <- tempfile()

daily19th <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/10/COVID-19-daily-announced-vaccinations-19-October-2021.xlsx"

temp <- curl_download(url=daily19th, destfile=temp, quiet=FALSE, mode="wb")

dailydata <- read_excel(temp, sheet=1, range="B14:F14", col_names=FALSE) %>% 
  select(1,5) %>% 
  set_names(c("Region", "Booster")) %>% 
  mutate(date=as.Date("2021-10-18"), Region=if_else(Region=="England4", "England", Region))

daily18th <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/10/COVID-19-daily-announced-vaccinations-18-October-2021.xlsx"

temp <- curl_download(url=daily18th, destfile=temp, quiet=FALSE, mode="wb")

dailydata <- dailydata %>% bind_rows(read_excel(temp, sheet=1, range="B14:F14", col_names=FALSE) %>% 
  select(1,5) %>% 
  set_names(c("Region", "Booster")) %>% 
  mutate(date=as.Date("2021-10-17"), Region=if_else(Region=="England4", "England", Region)))

daily17th <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/10/COVID-19-daily-announced-vaccinations-17-October-2021.xlsx"

temp <- curl_download(url=daily17th, destfile=temp, quiet=FALSE, mode="wb")

dailydata <- dailydata %>% 
  bind_rows(read_excel(temp, sheet=1, range="B14:F14", col_names=FALSE) %>% 
              select(1,5) %>% 
              set_names(c("Region", "Booster")) %>% 
              mutate(date=as.Date("2021-10-16"), Region=if_else(Region=="England4", "England", Region)))

daily16th <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/10/COVID-19-daily-announced-vaccinations-16-October-2021.xlsx"

temp <- curl_download(url=daily16th, destfile=temp, quiet=FALSE, mode="wb")

dailydata <- dailydata %>% 
  bind_rows(read_excel(temp, sheet=1, range="B13:F13", col_names=FALSE) %>% 
              select(1,5) %>% 
              set_names(c("Region", "Booster")) %>% 
              mutate(date=as.Date("2021-10-15"), Region=if_else(Region=="England4", "England", Region)))

daily15th <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/10/COVID-19-daily-announced-vaccinations-15-October-2021.xlsx"

temp <- curl_download(url=daily15th, destfile=temp, quiet=FALSE, mode="wb")

dailydata <- dailydata %>% 
  bind_rows(read_excel(temp, sheet=1, range="B13:F13", col_names=FALSE) %>% 
              select(1,5) %>% 
              set_names(c("Region", "Booster")) %>% 
              mutate(date=as.Date("2021-10-14"), Region=if_else(Region=="England4", "England", Region)))

daily14th <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/10/COVID-19-daily-announced-vaccinations-14-October-2021.xlsx"

temp <- curl_download(url=daily14th, destfile=temp, quiet=FALSE, mode="wb")

dailydata <- dailydata %>% 
  bind_rows(read_excel(temp, sheet=1, range="B14:F14", col_names=FALSE) %>% 
              select(1,5) %>% 
              set_names(c("Region", "Booster")) %>% 
              mutate(date=as.Date("2021-10-13"), Region=if_else(Region=="England4", "England", Region)))

daily13th <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/10/COVID-19-daily-announced-vaccinations-13-October-2021.xlsx"

temp <- curl_download(url=daily13th, destfile=temp, quiet=FALSE, mode="wb")

dailydata <- dailydata %>% 
  bind_rows(read_excel(temp, sheet=1, range="B14:F14", col_names=FALSE) %>% 
              select(1,5) %>% 
              set_names(c("Region", "Booster")) %>% 
              mutate(date=as.Date("2021-10-12"), Region=if_else(Region=="England4", "England", Region)))

daily12th <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/10/COVID-19-daily-announced-vaccinations-12-October-2021.xlsx"

temp <- curl_download(url=daily12th, destfile=temp, quiet=FALSE, mode="wb")

dailydata <- dailydata %>% 
  bind_rows(read_excel(temp, sheet=1, range="B13:F13", col_names=FALSE) %>% 
              select(1,5) %>% 
              set_names(c("Region", "Booster")) %>% 
              mutate(date=as.Date("2021-10-11"), Region=if_else(Region=="England4", "England", Region)))

daily11th <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/10/COVID-19-daily-announced-vaccinations-11-October-2021.xlsx"

temp <- curl_download(url=daily11th, destfile=temp, quiet=FALSE, mode="wb")

dailydata <- dailydata %>% 
  bind_rows(read_excel(temp, sheet=1, range="B14:F14", col_names=FALSE) %>% 
              select(1,5) %>% 
              set_names(c("Region", "Booster")) %>% 
              mutate(date=as.Date("2021-10-10"), Region=if_else(Region=="England4", "England", Region)))

daily10th <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/10/COVID-19-daily-announced-vaccinations-10-October-2021.xlsx"

temp <- curl_download(url=daily10th, destfile=temp, quiet=FALSE, mode="wb")

dailydata <- dailydata %>% 
  bind_rows(read_excel(temp, sheet=1, range="B13:F13", col_names=FALSE) %>% 
              select(1,5) %>% 
              set_names(c("Region", "Booster")) %>% 
              mutate(date=as.Date("2021-10-09"), Region=if_else(Region=="England4", "England", Region)))

daily9th <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/10/COVID-19-daily-announced-vaccinations-09-October-2021.xlsx"

temp <- curl_download(url=daily9th, destfile=temp, quiet=FALSE, mode="wb")

dailydata <- dailydata %>% 
  bind_rows(read_excel(temp, sheet=1, range="B14:F14", col_names=FALSE) %>% 
              select(1,5) %>% 
              set_names(c("Region", "Booster")) %>% 
              mutate(date=as.Date("2021-10-08"), Region=if_else(Region=="England4", "England", Region)))

daily8th <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/10/COVID-19-daily-announced-vaccinations-08-October-2021.xlsx"

temp <- curl_download(url=daily8th, destfile=temp, quiet=FALSE, mode="wb")

dailydata <- dailydata %>% 
  bind_rows(read_excel(temp, sheet=1, range="B13:F13", col_names=FALSE) %>% 
              select(1,5) %>% 
              set_names(c("Region", "Booster")) %>% 
              mutate(date=as.Date("2021-10-07"), Region=if_else(Region=="England4", "England", Region)))

daily7th <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/10/COVID-19-daily-announced-vaccinations-07-October-2021.xlsx"

temp <- curl_download(url=daily7th, destfile=temp, quiet=FALSE, mode="wb")

dailydata <- dailydata %>% 
  bind_rows(read_excel(temp, sheet=1, range="B14:F14", col_names=FALSE) %>% 
              select(1,5) %>% 
              set_names(c("Region", "Booster")) %>% 
              mutate(date=as.Date("2021-10-06"), Region=if_else(Region=="England4", "England", Region)))

daily6th <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/10/COVID-19-daily-announced-vaccinations-06-October-2021.xlsx"

temp <- curl_download(url=daily6th, destfile=temp, quiet=FALSE, mode="wb")

dailydata <- dailydata %>% 
  bind_rows(read_excel(temp, sheet=1, range="B14:F14", col_names=FALSE) %>% 
              select(1,5) %>% 
              set_names(c("Region", "Booster")) %>% 
              mutate(date=as.Date("2021-10-05"), Region=if_else(Region=="England4", "England", Region)))

daily5th <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/10/COVID-19-daily-announced-vaccinations-05-October-2021.xlsx"

temp <- curl_download(url=daily5th, destfile=temp, quiet=FALSE, mode="wb")

dailydata <- dailydata %>% 
  bind_rows(read_excel(temp, sheet=1, range="B14:F14", col_names=FALSE) %>% 
              select(1,5) %>% 
              set_names(c("Region", "Booster")) %>% 
              mutate(date=as.Date("2021-10-04"), Region=if_else(Region=="England4", "England", Region)))

daily4th <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/10/COVID-19-daily-announced-vaccinations-04-October-2021.xlsx"

temp <- curl_download(url=daily4th, destfile=temp, quiet=FALSE, mode="wb")

dailydata <- dailydata %>% 
  bind_rows(read_excel(temp, sheet=1, range="B14:F14", col_names=FALSE) %>% 
              select(1,5) %>% 
              set_names(c("Region", "Booster")) %>% 
              mutate(date=as.Date("2021-10-03"), Region=if_else(Region=="England4", "England", Region)))

daily3rd <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/10/COVID-19-daily-announced-vaccinations-03-October-2021.xlsx"

temp <- curl_download(url=daily3rd, destfile=temp, quiet=FALSE, mode="wb")

dailydata <- dailydata %>% 
  bind_rows(read_excel(temp, sheet=1, range="B13:F13", col_names=FALSE) %>% 
              select(1,5) %>% 
              set_names(c("Region", "Booster")) %>% 
              mutate(date=as.Date("2021-10-02"), Region=if_else(Region=="England4", "England", Region)))

daily2nd <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/10/COVID-19-daily-announced-vaccinations-02-October-2021.xlsx"

temp <- curl_download(url=daily2nd, destfile=temp, quiet=FALSE, mode="wb")

dailydata <- dailydata %>% 
  bind_rows(read_excel(temp, sheet=1, range="B14:F14", col_names=FALSE) %>% 
              select(1,5) %>% 
              set_names(c("Region", "Booster")) %>% 
              mutate(date=as.Date("2021-10-01"), Region=if_else(Region=="England4", "England", Region)))

daily1st <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/10/COVID-19-daily-announced-vaccinations-01-October-2021.xlsx"

temp <- curl_download(url=daily1st, destfile=temp, quiet=FALSE, mode="wb")

dailydata <- dailydata %>% 
  bind_rows(read_excel(temp, sheet=1, range="B14:F14", col_names=FALSE) %>% 
              select(1,5) %>% 
              set_names(c("Region", "Booster")) %>% 
              mutate(date=as.Date("2021-09-30"), Region=if_else(Region=="England4", "England", Region)))

#Bring in eligible population
Engurl <- "https://api.coronavirus.data.gov.uk/v2/data?areaType=nation&areaCode=E92000001&metric=cumPeopleVaccinatedSecondDoseByVaccinationDate&format=csv"
temp <- curl_download(url=Engurl, destfile=temp, quiet=FALSE, mode="wb")

Engdata <- read.csv(temp) %>% 
  select(4,5) %>% 
  set_names("date", "Eligible") %>% 
  mutate(date=as.Date(date)+days(182)) %>% 
  merge(dailydata, by="date", all.x=TRUE) %>% 
  mutate(EligibleUnvax=Eligible-Booster,
         Boosterprop=Booster/Eligible,
         Forecast=if_else(date>max(date[!is.na(Booster)]), 
                          Booster[date==max(date[!is.na(Booster)])]+
                            166451*as.numeric(interval(max(date[!is.na(Booster)]), date), "days"),
                          NA_real_),
         Forecastprop=Forecast/Eligible)

agg_tiff("Outputs/COVIDBoostersEng.tiff", units="in", width=9, height=6, res=500)
ggplot(Engdata %>% filter(date>=as.Date("2021-09-21") & date<=max(date[!is.na(Booster)])))+
  geom_line(aes(x=date, y=Eligible), colour="#CC3300")+
  geom_line(aes(x=date, y=Booster), colour="#006666")+
  scale_x_date(name="")+
  scale_y_continuous(name="Number of people", limits=c(0,NA))+
  theme_custom()+
  theme(plot.subtitle=element_markdown())+
  labs(title="England has delivered booster jabs to 43% of eligible people",
       subtitle="Total number of <span style='color:#CC3300;'>people eligible</span> and <span style='color:#006666;'>having received</span> a COVID booster jab in Scotland since bookings were opened on 21st September",
       caption="Data from NHS England & coronavirus.data.gov.uk | Plot by @VictimOfMaths")
dev.off()

#Total Scottish doses
Scotdoseurl <- "https://www.opendata.nhs.scot/dataset/6dbdd466-45e3-4348-9ee3-1eac72b5a592/resource/42f17a3c-a4db-4965-ba68-3dffe6bca13a/download/daily_vacc_scot_20211018.csv"
temp <- tempfile()
temp <- curl_download(url=Scotdoseurl, destfile=temp, quiet=FALSE, mode="wb")

ScotDoses <- read.csv(temp) %>% 
  filter(Product=="Total" & AgeBand=="16 years and over") %>% 
  mutate(date=as.Date(as.character(Date), format="%Y%m%d"))

ggplot(ScotDoses, aes(x=date, y=CumulativeNumberVaccinated, colour=Dose))+
  geom_line()

Scotdata <- ScotDoses %>% 
  filter(Dose=="Dose 2") %>% 
  mutate(date=date+days(182)) %>% 
  select(date, CumulativeNumberVaccinated) %>% 
  rename("Eligible"="CumulativeNumberVaccinated") %>% 
  merge(ScotDoses %>% filter(Dose=="Booster"), by="date", all.x=TRUE) %>% 
  filter(date>=max(date[Eligible==0])) %>% 
  mutate(EligibleUnvax=Eligible-CumulativeNumberVaccinated,
         Boosterprop=CumulativeNumberVaccinated/Eligible,      
         Forecast=if_else(date>max(date[!is.na(CumulativeNumberVaccinated)]), 
                          CumulativeNumberVaccinated[date==max(date[!is.na(CumulativeNumberVaccinated)])]+ 
                            20718*as.numeric(interval(max(date[!is.na(CumulativeNumberVaccinated)]), date), "days"),
                          NA_real_),
         Forecastprop=Forecast/Eligible)

agg_tiff("Outputs/COVIDBoostersScot.tiff", units="in", width=9, height=6, res=500)
ggplot(Scotdata %>% filter(date>=as.Date("2021-09-21")))+
  geom_line(aes(x=date, y=Eligible), colour="#CC3300")+
  geom_line(aes(x=date, y=CumulativeNumberVaccinated), colour="#006666")+
  scale_x_date(name="")+
  scale_y_continuous(name="Number of people", limits=c(0,NA))+
  theme_custom()+
  theme(plot.subtitle=element_markdown())+
  labs(title="Scotland has delivered booster jabs to 41% of eligible people",
       subtitle="Total number of <span style='color:#CC3300;'>people eligible</span> and <span style='color:#006666;'>having received</span> a COVID booster jab in Scotland since bookings were opened on 21st September",
       caption="Data from Public Health Scotland | Plot by @VictimOfMaths")
dev.off()

#Comparison of both countries
agg_tiff("Outputs/COVIDBoostersEngScot.tiff", units="in", width=9, height=6, res=500)
ggplot()+
  geom_line(data=Engdata, aes(x=date, y=Boosterprop), colour="Red")+
  geom_line(data=Scotdata, aes(x=date, y=Boosterprop), colour="RoyalBlue")+
  scale_x_date(name="", limits=c(as.Date("2021-09-21"), NA_Date_))+
  scale_y_continuous(name="Proportion of eligible population who have received a booster", limits=c(0,NA),
               labels=label_percent(accuracy=1))+
  theme_custom()+
  theme(plot.title=element_markdown())+
  labs(title="<span style='color:Red;'>England</span> has better COVID booster coverage than <span style='color:RoyalBlue;'>Scotland</span> (for now).",
       subtitle="Proportion of people who received their 2nd COVID jab at least 6 months ago who have received a booster since bookings\nwere opened on 21st September. Scottish data is only available for more recent days",
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
  labs(title="<span style='color:RoyalBlue;'>Scotland</span> could overtake <span style='color:Red;'>England</span> in terms of Booster coverage within a few weeks",
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
  labs(title="Both England and Scotland could do with accelerating their booster programmes",
       subtitle="Total number of <span style='color:#D72000;'>people eligible for a COVID booster</span> and the number of those who have <span style='color:#172869;'>already been vaccinated</span>, or <span style='color:#088BBE;'>will be in coming months</span><br>at current vaccination rates",
       caption="Data from Public Health Scotland, NHS England & coronavirus.data.gov.uk | Plot by @VictimOfMaths")
dev.off()
