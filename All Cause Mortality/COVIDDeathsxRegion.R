rm(list=ls())

library(tidyverse)
library(ukcovid19) #remotes::install_github("publichealthengland/coronavirus-dashboard-api-R-sdk")
library(paletteer)
library(RcppRoll)
library(lubridate)

#Get daily regional death data
deaths.reg <- get_data(filters="areaType=region", structure=list(date="date",
                                                                 name="areaName",
                                                                 deaths="newDeaths28DaysByDeathDate"))

#Bring in regional population
deaths.reg <- deaths.reg %>% 
  mutate(pop=case_when(
    name=="East of England" ~ 6236072,
    name=="London" ~ 8961989,
    name=="West Midlands" ~ 5934037,
    name=="East Midlands" ~ 4835928,
    name=="North East" ~ 2669941,
    name=="Yorkshire and The Humber" ~ 5502967,
    name=="North West" ~ 7341196,
    name=="South East" ~ 9180135,
    name=="South West" ~ 5624696),
    mortrate=deaths*100000/pop,
    date=as.Date(date),
    mortrateroll=roll_mean(mortrate, 7, align="center", fill=NA)) %>% 
  arrange(date)

maxdate <- max(deaths.reg$date)

tiff("Outputs/COVIDDeathsxRegionLine.tiff", units="in", width=10, height=7, res=500)
deaths.reg %>% 
  filter(date<maxdate-days(3)) %>% 
ggplot()+
  geom_line(aes(x=date, y=mortrateroll, colour=name))+
  theme_classic()+
  scale_x_date(name="")+
  scale_y_continuous(name="Daily COVID-19 deaths per 100,000")+
  scale_colour_paletteer_d("LaCroixColoR::paired", name="Region")+
  labs(title="The 'second wave' is more geographically unequal",
       subtitle="Daily deaths per 100,000 within 28 days of a positive COVID-19 test",
       caption="Data from PHE | Plot by @VictimOfMaths")
dev.off()

#Pull out peak Spring and Autumn deaths for each region
peaks <- deaths.reg %>% 
  mutate(period=if_else(date<as.Date("2020-08-01"), "Wave 1", "Wave 2")) %>% 
  group_by(name, period) %>% 
  summarise(peak=max(mortrateroll, na.rm=TRUE)) %>% 
  spread(period, peak)

tiff("Outputs/COVIDDeathsxRegionScatter.tiff", units="in", width=8, height=6, res=500)
ggplot(peaks)+
  geom_point(aes(x=`Wave 1`, y=`Wave 2`, colour=name))+
  geom_abline(intercept=0)+
  scale_colour_paletteer_d("LaCroixColoR::paired", name="Region")+
  theme_classic()+
  scale_x_continuous(limits=c(0,2.5), name="Peak Spring mortality rate\n7-day rolling average")+
  scale_y_continuous(limits=c(0,2.5), name="Peak Autumn mortality rate\n7-day rolling average")+
  labs(title="Correlation between the Spring and Autumn peaks is fairly weak",
       subtitle="Deaths per 100,000 within 28 days of a positive COVID-19 test",
       caption="Date from PHE | Plot by @VictimOfMaths")
dev.off()

