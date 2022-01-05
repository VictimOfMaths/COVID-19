rm(list=ls())

#Code to replicate this nice graph from Paul Mainwood
#https://twitter.com/PaulMainwood/status/1449467211442098181

library(tidyverse)
library(curl)
library(lubridate)
library(RcppRoll)
library(scales)
library(extrafont)
library(paletteer)
library(ggtext)
library(ragg)
library(geofacet)

theme_custom <- function() {
  theme_classic() %+replace%
    theme(plot.title.position="plot", plot.caption.position="plot",
          strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)),
          plot.title=element_text(face="bold", size=rel(1.5), hjust=0,
                                  margin=margin(0,0,5.5,0)),
          text=element_text(family="Lato"))
}

#UK version

#Download data from dashboard
url <- "https://api.coronavirus.data.gov.uk/v2/data?areaType=overview&metric=newAdmissions&metric=newCasesBySpecimenDate&metric=newDeaths28DaysByDeathDate&format=csv"
temp <- tempfile()
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

#Read the data in and tidy it up
data <- read.csv(temp) %>% 
  select(-c(1:3)) %>% 
  set_names(c("date", "Admissions", "Cases", "Deaths")) %>% 
  gather(metric, count, c(2:4)) %>% 
  mutate(date=as.Date(date),
         metric=factor(metric, levels=c("Cases", "Admissions", "Deaths"))) %>% 
  group_by(metric) %>% 
  #Calculate rolling means
  mutate(count_roll=roll_mean(count, 7, align="center", fill=NA)) %>% 
  #Normalise against the peak.
  #Step 1 - grab the peak value for each metric
  #Option 1, just using the max value across the entire time series
  #mutate(max=max(count, na.rm=TRUE)) %>% 
  #Option 2, normalising against the max value within a chosen date range
  mutate(max=max(count_roll[date>as.Date("2020-08-01") & date<as.Date("2021-05-01")], 
                 na.rm=TRUE)) %>% 
  ungroup() %>% 
  #Step 2 - calculate each date's value as a proportion of the peak
  mutate(prop=count_roll/max)

#Simple replication
ggplot(data, aes(x=date, y=prop, colour=metric))+
  geom_line()

#Prettier version (because why not)

agg_tiff("Outputs/COVIDMetricsNormalised.tiff", units="in", width=9, height=6, res=500)
ggplot(data %>% filter(date>as.Date("2020-04-01")), 
       aes(x=date, y=prop, colour=metric))+
  geom_line(show.legend=FALSE)+
  scale_x_date(name="", date_labels="%B %y")+
  scale_y_continuous(labels=label_percent(accuracy=1), 
                     name="Level as a proportion of January 2021 peak")+
  scale_colour_paletteer_d("wesanderson::Darjeeling1")+
  theme_classic()+
  theme(text=element_text(family="Lato"), plot.title.position="plot",
        plot.title=element_text(face="bold", size=rel(1.5)),
        plot.subtitle=element_markdown(),
        panel.grid.major.y=element_line(colour="Grey90"))+
  labs(title="Vaccination is keeping admissions and deaths lower than previous waves",
       subtitle="Rolling 7-day average of new COVID <span style='color:#FF0000;'>cases</span>, <span style='color:#00A08A;'>admissions</span> and <span style='color:#F2AD00;'>deaths</span> as a proportion of their peak value in January 2021",
       caption="Plot inspired by @PaulMainwood | Data from coronavirus.data.gov.uk | Plot by @VictimOfMaths")
dev.off()

#Now replicate for the USA using Our World in Data data (the CDC website makes it bafflingly hard
#to machine read their data and their API confuses my simple brain - long live the UK dashboard)
url.us <- "https://covid.ourworldindata.org/data/owid-covid-data.csv"
temp <- curl_download(url=url.us, destfile=temp, quiet=FALSE, mode="wb")

usukdata <- read.csv(temp) %>% 
  filter(iso_code %in% c("USA", "GBR")) %>% 
  select(location, date, new_cases, new_deaths, hosp_patients) %>% 
  mutate(date=as.Date(date)) %>% 
  group_by(location) %>% 
  arrange(date) %>% 
  mutate(Cases=roll_mean(new_cases, 7, align="center", fill=NA),
         Admissions=roll_mean(new_deaths, 7, align="center", fill=NA),
         Deaths=roll_mean(hosp_patients, 7, align="center", fill=NA)) %>% 
  ungroup() %>% 
  gather(metric, count, c(6:8)) %>% 
  mutate(metric=factor(metric, levels=c("Cases", "Admissions", "Deaths"))) %>% 
  group_by(location, metric) %>% 
  #Normalise against the peak.
  #Step 1 - grab the peak value for each metric
  #Option 1, just using the max value across the entire time series
  #mutate(max=max(count, na.rm=TRUE)) %>% 
  #Option 2, normalising against the max value within a chosen date range
  mutate(max=max(count[date>as.Date("2020-08-01") & date<as.Date("2021-05-01")], 
                 na.rm=TRUE)) %>% 
  ungroup() %>% 
  #Step 2 - calculate each date's value as a proportion of the peak
  mutate(prop=count/max)
  
agg_tiff("Outputs/COVIDMetricsNormalisedUKUSA.tiff", units="in", width=12, height=6, res=500)
ggplot(usukdata %>% filter(date>as.Date("2020-04-01")), 
       aes(x=date, y=prop, colour=metric))+
  geom_line(show.legend=FALSE)+
  scale_x_date(name="", date_labels="%B %y")+
  scale_y_continuous(labels=label_percent(accuracy=1), 
                     name="Level as a proportion of January 2021 peak")+
  scale_colour_paletteer_d("wesanderson::Darjeeling1")+
  facet_wrap(~location)+
  theme_classic()+
  theme(text=element_text(family="Lato"), plot.title.position="plot",
        plot.title=element_text(face="bold", size=rel(1.5)),
        plot.subtitle=element_markdown(),
        panel.grid.major.y=element_line(colour="Grey90"),
        strip.background=element_blank(),
        strip.text=element_text(face="bold", size=rel(1)))+
  labs(title="The US's Delta wave is far more deadly than the UK's",
       subtitle="Rolling 7-day average of new COVID <span style='color:#FF0000;'>cases</span>, <span style='color:#00A08A;'>hospital bed occupancy</span> and <span style='color:#F2AD00;'>deaths</span> as a proportion of their peak value in January 2021",
       caption="Plot inspired by @PaulMainwood | Data from Our World In Data | Plot by @VictimOfMaths")
dev.off()

################
#Version by region of England
url <- "https://api.coronavirus.data.gov.uk/v2/data?areaType=region&metric=newCasesBySpecimenDate&metric=newDeaths28DaysByDeathDate&format=csv"
temp <- tempfile()
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

regdata1 <- read.csv(temp) %>% 
  select(-c(1,3)) %>% 
  set_names(c("Region", "date", "Cases", "Deaths")) %>% 
  gather(metric, count, c(3:4)) %>% 
  mutate(date=as.Date(date)) %>% 
  group_by(metric, Region) %>% 
  #Calculate rolling means
  mutate(count_roll=roll_mean(count, 7, align="center", fill=NA)) %>% 
  #Normalise against the peak.
  #Step 1 - grab the peak value for each metric
  #Option 1, just using the max value across the entire time series
  #mutate(max=max(count, na.rm=TRUE)) %>% 
  #Option 2, normalising against the max value within a chosen date range
  mutate(max=max(count_roll[date>as.Date("2020-08-01") & date<as.Date("2021-05-01")], 
                 na.rm=TRUE)) %>% 
  ungroup() %>% 
  #Step 2 - calculate each date's value as a proportion of the peak
  mutate(prop=count_roll/max)

mygrid <- data.frame(name=c("North East", "North West", "Yorkshire and The Humber",
                            "West Midlands", "East Midlands", "East of England",
                            "South West", "London", "South East"),
                     row=c(1,2,2,3,3,3,4,4,4), col=c(2,1,2,1,2,3,1,2,3),
                     code=c(1:9))

agg_tiff("Outputs/COVIDMetricsNormalisedReg.tiff", units="in", width=9, height=6, res=500)
ggplot(regdata1 %>% filter(date>as.Date("2020-04-01")), 
       aes(x=date, y=prop, colour=metric))+
  geom_line(show.legend=FALSE)+
  scale_x_date(name="", date_labels="%B %y")+
  scale_y_continuous(labels=label_percent(accuracy=1), 
                     name="Level as a proportion of January 2021 peak")+
  scale_colour_manual(values=c("#FF0000", "#F2AD00"))+
  facet_geo(~Region, grid=mygrid)+
  theme_custom()+
  theme(plot.subtitle=element_markdown(),
        panel.grid.major.y=element_line(colour="Grey90"))+
  labs(title="Vaccination is keeping deaths lower than previous waves",
       subtitle="Rolling 7-day average of new COVID <span style='color:#FF0000;'>cases</span> and <span style='color:#F2AD00;'>deaths</span> as a proportion of their peak value in January 2021 in English regions",
       caption="Plot inspired by @PaulMainwood | Data from coronavirus.data.gov.uk | Plot by @VictimOfMaths")

dev.off()

#Admissions only available by NHS region
url <- "https://api.coronavirus.data.gov.uk/v2/data?areaType=nhsRegion&metric=newAdmissions&format=csv"
temp <- tempfile()
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

regdata2 <- read.csv(temp) %>% 
  select(-c(1,3)) %>% 
  set_names(c("Region", "date", "count")) %>% 
  mutate(date=as.Date(date), metric="Admissions") %>% 
  group_by(Region) %>% 
  #Calculate rolling means
  mutate(count_roll=roll_mean(count, 7, align="center", fill=NA)) %>% 
  #Normalise against the peak.
  #Step 1 - grab the peak value for each metric
  #Option 1, just using the max value across the entire time series
  #mutate(max=max(count, na.rm=TRUE)) %>% 
  #Option 2, normalising against the max value within a chosen date range
  mutate(max=max(count_roll[date>as.Date("2020-08-01") & date<as.Date("2021-05-01")], 
                 na.rm=TRUE)) %>% 
  ungroup() %>% 
  #Step 2 - calculate each date's value as a proportion of the peak
  mutate(prop=count_roll/max)

#Stick together (ignoring differences in regional boundaries)
regdata <- bind_rows(regdata1, regdata2) %>% 
  mutate(metric=factor(metric, levels=c("Cases", "Admissions", "Deaths")))

agg_tiff("Outputs/COVIDMetricsNormalisedLondon.tiff", units="in", width=9, height=6, res=500)
ggplot(regdata %>% filter(Region=="London" & date>as.Date("2020-04-01")), 
       aes(x=date, y=prop, colour=metric))+
  geom_line(show.legend=FALSE)+
  scale_x_date(name="", date_labels="%B %y")+
  scale_y_continuous(labels=label_percent(accuracy=1), 
                     name="Level as a proportion of January 2021 peak")+
  scale_colour_paletteer_d("wesanderson::Darjeeling1")+
  theme_classic()+
  theme(text=element_text(family="Lato"), plot.title.position="plot",
        plot.title=element_text(face="bold", size=rel(1.5)),
        plot.subtitle=element_markdown(),
        panel.grid.major.y=element_line(colour="Grey90"))+
  labs(title="Hospital admissions in London *might* have peaked 🤞",
       subtitle="Rolling 7-day average of new COVID <span style='color:#FF0000;'>cases</span>, <span style='color:#00A08A;'>admissions</span> and <span style='color:#F2AD00;'>deaths</span> in London* as a proportion of their peak value in January 2021",
       caption="Plot inspired by @PaulMainwood | Data from coronavirus.data.gov.uk | Plot by @VictimOfMaths\n\n*Admissions data is based on the London NHS region, which does not exactly match the government region that cases and deaths data is based on")

dev.off()
