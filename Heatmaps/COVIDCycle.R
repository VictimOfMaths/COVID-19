rm(list=ls())

library(tidyverse)
library(curl)
library(lubridate)
library(paletteer)
library(ggtext)

temp <- tempfile()
source <- "https://api.coronavirus.data.gov.uk/v1/data?filters=areaType=nation&structure=%7B%22areaType%22:%22areaType%22,%22areaName%22:%22areaName%22,%22areaCode%22:%22areaCode%22,%22date%22:%22date%22,%22newAdmissions%22:%22newAdmissions%22,%22cumAdmissions%22:%22cumAdmissions%22%7D&format=csv"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
admissions <- read.csv(temp)

admissions <- admissions %>% 
  filter(areaName=="England") %>% 
  mutate(date=as.Date(date)) %>% 
  filter(date>as.Date("2020-03-19"))


temp <- tempfile()
source <- "https://api.coronavirus.data.gov.uk/v1/data?filters=areaType=nation&structure=%7B%22areaType%22:%22areaType%22,%22areaName%22:%22areaName%22,%22areaCode%22:%22areaCode%22,%22date%22:%22date%22,%22newDeaths28DaysByDeathDate%22:%22newDeaths28DaysByDeathDate%22,%22cumDeaths28DaysByDeathDate%22:%22cumDeaths28DaysByDeathDate%22%7D&format=csv"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
deaths <- read.csv(temp)

deaths <- deaths %>% 
  filter(areaName=="England") %>% 
  mutate(date=as.Date(date)) %>% 
  filter(date>as.Date("2020-03-19"))

data <- merge(admissions, deaths) %>% 
  mutate(days=yday(date), weekno=week(date))

weekdata <- data %>% 
  group_by(weekno) %>% 
  summarise(admissions=mean(newAdmissions), deaths=mean(newDeaths28DaysByDeathDate)) %>% 
  mutate(label=as.Date("2020-01-01")+days(weekno)*7-1, label=format(label, "%d %b"))

tiff("Outputs/COVIDCycle.tiff", units="in", width=8, height=6, res=500)
ggplot()+
  geom_path(data=data, aes(x=newAdmissions, y=newDeaths28DaysByDeathDate), alpha=0.1,
            arrow = arrow(type = "closed", angle = 30, length = unit(0.05, "inches")))+
  geom_path(data=weekdata, aes(x=admissions, y=deaths), colour="tomato",
            arrow = arrow(type = "closed", angle = 30, length = unit(0.05, "inches")))+
  geom_text(data=weekdata, aes(x=admissions, y=deaths, label=label), size=rel(2), colour="Grey40",
            vjust=-0.4)+
  scale_x_continuous(trans="log10", name="Daily COVID-19 admissions (log scale)")+
  scale_y_continuous(trans="log10", name="Daily COVID-19 deaths (log scale)")+
  theme_classic()+
  theme(plot.subtitle=element_markdown(), plot.title=element_text(face="bold"))+
  labs(title="Breaking the cycle",
       subtitle="COVID-19 hospital admissions and deaths in England <span style='color:Grey60;'>by day</span> and <span style='color:tomato;'>the weekly average",
       caption="Inspired by @maartenzam | Data from coronavirus.data.gov.uk | Plot by @VictimOfMaths")
dev.off()