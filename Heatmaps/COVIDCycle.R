rm(list=ls())

library(tidyverse)
library(curl)
library(lubridate)
library(ukcovid19) #remotes::install_github("publichealthengland/coronavirus-dashboard-api-R-sdk")
library(paletteer)
library(ggtext)

options(scipen=9999)

admissions <- get_data(filters="areaType=nation", structure=list(date="date",
                                                             name="areaName",
                                                             admissions="newAdmissions"))

admissions <- admissions %>% 
  mutate(date=as.Date(date)) %>% 
  filter(date>as.Date("2020-03-19"))

deaths <- get_data(filters="areaType=nation", structure=list(date="date",
                                                             name="areaName",
                                                             deaths="newDeaths28DaysByDeathDate"))

deaths <- deaths %>% 
  mutate(date=as.Date(date)) %>% 
  filter(date>as.Date("2020-03-19"))

data <- merge(admissions, deaths) %>% 
  mutate(days=yday(date), weekno=week(date), pop=case_when(
    name=="England" ~ 56286961,
    name=="Northern Ireland" ~ 1893667,
    name=="Scotland" ~ 5463300,
    name=="Wales" ~ 3152879),
    admrate=admissions*100000/pop, mortrate=deaths*100000/pop) %>% 
  arrange(date)

weekdata <- data %>% 
  group_by(weekno, name) %>% 
  summarise(admissions=mean(admissions), deaths=mean(deaths),
            admrate=mean(admrate), mortrate=mean(mortrate)) %>% 
  mutate(label=as.Date("2020-01-01")+days(weekno)*7-1, label=format(label, "%d %b")) %>% 
  ungroup() %>% 
  arrange(weekno)

#Faceted plot for UK nations
tiff("Outputs/COVIDCycleUK.tiff", units="in", width=12, height=8, res=500)
ggplot()+
  geom_path(data=data, aes(x=admrate, y=mortrate), alpha=0.1,
            arrow = arrow(type = "closed", angle = 30, length = unit(0.05, "inches")))+
  geom_path(data=weekdata, aes(x=admrate, y=mortrate), colour="tomato",
            arrow = arrow(type = "closed", angle = 30, length = unit(0.05, "inches")))+
  geom_text(data=weekdata, aes(x=admrate, y=mortrate, label=label), size=rel(2), colour="Grey40",
            vjust=-0.4)+
  scale_x_continuous(trans="log10", name="Daily COVID-19 admissions per 100,000 (log scale)")+
  scale_y_continuous(trans="log10", name="Daily COVID-19 deaths per 100,000 (log scale)")+
  facet_wrap(~name)+
  theme_classic()+
  theme(plot.subtitle=element_markdown(), plot.title=element_text(face="bold"),
        strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)))+
  labs(title="Breaking the cycle",
       subtitle="COVID-19 hospital admissions and deaths across the UK <span style='color:Grey60;'>by day</span> and <span style='color:tomato;'>the weekly average",
       caption="Inspired by @maartenzam | Data from coronavirus.data.gov.uk | Plot by @VictimOfMaths")

dev.off()

#Plot for England only
tiff("Outputs/COVIDCycleEng.tiff", units="in", width=8, height=6, res=500)
ggplot()+
  geom_path(data=subset(data, name=="England"), aes(x=admissions, y=deaths), alpha=0.1,
            arrow = arrow(type = "closed", angle = 30, length = unit(0.05, "inches")))+
  geom_path(data=subset(weekdata, name=="England"), aes(x=admissions, y=deaths), colour="tomato",
            arrow = arrow(type = "closed", angle = 30, length = unit(0.05, "inches")))+
  geom_text(data=subset(weekdata, name=="England"), aes(x=admissions, y=deaths, label=label), size=rel(2), colour="Grey40",
            vjust=-0.4)+
  scale_x_continuous(trans="log10", name="Daily COVID-19 admissions (log scale)")+
  scale_y_continuous(trans="log10", name="Daily COVID-19 deaths (log scale)")+
  theme_classic()+
  theme(plot.subtitle=element_markdown(), plot.title=element_text(face="bold"))+
  labs(title="Breaking the cycle",
       subtitle="COVID-19 hospital admissions and deaths in England <span style='color:Grey60;'>by day</span> and <span style='color:tomato;'>the weekly average",
       caption="Inspired by @maartenzam | Data from coronavirus.data.gov.uk | Plot by @VictimOfMaths")

dev.off()

#Plot for Wales only
tiff("Outputs/COVIDCycleWal.tiff", units="in", width=8, height=6, res=500)
ggplot()+
  geom_path(data=subset(data, name=="Wales"), aes(x=admissions, y=deaths), alpha=0.1,
            arrow = arrow(type = "closed", angle = 30, length = unit(0.05, "inches")))+
  geom_path(data=subset(weekdata, name=="Wales"), aes(x=admissions, y=deaths), colour="tomato",
            arrow = arrow(type = "closed", angle = 30, length = unit(0.05, "inches")))+
  geom_text(data=subset(weekdata, name=="Wales"), aes(x=admissions, y=deaths, label=label), size=rel(2), colour="Grey40",
            vjust=-0.4)+
  scale_x_continuous(trans="log10", name="Daily COVID-19 admissions (log scale)")+
  scale_y_continuous(trans="log10", name="Daily COVID-19 deaths (log scale)")+
  theme_classic()+
  theme(plot.subtitle=element_markdown(), plot.title=element_text(face="bold"))+
  labs(title="Breaking the cycle",
       subtitle="COVID-19 hospital admissions and deaths in Wales <span style='color:Grey60;'>by day</span> and <span style='color:tomato;'>the weekly average",
       caption="Inspired by @maartenzam | Data from coronavirus.data.gov.uk | Plot by @VictimOfMaths")

dev.off()

#Plot for Scotland only
tiff("Outputs/COVIDCycleSco.tiff", units="in", width=8, height=6, res=500)
ggplot()+
  geom_path(data=subset(data, name=="Scotland"), aes(x=admissions, y=deaths), alpha=0.1,
            arrow = arrow(type = "closed", angle = 30, length = unit(0.05, "inches")))+
  geom_path(data=subset(weekdata, name=="Scotland"), aes(x=admissions, y=deaths), colour="tomato",
            arrow = arrow(type = "closed", angle = 30, length = unit(0.05, "inches")))+
  geom_text(data=subset(weekdata, name=="Scotland"), aes(x=admissions, y=deaths, label=label), size=rel(2), colour="Grey40",
            vjust=-0.4)+
  scale_x_continuous(trans="log10", name="Daily COVID-19 admissions (log scale)")+
  scale_y_continuous(trans="log10", name="Daily COVID-19 deaths (log scale)")+
  theme_classic()+
  theme(plot.subtitle=element_markdown(), plot.title=element_text(face="bold"))+
  labs(title="Breaking the cycle",
       subtitle="COVID-19 hospital admissions and deaths in Scotland <span style='color:Grey60;'>by day</span> and <span style='color:tomato;'>the weekly average",
       caption="Inspired by @maartenzam | Data from coronavirus.data.gov.uk | Plot by @VictimOfMaths")

dev.off()

#Plot for NI only
tiff("Outputs/COVIDCycleNI.tiff", units="in", width=8, height=6, res=500)
ggplot()+
  geom_path(data=subset(data, name=="Northern Ireland"), aes(x=admissions, y=deaths), alpha=0.1,
            arrow = arrow(type = "closed", angle = 30, length = unit(0.05, "inches")))+
  geom_path(data=subset(weekdata, name=="Northern Ireland"), aes(x=admissions, y=deaths), colour="tomato",
            arrow = arrow(type = "closed", angle = 30, length = unit(0.05, "inches")))+
  geom_text(data=subset(weekdata, name=="Northern Ireland"), aes(x=admissions, y=deaths, label=label), size=rel(2), colour="Grey40",
            vjust=-0.4)+
  scale_x_continuous(trans="log10", name="Daily COVID-19 admissions (log scale)")+
  scale_y_continuous(trans="log10", name="Daily COVID-19 deaths (log scale)")+
  theme_classic()+
  theme(plot.subtitle=element_markdown(), plot.title=element_text(face="bold"))+
  labs(title="Breaking the cycle",
       subtitle="COVID-19 hospital admissions and deaths in Northern Ireland <span style='color:Grey60;'>by day</span> and <span style='color:tomato;'>the weekly average",
       caption="Inspired by @maartenzam | Data from coronavirus.data.gov.uk | Plot by @VictimOfMaths")

dev.off()

