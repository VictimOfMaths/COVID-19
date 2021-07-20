rm(list=ls())

library(tidyverse)
library(curl)
library(lubridate)
library(paletteer)
library(ggrepel)
library(extrafont)
library(ragg)
library(ggtext)
library(patchwork)

theme_custom <- function() {
  theme_classic() %+replace%
    theme(plot.title.position="plot", plot.caption.position="plot",
          strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)),
          plot.title=element_text(face="bold", size=rel(1.5), hjust=0,
                                  margin=margin(0,0,5.5,0)),
          text=element_text(family="Lato"))
}

source <- "https://api.coronavirus.data.gov.uk/v2/data?areaType=nation&metric=newAdmissionsRollingRate&metric=newCasesBySpecimenDateRollingRate&metric=newDeaths28DaysByDeathDateRollingRate&format=csv"

threshold <- 100

temp <- tempfile()
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

rawdata <- read.csv(temp) %>% 
  mutate(date=as.Date(date)) %>% 
  set_names(c("code", "name", "type", "date", "admissions", "cases", "deaths"))

ggplot(rawdata, aes(x=date, y=cases, group=name))+
  geom_line()+
  geom_hline(yintercept=threshold)

#Pick out date cases first exceeded 100/100,000 in 7-day period since 1st May
data <- rawdata %>%
  merge(rawdata %>% 
          group_by(name, code) %>% 
          filter(date>as.Date("2021-05-01") & cases>threshold) %>% 
          summarise(startdatew4=min(date)) %>% 
          ungroup() %>% 
          select(code, startdatew4), by="code", all.x=TRUE) %>% 
  merge(rawdata %>% 
          group_by(name, code) %>% 
          filter(date>as.Date("2020-09-01") & cases>threshold) %>% 
          summarise(startdatew2=min(date)) %>% 
          ungroup() %>% 
          select(code, startdatew2), by="code", all.x=TRUE) %>% 
  mutate(dayssincew2=as.integer(if_else(date-startdatew2>=0, date-startdatew2, NA_real_)),
         dayssincew4=as.integer(if_else(date-startdatew4>=0, date-startdatew4, NA_real_)))

cases <- ggplot()+
  geom_line(data=data %>% filter(dayssincew2<threshold),
            aes(x=dayssincew2, y=cases), colour="Grey70")+
  geom_line(data=data %>% filter(!is.na(dayssincew4)),
            aes(x=dayssincew4, y=cases), colour="#FF4E86")+
  scale_x_continuous(name="", limits=c(0,50))+
  scale_y_continuous(name="Weekly cases per 100,000", limits=c(0,NA))+
  facet_wrap(~name, ncol=1)+
  theme_custom()+
  theme(plot.title=element_text(face="plain"))+
  labs(title="Cases")

admissions <- ggplot()+
  geom_line(data=data %>% filter(dayssincew2<threshold),
            aes(x=dayssincew2, y=admissions), colour="Grey70")+
  geom_line(data=data %>% filter(!is.na(dayssincew4)),
            aes(x=dayssincew4, y=admissions), colour="#FF4E86")+
  scale_x_continuous(name="Days since start of wave", 
                     limits=c(0,50))+
  scale_y_continuous(name="Weekly admissions per 100,000", limits=c(0,NA))+
  facet_wrap(~name, ncol=1)+
  theme_custom()+
  theme(plot.title=element_text(face="plain"))+
  labs(title="Admissions")

deaths <- ggplot()+
  geom_line(data=data %>% filter(dayssincew2<threshold),
            aes(x=dayssincew2, y=deaths), colour="Grey70")+
  geom_line(data=data %>% filter(!is.na(dayssincew4)),
            aes(x=dayssincew4, y=deaths), colour="#FF4E86")+
  scale_x_continuous(name="", limits=c(0,50))+
  scale_y_continuous(name="Weekly deaths per 100,000", limits=c(0,NA))+
  facet_wrap(~name, ncol=1)+
  theme_custom()+
  theme(plot.title=element_text(face="plain"))+
  labs(title="Deaths")

plot <- cases+admissions+deaths+
  plot_annotation(
    title="COVID admissions and deaths are lower in this wave than in the Autumn",
    subtitle=paste0("Trajectories of cases, admissions and deaths in <span style='color:#FF4E86;'>the current wave</span> compared to <span style='color:Grey70;'>the second, autumn wave</span>.<br>The start of each wave in each country is defined as the point at which cases first exceeded ", 
                    threshold, " per 100,000 people per week."),
    caption="Data from coronavirus.data.gov.uk | Plot by @VictimOfMaths",
    theme=theme(plot.title=element_text(face="bold", size=rel(1.8)),
                plot.subtitle=element_markdown(),
                text=element_text(family="Lato")))

agg_tiff("Outputs/COVIDWaveComparisons.tiff", units="in", width=10, height=7, res=800)
plot
dev.off()

ggplot()+
  geom_path(data=data %>% filter(dayssincew2<threshold),
            aes(x=cases, y=admissions, colour=dayssincew2))+
  geom_path(data=data %>% filter(!is.na(dayssincew4)),
            aes(x=cases, y=admissions, colour=dayssincew4))+
  facet_wrap(~name)+
  theme_custom()
