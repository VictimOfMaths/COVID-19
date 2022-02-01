rm(list=ls())

library(tidyverse)
library(lubridate)
library(scales)
library(RcppRoll)
library(curl)
library(paletteer)
library(ggtext)
library(ragg)

theme_custom <- function() {
  theme_classic() %+replace%
    theme(plot.title.position="plot", plot.caption.position="plot",
          strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)),
          plot.title=element_text(face="bold", size=rel(1.5), hjust=0,
                                  margin=margin(0,0,5.5,0)),
          text=element_text(family="Lato"))
}

#Read in data on infections and reinfections by LTLA
url <- "https://api.coronavirus.data.gov.uk/v2/data?areaType=ltla&metric=newCasesBySpecimenDate&metric=newFirstEpisodesBySpecimenDate&format=csv"

temp <- tempfile()
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

#Get rid of Scottish and Welsh data (not using new definition yet)
rawdata <- read.csv(temp) %>% 
  filter(substr(areaCode,1,1) %in% c("E")) %>% 
  mutate(date=as.Date(date))

#National level plot
natdata <- rawdata %>% 
  mutate(country=if_else(substr(areaCode,1,1)=="E", "England", "Northern Ireland")) %>% 
  group_by(country, date) %>% 
  summarise(TotalCases=sum(newCasesBySpecimenDate), 
            NewInfections=sum(newFirstEpisodesBySpecimenDate)) %>% 
  ungroup() %>% 
  mutate(Reinfections=TotalCases-NewInfections) %>% 
  gather(Metric, Cases, c(3:5)) %>% 
  group_by(Metric) %>% 
  mutate(Cases_roll=roll_mean(Cases, 7, align="center", fill=NA)) %>% 
  filter(date>as.Date("2020-03-01"))

agg_tiff("Outputs/COVIDReinfections.tiff", units="in", width=8, height=6, res=500)
ggplot(natdata %>% filter(Metric!="TotalCases"), aes(x=date, y=Cases_roll, fill=Metric))+
  geom_col(show.legend=FALSE)+
  scale_x_date(name="")+
  scale_y_continuous(name="Daily cases")+
  scale_fill_manual(values=c("#FF9E44", "#FF4E86"))+
  theme_custom()+
  theme(plot.subtitle=element_markdown())+
  labs(title="Reinfections are an Omicron phenomenon",
       subtitle="Rolling 7-day average number of <span style='color:#FF9E44;'>first time COVID cases</span> and <span style='color:#FF4E86;'>reinfections</span> in England",
       caption="Data from coronavirus.data.gov.uk | Plot by @VictimOfMaths")
dev.off()

agg_tiff("Outputs/COVIDReinfectionsProp.tiff", units="in", width=8, height=6, res=500)
ggplot(natdata %>% filter(Metric!="TotalCases"), aes(x=date, y=Cases_roll, fill=Metric))+
  geom_col(show.legend=FALSE, position="fill")+
  scale_x_date(name="")+
  scale_y_continuous(name="Proportion of cases", labels=label_percent(accuracy=1))+
  scale_fill_manual(values=c("#FF9E44", "#FF4E86"))+
  theme_custom()+
  theme(plot.subtitle=element_markdown())+
  labs(title="Reinfections are an Omicron phenomenon",
       subtitle="Rolling 7-day average proportion of cases in England that are <span style='color:#FF9E44;'>first time COVID cases</span> and <span style='color:#FF4E86;'>reinfections",
       caption="Data from coronavirus.data.gov.uk | Plot by @VictimOfMaths")
dev.off()

