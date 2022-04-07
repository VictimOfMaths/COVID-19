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
  labs(title="The proportion of cases that are reinfections has remained steady",
       subtitle="Rolling 7-day average proportion of cases in England that are <span style='color:#FF9E44;'>first time COVID cases</span> and <span style='color:#FF4E86;'>reinfections",
       caption="Data from coronavirus.data.gov.uk | Plot by @VictimOfMaths")
dev.off()

#####################
#Age-stratified reinfection data
#Read in data on infections and reinfections by LTLA
url1 <- "https://api.coronavirus.data.gov.uk/v2/data?areaType=nation&metric=newCasesBySpecimenDateAgeDemographics&format=csv"
url2 <- "https://api.coronavirus.data.gov.uk/v2/data?areaType=nation&metric=newFirstEpisodesBySpecimenDateAgeDemographics&format=csv"
temp1 <- tempfile()
temp2 <- tempfile()
temp1 <- curl_download(url=url1, destfile=temp1, quiet=FALSE, mode="wb")
temp2 <- curl_download(url=url2, destfile=temp2, quiet=FALSE, mode="wb")

#Get rid of Scottish and Welsh data (not using new definition yet)
agedata <- read.csv(temp1) %>% 
  rename("TotalCases"="cases") %>% 
  select(-c("rollingSum", "rollingRate")) %>% 
  merge(read.csv(temp2) %>% rename("NewInfections"="cases") %>% 
          select(-c("rollingSum", "rollingRate")), all=TRUE) %>% 
  mutate(date=as.Date(date),
         Reinfections=TotalCases-NewInfections,
         age=gsub("_", "-", age)) %>% 
  gather(Metric, Cases, c(6:8)) %>% 
  group_by(Metric) %>% 
  mutate(Cases_roll=roll_mean(Cases, 7, align="center", fill=NA)) %>% 
  select(-Cases) %>% 
  filter(date>as.Date("2020-03-01") & age!="00-59" & age!="unassigned" & age!="60+") %>% 
  spread(Metric, Cases_roll) %>% 
  mutate(ReinfectionProp=Reinfections/TotalCases) %>% 
  gather(Metric, Cases_roll, c(6:9))

agg_tiff("Outputs/COVIDReinfectionsPropxAge.tiff", units="in", width=8, height=6, res=500)
ggplot(agedata %>% filter(Metric=="ReinfectionProp" & date>as.Date("2020-06-01")), 
       aes(x=date, y=Cases_roll, colour=age))+
  geom_line(size=0.2)+
  scale_x_date(name="")+
  scale_y_continuous(name="Proportion of new cases that are reinfections", 
                     labels=label_percent(accuracy=1))+
  scale_colour_paletteer_d("pals::stepped", name="Age")+
  theme_custom()
dev.off()

agg_tiff("Outputs/COVIDReinfectionsPropxAgeHeatmap.tiff", units="in", width=10, height=6, res=500)
ggplot(agedata %>% filter(Metric=="ReinfectionProp" & date>as.Date("2020-06-01") & !is.na(Cases_roll)), 
       aes(x=date, y=age, fill=Cases_roll, colour=Cases_roll))+
  geom_tile()+
  scale_x_date(name="")+
  scale_y_discrete(name="Age")+
  scale_fill_paletteer_c("viridis::rocket", name="Proportion of new cases\nthat are reinfections", 
                         labels=label_percent(accuracy=1))+
  scale_colour_paletteer_c("viridis::rocket", name="Proportion of new cases\nthat are reinfections", 
                           labels=label_percent(accuracy=1))+
  theme_custom()+
  theme(legend.position="top")+
  guides(fill = guide_colorbar(title.position = 'top', title.hjust = .5,
                               barwidth = unit(20, 'lines'), barheight = unit(.5, 'lines')))+
  labs(title="The age composition of reinfections has changed with Omicron",
       subtitle="Rolling 7-day average proportion of new cases that are reinfections by age",
       caption="Data from coronavirus.data.gov.uk | Plot by @VictimOfMaths")
dev.off()
