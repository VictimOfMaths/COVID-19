rm(list=ls())

library(tidyverse)
library(curl)
library(readxl)
library(ggtext)
library(ggridges)
library(paletteer)
library(ggstream)

#Scottish age data
temp <- tempfile()
source <- "https://www.opendata.nhs.scot/dataset/b318bddf-a4dc-4262-971f-0ba329e09b87/resource/9393bd66-5012-4f01-9bc5-e7a10accacf4/download/trend_agesex_20200909.csv"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

data <- read.csv(temp)

data <- data %>% 
  filter(AgeGroup!="Total") %>% 
  mutate(date=as.Date(as.character(Date), format="%Y%m%d"))


tiff("Outputs/COVIDCasesStreamgraphScotlandxSex.tiff", units="in", width=10, height=6, res=500)
ggplot(subset(data, Sex!="Total"), aes(x=date, y=DailyPositive, fill=AgeGroup))+
  geom_stream(bw=0.2)+
  scale_fill_paletteer_d("awtools::a_palette", name="Age",
                         labels=c("15-19", "20-24", "25-44", "45-64", "65-74", "75-84", "85+"))+
  scale_y_continuous(name="New cases per day", breaks=c(-100,-50,0,50,100),
                     labels=c("100", "50", "0", "50", "100"))+
  scale_x_date(name="")+
  facet_wrap(~Sex)+
  theme_classic()+
  theme(strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)))+
  labs(title="The resurgence in COVID-19 cases in Scotland is among the under-45s",
       subtitle="Confirmed new cases in Scotland by sex and age",
       caption="Date from Public Health Scotland | Plot by @VictimOfMaths")
dev.off()

tiff("Outputs/COVIDCasesStreamgraphScotland.tiff", units="in", width=10, height=6, res=500)
ggplot(subset(data, Sex=="Total"), aes(x=date, y=DailyPositive, fill=AgeGroup))+
  geom_stream(bw=0.2)+
  scale_fill_paletteer_d("awtools::a_palette", name="Age",
                         labels=c("15-19", "20-24", "25-44", "45-64", "65-74", "75-84", "85+"))+
  scale_y_continuous(name="New cases per day", breaks=c(-150,-100,-50,0,50,100,150),
                     labels=c("150", "100", "50", "0", "50", "100","150"))+
  scale_x_date(name="")+
  theme_classic()+
  theme(strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)))+
  labs(title="The resurgence in COVID-19 cases in Scotland is among the under-45s",
       subtitle="Confirmed new cases in Scotland by age",
       caption="Date from Public Health Scotland | Plot by @VictimOfMaths")
dev.off()

#Heatmap
tiff("Outputs/COVIDCasesHeatmapScotland.tiff", units="in", width=10, height=3, res=500)
ggplot(subset(data, Sex=="Total" & date>=as.Date("2020-07-01") & date<max(data$date)), 
       aes(x=date, y=AgeGroup, fill=DailyPositive))+
  geom_tile()+
  scale_x_date(name="")+
  scale_y_discrete(name="Age group",
                   labels=c("15-19", "20-24", "25-44", "45-64", "65-74", "75-84", "85+"))+
  scale_fill_paletteer_c("viridis::magma", name="New cases")+
  theme_classic()+
  labs(title="The rise in new COVID-19 cases in Scotland hasn't affected pensioners yet",
       subtitle="Confirmed daily new cases in Scotland by age",
       caption="Date from Public Health Scotland | Plot by @VictimOfMaths")
dev.off()