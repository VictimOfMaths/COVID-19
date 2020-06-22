rm(list=ls())

library(tidyverse)
library(curl)
library(paletteer)
library(ggtext)
library(RcppRoll)

#Download latest testing data from 
# https://www.gov.uk/guidance/coronavirus-covid-19-information-for-the-public
temp <- tempfile()
source <- "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/893818/2020-06-21_COVID-19_UK_testing_time_series.csv"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
rawdata <- read.csv(temp)[,c(1,3,4,9)]
colnames(rawdata) <- c("Date", "Nation", "Pillar", "Cases")
rawdata$Date <- as.Date(rawdata$Date, format="%d/%m/%Y")

#Calculate rolling 7 day average
rawdata <- rawdata %>% 
  group_by(Pillar) %>% 
  mutate(Cases_roll=roll_mean(Cases, 7, align="right", fill=0))

tiff("Outputs/COVIDPillars.tiff", units="in", width=8, height=6, res=500)
ggplot(subset(rawdata, Nation=="UK"), aes(x=Date, y=Cases_roll, fill=Pillar))+
  geom_area(show.legend=FALSE)+
  scale_fill_paletteer_d("NineteenEightyR::malibu")+
  scale_y_continuous("New confirmed COVID-19 cases")+
  theme_classic()+
  theme(plot.subtitle=element_markdown())+
  labs(title="Pillar 1 tests represent a minority of new confirmed COVID-19 cases",
       subtitle="Rolling 7-day average of new COVID-19 cases in the UK identified through <span style='color:#FF4E86;'>Pillar 1</span> and <span style='color:#FF9E44;'>Pillar 2</span> testing<br>(Pillar 1 data includes Welsh data on both Pillars).",
       caption="Data from DHSC & PHE | Plot by @VictimOfMaths")
dev.off()
