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
  filter(date>=max(date[Eligible==0]) & date<=max(date[!is.na(CumulativeNumberVaccinated)])) %>% 
  mutate(EligibleUnvax=Eligible-CumulativeNumberVaccinated,
         Boosterprop=CumulativeNumberVaccinated/Eligible)

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



