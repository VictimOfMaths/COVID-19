rm(list=ls())

library(tidyverse)
library(curl)
library(readxl)
library(paletteer)
library(extrafont)
library(ragg)
library(lubridate)
library(scales)
library(ggtext)
library(geofacet)

theme_custom <- function() {
  theme_classic() %+replace%
    theme(plot.title.position="plot", plot.caption.position="plot",
          strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)),
          plot.title=element_text(face="bold", size=rel(1.5), hjust=0,
                                  margin=margin(0,0,5.5,0)),
          text=element_text(family="Lato"))
}

#Download latest absence data
source <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2022/01/Staff-Absences-Web-File-Timeseries.xlsx"
temp <- tempfile()
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

totalraw <- read_excel(temp, sheet="Total Absences", range="C16:AM163", col_names=FALSE) 
  
COVIDraw <- read_excel(temp, sheet="COVID Absences", range="C16:AM163", col_names=FALSE) 

#Pull out national figures
nattotals <- totalraw %>% 
  slice(1) %>% 
  gather(Date, Total, c(3:ncol(.))) %>% 
  select(-c(1,2)) %>% 
  mutate(Date=as.Date("2021-11-29")+days(as.numeric(substr(Date, 4, 6))-3))

natCOVID <- COVIDraw %>% 
  slice(1) %>% 
  gather(Date, COVID, c(3:ncol(.))) %>% 
  select(-c(1,2)) %>% 
  mutate(Date=as.Date("2021-11-29")+days(as.numeric(substr(Date, 4, 6))-3))

natdata <- merge(nattotals, natCOVID) %>% 
  mutate(Other=Total-COVID) %>% 
  gather(Cause, Count, c(2:4))

agg_tiff("Outputs/COVIDNHSAbsences.tiff", units="in", width=8, height=6, res=500)
ggplot(natdata %>% filter(Cause!="Total"), aes(x=Date, y=Count, fill=Cause))+
  geom_area(show.legend=FALSE)+
  scale_x_date(name="")+
  scale_y_continuous(name="Total staff absent")+
  scale_fill_paletteer_d("lisa::Jean_MichelBasquiat_1")+
  theme_custom()+
  theme(plot.subtitle=element_markdown())+
  labs(title="NHS staff absences are rising fast",
       subtitle="Staff ill or isolating <span style='color:#C11432FF;'>due to COVID</span> or <span style='color:#009ADAFF ;'>absent for other reasons</span> in English acute NHS trusts",
       caption="Data from NHS England | Plot by @VictimOfMaths")

dev.off()

#Regional version
regtotals <- totalraw[c(3:9),] %>% 
  gather(Date, Total, c(3:ncol(.))) %>% 
  select(-c(1)) %>% 
  rename(Region=`...2`) %>% 
  mutate(Date=as.Date("2021-11-29")+days(as.numeric(substr(Date, 4, 6))-3))

regCOVID <- COVIDraw[c(3:9),] %>% 
  gather(Date, COVID, c(3:ncol(.))) %>% 
  select(-c(1)) %>% 
  rename(Region=`...2`) %>%   
  mutate(Date=as.Date("2021-11-29")+days(as.numeric(substr(Date, 4, 6))-3))

regdata <- merge(regtotals, regCOVID) %>% 
  mutate(Other=Total-COVID) %>% 
  gather(Cause, Count, c(3:5))

#Set up geofacet grid of NHS regions
mygrid <- data.frame(name=c("North West", "North East and Yorkshire", 
                            "Midlands","East of England",
                            "South West", "London", "South East"),
                     row=c(1,1,2,2,3,3,3), col=c(2,3,2,3,1,2,3),
                     code=c(1:7))

agg_tiff("Outputs/COVIDNHSAbsencesxReg.tiff", units="in", width=8, height=6, res=500)
ggplot(regdata %>% filter(Cause!="Total"), aes(x=Date, y=Count, fill=Cause))+
  geom_area(show.legend=FALSE)+
  scale_x_date(name="")+
  scale_y_continuous(name="Total staff absent")+
  scale_fill_paletteer_d("lisa::Jean_MichelBasquiat_1")+
  facet_geo(~Region, grid=mygrid)+
  theme_custom()+
  theme(plot.subtitle=element_markdown())+
  labs(title="NHS staff absences are most acute in the Midlands and North of England",
       subtitle="Staff ill or isolating <span style='color:#C11432FF;'>due to COVID</span> or <span style='color:#009ADAFF ;'>absent for other reasons</span> in English acute NHS trusts",
       caption="Data from NHS England | Plot by @VictimOfMaths")

dev.off()

  
