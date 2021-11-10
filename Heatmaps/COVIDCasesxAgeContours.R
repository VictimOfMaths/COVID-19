rm(list=ls())

library(tidyverse)
library(curl)
library(RcppRoll)
library(lubridate)
library(ggtext)
library(extrafont)
library(ragg)
library(geofacet)
library(readxl)

theme_custom <- function() {
  theme_classic() %+replace%
    theme(plot.title.position="plot", plot.caption.position="plot",
          strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)),
          plot.title=element_text(face="bold", size=rel(1.5), hjust=0,
                                  margin=margin(0,0,5.5,0)),
          text=element_text(family="Lato"))
}

temp <- tempfile()
regurl <- "https://api.coronavirus.data.gov.uk/v2/data?areaType=region&metric=newCasesBySpecimenDateAgeDemographics&format=csv"
temp <- curl_download(url=regurl, destfile=temp, quiet=FALSE, mode="wb")

regdata <- read.csv(temp) 

naturl <- "https://api.coronavirus.data.gov.uk/v2/data?areaType=nation&areaCode=E92000001&metric=newCasesBySpecimenDateAgeDemographics&format=csv"
temp <- curl_download(url=naturl, destfile=temp, quiet=FALSE, mode="wb")

natdata <- read.csv(temp) 

data <- bind_rows(regdata, natdata) %>% 
  mutate(date=as.Date(date)) %>% 
  filter(!age %in% c("00_59", "60+", "unassigned")) %>% 
  mutate(agecont=as.numeric(substr(age, 1, 2)))
  
agg_tiff("Outputs/COVIDCasesxAgeContours.tiff", units="in", width=10, height=7, res=500)
ggplot(data %>% filter(areaName=="England" & date>as.Date("2020-08-01")), 
       aes(x=date, y=agecont, z=rollingRate))+
  geom_contour_filled(colour="white")+
  scale_x_date(name="")+
  scale_y_continuous(name="Age")+
  scale_fill_viridis_d(option="turbo", name="")+
  theme_custom()+
  labs(title="The changing age distribution of COVID cases in England",
       subtitle="Rolling 7-day rate per 100,000 of people testing positive for COVID-19",
       caption="Date from coronavirus.data.gov.uk | Plot by @VictimOfMaths")
dev.off()  
  
agg_tiff("Outputs/COVIDCasesxAgeContoursLondon.tiff", units="in", width=10, height=7, res=500)
ggplot(data %>% filter(areaName=="London" & date>as.Date("2020-08-01")), 
       aes(x=date, y=agecont, z=rollingRate))+
  geom_contour_filled(colour="white")+
  scale_x_date(name="")+
  scale_y_continuous(name="Age")+
  scale_fill_viridis_d(option="turbo", name="")+
  theme_custom()+
  labs(title="The changing age distribution of COVID cases in London",
       subtitle="Rolling 7-day rate per 100,000 of people testing positive for COVID-19",
       caption="Date from coronavirus.data.gov.uk | Plot by @VictimOfMaths")
dev.off()    
  
agg_tiff("Outputs/COVIDCasesxAgeContoursNW.tiff", units="in", width=10, height=7, res=500)
ggplot(data %>% filter(areaName=="North West" & date>as.Date("2020-08-01")), 
       aes(x=date, y=agecont, z=rollingRate))+
  geom_contour_filled(colour="white")+
  scale_x_date(name="")+
  scale_y_continuous(name="Age")+
  scale_fill_viridis_d(option="turbo", name="")+
  theme_custom()+
  labs(title="The changing age distribution of COVID cases in the North West",
       subtitle="Rolling 7-day rate per 100,000 of people testing positive for COVID-19",
       caption="Date from coronavirus.data.gov.uk | Plot by @VictimOfMaths")
dev.off()  

mygrid <- data.frame(name=c("North East", "North West", "Yorkshire and The Humber",
                            "West Midlands", "East Midlands", "East of England",
                            "South West", "London", "South East"),
                     row=c(1,2,2,3,3,3,4,4,4), col=c(2,1,2,1,2,3,1,2,3),
                     code=c(1:9))

agg_tiff("Outputs/COVIDCasesxAgeContoursxReg.tiff", units="in", width=12, height=10, res=500)
ggplot(data %>% filter(areaName!="England" & date>as.Date("2020-08-01")), 
       aes(x=date, y=agecont, z=rollingRate))+
  geom_contour_filled(colour="white", bins=10, size=0.1)+
  scale_x_date(name="")+
  scale_y_continuous(name="Age")+
  scale_fill_viridis_d(option="turbo", name="")+
  facet_geo(~areaName, grid=mygrid)+
  theme_custom()+
  labs(title="The changing age distribution of COVID cases in England",
       subtitle="Rolling 7-day rate per 100,000 of people testing positive for COVID-19",
       caption="Date from coronavirus.data.gov.uk | Plot by @VictimOfMaths")
dev.off()  