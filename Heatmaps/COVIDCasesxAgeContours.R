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
library(ggridges)

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

agg_tiff("Outputs/COVIDCasesxAgeRidges.tiff", units="in", width=10, height=7, res=500)
ggplot(data %>% filter(areaName=="England" & date>as.Date("2020-08-01")), 
       aes(x=date, y=age, height=rollingRate))+
  geom_ridgeline(colour="white", scale=0.005, fill="Black")+
  scale_x_date(name="")+
  scale_y_discrete(name="Age")+
  #scale_fill_viridis_d(option="turbo", name="")+
  theme_custom()+
  theme(text=element_text(colour="White"), 
        plot.background=element_rect(fill="black", colour="black"),
        panel.background=element_rect(fill="black", colour="black"))+
  labs(title="The changing age distribution of COVID cases in England",
       subtitle="Rolling 7-day rate per 100,000 of people testing positive for COVID-19",
       caption="Date from coronavirus.data.gov.uk | Plot by @VictimOfMaths")
dev.off()

#Scottish version
scoturl <- "https://www.opendata.nhs.scot/dataset/b318bddf-a4dc-4262-971f-0ba329e09b87/resource/9393bd66-5012-4f01-9bc5-e7a10accacf4/download/trend_agesex_20211109.csv"
temp <- curl_download(url=scoturl, destfile=temp, quiet=FALSE, mode="wb")

scotdata <- read.csv(temp) %>% 
  filter(Sex=="Total" & !AgeGroup %in% c("Total", "0 to 59", "60+")) %>% 
  mutate(date=as.Date(as.character(Date), format="%Y%m%d"),
         agecont=as.numeric(substr(AgeGroup, 1, 2)))

#Bring in LA populations
temp2 <- tempfile()
source2 <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fpopulationandmigration%2fpopulationestimates%2fdatasets%2fpopulationestimatesforukenglandandwalesscotlandandnorthernireland%2fmid2019april2020localauthoritydistrictcodes/ukmidyearestimates20192020ladcodes.xls"
temp2 <- curl_download(url=source2, destfile=temp2, quiet=FALSE, mode="wb")
pop.m <- as.data.frame(t(read_excel(temp2, sheet="MYE2 - Males", range="E387:CQ387", col_names=FALSE)))
pop.m$age <- c(0:90)
pop.m$Sex <- "Male"
pop.f <- as.data.frame(t(read_excel(temp2, sheet="MYE2 - Females", range="E387:CQ387", col_names=FALSE)))
pop.f$age <- c(0:90)
pop.f$Sex <- "Female"
pop <- bind_rows(pop.m, pop.f)

pop$age <- c(0:90)
pop$AgeGroup <- case_when(
  pop$age<15 ~ "0 to 14",
  pop$age<20 ~ "15 to 19",
  pop$age<25 ~ "20 to 24",
  pop$age<45 ~ "25 to 44",
  pop$age<65 ~ "45 to 64",
  pop$age<75 ~ "65 to 74",
  pop$age<85 ~ "75 to 84",
  TRUE ~ "85plus"
)

pop1 <- pop %>% 
  group_by(AgeGroup, Sex) %>% 
  summarise(pop=sum(V1))

pop2 <- pop %>% 
  group_by(AgeGroup) %>%
  summarise(pop=sum(V1)) %>% 
  mutate(Sex="Total")

pop <- bind_rows(pop1, pop2)

scotdata <- merge(scotdata, pop, by=c("Sex", "AgeGroup"), all.x=TRUE)

scotdata$posrate <- scotdata$DailyPositive*100000/scotdata$pop

#Take rolling 7-day averages
scotdata <- scotdata %>% 
  group_by(AgeGroup) %>% 
  arrange(date) %>% 
  mutate(cases_avg=roll_mean(DailyPositive, 7, align="right", fill=0),
         posrate_avg=roll_mean(posrate*7, 7, align="right", fill=0))
         
agg_tiff("Outputs/COVIDCasesxAgeContoursScot.tiff", units="in", width=10, height=7, res=500)
ggplot(scotdata %>% filter(date>as.Date("2020-08-01")), 
       aes(x=date, y=agecont, z=posrate_avg))+
  geom_contour_filled(colour="white")+
  scale_x_date(name="")+
  scale_y_continuous(name="Age")+
  scale_fill_viridis_d(option="turbo", name="")+
  theme_custom()+
  labs(title="The changing age distribution of COVID cases in Scotland",
       subtitle="Rolling 7-day rate per 100,000 of people testing positive for COVID-19",
       caption="Date from Public Health Scotland | Plot by @VictimOfMaths")
dev.off()  

