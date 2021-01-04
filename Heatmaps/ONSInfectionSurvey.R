rm(list=ls())

library(tidyverse)
library(curl)
library(readxl)
library(paletteer)

#Read in data
temp <- tempfile()
#source <- "https://www.ons.gov.uk/peoplepopulationandcommunity/healthandsocialcare/conditionsanddiseases/datasets/coronaviruscovid19infectionsurveydata"
source <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fhealthandsocialcare%2fconditionsanddiseases%2fdatasets%2fcoronaviruscovid19infectionsurveydata%2f2020/covid19infectionsurveydatasets2020122423122020174305.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
rawdata <- read_excel(temp, sheet="1g", range="A8:V49", col_names=FALSE)

data <- rawdata %>% 
  select(c(1,2,5,8,11,14,17,20)) %>% 
  gather(age, prevalence, c(2:8)) %>% 
  mutate(age=case_when(
    age=="...2" ~ "Age 2 - Year 6",
    age=="...5" ~ "Year 7 - Year 11",
    age=="...8" ~ "Year 12 - Age 24",
    age=="...11" ~ "Ages 25 - 34",
    age=="...14" ~ "Ages 35 - 49",
    age=="...17" ~ "Ages 50 - 69",
    age=="...20" ~ "Ages 70+"
         ),
    date=as.Date(`...1`),
    age=factor(age, levels=c("Age 2 - Year 6","Year 7 - Year 11","Year 12 - Age 24",
                             "Ages 25 - 34","Ages 35 - 49","Ages 50 - 69","Ages 70+")))

tiff("Outputs/ONSInfSurveyxAge.tiff", units="in", width=11, height=4, res=500)
ggplot(data, aes(x=date, y=age, fill=prevalence))+
         geom_tile()+
  scale_x_date(name="")+
  scale_y_discrete(name="")+
  scale_fill_paletteer_c("viridis::magma", name="Prevalence", labels=scales::label_percent())+
  labs(title="The ONS infection survey gives a very different picture of the age distribution of cases",
       subtitle="Age-specific COVID-19 prevalence estimates from the latest ONS Infection Survey",
       caption="Data from ONS | Plot by @VictimOfMaths")+
  theme_classic()+
  theme(plot.title=element_text(face="bold", size=rel(1.2)))
dev.off()