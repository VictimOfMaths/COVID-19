rm(list=ls())

library(tidyverse)
library(curl)
library(readxl)
library(paletteer)
library(scales)
library(ragg)
library(extrafont)

#Read in data
temp <- tempfile()
#source <- "https://www.ons.gov.uk/peoplepopulationandcommunity/healthandsocialcare/conditionsanddiseases/datasets/coronaviruscovid19infectionsurveydata"
source <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/healthandsocialcare/conditionsanddiseases/datasets/coronaviruscovid19infectionsurveydata/2021/covid19infectionsurveydatasets20210618england1.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
rawdata <- read_excel(temp, sheet="1h", range="A7:V48", col_names=FALSE)

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

agg_tiff("Outputs/ONSInfSurveyxAge.tiff", units="in", width=11, height=4, res=500)
ggplot(data, aes(x=date, y=age, fill=prevalence))+
         geom_tile()+
  scale_x_date(name="")+
  scale_y_discrete(name="")+
  scale_fill_paletteer_c("viridis::inferno", name="Prevalence", labels=scales::label_percent(),
                         limits=c(0,NA))+
  labs(title="The ONS infection survey shows a sharp rise in cases in 25-34 year olds",
       subtitle="Age-specific COVID-19 prevalence estimates from the latest ONS Infection Survey",
       caption="Data from ONS | Plot by @VictimOfMaths")+
  theme_classic()+
  theme(plot.title=element_text(face="bold", size=rel(1.2)), 
        text=element_text(family="Lato"), plot.title.position="plot")
dev.off()

agg_tiff("Outputs/ONSInfSurveyxAgeLine.tiff", units="in", width=11, height=4, res=500)
ggplot(data, aes(x=date, y=prevalence, colour=age))+
  geom_line()+
  scale_x_date(name="")+
  scale_y_continuous(name="", label=label_percent(accuracy=2), limits=c(0,NA))+
  scale_colour_paletteer_d("awtools::a_palette", name="Age")+
  labs(title="The ONS infection survey shows a sharp rise in cases in 25-34 year olds",
       subtitle="Age-specific COVID-19 prevalence estimates from the latest ONS Infection Survey",
       caption="Data from ONS | Plot by @VictimOfMaths")+
  theme_classic()+
  theme(plot.title=element_text(face="bold", size=rel(1.2)), 
        text=element_text(family="Lato"), plot.title.position="plot")
dev.off()
