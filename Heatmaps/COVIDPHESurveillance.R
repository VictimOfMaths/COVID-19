rm(list=ls())

library(tidyverse)
library(curl)
library(readxl)
library(ggtext)
library(ggridges)
library(paletteer)

#Surveillance reports taken from:
#https://www.gov.uk/government/publications/national-covid-19-surveillance-reports

#Confusingly, PHE number them by the week of publication, which is 1 week later
#than the data relates to. Numbers here relate to actual weeks of data.

#As of 28th August, PHE are actually publishing a time series of case numbers by age and sex
temp <- tempfile()
source <- "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/914814/Weekly_COVID19_report_data_w36.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
case.m <- read_excel(temp, sheet="Figure 2b Cases by age and sex ", range="B12:L36", 
                     col_names=FALSE)
colnames(case.m) <- c("Week", "0-4", "5-9", "10-19", "20-29", "30-39", "40-49", 
                          "50-59", "60-69", "70-79", "80+")
case.m$sex <- "Male"
case.m <- gather(case.m, age, cases, c(2:11))

case.f <- read_excel(temp, sheet="Figure 2b Cases by age and sex ", range="B41:L65", 
                     col_names=FALSE)
colnames(case.f) <- c("Week", "0-4", "5-9", "10-19", "20-29", "30-39", "40-49", 
                      "50-59", "60-69", "70-79", "80+")
case.f$sex <- "Female"
case.f <- gather(case.f, age, cases, c(2:11))

cases <- bind_rows(case.m, case.f)

#Bring in LA populations
temp <- tempfile()
source <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fpopulationandmigration%2fpopulationestimates%2fdatasets%2fpopulationestimatesforukenglandandwalesscotlandandnorthernireland%2fmid20182019laboundaries/ukmidyearestimates20182019ladcodes.xls"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
pop.m <- as.data.frame(t(read_excel(temp, sheet="MYE2 - Males", range="E9:CQ9", col_names=FALSE)))
pop.m$age <- c(0:90)
pop.m$sex <- "Male"
pop.f <- as.data.frame(t(read_excel(temp, sheet="MYE2 - Females", range="E9:CQ9", col_names=FALSE)))
pop.f$age <- c(0:90)
pop.f$sex <- "Female"

pop <- bind_rows(pop.m, pop.f)

pop$ageband <- case_when(
  pop$age<5 ~ "0-4",
  pop$age<10 ~ "5-9",
  pop$age<20 ~ "10-19",
  pop$age<30 ~ "20-29",
  pop$age<40 ~ "30-39",
  pop$age<50 ~ "40-49",
  pop$age<60 ~ "50-59",
  pop$age<70 ~ "60-69",
  pop$age<80 ~ "70-79",
  TRUE ~ "80+"
)

pop <- pop %>% 
  group_by(ageband, sex) %>% 
  summarise(pop=sum(V1))

cases <- merge(cases, pop, by.x=c("age", "sex"), by.y=c("ageband", "sex"), all.x=TRUE)

cases$cases <- if_else(cases$cases=="*", "0", cases$cases)
cases$cases <- if_else(cases$cases=="-", "0", cases$cases)
cases$cases <- as.numeric(cases$cases)
cases$caserate <- cases$cases*100000/cases$pop

cases$age <- factor(cases$age, levels=c("0-4", "5-9", "10-19", "20-29", "30-39",
                                        "40-49", "50-59", "60-69", "70-79", "80+"))

#Heatmaps of cases by age: rates
tiff("Outputs/COVIDNewCasesHeatmap.tiff", units="in", width=13, height=4, res=500)
ggplot(cases)+
  geom_tile(aes(x=Week, y=age, fill=caserate))+
  scale_y_discrete(name="Age")+
  scale_fill_paletteer_c("viridis::magma", name="Cases per 100,000")+ 
  facet_wrap(~sex)+
  theme_classic()+
  labs(title="New COVID-19 cases by age during the pandemic",
       caption="Data from Public Health England | Plot by @VictimOfMaths")
dev.off()

#Absolute numbers
tiff("Outputs/COVIDNewCasesHeatmapAbs.tiff", units="in", width=13, height=4, res=500)
ggplot(cases)+
  geom_tile(aes(x=Week, y=age, fill=cases))+
  scale_y_discrete(name="Age")+
  scale_fill_paletteer_c("viridis::magma", name="Confirmed cases")+ 
  facet_wrap(~sex)+
  theme_classic()+
  labs(title="COVID-19 cases are currently concentrated in working ages",
       subtitle="New confirmed COVID-19 cases by age in England",
       caption="Data from Public Health England | Plot by @VictimOfMaths")
dev.off()

library(ggstream) #devtools::install_github("davidsjoberg/ggstream")

tiff("Outputs/COVIDCasesStreamgraph.tiff", units="in", width=10, height=6, res=500)
ggplot(cases, aes(x=Week, y=cases, fill=age))+
  geom_stream(bw=0.2)+
  scale_fill_paletteer_d("RColorBrewer::RdYlGn", name="Age", direction=-1)+
  scale_y_continuous(name="New cases per week", breaks=c(-15000,-10000,-5000,0,5000,10000,15000),
                     labels=c("15,000", "10,000", "5,000", "0", "5,000", "10,000", "15,000"))+
  #scale_x_date(name="", date_breaks="1 month", date_labels="%B", 
  #             limits=c(as.Date("2020-03-01"), max(cases$Date)))+
  facet_wrap(~sex)+
  theme_classic()+
  theme(strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)))+
  labs(title="The rise in COVID-19 cases is largely among 20-somethings",
       subtitle="Confirmed new cases in England by age band",
       caption="Date from PHE | Plot by @VictimOfMaths")
dev.off()

#Combined sex version
tiff("Outputs/COVIDCasesStreamgraphxSex.tiff", units="in", width=10, height=6, res=500)
cases %>% 
  group_by(Week, age) %>% 
  summarise(cases=sum(cases)) %>% 
  ggplot(aes(x=Week, y=cases, fill=age))+
  geom_stream(bw=0.2)+
  scale_fill_paletteer_d("RColorBrewer::RdYlGn", name="Age", direction=-1)+
  scale_y_continuous(name="New cases per week", breaks=c(-15000,-10000,-5000,0,5000,10000,15000),
                     labels=c("15,000", "10,000", "5,000", "0", "5,000", "10,000", "15,000"))+
  #scale_x_date(name="", date_breaks="1 month", date_labels="%B", 
  #             limits=c(as.Date("2020-03-01"), max(cases$Date)))+
  theme_classic()+
  theme(strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)))+
  labs(title="The rise in COVID-19 cases is largely among 20-somethings",
       subtitle="Confirmed new cases in England by age band",
       caption="Date from PHE | Plot by @VictimOfMaths")
dev.off()
