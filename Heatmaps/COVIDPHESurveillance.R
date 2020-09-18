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
temp1 <- tempfile()
source1 <- "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/919094/Weekly_COVID19_report_data_w38.xlsx"
temp1 <- curl_download(url=source1, destfile=temp1, quiet=FALSE, mode="wb")
case.m <- read_excel(temp1, sheet="Figure 2b Cases by age and sex ", range="B43:L69", 
                     col_names=FALSE)
colnames(case.m) <- c("Week", "0-4", "5-9", "10-19", "20-29", "30-39", "40-49", 
                          "50-59", "60-69", "70-79", "80+")
case.m$sex <- "Male"
case.m <- gather(case.m, age, cases, c(2:11))
case.m$cases <- as.character(case.m$cases)

case.f <- read_excel(temp1, sheet="Figure 2b Cases by age and sex ", range="B12:L38", 
                     col_names=FALSE)
colnames(case.f) <- c("Week", "0-4", "5-9", "10-19", "20-29", "30-39", "40-49", 
                      "50-59", "60-69", "70-79", "80+")
case.f$sex <- "Female"
case.f <- gather(case.f, age, cases, c(2:11))
case.f$cases <- as.character(case.f$cases)

cases <- bind_rows(case.m, case.f)

#Bring in LA populations
temp2 <- tempfile()
source2 <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fpopulationandmigration%2fpopulationestimates%2fdatasets%2fpopulationestimatesforukenglandandwalesscotlandandnorthernireland%2fmid20182019laboundaries/ukmidyearestimates20182019ladcodes.xls"
temp2 <- curl_download(url=source2, destfile=temp2, quiet=FALSE, mode="wb")
pop <- as.data.frame(t(read_excel(temp2, sheet="MYE2-All", range="E9:CQ9", col_names=FALSE)))
pop$age <- c(0:90)
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
  group_by(ageband) %>% 
  summarise(pop=sum(V1))

cases <- merge(cases, pop, by.x="age", by.y="ageband", all.x=TRUE)

cases$cases <- if_else(cases$cases=="*", "0", cases$cases)
cases$cases <- if_else(cases$cases=="-", "0", cases$cases)
cases$cases <- if_else(is.na(cases$cases), "0", cases$cases)
cases$cases <- as.numeric(cases$cases)
cases$caserate <- cases$cases*100000/cases$pop

cases$age <- factor(cases$age, levels=c("0-4", "5-9", "10-19", "20-29", "30-39",
                                        "40-49", "50-59", "60-69", "70-79", "80+"))

#Heatmaps of cases by age: rates
###############################
#Need to split populations out by sex as there are currently wrong
###############################
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

tiff("Outputs/COVIDNewCasesHeatmapRecent.tiff", units="in", width=10, height=5, res=500)
ggplot(subset(cases, Week>=26))+
  geom_tile(aes(x=Week, y=age, fill=caserate))+
  scale_y_discrete(name="Age")+
  scale_fill_paletteer_c("viridis::magma", name="Cases per 100,000")+ 
  scale_x_continuous(breaks=c(25,27,29,31,33,35,37))+
  facet_wrap(~sex)+
  theme_classic()+ 
  theme(strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)))+
  labs(title="Cases are concentrated in young people, but now rising in older age groups",
    subtitle="Rates of new COVID-19 cases in England by age during the pandemic",
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
  labs(title="COVID-19 cases are rising in almost all age groups",
       subtitle="Confirmed new cases in England by age band",
       caption="Date from PHE | Plot by @VictimOfMaths")
dev.off()

#Analysis of positivity data
#Overall by pillar
pos.pillars <- read_excel(temp1, sheet="Figure 1. Pillar 1+2 epicurve", range="B9:F40",
                          col_names=FALSE)
colnames(pos.pillars) <- c("Week", "P1cases", "P2cases", "P1pos", "P2pos")

pos.pillars <- gather(pos.pillars, Pillar, posrate, c(4,5))
pos.pillars$posrate <- as.numeric(if_else(pos.pillars$posrate=="-", "NA", pos.pillars$posrate))

tiff("Outputs/COVIDPillarsPosRate.tiff", units="in", width=8, height=6, res=500)
ggplot(pos.pillars, aes(x=Week, y=posrate/100, colour=Pillar))+
  geom_line(show.legend=FALSE)+
  scale_colour_paletteer_d("NineteenEightyR::malibu")+
  scale_y_continuous(name="Proportion of tests which are positive", 
                     labels = scales::percent_format(accuracy = 1))+  
  theme_classic()+
  theme(plot.subtitle=element_markdown())+
  labs(title="The positivity rate of Pillar 2 tests is rising",
       subtitle="Weekly positivity rates for <span style='color:#FF4E86;'>Pillar 1</span> and <span style='color:#FF9E44;'>Pillar 2 </span>tests in England",
       caption="Date from PHE | Visualisation by @VictimOfMaths")
dev.off()

#By age and sex
pos.age.m <- read_excel(temp1, sheet="Figure 6. Positivity by agegrp", range="B80:I111",
                        col_names=FALSE)
colnames(pos.age.m) <- c("Week", "0-4", "5-14", "15-44", "45-64", "65-74", "75-84", "85+")
pos.age.m <- gather(pos.age.m, age, posrate, c(2:8))
pos.age.m$posrate <- as.numeric(if_else(pos.age.m$posrate=="-", "NA", pos.age.m$posrate))
pos.age.m$sex <- "Male"

pos.age.f <- read_excel(temp1, sheet="Figure 6. Positivity by agegrp", range="B115:I146",
                        col_names=FALSE)
colnames(pos.age.f) <- c("Week", "0-4", "5-14", "15-44", "45-64", "65-74", "75-84", "85+")
pos.age.f <- gather(pos.age.f, age, posrate, c(2:8))
pos.age.f$posrate <- as.numeric(if_else(pos.age.f$posrate=="-", "NA", pos.age.f$posrate))
pos.age.f$sex <- "Female"

pos.age <- bind_rows(pos.age.m, pos.age.f)

pos.age$age <- factor(pos.age$age, levels=c("0-4", "5-14", "15-44", "45-64", "65-74",
                                            "75-84", "85+"))

tiff("Outputs/COVIDPosRatexAge.tiff", units="in", width=10, height=6, res=500)
ggplot(pos.age, aes(x=Week, y=posrate/100, colour=age))+
  geom_line()+
  scale_colour_paletteer_d(pal="awtools::a_palette",name="Age")+
  scale_y_continuous(name="Proportion of tests which are positive", 
                     labels = scales::percent_format(accuracy = 1))+  
  xlim(c(18,max(pos.age$Week)+1))+
  facet_wrap(~sex)+
  theme_classic()+
  theme(strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)))+
  labs(title="The positivity rate of tests is rising in all age groups",
       subtitle="Weekly positivity rates for Pillar 2 tests in England by age group",
       caption="Date from PHE | Visualisation by @VictimOfMaths")
dev.off()

#By region

