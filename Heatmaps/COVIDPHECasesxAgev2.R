rm(list=ls())

library(tidyverse)
library(curl)
library(arrow)
library(readxl)
library(RcppRoll)
library(paletteer)
library(ggstream)
library(lubridate)
library(geofacet)
library(ggtext)
library(gghighlight)

temp <- tempfile()
source <- "https://coronavirus.data.gov.uk/downloads/demographic/cases/specimenDate_ageDemographic-stacked.csv"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

data <- read_csv_arrow(temp) %>% 
  select(c(1:6)) %>% 
  filter(age!="unassigned") %>% 
  rename(cases=newCasesBySpecimenDate)

#Tidy up
data <- data %>% 
  mutate(age = age %>% str_replace("_", "-") %>%
           factor(levels=c("0-4", "5-9", "10-14", "15-19",
                           "20-24", "25-29", "30-34", "35-39", 
                           "40-44", "45-49", "50-54", "55-59", 
                           "60-64", "65-69", "70-74", "75-79", 
                           "80-84", "85-89", "90+"))) %>% 
  #Remove two bonus age categories that we don't need (0-59 and 60+)
  filter(!is.na(age)) %>% 
  #Sort out Buckinghamsire (as 4 separate LTLAs in the data, but pop data only available for Bucks)
  mutate(Code=case_when(
    areaCode %in% c("E07000004", "E07000005", "E07000006", "E07000007") ~ "E06000060",
    TRUE ~ as.character(areaCode)
  ),
  areaName=case_when(
    Code=="E06000060" ~ "Buckinghamshire",
    TRUE ~ as.character(areaName)
  ),
  areaType=case_when(
    Code=="E06000060" ~ "ltla",
    TRUE ~ as.character(areaType)
  ),
  date=as.Date(date)) %>% 
  group_by(Code, areaName, areaType, date, age) %>% 
  mutate(cases=sum(cases)) %>% 
  ungroup()

#Bring in populations
temp <- tempfile()
source <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fpopulationandmigration%2fpopulationestimates%2fdatasets%2fpopulationestimatesforukenglandandwalesscotlandandnorthernireland%2fmid2019april2020localauthoritydistrictcodes/ukmidyearestimates20192020ladcodes.xls"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

pop <- read_excel(temp, sheet="MYE2 - Persons", range="A5:CQ431")

#Align age bands
pop <- pop %>% 
  gather(age.sgl, pop, c(5:95)) %>% 
  mutate(age.sgl=as.numeric(gsub("\\+", "", age.sgl)),
         age=case_when(
           age.sgl<5 ~ "0-4",
           age.sgl<10 ~ "5-9",
           age.sgl<15 ~ "10-14",
           age.sgl<20 ~ "15-19",
           age.sgl<25 ~ "20-24",
           age.sgl<30 ~ "25-29",
           age.sgl<35 ~ "30-34",
           age.sgl<40 ~ "35-39",
           age.sgl<45 ~ "40-44",
           age.sgl<50 ~ "45-49",
           age.sgl<55 ~ "50-54",
           age.sgl<60 ~ "55-59",
           age.sgl<65 ~ "60-64",
           age.sgl<70 ~ "65-69",
           age.sgl<75 ~ "70-74",
           age.sgl<80 ~ "75-79",
           age.sgl<85 ~ "80-84",
           age.sgl<90 ~ "85-89",
           TRUE ~ "90+"
         ) %>% factor(levels = levels(data$age))
  ) %>% 
  #And sort out Buckinghamshire codes
  mutate(Code=case_when(
    Code %in% c("E07000005", "E07000006", "E07000007", "E07000004") ~ "E06000060",
    TRUE ~ Code
  )) %>% 
  group_by(age, Code) %>%
  summarise(pop=sum(pop))

#Merge into case data
data <- data %>% 
  left_join(pop, by=c("Code", "age")) %>% 
  arrange(date) 

#Collapse age bands further
shortdata <- data %>% 
  mutate(ageband=case_when(
    age %in% c("0-4", "5-9", "10-14") ~ "0-14",
    age %in% c("15-19", "20-24") ~ "15-24",
    age %in% c("25-29", "30-34", "35-39", "40-44") ~ "25-44",
    age %in% c("45-49", "50-54", "55-59", "60-64") ~ "45-64",
    age %in% c("65-69", "70-74", "75-79") ~ "65-79",
    TRUE ~ "80+"
  )) %>% 
  group_by(ageband, areaCode, areaType, areaName, date) %>% 
  summarise(cases=sum(cases), pop=sum(pop)) %>% 
  ungroup() %>% 
  mutate(caserate=cases*100000/pop) %>% 
  group_by(ageband, areaCode, areaType) %>% 
  mutate(casesroll=roll_mean(cases, n=7, align="center", fill=NA),
         caserateroll=roll_mean(caserate, n=7, align="center", fill=NA)) %>% 
  ungroup()

data <- data %>% 
  mutate(caserate=cases*100000/pop) %>% 
  group_by(age, areaCode, areaType) %>% 
  mutate(casesroll=roll_mean(cases, n=7, align="center", fill=NA),
         caserateroll=roll_mean(caserate, n=7, align="center", fill=NA)) %>% 
  ungroup()

#Save files for the app to use
data %>% filter(areaType %in% c("ltla", "nation", "region")) %>% 
  write.csv("COVID_Cases_By_Age/data.csv")
data %>% filter(areaType %in% c("ltla", "nation", "region") & date>=as.Date("2021-01-01")) %>% 
  write.csv("COVID_Case_Trends_By_Age/data.csv")
shortdata %>% filter(areaType %in% c("ltla", "nation", "region")) %>% 
  write.csv("COVID_Cases_By_Age/shortdata.csv")

plotfrom <- as.Date("2020-09-01")

#England plot
tiff("Outputs/COVIDCasesxAgeEng.tiff", units="in", width=8, height=5, res=500)
data %>% 
  filter(areaType=="nation" & !is.na(caserateroll)) %>%
  ggplot()+
  geom_tile(aes(x=date, y=age, fill=caserateroll))+
  scale_fill_paletteer_c("viridis::inferno", name="Daily cases\nper 100,000")+
  scale_x_date(name="", limits=c(plotfrom, NA))+
  scale_y_discrete(name="Age")+
  theme_classic()+
  theme(plot.title=element_text(face="bold"))+
  labs(title="Case rates are highest in people of working age",
       subtitle="Rolling 7-day average of confirmed new COVID-19 cases in England per 100,000 by age group",
       caption="Data from Public Health England | Plot by @VictimOfMaths")
dev.off()

tiff("Outputs/COVIDCasesxAgeEngLine.tiff", units="in", width=10, height=8, res=500)
data %>% 
  filter(areaType=="nation" & !is.na(caserateroll)) %>%
  ggplot()+
  geom_line(aes(x=date, y=caserateroll, colour=age))+
  scale_colour_paletteer_d("pals::stepped", name="Age")+
  scale_x_date(name="", limits=c(plotfrom, NA))+
  scale_y_continuous(name="Daily new cases per 100,000")+
  theme_classic()+
  theme(plot.title=element_text(face="bold"))+
  labs(title="Case rates have finally started falling in the oldest age group",
       subtitle="Rolling 7-day average of confirmed new COVID-19 cases in England per 100,000 by age group",
       caption="Data from Public Health England | Plot by @VictimOfMaths")
dev.off()

tiff("Outputs/COVIDCasesOver90.tiff", units="in", width=10, height=8, res=500)
data %>% 
  filter(areaType=="nation" & !is.na(caserateroll)) %>%
  ggplot()+
  geom_line(aes(x=date, y=caserateroll, colour=age), show.legend=FALSE)+
  scale_colour_manual(values=c(rep("Grey80", times=18), "#FF4E86"))+
  scale_x_date(name="", limits=c(plotfrom, NA))+
  scale_y_continuous(name="Daily new cases per 100,000")+
  theme_classic()+
  theme(plot.title=element_text(face="bold"), plot.subtitle=element_markdown())+
  labs(title="New COVID-19 cases are rising fastest in the most vulnerable group",
       subtitle="Rolling 7-day average of confirmed new COVID-19 cases in England per 100,000 in the <span style='color:#FF4E86;'>**over 90s**</span> compared to <span style='color:Grey70;'>**other ages**",
       caption="Data from Public Health England | Plot by @VictimOfMaths")
dev.off()

#Compare regions
mygrid <- data.frame(name=c("North East", "North West", "Yorkshire and The Humber",
                            "West Midlands", "East Midlands", "East of England",
                            "South West", "London", "South East"),
                     row=c(1,2,2,3,3,3,4,4,4), col=c(2,1,2,1,2,3,1,2,3),
                     code=c(1:9))

tiff("Outputs/COVIDCasesxAgeReg.tiff", units="in", width=9, height=11, res=500)
data %>% 
  filter(areaType=="region" & !is.na(caserateroll)) %>% 
  ggplot()+
  geom_tile(aes(x=date, y=age, fill=caserateroll))+
  scale_fill_paletteer_c("viridis::magma", name="Daily cases\nper 100,000")+
  scale_x_date(name="", limits=c(plotfrom, NA))+
  scale_y_discrete(name="Age")+
  facet_geo(~areaName, grid=mygrid)+
  theme_classic()+
  theme(plot.title=element_text(face="bold"), strip.background=element_blank(),
        strip.text=element_text(face="bold", size=rel(1)))+
  labs(title="Everywhere has seen a 'student bump', but cases are rising in older ages across the North",
       subtitle="Rolling 7-day average of confirmed new COVID-19 cases per 100,000 by age group",
       caption="Data from Public Health England | Plot by @VictimOfMaths")
dev.off()

#Look at age patterns in Blackpool
tiff("Outputs/COVIDCasesxAgeBlackpool.tiff", units="in", width=12, height=8, res=500)
data %>% 
  filter(areaType=="ltla" & !is.na(caserateroll)) %>%
  mutate(flag=if_else(areaName=="Blackpool", "x", "y")) %>% 
  ggplot()+
  geom_line(aes(x=date, y=caserateroll, group=areaName), colour="Grey80")+
  geom_line(aes(x=date, y=caserateroll, group=areaName, colour=flag), show.legend=FALSE)+
  scale_x_date(name="", limits=c(plotfrom, NA))+
  scale_y_continuous(name="Daily cases per 100,000")+
  scale_colour_manual(values=c("#FF4E86", "transparent"))+
  facet_wrap(~age)+
  theme_classic()+
  theme(plot.title=element_text(face="bold"), strip.background=element_blank(),
        strip.text=element_text(face="bold", size=rel(1)), plot.subtitle=element_markdown())+
  labs(title="Blackpool's COVID cases are older than then average",
       subtitle="Rolling 7-day average of confirmed new COVID-19 cases per 100,000 in <span style='color:#FF4E86;'>Blackpool</span> compared to <span style='color:Grey60;'>other Local Authorities in England",
       caption="Data from Public Health England | Plot by @VictimOfMaths")
dev.off()

#Calculate mean age of cases
meanages <- data %>%
  mutate(point.age=case_when(
    age=="0-4" ~ 2.5, age=="5-9" ~ 7.5, age=="10-14" ~ 12.5, age=="15-19" ~ 17.5,
    age=="20-24" ~ 22.5, age=="25-29" ~ 27.5, age=="30-34" ~ 32.5,
    age=="35-39" ~ 37.5, age=="40-44" ~ 42.5, age=="45-49" ~ 47.5,
    age=="50-54" ~ 52.5, age=="55-59" ~ 57.5, age=="60-64" ~ 62.5,
    age=="65-69" ~ 67.5, age=="70-74" ~ 72.5, age=="75-79" ~ 77.5,
    age=="80-84" ~ 82.5, age=="85-89" ~ 87.5, age=="90+" ~ 92.5
  )) %>% 
  group_by(areaType, areaCode, areaName, date) %>% 
  summarise(mean.age=weighted.mean(point.age, casesroll), casesroll=sum(casesroll),
            pop=sum(pop)) %>%
  mutate(caserateroll=casesroll*100000/pop) %>% 
  ungroup()

tiff("Outputs/COVIDCasesxAgeBlackpool2.tiff", units="in", width=8, height=6, res=500)
ggplot()+
  geom_line(data=meanages, aes(x=date, y=mean.age, group=areaName), 
            colour="Grey80")+
  geom_line(data=subset(meanages, areaName=="Blackpool"), 
            aes(x=date, y=mean.age, group=areaName), colour="#FF4E86")+
  scale_x_date(name="", limits=c(plotfrom, NA))+
  scale_y_continuous(name="Mean age")+
  theme_classic()+
  theme(plot.title=element_text(face="bold"), strip.background=element_blank(),
        strip.text=element_text(face="bold", size=rel(1)), plot.subtitle=element_markdown())+
  labs(title="Blackpool's cases are older than then average",
       subtitle="Average age of new confirmed COVID-19 cases in <span style='color:#FF4E86;'>Blackpool</span> compared to <span style='color:Grey60;'>other Local Authorities in England",
       caption="Data from Public Health England | Plot by @VictimOfMaths")
dev.off()

tiff("Outputs/COVIDCasesxMeanAge.tiff", units="in", width=8, height=6, res=500)
meanages %>% 
  filter(date==as.Date("2020-10-31") & areaType=="ltla") %>% 
  mutate(flag=caserateroll+mean.age) %>% 
  ggplot()+
  geom_point(aes(x=caserateroll, y=mean.age), colour="#FF4E86")+
  gghighlight(flag>105, caserateroll>68, mean.age>42)+
  scale_x_continuous(name="Daily cases per 100,000")+
  scale_y_continuous(name="Mean age")+
  #scale_colour_paletteer_d("LaCroixColoR::paired")+
  theme_classic()+
  theme(plot.title=element_text(face="bold"))+
  labs(title="Blackpool has more, older, COVID-19 cases",
       subtitle="Rolling 7-day average of daily new cases per 100,000 plotted against their average age*",
       caption="Data from Public Health England | Plot by @VictimOfMaths")
dev.off()

#regional simple lines
tiff("Outputs/COVIDCasesxAgeRegLine.tiff", units="in", width=9, height=11, res=500)
shortdata %>% 
  filter(areaType=="region" & date>=plotfrom) %>% 
  ggplot()+
  geom_line(aes(x=date, y=caserateroll, colour=ageband))+
  scale_x_date(name="")+
  scale_y_continuous(name="Daily cases per 100,000")+
  scale_colour_paletteer_d("awtools::a_palette", name="Age")+
  facet_geo(~areaName, grid=mygrid)+
  theme_classic()+
  theme(plot.title=element_text(face="bold", size=rel(1.2)), strip.background=element_blank(),
        strip.text=element_text(face="bold", size=rel(1)))+
  labs(title="COVID-19 case rates in the oldest age group are not falling along with younger groups",
       subtitle="Rolling 7-day average of confirmed new cases by age",
       caption="Data from PHE | Plot by @VictimOfMaths")
dev.off()
