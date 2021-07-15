rm(list=ls())

library(tidyverse)
library(curl)
library(readxl)
library(ragg)
library(lubridate)
library(extrafont)
library(ggtext)

theme_custom <- function() {
  theme_classic() %+replace%
    theme(plot.title.position="plot", plot.caption.position="plot",
          strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)),
          plot.title=element_text(face="bold", size=rel(1.5), hjust=0,
                                  margin=margin(0,0,5.5,0)),
          text=element_text(family="Lato"))
}

#NE & Yorkshire version
latestcol <- "CV"

admurl <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/07/COVID-19-daily-admissions-and-beds-20210715.xlsx"

temp <- tempfile()
temp <- curl_download(url=admurl, destfile=temp, quiet=FALSE, mode="wb")

oldadmurl <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/04/COVID-19-daily-admissions-and-beds-20210406-1.xlsx"

oldtemp <- tempfile()
oldtemp <- curl_download(url=oldadmurl, destfile=oldtemp, quiet=FALSE, mode="wb")

occdata <- read_excel(temp, range=paste0("B90:",latestcol, "97"), col_names=FALSE) %>% 
  gather(date, occupancy, c(2:ncol(.))) %>% 
  mutate(date=as.Date("2021-04-07")+days(as.numeric(substr(date, 4,7))-2))

oldoccdata <- read_excel(oldtemp, range="B90:IQ97", col_names=FALSE) %>% 
  gather(date, occupancy, c(2:ncol(.))) %>% 
  mutate(date=as.Date("2020-08-01")+days(as.numeric(substr(date, 4,7))-2))
  
MVdata <-   read_excel(temp, range=paste0("B105:", latestcol, "112"), col_names=FALSE) %>% 
  gather(date, MVoccupancy, c(2:ncol(.))) %>% 
  mutate(date=as.Date("2021-04-07")+days(as.numeric(substr(date, 4,7))-2))

oldMVdata <-   read_excel(oldtemp, range="B105:IQ112", col_names=FALSE) %>% 
  gather(date, MVoccupancy, c(2:ncol(.))) %>% 
  mutate(date=as.Date("2020-08-01")+days(as.numeric(substr(date, 4,7))-2))

data <- merge(occdata, MVdata) %>% 
  bind_rows(merge(oldoccdata, oldMVdata)) %>% 
  rename(Region=`...1`) %>% 
  mutate(Otherbeds=occupancy-MVoccupancy) %>% 
  gather(type, beds, c(3:5)) %>% 
  filter(type!="occupancy") %>% 
  mutate(type=factor(type, levels=c("Otherbeds", "MVoccupancy")))

ggplot(data %>% filter(Region!="ENGLAND"), 
       aes(x=date, y=beds, fill=type))+
  geom_col(position="stack")+
  facet_wrap(~Region)+
  theme_custom()

#Bring in case data
caseurl <- "https://api.coronavirus.data.gov.uk/v2/data?areaType=region&metric=newCasesBySpecimenDateRollingSum&format=csv"

casetemp <- tempfile()
casetemp <- curl_download(url=caseurl, destfile=casetemp, quiet=FALSE, mode="wb")

cases <- read.csv(casetemp) %>% 
  mutate(Region=case_when(
    areaName %in% c("East Midlands", "West Midlands") ~ "Midlands",
    areaName %in% c("Yorkshire and The Humber", "North East") ~ 
      "North East and Yorkshire",
    TRUE ~ areaName),
    date=as.Date(date)) %>% 
  group_by(Region, date) %>% 
  summarise(cases=sum(newCasesBySpecimenDateRollingSum)/7) %>% 
  ungroup()

agg_tiff("Outputs/COVIDAdmissionsLakePlotxReg.tiff", units="in", width=10, height=7, 
         res=800)
ggplot()+
  geom_col(data=cases %>% filter(date>as.Date("2020-08-01")), 
           aes(x=date, y=cases), fill="#47d4ae")+
  geom_col(data=data %>% filter(Region!="ENGLAND"),
           aes(x=date, y=-beds, fill=type), position="stack", show.legend=FALSE)+
  geom_hline(yintercept=0, colour="Black")+
  scale_x_date(name="")+
  scale_y_continuous(name="", labels=abs, position = "right")+
  scale_fill_manual(values=c("#ff9f55", "#ff1437"))+
  facet_wrap(~Region)+
  theme_custom()+
  theme(plot.subtitle=element_markdown())+
  labs(title="COVID beds in hospitals follow COVID cases, but the link looks weaker in this latest wave",
       subtitle="Daily confirmed <span style='color:#47d4ae;'>new COVID-19 cases</span> and patients in hospital with COVID-19 in <span style='color:#ff1437;'>Mechanically Ventilated</span> and<span style='color:#ff9f55;'> all other</span> beds",
       caption="Data from NHS England and coronavirus.data.gov.uk | Plot by @VictimOfMaths")
dev.off()

agg_tiff("Outputs/COVIDAdmissionsLakePlotEng.tiff", units="in", width=10, height=7, 
         res=800)
ggplot()+
  geom_col(data=cases %>% filter(date>as.Date("2020-08-01")) %>% 
                                   group_by(date) %>% 
                                   summarise(cases=sum(cases)) %>% 
                                   ungroup(), 
           aes(x=date, y=cases), fill="#47d4ae")+
  geom_col(data=data %>% filter(Region=="ENGLAND"),
           aes(x=date, y=-beds, fill=type), position="stack", show.legend=FALSE)+
  geom_hline(yintercept=0, colour="Black")+
  scale_x_date(name="")+
  scale_y_continuous(name="", labels=abs, position = "right")+
  scale_fill_manual(values=c("#ff9f55", "#ff1437"))+
  annotate(geom="text", x=as.Date("2020-09-01"), y=25000, 
           label="New cases in the population", hjust=0, family="Lato")+
  annotate(geom="text", x=as.Date("2020-09-01"), y=-20000, 
           label="Total patients in hospital", hjust=0, family="Lato")+
  theme_custom()+
  theme(plot.subtitle=element_markdown())+
  labs(title="COVID beds in hospitals follow COVID cases, but the link looks weaker in this latest wave",
       subtitle="Daily confirmed <span style='color:#47d4ae;'>new COVID-19 cases</span> and patients in hospital with COVID-19 in <span style='color:#ff1437;'>Mechanically Ventilated</span> and<span style='color:#ff9f55;'> all other</span> beds in England",
       caption="Data from NHS England and coronavirus.data.gov.uk | Plot by @VictimOfMaths")

dev.off()

#######################################
#LA-level version

url.adm.old <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/04/Weekly-covid-admissions-and-beds-publication-210429-up-to-210406.xlsx"
temp1 <- tempfile()
temp1 <- curl_download(url=url.adm.old, destfile=temp1, quiet=FALSE, mode="wb")

url.adm.new <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/07/Weekly-covid-admissions-and-beds-publication-210715.xlsx"
temp2 <- tempfile()
temp2 <- curl_download(url=url.adm.new, destfile=temp2, quiet=FALSE, mode="wb")

MV.old <- read_excel(temp1, sheet="MV beds COVID", range="C25:IS512", 
                     col_names=FALSE) %>% 
  gather(date, MVbeds, c(3:ncol(.))) %>% 
  mutate(date=as.Date("2020-08-01")+days(as.integer(substr(date, 4, 6))-3)) %>% 
  rename(code=...1, name=...2)

MV.new <- read_excel(temp2, sheet="MV beds COVID", range="C25:CX304", col_names=FALSE) %>% 
  gather(date, MVbeds, c(3:ncol(.))) %>% 
  mutate(date=as.Date("2021-04-07")+days(as.integer(substr(date, 4, 6))-3)) %>% 
  rename(code=...1, name=...2)

MVdata <- bind_rows(MV.old, MV.new)

all.old <- read_excel(temp1, sheet="All beds COVID", range="C25:IS512", col_names=FALSE) %>% 
  gather(date, Allbeds, c(3:ncol(.))) %>% 
  mutate(date=as.Date("2020-08-01")+days(as.integer(substr(date, 4, 6))-3)) %>% 
  rename(code=...1, name=...2)

all.new <- read_excel(temp2, sheet="All beds COVID", range="C25:CX304", col_names=FALSE) %>% 
  gather(date, Allbeds, c(3:ncol(.))) %>% 
  mutate(date=as.Date("2021-04-07")+days(as.integer(substr(date, 4, 6))-3)) %>% 
  rename(code=...1, name=...2)

alldata <- bind_rows(all.old, all.new)

trustdata <- merge(MVdata, alldata, all=TRUE) %>% 
  mutate(MVbeds=if_else(is.na(MVbeds), 0, MVbeds),
         #Fix a couple of apparent errors in the MV data
         MVbeds=if_else(date==as.Date("2020-09-11") & code=="RT1", 0, MVbeds),
         MVbeds=if_else(date==as.Date("2020-08-02") & code=="RNU", 0, MVbeds),
         Allbeds=if_else(is.na(Allbeds), 0, Allbeds),
         Otherbeds=Allbeds-MVbeds)

#Bring in PHE data summarising admissions in HES to each trust by MSOA
MSOA.adm <- read.csv("COVID_LA_Plots/Trust to MSOA HES data.csv")

#1st lookup for admissions up to 4th October, when RD3 and RDZ merged to form R0D in the admissions (but not deaths) data
MSOA.adm1 <- MSOA.adm %>% 
  mutate(TrustCode=case_when(
    TrustCode %in% c("RE9", "RLN") ~ "R0B",
    TrustCode=="R1J" ~ "RTQ",
    TrustCode=="RQ6" ~ "REM",
    TrustCode=="RNL" ~ "RNN",
    TrustCode %in% c("RQ8", "RDD") ~ "RAJ",
    TrustCode=="RA3" ~ "RA7",
    TrustCode=="RC1" ~ "RC9",
    TrustCode=="RBA" ~ "RH5",
    TRUE ~ as.character(TrustCode))) %>% 
  group_by(CatchmentYear, msoa, TrustCode) %>% 
  summarise(msoa_total_catchment1=sum(msoa_total_catchment)) %>% 
  ungroup() 

#2nd lookup for after 4th October
MSOA.adm2 <- MSOA.adm %>% 
  mutate(TrustCode=case_when(
    TrustCode %in% c("RE9", "RLN") ~ "R0B",
    TrustCode=="R1J" ~ "RTQ",
    TrustCode=="RQ6" ~ "REM",
    TrustCode=="RNL" ~ "RNN",
    TrustCode %in% c("RQ8", "RDD") ~ "RAJ",
    TrustCode=="RA3" ~ "RA7",
    TrustCode=="RC1" ~ "RC9",
    TrustCode=="RBA" ~ "RH5",
    TrustCode %in% c("RDZ", "RD3") ~ "R0D",
    TRUE ~ as.character(TrustCode))) %>% 
  group_by(CatchmentYear, msoa, TrustCode) %>% 
  summarise(msoa_total_catchment2=sum(msoa_total_catchment)) %>% 
  ungroup()

temp1 <- data.frame(TrustCode=unique(MSOA.adm1$TrustCode))
temp2 <- data.frame(TrustCode=unique(MSOA.adm2$TrustCode))
temp <- bind_rows(temp1, temp2) %>%
  unique()

MSOA.adm <- merge(temp, MSOA.adm1, by="TrustCode", all=TRUE) %>% 
  merge(., MSOA.adm2, all=TRUE) 

#Bring in MSOA to LTLA lookup
temp <- tempfile()
source <- "http://geoportal1-ons.opendata.arcgis.com/datasets/0b3c76d1eb5e4ffd98a3679ab8dea605_0.csv"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
MSOA.lookup <- read.csv(temp) %>% 
  select(MSOA11CD, LAD19CD, LAD19NM) %>% 
  unique() %>% 
  rename(msoa=MSOA11CD)

#Merge into PHE lookup
MSOA.adm <- merge(MSOA.adm, MSOA.lookup, by="msoa", all.x=TRUE)

#Convert to the lookup we want (trust to LTLA), averaging across last 3 years in data (2016-18)
trust.lookup <- MSOA.adm %>% 
  filter(CatchmentYear>=2016) %>% 
  group_by(TrustCode, LAD19CD, LAD19NM) %>% 
  summarise(catchment1=sum(msoa_total_catchment1, na.rm=TRUE),
            catchment2=sum(msoa_total_catchment2, na.rm=TRUE)) %>% 
  ungroup() %>% 
  group_by(TrustCode) %>% 
  mutate(pop1=sum(catchment1, na.rm=TRUE), popprop1=catchment1/pop1,
         pop2=sum(catchment2, na.rm=TRUE), popprop2=catchment2/pop2) %>% 
  ungroup() %>% 
  rename(code=TrustCode)

admissions <- merge(trustdata, trust.lookup, by.x="code", by.y="code", all=TRUE)

LAadmissions <- admissions %>% 
  mutate(MVbeds=case_when(
    date<=as.Date("2020-10-04") ~ MVbeds*popprop1,
    TRUE ~ MVbeds*popprop2),
    Otherbeds=case_when(
      date<=as.Date("2020-10-04") ~ Otherbeds*popprop1,
      TRUE ~ Otherbeds*popprop2)
    ) %>% 
  group_by(LAD19CD, date, LAD19NM) %>% 
  summarise(MVbeds=sum(MVbeds, na.rm=TRUE),
            Otherbeds=sum(Otherbeds, na.rm=TRUE)) %>% 
  ungroup() %>% 
  filter(!is.na(LAD19CD)) %>% 
  #merge City of London into Hackney and Isles of Scilly into Cornwall
  mutate(LAD19CD=case_when(LAD19CD=="E09000001" ~ "E09000012",
                           LAD19CD=="E06000053" ~ "E06000052",
                           TRUE ~ as.character(LAD19CD))) %>%
  group_by(LAD19CD, date, LAD19NM) %>% 
  summarise(MVbeds=sum(MVbeds),
            Otherbeds=sum(Otherbeds)) %>% 
  ungroup() %>% 
  gather(type, beds, c("MVbeds", "Otherbeds"))

#Bring in LA level cases
LAcasesurl <- "https://api.coronavirus.data.gov.uk/v2/data?areaType=ltla&metric=newCasesBySpecimenDateRollingSum&format=csv" 

casetemp2 <- tempfile()
casetemp2 <- curl_download(url=LAcasesurl, destfile=casetemp2, quiet=FALSE, mode="wb")

LAcases <- read.csv(casetemp2) %>% 
  mutate(date=as.Date(date))

#Get Yorkshire-only datasets
Yorksbeds <- LAadmissions %>% 
  filter(LAD19NM %in% c("Barnsley", "Bradford", "Calderdale", "Doncaster", 
                        "East Riding of Yorkshire", "Hull", "Kirklees", "Leeds",
                        "Rotherham", "Sheffield", "Wakefield", "York", "Craven",
                        "Hambleton", "Harrogate", "Richmondshire", "Ryedale",
                        "Scarborough", "Selby")) %>% 
  group_by(date, type) %>% 
  summarise(beds=sum(beds)) %>% 
  ungroup() %>% 
  mutate(type=factor(type, levels=c("Otherbeds", "MVbeds")))

Yorkscases <- LAcases %>% 
  filter(areaName %in% c("Barnsley", "Bradford", "Calderdale", "Doncaster", 
                        "Hull", "Kirklees", "Leeds",
                        "Rotherham", "Sheffield", "Wakefield", "York", "Craven",
                        "Hambleton", "Harrogate", "Richmondshire", "Ryedale",
                        "Scarborough", "Selby")) %>% 
  group_by(date) %>% 
  summarise(cases=sum(newCasesBySpecimenDateRollingSum)/7) %>% 
  ungroup()

agg_tiff("Outputs/COVIDYorkshireAdmissionsvsOccupancy.tiff", units="in", width=9, height=6,
         res=800)
ggplot()+
  geom_col(data=Yorkscases %>% filter(date>as.Date("2020-08-01")), 
           aes(x=date, y=cases), fill="#47d4ae")+
  geom_col(data=Yorksbeds,
           aes(x=date, y=-beds, fill=type), position="stack", show.legend=FALSE)+
  geom_hline(yintercept=0, colour="Black")+
  scale_x_date(name="")+
  scale_y_continuous(name="", labels=abs, position = "right")+
  scale_fill_manual(values=c("#ff9f55", "#ff1437"))+
  annotate(geom="text", x=as.Date("2021-04-01"), y=1200, 
           label="New cases in the population", hjust=0, family="Lato")+
  annotate(geom="text", x=as.Date("2021-04-01"), y=-800, 
           label="Total patients in hospital", hjust=0, family="Lato")+
  theme_custom()+
  theme(plot.subtitle=element_markdown())+
  labs(title="New COVID-19 cases in Yorkshire are rising, but hospital beds have yet to follow",
       subtitle="Daily confirmed <span style='color:#47d4ae;'>new COVID-19 cases</span> and patients in hospital with COVID-19 in <span style='color:#ff1437;'>Mechanically Ventilated</span> and<br><span style='color:#ff9f55;'> all other</span> beds in Yorkshire",
       caption="Data from NHS England and coronavirus.data.gov.uk | Plot and analysis by Colin Angus")
dev.off()

#Generic version for any LAs
plotbeds <- LAadmissions %>% 
  filter(LAD19NM %in% c("Sheffield")) %>% 
  group_by(date, type) %>% 
  summarise(beds=sum(beds)) %>% 
  ungroup() %>% 
  mutate(type=factor(type, levels=c("Otherbeds", "MVbeds")))

plotcases <- LAcases %>% 
  filter(areaName %in% c("Sheffield")) %>% 
  group_by(date) %>% 
  summarise(cases=sum(newCasesBySpecimenDateRollingSum)/7) %>% 
  ungroup()

plotname <- "Sheffield"

agg_tiff(paste0("Outputs/COVIDAdmissionsvsOccupancy", plotname, ".tiff"), units="in", width=9, height=6,
         res=800)
ggplot()+
  geom_col(data=plotcases %>% filter(date>as.Date("2020-08-01")), 
           aes(x=date, y=cases), fill="#47d4ae")+
  geom_col(data=plotbeds,
           aes(x=date, y=-beds, fill=type), position="stack", show.legend=FALSE)+
  geom_hline(yintercept=0, colour="Black")+
  scale_x_date(name="")+
  scale_y_continuous(name="", labels=abs, position = "right")+
  scale_fill_manual(values=c("#ff9f55", "#ff1437"))+
  annotate(geom="text", x=as.Date("2021-04-01"), y=400, 
           label="New cases in the population", hjust=0, family="Lato")+
  annotate(geom="text", x=as.Date("2021-04-01"), y=-200, 
           label="Total patients in hospital", hjust=0, family="Lato")+
  theme_custom()+
  theme(plot.subtitle=element_markdown())+
  labs(title=paste0("New COVID-19 cases in ", plotname, " are rising, but hospital beds have yet to follow"),
       subtitle=paste0("Daily confirmed <span style='color:#47d4ae;'>new COVID-19 cases</span> and patients in hospital with COVID-19 in <span style='color:#ff1437;'>Mechanically Ventilated</span> and<br><span style='color:#ff9f55;'> all other</span> beds in ", plotname),
       caption="Data from NHS England and coronavirus.data.gov.uk | Plot and analysis by Colin Angus")
dev.off()

#Welsh version
#Because Wales loves pretty, but not very friendly dashboards, I've had to manually download the data
#My gratitude to anyone who can work out how to automate this
#The web page is here: https://statswales.gov.wales/Catalogue/Health-and-Social-Care/NHS-Hospital-Activity/nhs-activity-and-capacity-during-the-coronavirus-pandemic/hospitalisations-by-date-patientype

Walesdata <- read.csv("Data/WalesHospData.csv") %>% 
  mutate(Date=as.Date(Date, format="%d-%b-%y"))

Walesurl <- "https://api.coronavirus.data.gov.uk/v2/data?areaType=nation&areaCode=W92000004&metric=newCasesBySpecimenDateRollingSum&format=csv"

temp <- tempfile()
temp <- curl_download(url=Walesurl, destfile=temp, quiet=FALSE, mode="wb")

Walescases <- read.csv(temp) %>% 
  mutate(Date=as.Date(date), cases=newCasesBySpecimenDateRollingSum/7)

agg_tiff("Outputs/COVIDWalesAdmissionsvsOccupancy.tiff", units="in", width=9, height=6,
         res=800)
ggplot()+
  geom_col(data=Walescases, aes(x=Date, y=cases), fill="#47d4ae")+
  geom_col(data=Walesdata, aes(x=Date, y=-Hospitalisations), fill="#ff9f55")+ 
  geom_hline(yintercept=0, colour="Black")+
  scale_x_date(name="")+
  scale_y_continuous(name="", labels=abs, position = "right")+
  scale_fill_manual(values=c("#ff9f55", "#ff1437"))+
  annotate(geom="text", x=as.Date("2020-05-01"), y=800, 
           label="New cases in the population", hjust=0, family="Lato")+
  annotate(geom="text", x=as.Date("2020-05-01"), y=-1500, 
           label="Total patients in hospital", hjust=0, family="Lato")+
  theme_custom()+
  theme(plot.subtitle=element_markdown())+
  labs(title="New COVID-19 cases in Wales are rising, but hospital beds have yet to follow",
       subtitle="Rolling 7-day average of daily confirmed <span style='color:#47d4ae;'>new COVID-19 cases</span> and the number of <span style='color:#ff9f55;'>patients in hospital </span>with suspected<br>or confirmed COVID-19 in Wales",
       caption="Data from StatsWales and coronavirus.data.gov.uk | Plot and analysis by @VictimOfMaths")
dev.off()
