rm(list=ls())

library(tidyverse)
library(curl)
library(lubridate)
library(ggtext)
library(extrafont)
library(ragg)
library(paletteer)
library(readxl)
library(geofacet)

theme_custom <- function() {
  theme_classic() %+replace%
    theme(plot.title.position="plot", plot.caption.position="plot",
          strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)),
          plot.title=element_text(face="bold", size=rel(1.5), hjust=0,
                                  margin=margin(0,0,5.5,0)),
          text=element_text(family="Lato"))
}

temp <- tempfile()

#Surely there is a sensible full time series of this???
#Downloading each week's data individually seems kind of daft, but hey ho...
#I'm sure I'm missing something obvious here...

#28th Dec
source <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fhealthandsocialcare%2fconditionsanddiseases%2fdatasets%2fcoronaviruscovid19infectionsurveyheadlineresultsuk%2f2021/20220105covidinfectionsurveyheadlinedataset.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
regdata281221 <- read_excel(temp, sheet="1b", range="A5:E14") %>% 
  mutate(date=as.Date("2021-12-28"))
agedata281221 <- read_excel(temp, sheet="1c", range="A5:D12") %>% 
  mutate(date=as.Date("2021-12-28"))

#16th Dec
source <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/healthandsocialcare/conditionsanddiseases/datasets/coronaviruscovid19infectionsurveydata/2021/covid19infectionsurvey24122021england.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
regdata161221 <- read_excel(temp, sheet="1c England", range="A5:E14") %>% 
  mutate(date=as.Date("2021-12-16"))
agedata161221 <- read_excel(temp, sheet="1e England", range="A5:D12") %>% 
  mutate(date=as.Date("2021-12-16"))

#8th Dec
source <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/healthandsocialcare/conditionsanddiseases/datasets/coronaviruscovid19infectionsurveydata/2021/previous/v55/covid19infectionsurveydatasets20211217england.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
regdata081221 <- read_excel(temp, sheet="1e", range="A5:E14") %>% 
  mutate(date=as.Date("2021-12-08"))
agedata081221 <- read_excel(temp, sheet="1g", range="A5:D12") %>% 
  mutate(date=as.Date("2021-12-08"))

#28th Nov
source <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/healthandsocialcare/conditionsanddiseases/datasets/coronaviruscovid19infectionsurveydata/2021/previous/v54/covid19infectionsurveydatasets20211210england1.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
regdata281121 <- read_excel(temp, sheet="1e", range="A5:E14") %>% 
  mutate(date=as.Date("2021-11-28"))
agedata281121 <- read_excel(temp, sheet="1g", range="A5:D12") %>% 
  mutate(date=as.Date("2021-11-28"))

#24th Nov
source <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/healthandsocialcare/conditionsanddiseases/datasets/coronaviruscovid19infectionsurveydata/2021/previous/v53/covid19infectionsurveydatasets20211203england1.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
regdata241121 <- read_excel(temp, sheet="1e", range="A5:E14") %>% 
  mutate(date=as.Date("2021-11-24"))
agedata241121 <- read_excel(temp, sheet="1g", range="A5:D12") %>% 
  mutate(date=as.Date("2021-11-24"))

#17th Nov
source <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/healthandsocialcare/conditionsanddiseases/datasets/coronaviruscovid19infectionsurveydata/2021/previous/v52/covid19infectionsurveydatasets20211126england.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
regdata171121 <- read_excel(temp, sheet="1e", range="A5:E14") %>% 
  mutate(date=as.Date("2021-11-17"))
agedata171121 <- read_excel(temp, sheet="1g", range="A5:D12") %>% 
  mutate(date=as.Date("2021-11-17"))

#10th Nov
source <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/healthandsocialcare/conditionsanddiseases/datasets/coronaviruscovid19infectionsurveydata/2021/previous/v51/covid19infectionsurveydatasets20211119england.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
regdata101121 <- read_excel(temp, sheet="1e", range="A5:E14") %>% 
  mutate(date=as.Date("2021-11-10"))
agedata101121 <- read_excel(temp, sheet="1g", range="A5:D12") %>% 
  mutate(date=as.Date("2021-11-10"))

#3rd Nov
source <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/healthandsocialcare/conditionsanddiseases/datasets/coronaviruscovid19infectionsurveydata/2021/previous/v50/covid19infectionsurveydatasets20211112england.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
regdata031121 <- read_excel(temp, sheet="1e", range="A5:E14") %>% 
  mutate(date=as.Date("2021-11-03"))
agedata031121 <- read_excel(temp, sheet="1g", range="A5:D12") %>% 
  mutate(date=as.Date("2021-11-03"))

#27th Oct
source <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/healthandsocialcare/conditionsanddiseases/datasets/coronaviruscovid19infectionsurveydata/2021/previous/v49/covid19infectionsurveydatasets20211105england.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
regdata271021 <- read_excel(temp, sheet="1e", range="A5:E14") %>% 
  mutate(date=as.Date("2021-10-27"))
agedata271021 <- read_excel(temp, sheet="1g", range="A5:D12") %>% 
  mutate(date=as.Date("2021-10-27"))

#19th Oct
source <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/healthandsocialcare/conditionsanddiseases/datasets/coronaviruscovid19infectionsurveydata/2021/previous/v48/covid19infectionsurveydatasets20211029england.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
regdata191021 <- read_excel(temp, sheet="1e", range="A5:E14") %>% 
  mutate(date=as.Date("2021-10-19"))
agedata191021 <- read_excel(temp, sheet="1g", range="A5:D12") %>% 
  mutate(date=as.Date("2021-10-19"))

#13th Oct
source <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/healthandsocialcare/conditionsanddiseases/datasets/coronaviruscovid19infectionsurveydata/2021/previous/v47/covid19infectionsurveydatasets20211022england.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
regdata131021 <- read_excel(temp, sheet="1e", range="A5:E14") %>% 
  mutate(date=as.Date("2021-10-13"))
agedata131021 <- read_excel(temp, sheet="1g", range="A5:D12") %>% 
  mutate(date=as.Date("2021-10-13"))

#6th Oct
source <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/healthandsocialcare/conditionsanddiseases/datasets/coronaviruscovid19infectionsurveydata/2021/previous/v46/covid19infectionsurveydatasets20211015england.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
regdata061021 <- read_excel(temp, sheet="1e", range="A5:E14") %>% 
  mutate(date=as.Date("2021-10-06"))
agedata061021 <- read_excel(temp, sheet="1g", range="A5:D12") %>% 
  mutate(date=as.Date("2021-10-06"))

#29th Sept
source <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/healthandsocialcare/conditionsanddiseases/datasets/coronaviruscovid19infectionsurveydata/2021/previous/v45/covid19infectionsurveydatasets20211008england08102021091213.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
regdata290921 <- read_excel(temp, sheet="1e", range="A5:E14") %>% 
  mutate(date=as.Date("2021-09-29"))
agedata290921 <- read_excel(temp, sheet="1g", range="A5:D12") %>% 
  mutate(date=as.Date("2021-09-29"))

#22nd Sept
source <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/healthandsocialcare/conditionsanddiseases/datasets/coronaviruscovid19infectionsurveydata/2021/previous/v44/covid19infectionsurveydatasets20211001england.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
regdata220921 <- read_excel(temp, sheet="1e", range="A5:E14") %>% 
  mutate(date=as.Date("2021-09-22"))
agedata220921 <- read_excel(temp, sheet="1g", range="A5:D12") %>% 
  mutate(date=as.Date("2021-09-22"))

#15th Sept
source <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/healthandsocialcare/conditionsanddiseases/datasets/coronaviruscovid19infectionsurveydata/2021/previous/v43/covid19infectionsurveydatasets20210924england.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
regdata150921 <- read_excel(temp, sheet="1e", range="A5:E14") %>% 
  mutate(date=as.Date("2021-09-15"))
agedata150921 <- read_excel(temp, sheet="1g", range="A5:D12") %>% 
  mutate(date=as.Date("2021-09-15"))

#8th Sept
source <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/healthandsocialcare/conditionsanddiseases/datasets/coronaviruscovid19infectionsurveydata/2021/previous/v42/covid19infectionsurveydatasets20210917england.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
regdata080921 <- read_excel(temp, sheet="1e", range="A5:E14") %>% 
  mutate(date=as.Date("2021-09-08"))
agedata080921 <- read_excel(temp, sheet="1g", range="A5:D12") %>% 
  mutate(date=as.Date("2021-09-08"))

#1st Sept
source <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/healthandsocialcare/conditionsanddiseases/datasets/coronaviruscovid19infectionsurveydata/2021/previous/v41/covid19infectionsurveydatasets20210910england.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
regdata010921 <- read_excel(temp, sheet="1e", range="A5:E14") %>% 
  mutate(date=as.Date("2021-09-01"))
agedata010921 <- read_excel(temp, sheet="1g", range="A5:D12") %>% 
  mutate(date=as.Date("2021-09-01"))

#24th August
source <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/healthandsocialcare/conditionsanddiseases/datasets/coronaviruscovid19infectionsurveydata/2021/previous/v40/covid19infectionsurveydatasets20210903england.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
regdata240821 <- read_excel(temp, sheet="1e", range="A5:E14") %>% 
  mutate(date=as.Date("2021-08-24"))
agedata240821 <- read_excel(temp, sheet="1g", range="A5:D12") %>% 
  mutate(date=as.Date("2021-08-24"))

#17th August
source <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/healthandsocialcare/conditionsanddiseases/datasets/coronaviruscovid19infectionsurveydata/2021/previous/v39/covid19infectionsurveydatasets20210827england.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
regdata170821 <- read_excel(temp, sheet="1e", range="A5:E14") %>% 
  mutate(date=as.Date("2021-08-17"))
agedata170821 <- read_excel(temp, sheet="1g", range="A5:D12") %>% 
  mutate(date=as.Date("2021-08-17"))

#11th August
source <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/healthandsocialcare/conditionsanddiseases/datasets/coronaviruscovid19infectionsurveydata/2021/previous/v38/covid19infectionsurveydatasetsengland20210820.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
regdata110821 <- read_excel(temp, sheet="1e", range="A5:E14") %>% 
  mutate(date=as.Date("2021-08-11"))
agedata110821 <- read_excel(temp, sheet="1g", range="A5:D12") %>% 
  mutate(date=as.Date("2021-08-11"))

#3rd August
source <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/healthandsocialcare/conditionsanddiseases/datasets/coronaviruscovid19infectionsurveydata/2021/previous/v37/covid19infectionsurveydatasetsengland20210813.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
regdata030821 <- read_excel(temp, sheet="1e", range="A5:E14") %>% 
  mutate(date=as.Date("2021-08-03"))
agedata030821 <- read_excel(temp, sheet="1g", range="A5:D12") %>% 
  mutate(date=as.Date("2021-08-03"))

#28th July
source <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/healthandsocialcare/conditionsanddiseases/datasets/coronaviruscovid19infectionsurveydata/2021/previous/v36/covid19infectionsurveydatasets20210806england05082021171453.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
regdata280721 <- read_excel(temp, sheet="1e", range="A5:E14") %>% 
  mutate(date=as.Date("2021-07-28"))
agedata280721 <- read_excel(temp, sheet="1g", range="A5:D12") %>% 
  mutate(date=as.Date("2021-07-28"))

#21st July
source <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/healthandsocialcare/conditionsanddiseases/datasets/coronaviruscovid19infectionsurveydata/2021/previous/v35/covid19infectionsurveydatasets20210730englandpostsdc2.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
regdata210721 <- read_excel(temp, sheet="1e", range="A5:E14") %>% 
  mutate(date=as.Date("2021-07-21"))
agedata210721 <- read_excel(temp, sheet="1g", range="A5:D12") %>% 
  mutate(date=as.Date("2021-07-21"))

#14th July
source <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/healthandsocialcare/conditionsanddiseases/datasets/coronaviruscovid19infectionsurveydata/2021/previous/v34/covid19infectionsurveydatasets20210723england1.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
regdata140721 <- read_excel(temp, sheet="1e", range="A5:E14") %>% 
  mutate(date=as.Date("2021-07-14"))
agedata140721 <- read_excel(temp, sheet="1g", range="A5:D12") %>% 
  mutate(date=as.Date("2021-07-14"))

#7th July
source <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/healthandsocialcare/conditionsanddiseases/datasets/coronaviruscovid19infectionsurveydata/2021/previous/v33/covid19infectionsurveydatasets20210716england1.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
regdata070721 <- read_excel(temp, sheet="1e", range="A5:E14") %>% 
  mutate(date=as.Date("2021-07-07"))
agedata070721 <- read_excel(temp, sheet="1g", range="A5:D12") %>% 
  mutate(date=as.Date("2021-07-07"))

#30th June
source <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/healthandsocialcare/conditionsanddiseases/datasets/coronaviruscovid19infectionsurveydata/2021/previous/v32/covid19infectionsurveydatasets20210709england08072021180807.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
regdata300621 <- read_excel(temp, sheet="1e", range="A5:E14") %>% 
  mutate(date=as.Date("2021-06-30"))
agedata300621 <- read_excel(temp, sheet="1g", range="A5:D12") %>% 
  mutate(date=as.Date("2021-06-30"))

#23rd June
source <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/healthandsocialcare/conditionsanddiseases/datasets/coronaviruscovid19infectionsurveydata/2021/previous/v31/covid19infectionsurveydatasets20210702england.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
regdata230621 <- read_excel(temp, sheet="1e", range="A5:E14") %>% 
  mutate(date=as.Date("2021-06-23"))
agedata230621 <- read_excel(temp, sheet="1g", range="A5:D12") %>% 
  mutate(date=as.Date("2021-06-23"))

#16th June
source <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/healthandsocialcare/conditionsanddiseases/datasets/coronaviruscovid19infectionsurveydata/2021/previous/v30/covid19infectionsurveydatasets20210625england.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
regdata160621 <- read_excel(temp, sheet="1e", range="A5:E14") %>% 
  mutate(date=as.Date("2021-06-16"))
agedata160621 <- read_excel(temp, sheet="1g", range="A5:D12") %>% 
  mutate(date=as.Date("2021-06-16"))

#9th June
source <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/healthandsocialcare/conditionsanddiseases/datasets/coronaviruscovid19infectionsurveydata/2021/previous/v29/covid19infectionsurveydatasets20210618england1.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
regdata090621 <- read_excel(temp, sheet="1e", range="A5:E14") %>% 
  mutate(date=as.Date("2021-06-09"))
agedata090621 <- read_excel(temp, sheet="1g", range="A5:D12") %>% 
  mutate(date=as.Date("2021-06-09"))

#2nd June
source <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/healthandsocialcare/conditionsanddiseases/datasets/coronaviruscovid19infectionsurveydata/2021/previous/v28/covid19infectionsurveydatasets20210611england.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
regdata020621 <- read_excel(temp, sheet="1e", range="A5:E14") %>% 
  mutate(date=as.Date("2021-06-02"))
agedata020621 <- read_excel(temp, sheet="1g", range="A5:D12") %>% 
  mutate(date=as.Date("2021-06-02"))

#26th May
source <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/healthandsocialcare/conditionsanddiseases/datasets/coronaviruscovid19infectionsurveydata/2021/previous/v27/covid19infectionsurveydatasets20210604england.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
regdata260521 <- read_excel(temp, sheet="1e", range="A5:E14") %>% 
  mutate(date=as.Date("2021-05-26"))
agedata260521 <- read_excel(temp, sheet="1g", range="A5:D12") %>% 
  mutate(date=as.Date("2021-05-26"))

#19th May
source <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/healthandsocialcare/conditionsanddiseases/datasets/coronaviruscovid19infectionsurveydata/2021/previous/v26/covid19infectionsurveydatasets20210528eng.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
regdata190521 <- read_excel(temp, sheet="1e", range="A5:E14") %>% 
  mutate(date=as.Date("2021-05-19"))
agedata190521 <- read_excel(temp, sheet="1g", range="A5:D12") %>% 
  mutate(date=as.Date("2021-05-19"))

#12th May
source <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/healthandsocialcare/conditionsanddiseases/datasets/coronaviruscovid19infectionsurveydata/2021/previous/v25/covid19infectionsurveydatasets20210521eng.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
regdata120521 <- read_excel(temp, sheet="1e", range="A5:E14") %>% 
  mutate(date=as.Date("2021-05-12"))
agedata120521 <- read_excel(temp, sheet="1g", range="A5:D12") %>% 
  mutate(date=as.Date("2021-05-12"))

#5th May
source <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/healthandsocialcare/conditionsanddiseases/datasets/coronaviruscovid19infectionsurveydata/2021/previous/v24/covid19infectionsurveydatasets20210514engfinal.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
regdata050521 <- read_excel(temp, sheet="1e", range="A6:E15") %>% 
  mutate(date=as.Date("2021-05-05"))%>% 
  set_names(c("Region", "Pop", "PosProp", "LowerCI", "UpperCI", "Date"))
agedata050521 <- read_excel(temp, sheet="1g", range="A6:D13") %>% 
  mutate(date=as.Date("2021-05-05"))%>% 
  set_names(c("Age", "PosProp", "LowerCI", "UpperCI", "Date"))

#29th April
source <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/healthandsocialcare/conditionsanddiseases/datasets/coronaviruscovid19infectionsurveydata/2021/previous/v23/covid19infectionsurveydatasets20210507eng.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
regdata290421 <- read_excel(temp, sheet="1e", range="A6:E15") %>% 
  mutate(date=as.Date("2021-04-29"))%>% 
  set_names(c("Region", "Pop", "PosProp", "LowerCI", "UpperCI", "Date"))
agedata290421 <- read_excel(temp, sheet="1g", range="A6:D13") %>% 
  mutate(date=as.Date("2021-04-29"))%>% 
  set_names(c("Age", "PosProp", "LowerCI", "UpperCI", "Date"))

#21st April
source <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/healthandsocialcare/conditionsanddiseases/datasets/coronaviruscovid19infectionsurveydata/2021/previous/v22/covid19infectionsurveydatasets20210430eng1.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
regdata210421 <- read_excel(temp, sheet="1e", range="A6:E15") %>% 
  mutate(date=as.Date("2021-04-21"))%>% 
  set_names(c("Region", "Pop", "PosProp", "LowerCI", "UpperCI", "Date"))
agedata210421 <- read_excel(temp, sheet="1g", range="A6:D13") %>% 
  mutate(date=as.Date("2021-04-21"))%>% 
  set_names(c("Age", "PosProp", "LowerCI", "UpperCI", "Date"))

#13th April
source <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/healthandsocialcare/conditionsanddiseases/datasets/coronaviruscovid19infectionsurveydata/2021/previous/v21/covid19infectionsurveydatasets20210423eng.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
regdata130421 <- read_excel(temp, sheet="1e", range="A6:E15") %>% 
  mutate(date=as.Date("2021-04-13"))%>% 
  set_names(c("Region", "Pop", "PosProp", "LowerCI", "UpperCI", "Date"))
agedata130421 <- read_excel(temp, sheet="1g", range="A6:D13") %>% 
  mutate(date=as.Date("2021-04-13"))%>% 
  set_names(c("Age", "PosProp", "LowerCI", "UpperCI", "Date"))

#7th April 
source <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/healthandsocialcare/conditionsanddiseases/datasets/coronaviruscovid19infectionsurveydata/2021/previous/v19/covid19infectionsurveydatasets202104161.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

#At last! A time series
regdataold <- read_excel(temp, sheet="1j", range="C8:AC31", col_names=FALSE) %>% 
  set_names(c(paste(rep(c("North_East", "North_West", "Yorkshire_and_The_Humber", "East_Midlands",
                    "West_Midlands", "East_of_England", "London", "South_East", "South_West"),
                  each=3), rep(c("PosProp", "LowerCI", "UpperCI"), times=9), sep="-"))) %>% 
  mutate(Date=seq.Date(from=as.Date("2020-05-17"), to=as.Date("2021-04-04"), by="2 weeks")) %>% 
  pivot_longer(cols=c(1:(ncol(.)-1)), names_to=c("Region", "Metric"), names_sep="-", 
               values_to="Values") %>% 
  spread(Metric, Values) %>% 
  mutate(Region=gsub("_", " ", Region))

agedataold <- read_excel(temp, sheet="1i", range="C8:W31", col_names=FALSE) %>% 
  set_names(c(paste(rep(c("Age_2_to_School_Year_6", "School_Year_7_to_School_Year_11",
                          "School_Year_12_to_Age_24", "Age_25_to_Age_34",
                          "Age_35_to_Age_49", "Age_50_to_Age_69", "Age_70+"),
                        each=3), rep(c("PosProp", "LowerCI", "UpperCI"), times=7), sep="-"))) %>% 
  mutate(Date=seq.Date(from=as.Date("2020-05-17"), to=as.Date("2021-04-04"), by="2 weeks")) %>% 
  pivot_longer(cols=c(1:(ncol(.)-1)), names_to=c("Age", "Metric"), names_sep="-", 
               values_to="Values") %>% 
  spread(Metric, Values) %>% 
  mutate(Age=gsub("_", " ", Age))

#Combine and plot
regdata <- bind_rows(regdata281221, regdata161221, regdata081221, regdata281121,
                     regdata241121, regdata171121, regdata101121, regdata031121,
                     regdata271021, regdata191021, regdata131021, regdata061021,
                     regdata290921, regdata220921, regdata150921, regdata080921,
                     regdata010921, regdata240821, regdata170821, regdata110821,
                     regdata030821, regdata280721, regdata210721, regdata140721,
                     regdata070721, regdata300621, regdata230621, regdata160621) %>% 
  set_names(c("Region", "Pop", "PosProp", "LowerCI", "UpperCI", "Date")) %>% 
  mutate(PosProp=PosProp/100, LowerCI=LowerCI/100, UpperCI=UpperCI/100) %>% 
  bind_rows(bind_rows(regdata090621, regdata020621, regdata260521, regdata190521,
                     regdata120521) %>% 
              set_names(c("Region", "Pop", "PosProp", "LowerCI", "UpperCI", "Date"))) %>% 
  bind_rows(regdata050521, regdata290421, regdata210421, regdata130421, regdataold)

agedata <- bind_rows(agedata281221, agedata161221, agedata081221, agedata281121,
                     agedata241121, agedata171121, agedata101121, agedata031121,
                     agedata271021, agedata191021, agedata131021, agedata061021,
                     agedata290921, agedata220921, agedata150921, agedata080921,
                     agedata010921, agedata240821, agedata170821, agedata110821,
                     agedata030821, agedata280721, agedata210721, agedata140721,
                     agedata070721, agedata300621, agedata230621, agedata160621) %>% 
  set_names(c("Age", "PosProp", "LowerCI", "UpperCI", "Date")) %>% 
  mutate(PosProp=PosProp/100, LowerCI=LowerCI/100, UpperCI=UpperCI/100) %>% 
  bind_rows(bind_rows(agedata090621, agedata020621, agedata260521, agedata190521,
                      agedata120521) %>% 
              set_names(c("Age", "PosProp", "LowerCI", "UpperCI", "Date")))%>% 
  bind_rows(agedata050521, agedata290421, agedata210421, agedata130421, agedataold) %>% 
  mutate(Age=factor(Age, levels=c("Age 2 to School Year 6", "School Year 7 to School Year 11",
                                  "School Year 12 to Age 24", "Age 25 to Age 34",
                                  "Age 35 to Age 49", "Age 50 to Age 69", "Age 70+")))

mygrid <- data.frame(name=c("North East", "North West", "Yorkshire and The Humber",
                            "West Midlands", "East Midlands", "East of England",
                            "South West", "London", "South East"),
                     row=c(1,2,2,3,3,3,4,4,4), col=c(2,1,2,1,2,3,1,2,3),
                     code=c(1:9))
  
agg_tiff("Outputs/COVIDONSInfSurvxReg.tiff", units="in", width=10, height=8, res=500)
ggplot(regdata, aes(x=Date, y=PosProp, ymin=LowerCI, ymax=UpperCI, colour=Region, fill=Region))+
  geom_ribbon(alpha=0.1, colour=NA, show.legend=FALSE)+
  geom_line(show.legend=FALSE)+
  scale_x_date(name="")+
  scale_y_continuous(name="Proportion of population testing positive", limits=c(0,NA),
                     labels=label_percent(accuracy=1))+
  scale_colour_paletteer_d("LaCroixColoR::paired")+
  scale_fill_paletteer_d("LaCroixColoR::paired")+
  facet_geo(~Region, grid=mygrid)+
  theme_custom()+
  theme(strip.text=element_blank(), plot.title=element_text(size=rel(2.2)))+
  geom_text(aes(x=as.Date("2021-02-02"), y=0.05, label=Region), family="Lato", fontface="bold",
            size=rel(4), show.legend=FALSE)+
  labs(title="Omicron is not like Alpha or Delta",
       subtitle="Estimated proportion of the population who would test positive for COVID according to the ONS infection survey",
       caption="Data from ONS | Plot by @VictimOfMaths")
dev.off()

agg_tiff("Outputs/COVIDONSInfSurvxAge.tiff", units="in", width=10, height=8, res=500)
ggplot(agedata, aes(x=Date, y=PosProp, ymin=LowerCI, ymax=UpperCI, colour=Age, fill=Age))+
  geom_ribbon(alpha=0.1, colour=NA, show.legend=FALSE)+
  geom_line(show.legend=FALSE)+
  scale_x_date(name="")+
  scale_y_continuous(name="Proportion of population testing positive", limits=c(0,NA),
                     labels=label_percent(accuracy=1))+
  scale_colour_paletteer_d("awtools::a_palette")+
  scale_fill_paletteer_d("awtools::a_palette")+
  facet_wrap(~Age)+
  theme_custom()+
  theme(strip.text=element_blank(), plot.title=element_text(size=rel(2.2)))+
  geom_text(aes(x=as.Date("2021-01-20"), y=0.05, label=Age), family="Lato", fontface="bold",
            size=rel(4), show.legend=FALSE)+
  labs(title="Omicron is not like Alpha or Delta",
       subtitle="Estimated proportion of the population who would test positive for COVID according to the ONS infection survey",
       caption="Data from ONS | Plot by @VictimOfMaths")

dev.off()

#Calculate national picture from weighted average of regional one
pops <- regdata010921 %>% 
  select(Region, `Population size`) %>% 
  set_names("Region", "Pop")

natdata <- regdata %>% 
  select(-Pop) %>% 
  merge(pops, all.x=TRUE, by="Region") %>% 
  group_by(Date) %>% 
  summarise(PosProp=weighted.mean(PosProp, Pop)) %>% 
  ungroup()

agg_tiff("Outputs/COVIDONSInfSurv.tiff", units="in", width=8, height=6, res=500)
ggplot(natdata, aes(x=Date, y=PosProp))+
  geom_line(colour="Tomato2", size=1)+
  scale_x_date(name="")+
  scale_y_continuous(name="Proportion of population testing positive", limits=c(0,NA),
                     labels=label_percent(accuracy=1))+
  theme_custom()+
  labs(title="Omicron is not like Alpha or Delta",
       subtitle="Estimated proportion of the population who would test positive for COVID according to the ONS infection survey",
       caption="Data from ONS | Plot by @VictimOfMaths")

dev.off()


agg_tiff("Outputs/COVIDONSInfSurvWaves.tiff", units="in", width=8, height=6, res=500)
ggplot(natdata, aes(x=Date, y=PosProp))+
  geom_rect(aes(xmin=as.Date("2020-12-01"), xmax=as.Date("2021-05-01"),
                ymin=0, ymax=0.065), fill="#EE6677", alpha=0.01)+
  geom_rect(aes(xmin=as.Date("2021-05-01"), xmax=as.Date("2021-12-10"),
                ymin=0, ymax=0.065), fill="#228833", alpha=0.01)+
  geom_rect(aes(xmin=as.Date("2021-12-10"), xmax=as.Date("2022-01-01"),
                ymin=0, ymax=0.065), fill="#66CCEE", alpha=0.01)+
  geom_line(colour="Black", size=1)+
  scale_x_date(name="", limits=c(as.Date("2020-12-01"), NA_Date_))+
  scale_y_continuous(name="Proportion of population testing positive", limits=c(0,NA),
                     labels=label_percent(accuracy=1))+
  theme_custom()+
  theme(plot.title=element_markdown())+
  labs(title="<span style='color:#66CCEE;'>Omicron</span> is not like <span style='color:#228833;'>Alpha</span> or <span style='color:#EE6677;'>Delta",
       subtitle="Estimated proportion of the population who would test positive for COVID according to the ONS infection survey",
       caption="Data from ONS | Plot by @VictimOfMaths")

dev.off()
