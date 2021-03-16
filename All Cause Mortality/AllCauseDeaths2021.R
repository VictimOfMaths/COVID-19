rm(list=ls())

library(tidyverse)
library(paletteer)
library(curl)
library(readxl)
library(lubridate)
library(forcats)
library(ggtext)
library(ragg)

#Latest date in the country-specific data
EWDate <- "5th March"
ScotDate <- "7th March"
NIDate <- "5th March"

#Locations for 2020/21 data
#England, released at 9:30 on Tuesday mornings 
#https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/datasets/weeklyprovisionalfiguresondeathsregisteredinenglandandwales
Eng2021 <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/datasets/weeklyprovisionalfiguresondeathsregisteredinenglandandwales/2021/publishedweek092021.xlsx"
#Scotland, released at noon on Wednesdays
#https://www.nrscotland.gov.uk/covid19stats
Scot2021 <- "https://www.nrscotland.gov.uk/files//statistics/covid19/covid-deaths-21-data-week-09.xlsx"
#Northern Ireland, released on Fridays
#https://www.nisra.gov.uk/publications/weekly-deaths
NI2021 <- "https://www.nisra.gov.uk/sites/nisra.gov.uk/files/publications/Weekly_Deaths.XLSX"

#Stupid Excel range controls
#These need to be incremented by one letter each week
EngRange <- "K" 
ScotRange <- "K" 
NIRange <- "13" 

##############################
#Read in English & Welsh data#
##############################

#Archive version with 2020 data in it
Eng2020 <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/datasets/weeklyprovisionalfiguresondeathsregisteredinenglandandwales/2020/publishedweek532020.xlsx"

#Start with 2021
temp <- tempfile()
temp <- curl_download(url=Eng2021, destfile=temp, quiet=FALSE, mode="wb")

#By age & sex
data2021.as.EW <- read_excel(temp, sheet="Weekly figures 2021", 
                              range=paste0("B40:", EngRange, "81"), col_names=FALSE) %>% 
  slice(-c(21,22)) %>% 
  mutate(sex=rep(c("Male", "Female"), each=20)) %>% 
  gather(index, deaths, c(2:(ncol(.)-1))) %>% 
  rename(age=`...1`) %>% 
  mutate(date=as.Date("2021-01-08")+weeks(as.numeric(substr(index, 4,6))-2),
         week=as.numeric(substr(index, 4,6))-1,
         year=year(date-days(1))) %>% 
  select(-index) %>% 
  mutate(age=case_when(
    age=="<1" ~ "Under 1 year",
    age %in% c("1-4", "5-9", "10-14") ~ "01-14",
    age %in% c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44") ~ "15-44",
    age %in% c("45-49", "50-54", "55-59", "60-64") ~ "45-64",
    age %in% c("65-69", "70-74") ~ "65-74",
    age %in% c("75-79", "80-84") ~ "75-84",
    TRUE ~ "85+"
  )) %>% 
  group_by(date, week, year, sex, age) %>% 
  summarise(deaths=sum(deaths)) %>% 
  ungroup()

#By region
data2021.reg.EW <- read_excel(temp, sheet="Weekly figures 2021",
                              range=paste0("B83:", EngRange, "92"), col_names=FALSE) %>% 
  gather(index, deaths, c(2:ncol(.))) %>%
  rename(region=`...1`) %>% 
  mutate(date=as.Date("2021-01-08")+weeks(as.numeric(substr(index, 4,6))-2),
         week=as.numeric(substr(index, 4,6))-1,
         year=year(date-days(1))) %>% 
  select(-index)

#2020
temp <- tempfile()
temp <- curl_download(url=Eng2020, destfile=temp, quiet=FALSE, mode="wb")

#By age & sex
data2020.as.EW <- read_excel(temp, sheet="Weekly figures 2020", 
                             range="B44:BC85", col_names=FALSE) %>% 
  slice(-c(21,22)) %>% 
  mutate(sex=rep(c("Male", "Female"), each=20)) %>% 
  gather(index, deaths, c(2:(ncol(.)-1))) %>% 
  rename(age=`...1`) %>% 
  mutate(date=as.Date("2020-01-03")+weeks(as.numeric(substr(index, 4,6))-2),
         week=as.numeric(substr(index, 4,6))-1,
         year=year(date-days(1))) %>% 
  select(-index) %>% 
  mutate(age=case_when(
    age=="<1" ~ "Under 1 year",
    age %in% c("1-4", "5-9", "10-14") ~ "01-14",
    age %in% c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44") ~ "15-44",
    age %in% c("45-49", "50-54", "55-59", "60-64") ~ "45-64",
    age %in% c("65-69", "70-74") ~ "65-74",
    age %in% c("75-79", "80-84") ~ "75-84",
    TRUE ~ "85+"
  )) %>% 
  group_by(date, week, year, sex, age) %>% 
  summarise(deaths=sum(deaths)) %>% 
  ungroup()

#By region
data2020.reg.EW <- read_excel(temp, sheet="Weekly figures 2020",
                              range="B87:BC96", col_names=FALSE) %>% 
  gather(index, deaths, c(2:ncol(.))) %>%
  rename(region=`...1`) %>% 
  mutate(date=as.Date("2020-01-03")+weeks(as.numeric(substr(index, 4,6))-2),
         week=as.numeric(substr(index, 4,6))-1,
         year=year(date-days(1))) %>% 
  select(-index)

#2019
temp <- tempfile()
source <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fbirthsdeathsandmarriages%2fdeaths%2fdatasets%2fweeklyprovisionalfiguresondeathsregisteredinenglandandwales%2f2019/publishedweek522019.xls"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

#By age & sex
data19.as.EW <- read_excel(temp, sheet="Weekly figures 2019", 
                           range="B25:BB40", col_names=FALSE) %>% 
  slice(-c(8,9)) %>% 
  mutate(sex=rep(c("Male", "Female"), each=7)) %>% 
  gather(index, deaths, c(2:(ncol(.)-1))) %>% 
  rename(age=`...1`) %>% 
  mutate(date=as.Date("2019-01-04")+weeks(as.numeric(substr(index, 4,6))-2),
         week=as.numeric(substr(index, 4,6))-1,
         year=year(date)) %>% 
  select(-index) 

#By region
data19.reg.EW <- read_excel(temp, sheet="Weekly figures 2019",
                              range=paste0("B43:", EngRange, "52"), col_names=FALSE) %>% 
  gather(index, deaths, c(2:ncol(.))) %>%
  rename(region=`...1`) %>% 
  mutate(date=as.Date("2019-01-04")+weeks(as.numeric(substr(index, 4,6))-2),
         week=as.numeric(substr(index, 4,6))-1,
         year=year(date)) %>% 
  select(-index)

#2019
temp <- tempfile()
source <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fbirthsdeathsandmarriages%2fdeaths%2fdatasets%2fweeklyprovisionalfiguresondeathsregisteredinenglandandwales%2f2019/publishedweek522019.xls"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

#By age & sex
data19.as.EW <- read_excel(temp, sheet="Weekly figures 2019", 
                           range="B25:BB40", col_names=FALSE) %>% 
  slice(-c(8,9)) %>% 
  mutate(sex=rep(c("Male", "Female"), each=7)) %>% 
  gather(index, deaths, c(2:(ncol(.)-1))) %>% 
  rename(age=`...1`) %>% 
  mutate(date=as.Date("2019-01-04")+weeks(as.numeric(substr(index, 4,6))-2),
         week=as.numeric(substr(index, 4,6))-1,
         year=year(date)) %>% 
  select(-index) 

#By region
data19.reg.EW <- read_excel(temp, sheet="Weekly figures 2019",
                            range=paste0("B43:BB52"), col_names=FALSE) %>% 
  gather(index, deaths, c(2:ncol(.))) %>%
  rename(region=`...1`) %>% 
  mutate(date=as.Date("2019-01-04")+weeks(as.numeric(substr(index, 4,6))-2),
         week=as.numeric(substr(index, 4,6))-1,
         year=year(date)) %>% 
  select(-index)
  
#2018 data
temp <- tempfile()
source <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fbirthsdeathsandmarriages%2fdeaths%2fdatasets%2fweeklyprovisionalfiguresondeathsregisteredinenglandandwales%2f2018/publishedweek522018withupdatedrespiratoryrow.xls"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

#By age & sex
data18.as.EW <- read_excel(temp, sheet="Weekly figures 2018", 
                           range="B25:BB40", col_names=FALSE) %>% 
  slice(-c(8,9)) %>% 
  mutate(sex=rep(c("Male", "Female"), each=7)) %>% 
  gather(index, deaths, c(2:(ncol(.)-1))) %>% 
  rename(age=`...1`) %>% 
  mutate(date=as.Date("2018-01-05")+weeks(as.numeric(substr(index, 4,6))-2),
         week=as.numeric(substr(index, 4,6))-1,
         year=year(date)) %>% 
  select(-index) 

#By region
data18.reg.EW <- read_excel(temp, sheet="Weekly figures 2018",
                            range=paste0("B43:BB52"), col_names=FALSE) %>% 
  gather(index, deaths, c(2:ncol(.))) %>%
  rename(region=`...1`) %>% 
  mutate(date=as.Date("2018-01-05")+weeks(as.numeric(substr(index, 4,6))-2),
         week=as.numeric(substr(index, 4,6))-1,
         year=year(date)) %>% 
  select(-index)  
  
#2017 data
temp <- tempfile()
source <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fbirthsdeathsandmarriages%2fdeaths%2fdatasets%2fweeklyprovisionalfiguresondeathsregisteredinenglandandwales%2f2017/publishedweek522017.xls"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

#By age & sex
data17.as.EW <- read_excel(temp, sheet="Weekly figures 2017", 
                           range="B25:BB40", col_names=FALSE) %>% 
  slice(-c(8,9)) %>% 
  mutate(sex=rep(c("Male", "Female"), each=7)) %>% 
  gather(index, deaths, c(2:(ncol(.)-1))) %>% 
  rename(age=`...1`) %>% 
  mutate(date=as.Date("2017-01-06")+weeks(as.numeric(substr(index, 4,6))-2),
         week=as.numeric(substr(index, 4,6))-1,
         year=year(date)) %>% 
  select(-index) 

#By region
data17.reg.EW <- read_excel(temp, sheet="Weekly figures 2017",
                            range=paste0("B43:BB52"), col_names=FALSE) %>% 
  gather(index, deaths, c(2:ncol(.))) %>%
  rename(region=`...1`) %>% 
  mutate(date=as.Date("2017-01-06")+weeks(as.numeric(substr(index, 4,6))-2),
         week=as.numeric(substr(index, 4,6))-1,
         year=year(date)) %>% 
  select(-index)    
  
#2016 data
temp <- tempfile()
source <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fbirthsdeathsandmarriages%2fdeaths%2fdatasets%2fweeklyprovisionalfiguresondeathsregisteredinenglandandwales%2f2016/publishedweek522016.xls"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

#By age & sex
data16.as.EW <- read_excel(temp, sheet="Weekly figures 2016", 
                           range="B25:BB40", col_names=FALSE) %>% 
  slice(-c(8,9)) %>% 
  mutate(sex=rep(c("Male", "Female"), each=7)) %>% 
  gather(index, deaths, c(2:(ncol(.)-1))) %>% 
  rename(age=`...1`) %>% 
  mutate(date=as.Date("2016-01-08")+weeks(as.numeric(substr(index, 4,6))-2),
         week=as.numeric(substr(index, 4,6))-1,
         year=year(date)) %>% 
  select(-index) 

#By region
data16.reg.EW <- read_excel(temp, sheet="Weekly figures 2016",
                            range=paste0("B43:BB52"), col_names=FALSE) %>% 
  gather(index, deaths, c(2:ncol(.))) %>%
  rename(region=`...1`) %>% 
  mutate(date=as.Date("2016-01-08")+weeks(as.numeric(substr(index, 4,6))-2),
         week=as.numeric(substr(index, 4,6))-1,
         year=year(date)) %>% 
  select(-index)    

#2015 data
temp <- tempfile()
source <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fbirthsdeathsandmarriages%2fdeaths%2fdatasets%2fweeklyprovisionalfiguresondeathsregisteredinenglandandwales%2f2015/publishedweek2015.xls"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

#By age & sex
data15.as.EW <- read_excel(temp, sheet="Weekly Figures 2015", 
                           range="A25:BB40", col_names=FALSE) %>% 
  slice(-c(8,9)) %>% 
  mutate(sex=rep(c("Male", "Female"), each=7)) %>% 
  gather(index, deaths, c(2:(ncol(.)-1))) %>% 
  rename(age=`...1`) %>% 
  mutate(date=as.Date("2015-01-02")+weeks(as.numeric(substr(index, 4,6))-2),
         week=as.numeric(substr(index, 4,6))-1,
         year=year(date-days(1))) %>% 
  select(-index) 

#By region
data15.reg.EW <- read_excel(temp, sheet="Weekly Figures 2015",
                            range=paste0("A43:BB52"), col_names=FALSE) %>% 
  gather(index, deaths, c(2:ncol(.))) %>%
  rename(region=`...1`) %>% 
  mutate(date=as.Date("2015-01-02")+weeks(as.numeric(substr(index, 4,6))-2),
         week=as.numeric(substr(index, 4,6))-1,
         year=year(date-days(1))) %>% 
  select(-index)    

#2014 data
temp <- tempfile()
source <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fbirthsdeathsandmarriages%2fdeaths%2fdatasets%2fweeklyprovisionalfiguresondeathsregisteredinenglandandwales%2f2014/publishedweek2014.xls"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

#By age & sex
data14.as.EW <- read_excel(temp, sheet="Weekly Figures 2014", 
                           range="A25:BA40", col_names=FALSE) %>% 
  slice(-c(8,9)) %>% 
  mutate(sex=rep(c("Male", "Female"), each=7)) %>% 
  gather(index, deaths, c(2:(ncol(.)-1))) %>% 
  rename(age=`...1`) %>% 
  mutate(date=as.Date("2014-01-03")+weeks(as.numeric(substr(index, 4,6))-2),
         week=as.numeric(substr(index, 4,6))-1,
         year=year(date)) %>% 
  select(-index) 

#By region
data14.reg.EW <- read_excel(temp, sheet="Weekly Figures 2014",
                            range=paste0("A43:BA52"), col_names=FALSE) %>% 
  gather(index, deaths, c(2:ncol(.))) %>%
  rename(region=`...1`) %>% 
  mutate(date=as.Date("2014-01-03")+weeks(as.numeric(substr(index, 4,6))-2),
         week=as.numeric(substr(index, 4,6))-1,
         year=year(date)) %>% 
  select(-index)   

#2013 data
temp <- tempfile()
source <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fbirthsdeathsandmarriages%2fdeaths%2fdatasets%2fweeklyprovisionalfiguresondeathsregisteredinenglandandwales%2f2013/publishedweek2013.xls"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

#By age & sex
data13.as.EW <- read_excel(temp, sheet="Weekly Figures 2013", 
                           range="A25:BA40", col_names=FALSE) %>% 
  slice(-c(8,9)) %>% 
  mutate(sex=rep(c("Male", "Female"), each=7)) %>% 
  gather(index, deaths, c(2:(ncol(.)-1))) %>% 
  rename(age=`...1`) %>% 
  mutate(date=as.Date("2013-01-04")+weeks(as.numeric(substr(index, 4,6))-2),
         week=as.numeric(substr(index, 4,6))-1,
         year=year(date)) %>% 
  select(-index) 

#By region
data13.reg.EW <- read_excel(temp, sheet="Weekly Figures 2013",
                            range=paste0("A43:BA52"), col_names=FALSE) %>% 
  gather(index, deaths, c(2:ncol(.))) %>%
  rename(region=`...1`) %>% 
  mutate(date=as.Date("2013-01-04")+weeks(as.numeric(substr(index, 4,6))-2),
         week=as.numeric(substr(index, 4,6))-1,
         year=year(date)) %>% 
  select(-index)  

#2012 data
temp <- tempfile()
source <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fbirthsdeathsandmarriages%2fdeaths%2fdatasets%2fweeklyprovisionalfiguresondeathsregisteredinenglandandwales%2f2012/publishedweek2012.xls"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

#By age & sex
data12.as.EW <- read_excel(temp, sheet="Weekly Figures 2012", 
                           range="A25:BA40", col_names=FALSE) %>% 
  slice(-c(8,9)) %>% 
  mutate(sex=rep(c("Male", "Female"), each=7)) %>% 
  gather(index, deaths, c(2:(ncol(.)-1))) %>% 
  rename(age=`...1`) %>% 
  mutate(date=as.Date("2012-01-06")+weeks(as.numeric(substr(index, 4,6))-2),
         week=as.numeric(substr(index, 4,6))-1,
         year=year(date)) %>% 
  select(-index) 

#By region
data12.reg.EW <- read_excel(temp, sheet="Weekly Figures 2012",
                            range=paste0("A43:BA52"), col_names=FALSE) %>% 
  gather(index, deaths, c(2:ncol(.))) %>%
  rename(region=`...1`) %>% 
  mutate(date=as.Date("2012-01-06")+weeks(as.numeric(substr(index, 4,6))-2),
         week=as.numeric(substr(index, 4,6))-1,
         year=year(date)) %>% 
  select(-index)    

#2011 data
temp <- tempfile()
source <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fbirthsdeathsandmarriages%2fdeaths%2fdatasets%2fweeklyprovisionalfiguresondeathsregisteredinenglandandwales%2f2011/publishedweek2011.xls"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

#By age & sex
data11.as.EW <- read_excel(temp, sheet="Weekly Figures 2011", 
                           range="A26:BA41", col_names=FALSE) %>% 
  slice(-c(8,9)) %>% 
  mutate(sex=rep(c("Male", "Female"), each=7)) %>% 
  gather(index, deaths, c(2:(ncol(.)-1))) %>% 
  rename(age=`...1`) %>% 
  mutate(date=as.Date("2011-01-07")+weeks(as.numeric(substr(index, 4,6))-2),
         week=as.numeric(substr(index, 4,6))-1,
         year=year(date)) %>% 
  select(-index) 

#By region
data11.reg.EW <- read_excel(temp, sheet="Weekly Figures 2011",
                            range=paste0("A44:BA53"), col_names=FALSE) %>% 
  gather(index, deaths, c(2:ncol(.))) %>%
  rename(region=`...1`) %>% 
  mutate(date=as.Date("2011-01-07")+weeks(as.numeric(substr(index, 4,6))-2),
         week=as.numeric(substr(index, 4,6))-1,
         year=year(date)) %>% 
  select(-index) 

#2010 data
temp <- tempfile()
source <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fbirthsdeathsandmarriages%2fdeaths%2fdatasets%2fweeklyprovisionalfiguresondeathsregisteredinenglandandwales%2f2010/publishedweek2010.xls"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

#By age & sex
data10.as.EW <- read_excel(temp, sheet="Weekly Figures 2010", 
                           range="A25:BA40", col_names=FALSE) %>% 
  slice(-c(8,9)) %>% 
  mutate(sex=rep(c("Male", "Female"), each=7)) %>% 
  gather(index, deaths, c(2:(ncol(.)-1))) %>% 
  rename(age=`...1`) %>% 
  mutate(date=as.Date("2010-01-08")+weeks(as.numeric(substr(index, 4,6))-2),
         week=as.numeric(substr(index, 4,6))-1,
         year=year(date)) %>% 
  select(-index) 

#By region
data10.reg.EW <- read_excel(temp, sheet="Weekly Figures 2010",
                            range=paste0("A43:BA52"), col_names=FALSE) %>% 
  gather(index, deaths, c(2:ncol(.))) %>%
  rename(region=`...1`) %>% 
  mutate(date=as.Date("2010-01-08")+weeks(as.numeric(substr(index, 4,6))-2),
         week=as.numeric(substr(index, 4,6))-1,
         year=year(date)) %>% 
  select(-index)      
  
#Merge together
data.as.EW <- bind_rows(data10.as.EW, data11.as.EW, data12.as.EW, data13.as.EW,
                        data14.as.EW, data15.as.EW, data16.as.EW, data17.as.EW,
                        data18.as.EW, data19.as.EW, data2020.as.EW, data2021.as.EW) %>% 
  #Join <1 and 1-14 age bands
  mutate(age=case_when(
    age %in% c("Under 1 year", "01-14") ~ "Under 15",
    TRUE ~ age)) %>% 
  group_by(age, sex, date, week, year) %>% 
  summarise(deaths=sum(deaths)) %>% 
  ungroup()

#Add total rows, by age, sex and overall
agetot <- data.as.EW %>% 
  group_by(age, date, week, year) %>%
  summarise(deaths=sum(deaths)) %>% 
  mutate(sex="Total") %>% 
  ungroup()

sextot <- data.as.EW %>% 
  group_by(sex, date, week, year) %>% 
  summarise(deaths=sum(deaths)) %>% 
  mutate(age="Total") %>% 
  ungroup()

alltot <- data.as.EW %>% 
  group_by(date, week, year) %>% 
  summarise(deaths=sum(deaths)) %>% 
  mutate(age="Total", sex="Total") %>% 
  ungroup()

data.as.EW <- bind_rows(data.as.EW, agetot, sextot, alltot)
  
data.reg.EW <- bind_rows(data10.reg.EW, data11.reg.EW, data12.reg.EW, data13.reg.EW,
                         data14.reg.EW, data15.reg.EW, data16.reg.EW, data17.reg.EW,
                         data18.reg.EW, data19.reg.EW, data2020.reg.EW, data2021.reg.EW)

rm(data10.as.EW, data11.as.EW, data12.as.EW, data13.as.EW, data14.as.EW, data15.as.EW, 
   data16.as.EW, data17.as.EW, data18.as.EW, data19.as.EW, data2021.as.EW, data10.reg.EW, 
   data11.reg.EW, data12.reg.EW, data13.reg.EW, data14.reg.EW, data15.reg.EW, 
   data16.reg.EW, data17.reg.EW, data18.reg.EW, data19.reg.EW, data2021.reg.EW,
   agetot, sextot, alltot, data2020.as.EW, data2020.reg.EW)

#Bring in data by location for 20/21 which is *horribly* formatted
temp <- tempfile()
temp <- curl_download(url=Eng2020, destfile=temp, quiet=FALSE, mode="wb")
#2020
temp1 <- as.data.frame(t(read_excel(temp, sheet=11, range="B9:B14", col_names=FALSE)))
temp2 <- as.data.frame(t(read_excel(temp, sheet=11, range="H9:H14", col_names=FALSE)))
temp3 <- as.data.frame(t(read_excel(temp, sheet=11, range="N9:N14", col_names=FALSE)))
temp4 <- as.data.frame(t(read_excel(temp, sheet=11, range="T9:T14", col_names=FALSE)))
temp5 <- as.data.frame(t(read_excel(temp, sheet=11, range="Z9:Z14", col_names=FALSE)))
temp6 <- as.data.frame(t(read_excel(temp, sheet=11, range="AF9:AF14", col_names=FALSE)))
temp7 <- as.data.frame(t(read_excel(temp, sheet=11, range="AL9:AL14", col_names=FALSE)))
temp8 <- as.data.frame(t(read_excel(temp, sheet=11, range="AR9:AR14", col_names=FALSE)))
temp9 <- as.data.frame(t(read_excel(temp, sheet=11, range="AX9:AX14", col_names=FALSE)))
temp10 <- as.data.frame(t(read_excel(temp, sheet=11, range="BD9:BD14", col_names=FALSE)))
temp11 <- as.data.frame(t(read_excel(temp, sheet=11, range="BJ9:BJ14", col_names=FALSE)))
temp12 <- as.data.frame(t(read_excel(temp, sheet=11, range="BP9:BP14", col_names=FALSE)))
temp13 <- as.data.frame(t(read_excel(temp, sheet=11, range="BV9:BV14", col_names=FALSE)))
temp14 <- as.data.frame(t(read_excel(temp, sheet=11, range="CB9:CB14", col_names=FALSE)))
temp15 <- as.data.frame(t(read_excel(temp, sheet=11, range="CH9:CH14", col_names=FALSE)))
temp16 <- as.data.frame(t(read_excel(temp, sheet=11, range="CN9:CN14", col_names=FALSE)))
temp17 <- as.data.frame(t(read_excel(temp, sheet=11, range="CT9:CT14", col_names=FALSE)))
temp18 <- as.data.frame(t(read_excel(temp, sheet=11, range="CZ9:CZ14", col_names=FALSE)))
temp19 <- as.data.frame(t(read_excel(temp, sheet=11, range="DF9:DF14", col_names=FALSE)))
temp20 <- as.data.frame(t(read_excel(temp, sheet=11, range="DL9:DL14", col_names=FALSE)))
temp21 <- as.data.frame(t(read_excel(temp, sheet=11, range="DR9:DR14", col_names=FALSE)))
temp22 <- as.data.frame(t(read_excel(temp, sheet=11, range="DX9:DX14", col_names=FALSE)))
temp23 <- as.data.frame(t(read_excel(temp, sheet=11, range="ED9:ED14", col_names=FALSE)))
temp24 <- as.data.frame(t(read_excel(temp, sheet=11, range="EJ9:EJ14", col_names=FALSE)))
temp25 <- as.data.frame(t(read_excel(temp, sheet=11, range="EP9:EP14", col_names=FALSE)))
temp26 <- as.data.frame(t(read_excel(temp, sheet=11, range="EV9:EV14", col_names=FALSE)))
temp27 <- as.data.frame(t(read_excel(temp, sheet=11, range="FB9:FB14", col_names=FALSE)))
temp28 <- as.data.frame(t(read_excel(temp, sheet=11, range="FH9:FH14", col_names=FALSE)))
temp29 <- as.data.frame(t(read_excel(temp, sheet=11, range="FN9:FN14", col_names=FALSE)))
temp30 <- as.data.frame(t(read_excel(temp, sheet=11, range="FT9:FT14", col_names=FALSE)))
temp31 <- as.data.frame(t(read_excel(temp, sheet=11, range="FZ9:FZ14", col_names=FALSE)))
temp32 <- as.data.frame(t(read_excel(temp, sheet=11, range="GF9:GF14", col_names=FALSE)))
temp33 <- as.data.frame(t(read_excel(temp, sheet=11, range="GL9:GL14", col_names=FALSE)))
temp34 <- as.data.frame(t(read_excel(temp, sheet=11, range="GR9:GR14", col_names=FALSE)))
temp35 <- as.data.frame(t(read_excel(temp, sheet=11, range="GX9:GX14", col_names=FALSE)))
temp36 <- as.data.frame(t(read_excel(temp, sheet=11, range="HD9:HD14", col_names=FALSE)))
temp37 <- as.data.frame(t(read_excel(temp, sheet=11, range="HJ9:HJ14", col_names=FALSE)))
temp38 <- as.data.frame(t(read_excel(temp, sheet=11, range="HP9:HP14", col_names=FALSE)))
temp39 <- as.data.frame(t(read_excel(temp, sheet=11, range="HV9:HV14", col_names=FALSE)))
temp40 <- as.data.frame(t(read_excel(temp, sheet=11, range="IB9:IB14", col_names=FALSE)))
temp41 <- as.data.frame(t(read_excel(temp, sheet=11, range="IH9:IH14", col_names=FALSE)))
temp42 <- as.data.frame(t(read_excel(temp, sheet=11, range="IN9:IN14", col_names=FALSE)))
temp43 <- as.data.frame(t(read_excel(temp, sheet=11, range="IT9:IT14", col_names=FALSE)))
#2021
temp <- tempfile()
temp <- curl_download(url=Eng2021, destfile=temp, quiet=FALSE, mode="wb")

temp44 <- as.data.frame(t(read_excel(temp, sheet=12, range="B10:B15", col_names=FALSE)))
temp45 <- as.data.frame(t(read_excel(temp, sheet=12, range="H10:H15", col_names=FALSE)))
temp46 <- as.data.frame(t(read_excel(temp, sheet=12, range="N10:N15", col_names=FALSE)))
temp47 <- as.data.frame(t(read_excel(temp, sheet=12, range="T10:T15", col_names=FALSE)))
temp48 <- as.data.frame(t(read_excel(temp, sheet=12, range="Z10:Z15", col_names=FALSE)))
temp49 <- as.data.frame(t(read_excel(temp, sheet=12, range="AF10:AF15", col_names=FALSE)))
temp50 <- as.data.frame(t(read_excel(temp, sheet=12, range="AL10:AL15", col_names=FALSE)))
temp51 <- as.data.frame(t(read_excel(temp, sheet=12, range="AR10:AR15", col_names=FALSE)))
temp52 <- as.data.frame(t(read_excel(temp, sheet=12, range="AX10:AX15", col_names=FALSE)))

data2021.loc <- bind_rows(temp1, temp2, temp3, temp4, temp5, temp6, temp7, temp8, temp9, temp10, 
                    temp11, temp12, temp13, temp14, temp15, temp16, temp17, temp18, temp19, 
                    temp20, temp21, temp22, temp23, temp24, temp25, temp26, temp27, temp28, 
                    temp29, temp30, temp31, temp32, temp33, temp34, temp35, temp36, temp37, 
                    temp38, temp39, temp40, temp41, temp42, temp43, temp44, temp45, temp46,
                    temp47, temp48, temp49, temp50, temp51, temp52) %>% 
  mutate(week=c(11:(nrow(.)+10)),
         year=if_else(week<=53, 2020, 2021),
         week=if_else(week>53, week-53, as.double(week)),
         "Home/Other"=(V1+V3+V5+V6)) %>% 
  rename("Care Home"=V4, "Hospital"=V2) %>% 
  select(-c(V1, V3, V5, V6)) %>% 
  gather(location, deaths, c(1,2,5))

#Data by location for 2015-19
temp <- tempfile()
source <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/adhocs/11622fiveyearaverageweeklydeathsbyplaceofdeathenglandandwalesdeathsoccurringbetween2015and2019/fiveyearavgweeklydeaths2015to2019podfinal.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

data1519.loc <- read_excel(temp, sheet="Table", range="A4:G56") %>% 
  mutate(week=as.numeric(substr(...1, 6,7)),
         year=1519,
         `Home/Other`=Elsewhere+Home+Hospice+`Other communal establishment`) %>% 
  select(-c(1, 3:5, 7)) %>% 
  rename(`Care Home`=`Care home`) %>% 
  gather(location, deaths, c(`Home/Other`, Hospital, `Care Home`))

#Join together
data.loc.EW <- bind_rows(data2021.loc, data1519.loc)

rm(data2021.loc, data1519.loc, temp1, temp2, temp3, temp4, temp5, temp6, temp7,
   temp8, temp9, temp10, temp11, temp12, temp13, temp14, temp15, temp16, temp17, 
   temp18, temp19, temp20, temp21, temp22, temp23, temp24, temp25, temp26, temp27,
   temp28, temp29, temp30, temp31, temp32, temp33, temp34, temp35, temp36, temp37,
   temp38, temp39, temp40, temp41, temp42, temp43, temp44)

#By cause
#2020
temp <- tempfile()
temp <- curl_download(url=Eng2020, destfile=temp, quiet=FALSE, mode="wb")

data2020.cause.EW <- read_excel(temp, sheet=5, range="C9:BC19", 
                            col_names=FALSE) %>% 
  slice(c(1,3,11)) %>% 
  gather(week, deaths) %>% 
  mutate(cause=rep(c("Total2020", "Mean1519", "COVID2020"), times=(nrow(.)/3)),
         week=as.numeric(substr(week, 4, 6))) %>% 
  spread(cause, deaths) %>% 
  mutate(other=Total2020-COVID2020, otherexcess=other-Mean1519,
         netexcess=Total2020-Mean1519)

#2021 (this data is now very unhelpfully spread over multiple sheets)
temp <- tempfile()
temp <- curl_download(url=Eng2021, destfile=temp, quiet=FALSE, mode="wb")

#Grab total deaths
allcause2021.EW <- as.data.frame(t(read_excel(temp, sheet="Weekly figures 2021", 
                              range=paste0("C9:", EngRange, "9"), col_names=FALSE))) %>% 
  mutate(week=seq(1:nrow(.))) %>% 
  rename(Total2021=V1)

#Grab COVID-19 deaths
COVID2021.EW <- as.data.frame(t(read_excel(temp, sheet="Covid-19 - Weekly registrations", 
                                              range=paste0("C9:", EngRange, "9"), col_names=FALSE))) %>% 
  mutate(week=seq(1:nrow(.))) %>% 
  rename(COVID2021=V1)

data2021.cause.EW <- data2020.cause.EW %>% 
  merge(allcause2021.EW, all.x=TRUE) %>% 
  merge(COVID2021.EW, all.x=TRUE) %>% 
  filter(!is.na(Total2021)) %>% 
  select(week, Mean1519, COVID2021, Total2021) %>% 
  mutate(other=Total2021-COVID2021, otherexcess=other-Mean1519,
         netexcess=Total2021-Mean1519, year=2021) %>% 
  select(week, year, COVID2021, otherexcess, netexcess)

data.cause.EW <- data2020.cause.EW %>% 
  mutate(year=2020) %>% 
  select(week, year, COVID2020, otherexcess, netexcess) %>% 
  rename(COVID=COVID2020) %>% 
  bind_rows(data2021.cause.EW %>% rename(COVID=COVID2021))%>% 
  gather(cause, deaths, c(3:5))

rm(data2020.cause.EW, data2021.cause.EW, allcause2021.EW, COVID2021.EW)

#######################
#Read in Scottish data#
#######################

temp <- tempfile()
temp <- curl_download(url=Scot2021, destfile=temp, quiet=FALSE, mode="wb")

#Dowload 2020/21 data from the latest spreadsheet

#By age and sex
data20.as.S <- read_excel(temp, sheet="Table 2  (2020)", 
                         range="B24:BC38", col_names=FALSE) %>% 
  slice(-c(8)) %>% 
  mutate(sex=rep(c("Female", "Male"), each=7)) %>% 
  gather(index, deaths, c(2:(ncol(.)-1))) %>% 
  rename(age=`...1`) %>% 
  mutate(date=as.Date("2020-01-05")+weeks(as.numeric(substr(index, 4,6))-2),
         week=as.numeric(substr(index, 4,6))-1,
         year=year(date-days(4)),
         deaths=as.numeric(deaths)) %>% 
  select(-index) 

data21.as.S <- read_excel(temp, sheet="Table 2 (2021)", 
                            range=paste0("B24:", ScotRange, "38"), col_names=FALSE) %>% 
  slice(-c(8)) %>% 
  mutate(sex=rep(c("Female", "Male"), each=7)) %>% 
  gather(index, deaths, c(2:(ncol(.)-1))) %>% 
  rename(age=`...1`) %>% 
  mutate(date=as.Date("2021-01-09")+weeks(as.numeric(substr(index, 4,6))-2),
         week=as.numeric(substr(index, 4,6))-1,
         year=year(date),
         deaths=as.numeric(deaths)) %>% 
  select(-index) 

#By Health Board
data20.HB.S <- read_excel(temp, sheet="Table 2  (2020)", 
                            range="B40:BC53", col_names=FALSE) %>% 
  gather(index, deaths, c(2:ncol(.))) %>% 
  rename(HB=`...1`) %>% 
  mutate(date=as.Date("2020-01-05")+weeks(as.numeric(substr(index, 4,6))-2),
         week=as.numeric(substr(index, 4,6))-1,
         year=year(date-days(4)),
         deaths=as.numeric(deaths)) %>% 
  select(-index) 

data21.HB.S <- read_excel(temp, sheet="Table 2 (2021)", 
                            range=paste0("B40:", ScotRange, "53"), col_names=FALSE) %>% 
 
  gather(index, deaths, c(2:ncol(.))) %>% 
  rename(HB=`...1`) %>% 
  mutate(date=as.Date("2021-01-09")+weeks(as.numeric(substr(index, 4,6))-2),
         week=as.numeric(substr(index, 4,6))-1,
         year=year(date),
         deaths=as.numeric(deaths)) %>% 
  select(-index) 

#By place of death
data20.loc.S <- read_excel(temp, sheet="Table 2  (2020)", 
                            range="B90:BC93", col_names=FALSE) %>% 
  gather(index, deaths, c(2:ncol(.))) %>% 
  rename(loc=`...1`) %>% 
  mutate(date=as.Date("2020-01-05")+weeks(as.numeric(substr(index, 4,6))-2),
         week=as.numeric(substr(index, 4,6))-1,
         year=year(date-days(4)),
         deaths=as.numeric(deaths)) %>% 
  select(-index) 

data21.loc.S <- read_excel(temp, sheet="Table 2 (2021)", 
                            range=paste0("B90:", ScotRange, "93"), col_names=FALSE) %>% 
  gather(index, deaths, c(2:ncol(.))) %>% 
  rename(loc=`...1`) %>% 
  mutate(date=as.Date("2021-01-09")+weeks(as.numeric(substr(index, 4,6))-2),
         week=as.numeric(substr(index, 4,6))-1,
         year=year(date),
         deaths=as.numeric(deaths)) %>% 
  select(-index) 

#Download historical data
#By sex and age
temp <- tempfile()
source <- "https://www.nrscotland.gov.uk/files//statistics/covid19/weekly-deaths-by-sex-age-2000-2019.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

data10.as.S <- read_excel(temp, sheet="2010", range="B5:BC44", col_names=FALSE) %>% 
  mutate(sex=rep(c("Female", "Male"), each=20), year=2010) %>% 
  select(-`...2`) %>% 
  rename(age=`...1`) %>% 
  gather(week, deaths, c(2:(ncol(.)-2))) %>% 
  mutate(week=as.numeric(substr(week, 4, 6))-2)

data11.as.S <- read_excel(temp, sheet="2011", range="B5:BC44", col_names=FALSE) %>% 
  mutate(sex=rep(c("Female", "Male"), each=20), year=2011) %>% 
  select(-`...2`) %>% 
  rename(age=`...1`) %>% 
  gather(week, deaths, c(2:(ncol(.)-2))) %>% 
  mutate(week=as.numeric(substr(week, 4, 6))-2)

data12.as.S <- read_excel(temp, sheet="2012", range="B5:BC44", col_names=FALSE) %>% 
  mutate(sex=rep(c("Female", "Male"), each=20), year=2012) %>% 
  select(-`...2`) %>% 
  rename(age=`...1`) %>% 
  gather(week, deaths, c(2:(ncol(.)-2))) %>% 
  mutate(week=as.numeric(substr(week, 4, 6))-2)

data13.as.S <- read_excel(temp, sheet="2013", range="B5:BC44", col_names=FALSE) %>% 
  mutate(sex=rep(c("Female", "Male"), each=20), year=2013) %>% 
  select(-`...2`) %>% 
  rename(age=`...1`) %>% 
  gather(week, deaths, c(2:(ncol(.)-2))) %>% 
  mutate(week=as.numeric(substr(week, 4, 6))-2)

data14.as.S <- read_excel(temp, sheet="2014", range="B5:BC44", col_names=FALSE) %>% 
  mutate(sex=rep(c("Female", "Male"), each=20), year=2014) %>% 
  select(-`...2`) %>% 
  rename(age=`...1`) %>% 
  gather(week, deaths, c(2:(ncol(.)-2))) %>% 
  mutate(week=as.numeric(substr(week, 4, 6))-2)

data15.as.S <- read_excel(temp, sheet="2015", range="B5:BD44", col_names=FALSE) %>% 
  mutate(sex=rep(c("Female", "Male"), each=20), year=2015) %>% 
  select(-`...2`) %>% 
  rename(age=`...1`) %>% 
  gather(week, deaths, c(2:(ncol(.)-2))) %>% 
  mutate(week=as.numeric(substr(week, 4, 6))-2)

data16.as.S <- read_excel(temp, sheet="2016", range="B5:BC44", col_names=FALSE) %>% 
  mutate(sex=rep(c("Female", "Male"), each=20), year=2016) %>% 
  select(-`...2`) %>% 
  rename(age=`...1`) %>% 
  gather(week, deaths, c(2:(ncol(.)-2))) %>% 
  mutate(week=as.numeric(substr(week, 4, 6))-2)

data17.as.S <- read_excel(temp, sheet="2017", range="B5:BC44", col_names=FALSE) %>% 
  mutate(sex=rep(c("Female", "Male"), each=20), year=2017) %>% 
  select(-`...2`) %>% 
  rename(age=`...1`) %>% 
  gather(week, deaths, c(2:(ncol(.)-2))) %>% 
  mutate(week=as.numeric(substr(week, 4, 6))-2)

data18.as.S <- read_excel(temp, sheet="2018", range="B5:BC44", col_names=FALSE) %>% 
  mutate(sex=rep(c("Female", "Male"), each=20), year=2018) %>% 
  select(-`...2`) %>% 
  rename(age=`...1`) %>% 
  gather(week, deaths, c(2:(ncol(.)-2))) %>% 
  mutate(week=as.numeric(substr(week, 4, 6))-2)

data19.as.S <- read_excel(temp, sheet="2019", range="B5:BC44", col_names=FALSE) %>% 
  mutate(sex=rep(c("Female", "Male"), each=20), year=2019) %>% 
  select(-`...2`) %>% 
  rename(age=`...1`) %>% 
  gather(week, deaths, c(2:(ncol(.)-2))) %>% 
  mutate(week=as.numeric(substr(week, 4, 6))-2)

#By health board
#Read in 2015-19 health board data (pre-2015 data seemingly not available)
temp <- tempfile()
source <- "https://www.nrscotland.gov.uk/files//statistics/covid19/weekly-HB-and-CA-2015-2019.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

data1519.HB.S <- read_excel(temp, sheet=1, range="A5:BC74", col_names=FALSE) %>% 
  rename(HB=`...1`, year=`...2`) %>% 
  fill(HB) %>% 
  gather(week, deaths, c(3:ncol(.))) %>% 
  mutate(week=as.numeric(substr(week, 4, 6))-2) %>% 
  filter(!(week==53 & year!=2015))

#By location
#Read in 2015-19 location data (pre-2015 data seemingly not available)
temp <- tempfile()
source <- "https://www.nrscotland.gov.uk/files//statistics/covid19/weekly-deaths-by-location-2015-2019.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

data1519.loc.S <- read_excel(temp, range="A4:BB31", col_names=FALSE) %>% 
  slice(-c(1,7,8,14,15,21,22,28)) %>% 
  mutate(loc=rep(c("Care Home", "Home / Non-institution", "Hospital", "Other institution"), 
                 each=5)) %>% 
  rename(year=`...1`) %>% 
  gather(week, deaths, c(2:(ncol(.)-1))) %>% 
  mutate(week=as.numeric(substr(week, 4, 6))-1,
         year=as.numeric(year)) %>% 
  filter(!(week==53 & year!=2015))

#By cause (and location)
temp <- curl_download(url=Scot2021, destfile=temp, quiet=FALSE, mode="wb")

#2015-19 data for all locations
data1519.all.cause.S <- read_excel(temp, sheet="Table 3 (2020)", range="B7:BC12", col_names=FALSE) %>% 
  rename(cause=`...1`) %>% 
  gather(week, deaths, c(2:ncol(.))) %>% 
  mutate(week=as.numeric(substr(week, 4, 6))-1,
         year=1519, loc="All")

#2015-19 data for care homes
data1519.ch.cause.S <- read_excel(temp, sheet="Table 3 (2020)", range="B31:BC36", col_names=FALSE) %>% 
  rename(cause=`...1`) %>% 
  gather(week, deaths, c(2:ncol(.))) %>% 
  mutate(week=as.numeric(substr(week, 4, 6))-1,
         year=1519, loc="Care Home")

#2015-19 data for home
data1519.home.cause.S <- read_excel(temp, sheet="Table 3 (2020)", range="B55:BC60", col_names=FALSE) %>% 
  rename(cause=`...1`) %>% 
  gather(week, deaths, c(2:ncol(.))) %>% 
  mutate(week=as.numeric(substr(week, 4, 6))-1,
         year=1519, loc="Home")

#2015-19 data for hospital
data1519.hosp.cause.S <- read_excel(temp, sheet="Table 3 (2020)", range="B79:BC84", col_names=FALSE) %>% 
  rename(cause=`...1`) %>% 
  gather(week, deaths, c(2:ncol(.))) %>% 
  mutate(week=as.numeric(substr(week, 4, 6))-1,
         year=1519, loc="Hospital")

#2015-19 data for other (to be combined with home)
data1519.oth.cause.S <- read_excel(temp, sheet="Table 3 (2020)", range="B103:BC108", col_names=FALSE) %>% 
  rename(cause=`...1`) %>% 
  gather(week, deaths, c(2:ncol(.))) %>% 
  mutate(week=as.numeric(substr(week, 4, 6))-1,
         year=1519, loc="Other")

#Stick 15-19 data together
data1519.cause.S <- bind_rows(data1519.ch.cause.S, data1519.home.cause.S, data1519.hosp.cause.S,
                              data1519.oth.cause.S, data1519.all.cause.S)

#Read in 2020 data
#all locations
data2020.all.cause.S <- read_excel(temp, sheet="Table 3 (2020)", range="B15:BC20", col_names=FALSE) %>% 
  rename(cause=`...1`) %>% 
  gather(week, deaths, c(2:ncol(.))) %>% 
  mutate(week=as.numeric(substr(week, 4, 6))-1,
         year=2020, loc="All")

#care homes
data2020.ch.cause.S <- read_excel(temp, sheet="Table 3 (2020)", range="B39:BC44", col_names=FALSE) %>% 
  rename(cause=`...1`) %>% 
  gather(week, deaths, c(2:ncol(.))) %>% 
  mutate(week=as.numeric(substr(week, 4, 6))-1,
         year=2020, loc="Care Home")

#home
data2020.home.cause.S <- read_excel(temp, sheet="Table 3 (2020)", range="B63:BC68", col_names=FALSE) %>% 
  rename(cause=`...1`) %>% 
  gather(week, deaths, c(2:ncol(.))) %>% 
  mutate(week=as.numeric(substr(week, 4, 6))-1,
         year=2020, loc="Home")

#hospital
data2020.hosp.cause.S <- read_excel(temp, sheet="Table 3 (2020)", range="B87:BC92", col_names=FALSE) %>% 
  rename(cause=`...1`) %>% 
  gather(week, deaths, c(2:ncol(.))) %>% 
  mutate(week=as.numeric(substr(week, 4, 6))-1,
         year=2020, loc="Hospital")

#other (to be combined with home)
data2020.oth.cause.S <- read_excel(temp, sheet="Table 3 (2020)", range="B111:BC116", col_names=FALSE) %>% 
  rename(cause=`...1`) %>% 
  gather(week, deaths, c(2:ncol(.))) %>% 
  mutate(week=as.numeric(substr(week, 4, 6))-1,
         year=2020, loc="Other")

#Stick 2020 data together
data2020.cause.S <- bind_rows(data2020.ch.cause.S, data2020.home.cause.S, data2020.hosp.cause.S,
                              data2020.oth.cause.S, data2020.all.cause.S)

#Read in 2021 data
#all locations
data2021.all.cause.S <- read_excel(temp, sheet="Table 3  (2021)", 
                                   range=paste0("B15:", ScotRange, "20"), col_names=FALSE) %>% 
  rename(cause=`...1`) %>% 
  gather(week, deaths, c(2:ncol(.))) %>% 
  mutate(week=as.numeric(substr(week, 4, 6))-1,
         year=2021, loc="All")

#care homes
data2021.ch.cause.S <- read_excel(temp, sheet="Table 3  (2021)", 
                                  range=paste0("B39:", ScotRange, "44"), col_names=FALSE) %>% 
  rename(cause=`...1`) %>% 
  gather(week, deaths, c(2:ncol(.))) %>% 
  mutate(week=as.numeric(substr(week, 4, 6))-1,
         year=2021, loc="Care Home")

#home
data2021.home.cause.S <- read_excel(temp, sheet="Table 3  (2021)", 
                                    range=paste0("B63:", ScotRange, "68"), col_names=FALSE) %>% 
  rename(cause=`...1`) %>% 
  gather(week, deaths, c(2:ncol(.))) %>% 
  mutate(week=as.numeric(substr(week, 4, 6))-1,
         year=2021, loc="Home")

#hospital
data2021.hosp.cause.S <- read_excel(temp, sheet="Table 3  (2021)", 
                                    range=paste0("B87:",ScotRange,  "92"), col_names=FALSE) %>% 
  rename(cause=`...1`) %>% 
  gather(week, deaths, c(2:ncol(.))) %>% 
  mutate(week=as.numeric(substr(week, 4, 6))-1,
         year=2021, loc="Hospital")

#other (to be combined with home)
data2021.oth.cause.S <- read_excel(temp, sheet="Table 3  (2021)", 
                                   range=paste0("B111:", ScotRange, "116"), col_names=FALSE) %>% 
  rename(cause=`...1`) %>% 
  gather(week, deaths, c(2:ncol(.))) %>% 
  mutate(week=as.numeric(substr(week, 4, 6))-1,
         year=2021, loc="Other")

#Stick 2020 data together
data2021.cause.S <- bind_rows(data2021.ch.cause.S, data2021.home.cause.S, data2021.hosp.cause.S,
                              data2021.oth.cause.S, data2021.all.cause.S)

#Combine years
data.cause.S <- bind_rows(data1519.cause.S, data2020.cause.S, data2021.cause.S) %>% 
  mutate(loc=case_when(
    loc %in% c("Home", "Other") ~ "Home/Other",
    TRUE ~ loc),
    cause=if_else(cause=="Circulatory (heart disease and stroke)", "Circulatory", cause)) %>% 
  group_by(cause, week, year, loc) %>% 
  summarise(deaths=sum(deaths)) %>% 
  ungroup() %>% 
  spread(year, deaths)

data2021.cause.excess.S <- data.cause.S %>% 
  filter(!is.na(`2021`)) %>% 
  mutate(excess=`2021`-`1519`, year=2021) %>% 
  select(cause, loc, week, year, excess)

data.cause.S <- data.cause.S %>% 
  mutate(excess=`2020`-`1519`, year=2020) %>% 
  select(cause, loc, week, year, excess) %>% 
  bind_rows(data2021.cause.excess.S)

#Merge together
data.as.S <- bind_rows(data10.as.S, data11.as.S, data12.as.S, data13.as.S,
                       data14.as.S, data15.as.S, data16.as.S, data17.as.S,
                       data18.as.S, data19.as.S) %>% 
  #Compress age bands to match 2020 data
  mutate(age=case_when(
    age %in% c("0", "1-4", "5-9", "10-14") ~ "Under 15",
    age %in% c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44") ~ "15-44",
    age %in% c("45-49", "50-54", "55-59", "60-64") ~ "45-64",
    age %in% c("65-69", "70-74") ~ "65-74",
    age %in% c("75-79", "80-84") ~ "75-84",
    TRUE ~ "85+")) %>% 
  group_by(age, sex, year, week) %>% 
  summarise(deaths=sum(deaths)) %>% 
  ungroup() 

data.as.S <- bind_rows(data20.as.S, data21.as.S) %>% 
  mutate(age=case_when(
    age %in% c("Under 1 year", "01-14") ~ "Under 15",
    TRUE ~ age)) %>% 
  group_by(age, sex, year, week, date) %>% 
  summarise(deaths=sum(deaths)) %>% 
  ungroup() %>% 
  bind_rows(data.as.S)

#Add total rows, by age, sex and overall
agetot.S <- data.as.S %>% 
  group_by(age, date, week, year) %>%
  summarise(deaths=sum(deaths)) %>% 
  mutate(sex="Total") %>% 
  ungroup()

sextot.S <- data.as.S %>% 
  group_by(sex, date, week, year) %>% 
  summarise(deaths=sum(deaths)) %>% 
  mutate(age="Total") %>% 
  ungroup()

alltot.S <- data.as.S %>% 
  group_by(date, week, year) %>% 
  summarise(deaths=sum(deaths)) %>% 
  mutate(age="Total", sex="Total") %>% 
  ungroup()

data.as.S <- bind_rows(data.as.S, agetot.S, sextot.S, alltot.S)

data.HB.S <- bind_rows(data1519.HB.S, data20.HB.S, data21.HB.S)

data.loc.S <- bind_rows(data1519.loc.S, data20.loc.S, data21.loc.S)

rm(data10.as.S, data11.as.S, data12.as.S, data13.as.S, data14.as.S, data15.as.S,
   data16.as.S, data17.as.S, data18.as.S, data19.as.S, data20.as.S, data21.as.S,
   data1519.HB.S, data20.HB.S, data21.HB.S, data1519.loc.S,
   data20.loc.S, data21.loc.S, agetot.S, sextot.S, alltot.S, data1519.all.cause.S,
   data1519.cause.S, data1519.ch.cause.S, data1519.home.cause.S, data1519.hosp.cause.S,
   data1519.oth.cause.S, data2020.cause.S, data2020.all.cause.S, data2020.ch.cause.S,
   data2020.home.cause.S, data2020.hosp.cause.S, data2020.oth.cause.S, data2021.cause.excess.S,
   data2021.cause.S, data2021.all.cause.S, data2021.ch.cause.S, data2021.home.cause.S, 
   data2021.hosp.cause.S, data2021.oth.cause.S)

#############################
#Read in Northern Irish data#
#############################

#No data is (easily) available for Northern Ireland on historic deaths by age or place of death
temp <- tempfile()
temp <- curl_download(url=NI2021, destfile=temp, quiet=FALSE, mode="wb")

#Download 2021 data from the latest spreadsheet
data2021.NI <- read_excel(temp, sheet="Table 1", range=paste0("B5:C", NIRange), col_names=FALSE) %>% 
  mutate(week=c(54:(nrow(.)+53)), year=2021)
colnames(data2021.NI) <- c("date", "deaths", "week", "year")

data2021.cause.NI <- read_excel(temp, sheet="Table 10", range=paste0("A57:C", as.numeric(NIRange)+52), 
                                col_names=FALSE) %>% 
  rename(week=`...1`, date=`...2`, COVID=`...3`) %>% 
  mutate(COVID=as.numeric(gsub("-", "0", COVID)),
         week=week+53, year=2021) 

#Read in 2020 data
temp <- tempfile()
source <- "https://www.nisra.gov.uk/sites/nisra.gov.uk/files/publications/Cumulative%20Weekly%20Deaths%2C%202020%20%28includes%20Covid-19%20deaths%29.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

data2020.NI <- read_excel(temp, sheet="Table 1", range="B6:C57", col_names=FALSE) %>% 
  mutate(week=2:53, year=2020)
colnames(data2020.NI) <- c("date", "deaths", "week", "year")

data2020.cause.NI <- read_excel(temp, sheet="Table 10", range="A5:C56", 
                                col_names=FALSE) %>% 
  rename(week=`...1`, date=`...2`, COVID=`...3`) %>% 
  mutate(COVID=as.numeric(gsub("-", "0", COVID)),
         year=2020, week=week+1) %>% 
  bind_rows(data2021.cause.NI) %>% 
  bind_rows(data.frame(week=1, date=as.Date("2020-01-03"), COVID=0, year=2020))

#Read in historical data
temp <- tempfile()
source <- "https://www.nisra.gov.uk/sites/nisra.gov.uk/files/publications/Weekly%20Deaths%20by%20Age%20and%20Respiratory%20Deaths%2C%202011-2019.xls"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
data2019.NI <- read_excel(temp, sheet="Weekly Deaths_2019", range="C5:D56", col_names=FALSE)
data2018.NI <- read_excel(temp, sheet="Weekly Deaths_2018", range="C5:D56", col_names=FALSE)
data2017.NI <- read_excel(temp, sheet="Weekly Deaths_2017", range="C5:D57", col_names=FALSE)
data2016.NI <- read_excel(temp, sheet="Weekly Deaths_2016", range="C5:D56", col_names=FALSE)
data2015.NI <- read_excel(temp, sheet="Weekly Deaths_2015", range="C5:D57", col_names=FALSE)
data2014.NI <- read_excel(temp, sheet="Weekly Deaths_2014", range="C5:D56", col_names=FALSE)
data2013.NI <- read_excel(temp, sheet="Weekly Deaths_2013", range="C5:D56", col_names=FALSE)
data2012.NI <- read_excel(temp, sheet="Weekly Deaths_2012", range="C5:D56", col_names=FALSE)
data2011.NI <- read_excel(temp, sheet="Weekly Deaths_2011", range="C5:D56", col_names=FALSE)

data.NI <- bind_rows(data2011.NI, data2012.NI, data2013.NI, data2014.NI, data2015.NI,
                     data2016.NI, data2017.NI, data2018.NI, data2019.NI) %>% 
  rename(date=`...1`, deaths=`...2`) %>% 
  mutate(week=week(date-days(1)), year=year(date-days(1))) %>% 
  bind_rows(data2020.NI, data2021.NI) %>% 
  arrange(date)

#Create cause dataset
data.cause.NI <- data.NI %>% 
  filter(year<2020) %>% 
  group_by(week) %>% 
  summarise(mean1119=mean(deaths)) 

data.cause.NI <- data.cause.NI %>% 
  filter(week<=max(data.NI$week)-53) %>% 
  mutate(week=week+53) %>% 
  bind_rows(data.cause.NI) %>% 
  merge(data.NI %>% filter(year>=2020), all.y=TRUE) %>% 
  merge(data2020.cause.NI, all.x=TRUE) %>% 
  mutate(other=deaths-COVID, otherexcess=other-mean1119,
         netexcess=deaths-mean1119) %>% 
  select(week, year, COVID, otherexcess, netexcess)
 

rm(data2011.NI, data2012.NI, data2013.NI, data2014.NI, data2015.NI, data2016.NI,
   data2017.NI, data2018.NI, data2019.NI, data2021.NI, data2021.cause.NI)

#Generate overall regional data for UK
data.reg.UK <- data.as.S %>% 
  filter(age=="Total" & sex=="Total") %>% 
  select(-c(age, sex)) %>% 
  mutate(region="Scotland") %>% 
  bind_rows(data.reg.EW, data.NI %>% mutate(region="Northern Ireland"))

#Save data
#data by age and sex
write.csv(data.as.EW, "Data/deaths_age_sex_EW.csv")
write.csv(data.as.S, "Data/deaths_age_sex_S.csv")
#data by region/Health Board
write.csv(data.reg.UK, "Data/deaths_reg_UK.csv")
write.csv(data.HB.S, "Data/deaths_HB_S.csv")
#data by location
write.csv(data.loc.EW, "Data/deaths_loc_EW.csv")
write.csv(data.loc.S, "Data/deaths_loc_S.csv")
#data by cause
write.csv(data.cause.EW, "Data/deaths_cause_EW.csv")
write.csv(data.cause.S, "Data/deaths_cause_S.csv")
write.csv(data.cause.NI, "Data/deaths_cause_NI.csv")
#Overall NI data
write.csv(data.NI, "Data/deaths_NI.csv")

###############################################################################################

###################
#Plots for England#
###################

#Overall plot
plot1 <- data.as.EW %>% 
  filter(age=="Total" & sex=="Total") %>% 
  select(-c(age, sex)) %>% 
  mutate(week=if_else(year==2021,week+53, week))

plot1.old <- plot1 %>% 
  filter(year<2020) %>% 
  group_by(week) %>% 
  summarise(min=min(deaths), max=max(deaths), mean=mean(deaths)) %>% 
  ungroup()

Eng2021MaxWeek <- max((plot1 %>% filter(year==2021))$week)

#Add extra weeks to old data
plot1.old <- plot1.old %>% 
  filter(week<=Eng2021MaxWeek-53) %>% 
  mutate(week=week+53) %>% 
  bind_rows(plot1.old)

plot1 <- plot1 %>% 
  filter(year>=2020) %>% 
  merge(plot1.old, by="week") %>% 
  mutate(excess=deaths-mean)

#Calculate excess deaths vs. mean in 2020/21
EW.excess <- plot1 %>%
  summarise(excess=sum(excess), total=sum(mean), percexcess=excess/total)

#Extract y=axis placement for excess deaths figure
labpos <- 14000

agg_tiff("Outputs/ONSWeeklyDeaths.tiff", units="in", width=10, height=8, res=500)
ggplot(plot1)+
  geom_ribbon(aes(x=date, ymin=min, ymax=max), fill="Skyblue2")+
  geom_ribbon(aes(x=date, ymin=mean, ymax=deaths), fill="Red", alpha=0.2)+
  geom_line(aes(x=date, y=mean), colour="Grey50", linetype=2)+
  geom_line(aes(x=date, y=deaths), colour="Red")+
  scale_x_date(name="")+
  scale_y_continuous(name="Weekly deaths registered", limits=c(0,NA))+
  theme_classic()+
  theme(plot.title=element_text(face="bold", size=rel(1.2)), plot.subtitle=element_markdown())+
  labs(title="Excess mortality in England & Wales continues to fall",
       subtitle=paste0("Weekly deaths registered in England & Wales in <span style='color:red;'>2020/21</span> compared to <span style='color:Skyblue4;'>the range in 2010-19</span>. Data up to ", EWDate, " 2021."),
       caption="Data from ONS | Plot by @VictimOfMaths")+
  annotate(geom="text", x=as.Date("2020-06-01"), y=labpos, 
           label=paste0("+", round(EW.excess$excess, 0)," more deaths in 2020/21 than average (+", 
                        round(EW.excess$percexcess*100, 0),"%)"), colour="Red", hjust=0)+
  annotate(geom="text", x=as.Date("2020-02-28"), y=13600, label="Historic maximum", 
           colour="Skyblue4")+
  annotate(geom="text", x=as.Date("2020-02-16"), y=9100, label="Historic minimum", 
           colour="Skyblue4")+
  annotate(geom="text", x=as.Date("2020-04-15"), y=7500, label="Historic mean", colour="grey30")+
  geom_curve(aes(x=as.Date("2020-04-20"), y=7700, xend=as.Date("2020-04-30"), yend=9700), 
             colour="grey30", curvature=0.15, arrow=arrow(length=unit(0.1, "cm"), type="closed"), 
             lineend="round")
dev.off()

#Plot by sex
plot2 <- data.as.EW %>% 
  filter(age=="Total" & sex!="Total") %>% 
  select(-age) %>% 
  mutate(week=if_else(year==2021,week+53, week))

plot2.old <- plot2 %>% 
  filter(year<2020) %>% 
  group_by(week, sex) %>% 
  summarise(min=min(deaths), max=max(deaths), mean=mean(deaths)) %>% 
  ungroup()

#Add extra weeks to old data
plot2.old <- plot2.old %>% 
  filter(week<=Eng2021MaxWeek-53) %>% 
  mutate(week=week+53) %>% 
  bind_rows(plot2.old)

plot2 <- plot2 %>% 
  filter(year>=2020) %>% 
  merge(plot2.old, by=c("sex", "week")) %>% 
  mutate(excess=deaths-mean,
         sex=factor(sex, levels=c("Male", "Female")))

#Calculate excess deaths vs. mean in 2020/21
EW.excess.sex <- plot2 %>%
  group_by(sex) %>% 
  summarise(excess=sum(excess), total=sum(mean), percexcess=excess/total) %>% 
  ungroup()

ann_text2 <- data.frame(date=rep(as.Date("2020-06-01"), times=2), deaths=c(7500,7000), 
                        sex=factor(c("Male", "Female"), levels=c("Male", "Female")))

agg_tiff("Outputs/ONSWeeklyDeathsxSex.tiff", units="in", width=12, height=8, res=500)
ggplot(plot2)+
  geom_ribbon(aes(x=date, ymin=min, ymax=max), fill="Skyblue2")+
  geom_ribbon(aes(x=date, ymin=mean, ymax=deaths), fill="Red", alpha=0.2)+
  geom_line(aes(x=date, y=mean), colour="Grey50", linetype=2)+
  geom_line(aes(x=date, y=deaths), colour="Red")+
  scale_x_date(name="")+
  scale_y_continuous(name="Weekly deaths registered", limits=c(0,NA))+
  facet_wrap(~sex)+
  theme_classic()+
  theme(plot.title=element_text(face="bold", size=rel(1.2)), plot.subtitle=element_markdown(),
        strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)))+
  labs(title="Excess mortality remains higher in men",
       subtitle=paste0("Weekly deaths registered in England & Wales in <span style='color:red;'>2020/21</span> compared to <span style='color:Skyblue4;'>the range in 2010-19</span>. Data up to ", EWDate, " 2021."),
       caption="Data from ONS | Plot by @VictimOfMaths")+
  geom_text(data=ann_text2, aes(x=date, y=deaths), label=c(paste0("+", round(EW.excess.sex[1,2],0)," excess deaths in 2020/21\nvs. 2010-19 average (+",
                                                                    round(EW.excess.sex[1,4]*100, 0),"%)"), 
                                                             paste0("+", round(EW.excess.sex[2,2],0)," deaths (+",
                                                                    round(EW.excess.sex[2,4]*100, 0),"%)")), 
            size=3, colour=c("Red", "Red"), hjust=0)
dev.off()  

#Plot by age
plot3 <- data.as.EW %>% 
  filter(age!="Total" & sex=="Total") %>% 
  select(-sex) %>% 
  mutate(week=if_else(year==2021,week+53, week))

plot3.old <- plot3 %>% 
  filter(year<2020) %>% 
  group_by(week, age) %>% 
  summarise(min=min(deaths), max=max(deaths), mean=mean(deaths)) %>% 
  ungroup()

#Add extra weeks to old data
plot3.old <- plot3.old %>% 
  filter(week<=Eng2021MaxWeek-53) %>% 
  mutate(week=week+53) %>% 
  bind_rows(plot3.old)

plot3 <- plot3 %>% 
  filter(year>=2020) %>% 
  merge(plot3.old, by=c("age", "week")) %>% 
  mutate(excess=deaths-mean,
         age=factor(age, levels=c("Under 15", "15-44", "45-64", "65-74", "75-84", "85+")))

#Calculate excess deaths vs. mean in 2020/21
EW.excess.age <- plot3 %>%
  group_by(age) %>% 
  summarise(excess=sum(excess), total=sum(mean), percexcess=excess/total) %>% 
  ungroup()

ann_text3 <- data.frame(date=rep(as.Date("2020-06-01"), times=6), 
                        deaths=c(1300, 1400, 2000, 3000, 5000, 7000), 
                        age=factor(c("Under 15", "15-44", "45-64", "65-74", "75-84", "85+"),
                                   levels=c("Under 15", "15-44", "45-64", "65-74", "75-84", "85+")))

agg_tiff("Outputs/ONSWeeklyDeathsxAge.tiff", units="in", width=12, height=8, res=500)
ggplot(plot3)+
  geom_ribbon(aes(x=date, ymin=min, ymax=max), fill="Skyblue2")+
  geom_ribbon(aes(x=date, ymin=mean, ymax=deaths), fill="Red", alpha=0.2)+
  geom_line(aes(x=date, y=mean), colour="Grey50", linetype=2)+
  geom_line(aes(x=date, y=deaths), colour="Red")+
  scale_x_date(name="", date_labels="%b-%y")+
  scale_y_continuous(name="Weekly deaths registered", limits=c(0,NA))+
  facet_wrap(~age)+
  theme_classic()+
  theme(plot.title=element_text(face="bold", size=rel(1.2)), plot.subtitle=element_markdown(),
        strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)))+
  labs(title="Deaths have continued to fall fastest in the oldest age groups",
       subtitle=paste0("Weekly deaths registered in England & Wales in <span style='color:red;'>2020/21</span> compared to <span style='color:Skyblue4;'>the range in 2010-19</span>. Data up to ", EWDate, " 2021."),
       caption="Data from ONS | Plot by @VictimOfMaths")+
  geom_text(data=ann_text3, aes(x=date, y=deaths), label=c(paste0(round(EW.excess.age[1,2],0)," excess deaths in 2020/21\nvs. 2010-19 average (",
                                                                    round(EW.excess.age[1,4]*100, 1),"%)"), 
                                                           paste0("+", round(EW.excess.age[2,2],0)," deaths (+",
                                                                    round(EW.excess.age[2,4]*100, 0),"%)"),
                                                           paste0("+", round(EW.excess.age[3,2],0)," deaths (+",
                                                                    round(EW.excess.age[3,4]*100, 0),"%)"),
                                                           paste0("+", round(EW.excess.age[4,2],0)," deaths (+",
                                                                    round(EW.excess.age[4,4]*100, 0),"%)"),
                                                           paste0("+", round(EW.excess.age[5,2],0)," deaths (+",
                                                                    round(EW.excess.age[5,4]*100, 0),"%)"),
                                                           paste0("+", round(EW.excess.age[6,2],0)," deaths (+",
                                                                  round(EW.excess.age[6,4]*100, 0),"%)")), 
            size=3, colour=rep("red", times=6), hjust=0)
dev.off()  

ann_text3 <- data.frame(date=rep(as.Date("2020-06-01"), times=6), 
                        deaths=c(120, 400, 2000, 3000, 5000, 7000), 
                        age=factor(c("Under 15", "15-44", "45-64", "65-74", "75-84", "85+"),
                                    levels=c("Under 15", "15-44", "45-64", "65-74", "75-84", "85+")))

agg_tiff("Outputs/ONSWeeklyDeathsxAgev2.tiff", units="in", width=12, height=8, res=500)
ggplot(plot3)+
  geom_ribbon(aes(x=date, ymin=min, ymax=max), fill="Skyblue2")+
  geom_ribbon(aes(x=date, ymin=mean, ymax=deaths), fill="Red", alpha=0.2)+
  geom_line(aes(x=date, y=mean), colour="Grey50", linetype=2)+
  geom_line(aes(x=date, y=deaths), colour="Red")+
  scale_x_date(name="", date_labels="%b-%y")+
  scale_y_continuous(name="Weekly deaths registered", limits=c(0,NA))+
  facet_wrap(~age, scales="free_y")+
  theme_classic()+
  theme(plot.title=element_text(face="bold", size=rel(1.2)), plot.subtitle=element_markdown(),
        strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)))+
  labs(title="Deaths are above 'normal' levels in 45-74 year olds",
       subtitle=paste0("Weekly deaths registered in England & Wales in <span style='color:red;'>2020/21</span> compared to <span style='color:Skyblue4;'>the range in 2010-19</span>. Data up to ", EWDate, " 2021."),
       caption="Data from ONS | Plot by @VictimOfMaths")+
  geom_text(data=ann_text3, aes(x=date, y=deaths), label=c(paste0(round(EW.excess.age[1,2],0)," excess deaths in 2020/21\nvs. 2010-19 average (",
                                                                  round(EW.excess.age[1,4]*100, 1),"%)"), 
                                                           paste0("+", round(EW.excess.age[2,2],0)," deaths (+",
                                                                  round(EW.excess.age[2,4]*100, 0),"%)"),
                                                           paste0("+", round(EW.excess.age[3,2],0)," deaths (+",
                                                                  round(EW.excess.age[3,4]*100, 0),"%)"),
                                                           paste0("+", round(EW.excess.age[4,2],0)," deaths (+",
                                                                  round(EW.excess.age[4,4]*100, 0),"%)"),
                                                           paste0("+", round(EW.excess.age[5,2],0)," deaths (+",
                                                                  round(EW.excess.age[5,4]*100, 0),"%)"),
                                                           paste0("+", round(EW.excess.age[6,2],0)," deaths (+",
                                                                  round(EW.excess.age[6,4]*100, 0),"%)")), 
            size=3, colour=rep("red", times=6), hjust=0)
dev.off()  

#Excess deaths by age stacked
agg_tiff("Outputs/ONSWeeklyDeathsxAgeBars.tiff", units="in", width=8, height=6, res=500)
ggplot(plot3)+
  geom_col(aes(x=date, y=excess, fill=age))+
  scale_x_date(name="")+
  scale_y_continuous(name="Excess deaths vs. 2010-19 average")+
  scale_fill_paletteer_d(name="Age", "awtools::a_palette")+
  theme_classic()+
  theme(plot.title=element_text(face="bold", size=rel(1.2)))+
  labs(title="Excess deaths are at their lowest level since October",
       subtitle=paste0("Weekly deaths registered in England & Wales by age compared to the 2010-19 average.\nData up to ", EWDate, " 2021."),
       caption="Data from ONS | Plot by @VictimOfMaths")
dev.off()

#Plot by location
plot4 <- data.loc.EW %>% 
  mutate(week=if_else(year==2021,week+53, week),
         location=factor(location, levels=c("Hospital", "Care Home", "Home/Other")))

#Add extra weeks to old data
plot4 <- plot4 %>% 
  filter(week<=Eng2021MaxWeek-53) %>% 
  mutate(week=week+53) %>% 
  bind_rows(plot4) %>% 
  mutate(date=as.Date("2020-01-03")+weeks(week-1))

agg_tiff("Outputs/ONSWeeklyDeathsxLocation.tiff", units="in", width=12, height=8, res=500)
ggplot()+
  geom_line(data=subset(plot4, year==1519 & week<53), aes(x=date, y=deaths), colour="Skyblue2")+
  geom_line(data=subset(plot4, year==1519 & week>53), aes(x=date, y=deaths), colour="Skyblue2")+
  geom_line(data=subset(plot4, year>1519), aes(x=date, y=deaths), colour="Red")+
  scale_x_date(name="", date_labels="%b-%y")+
  scale_y_continuous(name="Weekly deaths registered", limits=c(0,NA))+
  facet_wrap(~location)+
  theme_classic()+
  theme(plot.title=element_text(face="bold", size=rel(1.2)), plot.subtitle=element_markdown(),
        strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)))+
  labs(title="Deaths in hospitals and care homes have continued to fall sharply",
       subtitle=paste0("Weekly deaths in England & Wales in <span style='color:red;'>2020/21</span> compared to <span style='color:Skyblue4;'>the range in 2010-19</span>. Data up to ", EWDate, " 2021.<br>Historic data for week 53 is not available"),
       caption="Data from ONS | Plot by @VictimOfMaths")
  
dev.off()  

#Plot by cause
plot5 <- data.cause.EW %>% 
  mutate(week=if_else(year==2021,week+53, week),
         date=as.Date("2020-01-03")+weeks(week-1))

agg_tiff("Outputs/ONSExcessxCause.tiff", units="in", width=8, height=6, res=500)
ggplot()+
  geom_col(data=subset(plot5, cause!="netexcess"), aes(x=date, y=deaths, fill=cause))+
  geom_hline(yintercept=0, colour="Grey30")+
  geom_line(data=subset(plot5, cause=="netexcess"), aes(x=date, y=deaths, colour=cause))+
  scale_x_date(name="")+
  scale_y_continuous(name="Excess deaths vs. 2015-19 mean")+
  scale_fill_paletteer_d("LaCroixColoR::PinaFraise", name="Cause", labels=c("COVID-19", "Other causes"))+
  scale_colour_manual(values="NavyBlue", name="", labels="Net excess deaths")+
  theme_classic()+
  theme(plot.title=element_text(face="bold", size=rel(1.2)))+
  labs(title="The number of COVID-19 deaths has continued to fall",
       subtitle="Excess deaths vs. 2015-19 average by cause for England & Wales",
       caption="Data from ONS | Plot by @VictimOfMaths")
dev.off()
  
####################
#Plots for Scotland#
####################

#Overall plot
plot6 <- data.as.S %>% 
  filter(age=="Total" & sex=="Total") %>% 
  select(-c(age, sex)) %>% 
  mutate(week=if_else(year==2021,week+53, week))

plot6.old <- plot6 %>% 
  filter(year<2020) %>% 
  group_by(week) %>% 
  summarise(min=min(deaths), max=max(deaths), mean=mean(deaths)) %>% 
  ungroup()

Scot2021MaxWeek <- max((plot6 %>% filter(year==2021))$week)

#Add extra weeks to old data
plot6.old <- plot6.old %>% 
  filter(week<=Scot2021MaxWeek-53) %>% 
  mutate(week=week+53) %>% 
  bind_rows(plot6.old)

plot6 <- plot6 %>% 
  filter(year>=2020) %>% 
  merge(plot6.old, by="week") %>% 
  mutate(excess=deaths-mean)

#Calculate excess deaths vs. mean in 2020/21
S.excess <- plot6 %>%
  summarise(excess=sum(excess), total=sum(mean), percexcess=excess/total)

#Extract y=axis placement for excess deaths figure
labpos <- 1600

agg_tiff("Outputs/NRSWeeklyDeaths.tiff", units="in", width=10, height=8, res=500)
ggplot(plot6)+
  geom_ribbon(aes(x=date, ymin=min, ymax=max), fill="Skyblue2")+
  geom_ribbon(aes(x=date, ymin=mean, ymax=deaths), fill="Red", alpha=0.2)+
  geom_line(aes(x=date, y=mean), colour="Grey50", linetype=2)+
  geom_line(aes(x=date, y=deaths), colour="Red")+
  scale_x_date(name="")+
  scale_y_continuous(name="Weekly deaths registered", limits=c(0,NA))+
  theme_classic()+
  theme(plot.title=element_text(face="bold", size=rel(1.2)), plot.subtitle=element_markdown())+
  labs(title="All-cause deaths in Scotland are back to 'normal' levels",
       subtitle=paste0("Weekly deaths in Scotland in <span style='color:red;'>2020/21</span> compared to <span style='color:Skyblue4;'>the range in 2010-19</span>. Data up to ", ScotDate, " 2021."),
       caption="Data from NRS | Plot by @VictimOfMaths")+
  annotate(geom="text", x=as.Date("2020-06-01"), y=labpos, 
           label=paste0("+", round(S.excess$excess, 0)," more deaths in 2020/21 than average (+", 
                        round(S.excess$percexcess*100, 0),"%)"), colour="Red", hjust=0)+
  annotate(geom="text", x=as.Date("2020-02-28"), y=1500, label="Historic maximum", 
           colour="Skyblue4")+
  annotate(geom="text", x=as.Date("2020-02-16"), y=1000, label="Historic minimum", 
           colour="Skyblue4")+
  annotate(geom="text", x=as.Date("2020-04-15"), y=700, label="Historic mean", colour="grey30")+
  geom_curve(aes(x=as.Date("2020-04-20"), y=770, xend=as.Date("2020-04-30"), yend=1060), 
             colour="grey30", curvature=0.15, arrow=arrow(length=unit(0.1, "cm"), type="closed"), 
             lineend="round")
dev.off()

#Plot by sex
plot7 <- data.as.S %>% 
  filter(age=="Total" & sex!="Total") %>% 
  select(-age) %>% 
  mutate(week=if_else(year==2021,week+53, week))

plot7.old <- plot7 %>% 
  filter(year<2020) %>% 
  group_by(week, sex) %>% 
  summarise(min=min(deaths), max=max(deaths), mean=mean(deaths)) %>% 
  ungroup()

#Add extra weeks to old data
plot7.old <- plot7.old %>% 
  filter(week<=Scot2021MaxWeek-53) %>% 
  mutate(week=week+53) %>% 
  bind_rows(plot7.old)

plot7 <- plot7 %>% 
  filter(year>=2020) %>% 
  merge(plot7.old, by=c("sex", "week")) %>% 
  mutate(excess=deaths-mean,
         sex=factor(sex, levels=c("Male", "Female")))

#Calculate excess deaths vs. mean in 2020/21
S.excess.sex <- plot7 %>%
  group_by(sex) %>% 
  summarise(excess=sum(excess), total=sum(mean), percexcess=excess/total) %>% 
  ungroup()

ann_text7 <- data.frame(date=rep(as.Date("2020-06-01"), times=2), deaths=c(750,700), 
                        sex=factor(c("Male", "Female"), levels=c("Male", "Female")))

agg_tiff("Outputs/NRSWeeklyDeathsxSex.tiff", units="in", width=12, height=8, res=500)
ggplot(plot7)+
  geom_ribbon(aes(x=date, ymin=min, ymax=max), fill="Skyblue2")+
  geom_ribbon(aes(x=date, ymin=mean, ymax=deaths), fill="Red", alpha=0.2)+
  geom_line(aes(x=date, y=mean), colour="Grey50", linetype=2)+
  geom_line(aes(x=date, y=deaths), colour="Red")+
  scale_x_date(name="")+
  scale_y_continuous(name="Weekly deaths registered", limits=c(0,NA))+
  facet_wrap(~sex)+
  theme_classic()+
  theme(plot.title=element_text(face="bold", size=rel(1.2)), plot.subtitle=element_markdown(),
        strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)))+
  labs(title="All-cause deaths in men are still slightly higher than average",
       subtitle=paste0("Weekly deaths in Scotland in <span style='color:red;'>2020/21</span> compared to <span style='color:Skyblue4;'>the range in 2010-19</span>. Data up to ", ScotDate, " 2021."),
       caption="Data from NRS | Plot by @VictimOfMaths")+
  geom_text(data=ann_text7, aes(x=date, y=deaths), label=c(paste0("+", round(S.excess.sex[1,2],0)," excess deaths in 2020/21\nvs. 2010-19 average (+",
                                                                  round(S.excess.sex[1,4]*100, 0),"%)"), 
                                                           paste0("+", round(S.excess.sex[2,2],0)," deaths (+",
                                                                  round(S.excess.sex[2,4]*100, 0),"%)")), 
            size=3, colour=c("Red", "Red"), hjust=0)
dev.off()  

#Plot by age
plot8 <- data.as.S %>% 
  filter(age!="Total" & sex=="Total") %>% 
  select(-sex) %>% 
  mutate(week=if_else(year==2021,week+53, week))

plot8.old <- plot8 %>% 
  filter(year<2020) %>% 
  group_by(week, age) %>% 
  summarise(min=min(deaths), max=max(deaths), mean=mean(deaths)) %>% 
  ungroup()

#Add extra weeks to old data
plot8.old <- plot8.old %>% 
  filter(week<=Scot2021MaxWeek-53) %>% 
  mutate(week=week+53) %>% 
  bind_rows(plot8.old)

plot8 <- plot8 %>% 
  filter(year>=2020) %>% 
  merge(plot8.old, by=c("age", "week")) %>% 
  mutate(excess=deaths-mean,
         age=factor(age, levels=c("Under 15", "15-44", "45-64", "65-74", "75-84", "85+")))

#Calculate excess deaths vs. mean in 2020/21
S.excess.age <- plot8 %>%
  group_by(age) %>% 
  summarise(excess=sum(excess), total=sum(mean), percexcess=excess/total) %>% 
  ungroup()

ann_text8 <- data.frame(date=rep(as.Date("2020-06-01"), times=6), 
                        deaths=c(100, 150, 270, 350, 500, 600), 
                        age=factor(c("Under 15", "15-44", "45-64", "65-74", "75-84", "85+"),
                                   levels=c("Under 15", "15-44", "45-64", "65-74", "75-84", "85+")))

agg_tiff("Outputs/NRSWeeklyDeathsxAge.tiff", units="in", width=12, height=8, res=500)
ggplot(plot8)+
  geom_ribbon(aes(x=date, ymin=min, ymax=max), fill="Skyblue2")+
  geom_ribbon(aes(x=date, ymin=mean, ymax=deaths), fill="Red", alpha=0.2)+
  geom_line(aes(x=date, y=mean), colour="Grey50", linetype=2)+
  geom_line(aes(x=date, y=deaths), colour="Red")+
  scale_x_date(name="", date_labels="%b-%y")+
  scale_y_continuous(name="Weekly deaths registered", limits=c(0,NA))+
  facet_wrap(~age)+
  theme_classic()+
  theme(plot.title=element_text(face="bold", size=rel(1.2)), plot.subtitle=element_markdown(),
        strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)))+
  labs(title="Only 45-64 year olds are still seeing excess deaths",
       subtitle=paste0("Weekly deaths in Scotland in <span style='color:red;'>2020/21</span> compared to <span style='color:Skyblue4;'>the range in 2010-19</span>. Data up to ", ScotDate, " 2021."),
       caption="Data from NRS | Plot by @VictimOfMaths")+
  geom_text(data=ann_text8, aes(x=date, y=deaths), label=c(paste0(round(S.excess.age[1,2],0)," excess deaths in 2020\nvs. 2010-19 average (",
                                                                  round(S.excess.age[1,4]*100, 1),"%)"), 
                                                           paste0("+", round(S.excess.age[2,2],0)," deaths (+",
                                                                  round(S.excess.age[2,4]*100, 0),"%)"),
                                                           paste0("+", round(S.excess.age[3,2],0)," deaths (+",
                                                                  round(S.excess.age[3,4]*100, 0),"%)"),
                                                           paste0("+", round(S.excess.age[4,2],0)," deaths (+",
                                                                  round(S.excess.age[4,4]*100, 0),"%)"),
                                                           paste0("+", round(S.excess.age[5,2],0)," deaths (+",
                                                                  round(S.excess.age[5,4]*100, 0),"%)"),
                                                           paste0("+", round(S.excess.age[6,2],0)," deaths (+",
                                                                  round(S.excess.age[6,4]*100, 0),"%)")), 
            size=3, colour=rep("red", times=6), hjust=0)
dev.off()  

#Plot by location
plot9 <- data.loc.S %>% 
  mutate(week=if_else(year==2021,week+53, week),
         loc=case_when(
           loc=="Hospital" ~ "Hospital",
           loc=="Care Home" ~ "Care Home",
           TRUE ~ "Home/Other"
         )) %>% 
  group_by(year, loc, week, date) %>% 
  summarise(deaths=sum(deaths)) %>% 
  ungroup()

plot9.old <- plot9 %>% 
  filter(year<2020) %>% 
  group_by(week, loc) %>% 
  summarise(min=min(deaths), max=max(deaths), mean=mean(deaths)) %>% 
  ungroup()

#Add extra weeks to old data
plot9.old <- plot9.old %>% 
  filter(week<=Scot2021MaxWeek-53) %>% 
  mutate(week=week+53) %>% 
  bind_rows(plot9.old)

plot9 <- plot9 %>% 
  filter(year>=2020) %>% 
  merge(plot9.old, by=c("loc", "week")) %>% 
  mutate(excess=deaths-mean,
         loc=factor(loc, levels=c("Hospital", "Care Home", "Home/Other")))

#Calculate excess deaths vs. mean in 2020/21
S.excess.loc <- plot9 %>%
  group_by(loc) %>% 
  summarise(excess=sum(excess), total=sum(mean), percexcess=excess/total) %>% 
  ungroup()

ann_text9 <- data.frame(date=rep(as.Date("2020-05-10"), times=3), deaths=c(750,500, 600), 
                        loc=factor(c("Hospital", "Care Home", "Home/Other"),
                                   levels=c("Hospital", "Care Home", "Home/Other")))

agg_tiff("Outputs/NRSWeeklyDeathsxLocation.tiff", units="in", width=12, height=8, res=500)
ggplot(plot9)+
  geom_ribbon(aes(x=date, ymin=min, ymax=max), fill="Skyblue2")+
  geom_ribbon(aes(x=date, ymin=mean, ymax=deaths), fill="Red", alpha=0.2)+
  geom_line(aes(x=date, y=mean), colour="Grey50", linetype=2)+
  geom_line(aes(x=date, y=deaths), colour="Red")+
  scale_x_date(name="", date_labels="%b-%y")+
  scale_y_continuous(name="Weekly deaths registered", limits=c(0,NA))+
  facet_wrap(~loc)+
  theme_classic()+
  theme(plot.title=element_text(face="bold", size=rel(1.2)), plot.subtitle=element_markdown(),
        strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)))+
  labs(title="Scottish care home deaths have remained well below 'normal' levels",
       subtitle=paste0("Weekly deaths in Scotland in <span style='color:red;'>2020/21</span> compared to <span style='color:Skyblue4;'>the range in 2015-19</span>. Data up to ", ScotDate, " 2021."),
       caption="Data from NRS | Plot by @VictimOfMaths")+
  geom_text(data=ann_text9, aes(x=date, y=deaths), label=c(paste0(round(S.excess.loc[1,2],0)," excess deaths in 2020/21\nvs. 2010-19 average (",
                                                                  round(S.excess.loc[1,4]*100, 0),"%)"), 
                                                           paste0("+", round(S.excess.loc[2,2],0)," deaths (+",
                                                                  round(S.excess.loc[2,4]*100, 0),"%)"),
                                                           paste0("+", round(S.excess.loc[3,2],0)," deaths (+",
                                                                  round(S.excess.loc[3,4]*100, 0),"%)")), 
            size=3, colour=rep("Red", 3), hjust=0)
dev.off()  

#Plot excess by location
agg_tiff("Outputs/NRSWeeklyDeathsExcessxLocation.tiff", units="in", width=12, height=8, res=500)
ggplot(plot9)+
  geom_line(aes(x=date, y=excess, colour=loc))+
  geom_hline(yintercept=0, colour="Grey30")+
  scale_x_date(name="")+
  scale_y_continuous(name="Excess deaths compared to 2015-19 average")+
  scale_colour_paletteer_d(name="Place of death", "ggsci::planetexpress_futurama")+
  theme_classic()+
  theme(plot.title=element_text(face="bold", size=rel(1.2)))+
  labs(title="Excess mortality remains highest in Scottish homes",
       subtitle="Excess deaths by place of death in Scotland in 2020/21 compared to the 2015-19 average",
       caption="Data from NRS | Plot by @VictimOfMaths")
dev.off()

#Plot by cause
plot10 <- data.cause.S %>% 
  filter(loc=="All") %>% 
  mutate(week=if_else(year==2021,week+53, week),
         date=as.Date("2020-01-05")+weeks(week-1),
         cause=factor(cause, levels=c("COVID-19", "Cancer", "Circulatory", "Dementia / Alzheimers",
                                      "Respiratory", "Other")))

agg_tiff("Outputs/NRSExcessxcause.tiff", units="in", width=10, height=7, res=500)
ggplot(plot10)+
  geom_col(aes(x=date, y=excess, fill=cause))+
  geom_hline(yintercept=0, colour="Grey30")+
  scale_x_date(name="")+
  scale_y_continuous(name="Excess deaths compared to 2015-19 average")+
  scale_fill_paletteer_d("LaCroixColoR::paired", name="Cause of death")+
  theme_classic()+
  theme(plot.title=element_text(face="bold", size=rel(1.2)))+
  labs(title="Scotland is still seeing COVID deaths, but numbers keep falling",
       subtitle="Registered deaths by cause in Scotland in 2020/21 compared to the 2015-19 average",
       caption="Data from NRS | Plot by @VictimOfMaths")
dev.off()

#Plot by cause *and* location
plot11 <- data.cause.S %>% 
  filter(loc!="All") %>% 
  mutate(week=if_else(year==2021,week+53, week),
         date=as.Date("2020-01-05")+weeks(week-1),
         cause=factor(cause, levels=c("COVID-19", "Cancer", "Circulatory", "Dementia / Alzheimers",
                                      "Respiratory", "Other")),
         loc=factor(loc, levels=c("Hospital", "Care Home", "Home/Other")))

agg_tiff("Outputs/NRSExcessxcausexloc.tiff", units="in", width=12, height=8, res=500)
ggplot(plot11)+
  geom_col(aes(x=date, y=excess, fill=cause))+
  geom_hline(yintercept=0, colour="Grey30")+
  scale_x_date(name="", date_labels="%b-%y")+
  scale_y_continuous(name="Excess deaths compared to 2015-19 average")+
  scale_fill_paletteer_d("LaCroixColoR::paired", name="Cause of death")+
  facet_wrap(~loc)+
  theme_classic()+
  theme(plot.title=element_text(face="bold", size=rel(1.2)),
        strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)))+
  labs(title="Almost all COVID deaths in Scotland are taking place in hospital",
       subtitle="Registered deaths by cause in Scotland in 2020/21 compared to the 2015-19 average",
       caption="Data from NRS | Plot by @VictimOfMaths")
dev.off()

agg_tiff("Outputs/NRSExcessxlocxcause.tiff", units="in", width=12, height=8, res=500)
ggplot(plot11)+
  geom_line(aes(x=date, y=excess, colour=loc))+
  geom_hline(yintercept=0, colour="Grey30")+
  scale_x_date(name="", date_labels="%b-%y")+
  scale_y_continuous(name="Excess deaths compared to 2015-19 average")+
  scale_colour_paletteer_d("fishualize::Scarus_tricolor", name="Place of death")+
  facet_wrap(~cause)+
  theme_classic()+
  theme(plot.title=element_text(face="bold", size=rel(1.2)),
        strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)))+
  labs(title="COVID-19 deaths in Scottish hospitals have continued to fall",
       subtitle="Registered deaths by cause and place of death in Scotland in 2020/21 compared to the 2015-19 average",
       caption="Data from NRS | Plot by @VictimOfMaths")
dev.off()


#Plot by Health Board
plot12 <- data.HB.S %>% 
  mutate(week=if_else(year==2021,week+53, week)) 

plot12.old <- plot12 %>% 
  filter(year<2020) %>% 
  group_by(week, HB) %>% 
  summarise(min=min(deaths), max=max(deaths), mean=mean(deaths)) %>% 
  ungroup()

#Add extra weeks to old data
plot12.old <- plot12.old %>% 
  filter(week<=Scot2021MaxWeek-53) %>% 
  mutate(week=week+53) %>% 
  bind_rows(plot12.old)

plot12 <- plot12 %>% 
  filter(year>=2020) %>% 
  merge(plot12.old, by=c("HB", "week")) %>% 
  mutate(excess=deaths-mean)

#Calculate excess deaths vs. mean in 2020/21
S.excess.HB <- plot12 %>%
  group_by(HB) %>% 
  summarise(excess=sum(excess), total=sum(mean), percexcess=excess/total) %>% 
  ungroup() %>% 
  mutate(HB=fct_reorder(HB, -percexcess)) %>% 
  arrange(-percexcess)

#Order HBs by total excess deaths
plot12 <- plot12 %>% 
  mutate(HB=factor(HB, levels=levels(S.excess.HB$HB))) %>% 
  arrange(HB)

ann_text12 <- data.frame(date=rep(as.Date("2020-05-15"), times=14),
                         deaths=c(350, 250, 400, 230, 300, 210, 150, 250, 180, 200, 150, 100, 
                                  80, 80), 
                         HB=factor(S.excess.HB$HB, levels(S.excess.HB$HB)))

agg_tiff("Outputs/NRSWeeklyDeathsxHB.tiff", units="in", width=12, height=8, res=500)
ggplot(plot12)+
  geom_ribbon(aes(x=date, ymin=min, ymax=max), fill="Skyblue2")+
  geom_ribbon(aes(x=date, ymin=mean, ymax=deaths), fill="Red", alpha=0.2)+
  geom_line(aes(x=date, y=mean), colour="Grey50", linetype=2)+
  geom_line(aes(x=date, y=deaths), colour="Red")+
  scale_x_date(name="", date_labels="%b-%y")+
  scale_y_continuous(name="Weekly deaths registered", limits=c(0,NA))+
  facet_wrap(~HB)+
  theme_classic()+
  theme(plot.title=element_text(face="bold", size=rel(1.2)), plot.subtitle=element_markdown(),
        strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)))+
  labs(title="Excess mortality across Scotland",
       subtitle=paste0("Weekly deaths in Scotland in <span style='color:red;'>2020/21</span> compared to <span style='color:Skyblue4;'>the range in 2015-19</span>. Data up to ", ScotDate, " 2021."),
       caption="Data from NRS | Plot by @VictimOfMaths")+
  geom_text(data=ann_text12, aes(x=date, y=deaths), label=c(paste0("+", round(S.excess.HB[1,2],0)," excess deaths in 2020/21\nvs. 2010-19 average (+",
                                                                  round(S.excess.HB[1,4]*100, 0),"%)"), 
                                                           paste0("+", round(S.excess.HB[2,2],0)," deaths (+",
                                                                  round(S.excess.HB[2,4]*100, 0),"%)"),
                                                           paste0("+", round(S.excess.HB[3,2],0)," deaths (+",
                                                                  round(S.excess.HB[3,4]*100, 0),"%)"),
                                                           paste0("+", round(S.excess.HB[4,2],0)," deaths (+",
                                                                  round(S.excess.HB[4,4]*100, 0),"%)"),
                                                           paste0("+", round(S.excess.HB[5,2],0)," deaths (+",
                                                                  round(S.excess.HB[5,4]*100, 0),"%)"),
                                                           paste0("+", round(S.excess.HB[6,2],0)," deaths (+",
                                                                  round(S.excess.HB[6,4]*100, 0),"%)"),
                                                           paste0("+", round(S.excess.HB[7,2],0)," deaths (+",
                                                                  round(S.excess.HB[7,4]*100, 0),"%)"),
                                                           paste0("+", round(S.excess.HB[8,2],0)," deaths (+",
                                                                  round(S.excess.HB[8,4]*100, 0),"%)"),
                                                           paste0("+", round(S.excess.HB[9,2],0)," deaths (+",
                                                                  round(S.excess.HB[9,4]*100, 0),"%)"),
                                                           paste0("+", round(S.excess.HB[10,2],0)," deaths (+",
                                                                  round(S.excess.HB[10,4]*100, 0),"%)"),
                                                           paste0("+", round(S.excess.HB[11,2],0)," deaths (+",
                                                                  round(S.excess.HB[11,4]*100, 0),"%)"),
                                                           paste0("+", round(S.excess.HB[12,2],0)," deaths (+",
                                                                  round(S.excess.HB[12,4]*100, 0),"%)"),
                                                           paste0(round(S.excess.HB[13,2],0)," deaths (",
                                                                  round(S.excess.HB[13,4]*100, 0),"%)"),
                                                           paste0(round(S.excess.HB[14,2],0)," deaths (",
                                                                  round(S.excess.HB[14,4]*100, 0),"%)")), 
            size=3, colour=rep("Red", 14), hjust=0)
dev.off()  

############################
#Plots for Northern Ireland#
############################

#Overall plot
plot13 <- data.NI %>% 
  mutate(date=as.Date(date))

plot13.old <- plot13 %>% 
  filter(year<2020) %>% 
  group_by(week) %>% 
  summarise(min=min(deaths), max=max(deaths), mean=mean(deaths)) %>% 
  ungroup()

NI2021MaxWeek <- max((plot13 %>% filter(year==2021))$week)

#Add extra weeks to old data
plot13.old <- plot13.old %>% 
  filter(week<=NI2021MaxWeek-53) %>% 
  mutate(week=week+53) %>% 
  bind_rows(plot13.old)

plot13 <- plot13 %>% 
  filter(year>=2020) %>% 
  merge(plot13.old, by="week") %>% 
  mutate(excess=deaths-mean)

#Calculate excess deaths vs. mean in 2020/21
NI.excess <- plot13 %>%
  summarise(excess=sum(excess), total=sum(mean), percexcess=excess/total)

#Extract y=axis placement for excess deaths figure
labpos <- 440

agg_tiff("Outputs/NISRAWeeklyDeaths.tiff", units="in", width=9, height=7, res=500)
ggplot(plot13)+
  geom_ribbon(aes(x=date, ymin=min, ymax=max), fill="Skyblue2")+
  geom_ribbon(aes(x=date, ymin=mean, ymax=deaths), fill="Red", alpha=0.2)+
  geom_line(aes(x=date, y=mean), colour="Grey50", linetype=2)+
  geom_line(aes(x=date, y=deaths), colour="Red")+
  scale_x_date(name="")+
  scale_y_continuous(name="Weekly deaths registered", limits=c(0,NA))+
  theme_classic()+
  theme(plot.title=element_text(face="bold", size=rel(1.2)), plot.subtitle=element_markdown())+
  labs(title="All-cause mortality rates in Northern Ireland are back at 'normal' levels",
       subtitle=paste0("Weekly deaths in Northern Ireland in <span style='color:red;'>2020/21</span> compared to <span style='color:Skyblue4;'>the range in 2010-19</span>. Data up to ", NIDate, " 2021."),
       caption="Data from NISRA | Plot by @VictimOfMaths")+
  annotate(geom="text", x=as.Date("2020-06-01"), y=labpos, 
           label=paste0("+", round(NI.excess$excess, 0)," more deaths in 2020/21 than average (+", 
                        round(NI.excess$percexcess*100, 0),"%)"), colour="Red", hjust=0)+
  annotate(geom="text", x=as.Date("2020-02-28"), y=450, label="Historic maximum", 
           colour="Skyblue4")+
  annotate(geom="text", x=as.Date("2020-02-16"), y=200, label="Historic minimum", 
           colour="Skyblue4")+
  annotate(geom="text", x=as.Date("2020-05-15"), y=170, label="Historic mean", colour="grey30")+
  geom_curve(aes(x=as.Date("2020-05-20"), y=180, xend=as.Date("2020-05-08"), yend=273), 
             colour="grey30", curvature=-0.15, arrow=arrow(length=unit(0.1, "cm"), type="closed"), 
             lineend="round")
dev.off()

#Plot by cause
plot14 <- data.cause.NI %>% 
  mutate(date=as.Date("2020-01-03")+weeks(week-1)) %>% 
  gather(cause, deaths, c(3:5))

agg_tiff("Outputs/NISRAExcessxCause.tiff", units="in", width=8, height=6, res=500)
ggplot()+
  geom_col(data=subset(plot14, cause!="netexcess"), aes(x=date, y=deaths, fill=cause))+
  geom_hline(yintercept=0, colour="Grey30")+
  geom_line(data=subset(plot14, cause=="netexcess"), aes(x=date, y=deaths, colour=cause))+
  scale_x_date(name="")+
  scale_y_continuous(name="Excess deaths vs. 2015-19 mean")+
  scale_fill_paletteer_d("LaCroixColoR::PinaFraise", name="Cause", labels=c("COVID-19", "Other causes"))+
  scale_colour_manual(values="NavyBlue", name="", labels="Net excess deaths")+
  theme_classic()+
  theme(plot.title=element_text(face="bold", size=rel(1.2)))+
  labs(title="The number of COVID-19 deaths in Northern Ireland continues to fall",
       subtitle="Excess deaths vs. 2015-19 average by cause for England & Wales",
       caption="Data from NISRA | Plot by @VictimOfMaths")
dev.off()

#Regional plot for the whole of the UK
plot15 <- data.reg.UK %>% 
  mutate(week=case_when(
    region!= "Northern Ireland" & year==2021 ~ week+53, 
    TRUE~week),
         date=as.Date(date)) 

plot15.old <- plot15 %>% 
  filter(year<2020) %>% 
  group_by(week, region) %>% 
  summarise(min=min(deaths), max=max(deaths), mean=mean(deaths)) %>% 
  ungroup()

#Add extra weeks to old data
plot15.old <- plot15.old %>% 
  filter(week<=max(Eng2021MaxWeek-53, Scot2021MaxWeek-53, NI2021MaxWeek)) %>% 
  mutate(week=week+53) %>% 
  bind_rows(plot15.old)

plot15 <- plot15 %>% 
  filter(year>=2020) %>% 
  merge(plot15.old, by=c("region", "week")) %>% 
  mutate(excess=deaths-mean)

#Calculate excess deaths vs. mean in 2020/21
UK.excess.region <- plot15 %>%
  group_by(region) %>% 
  summarise(excess=sum(excess), total=sum(mean), percexcess=excess/total) %>% 
  ungroup() %>% 
  mutate(region=fct_reorder(region, -percexcess)) %>% 
  arrange(-percexcess)

#Order HBs by total excess deaths
plot15 <- plot15 %>% 
  mutate(region=factor(region, levels=levels(UK.excess.region$region))) %>% 
  arrange(region)

ann_text15 <- data.frame(date=rep(as.Date("2020-05-15"), times=12),
                         deaths=c(2500, 2100, 1700, 2200, 2100, 2300, 1400, 2000, 1100, 
                                  600, 1600, 1700), 
                         region=factor(UK.excess.region$region, levels(UK.excess.region$region)))

subtitle <- ifelse(EWDate==NIDate, paste0("Weekly deaths in <span style='color:red;'>2020/21</span> compared to <span style='color:Skyblue4;'>the range in 2010-19</span>.<br>England, Wales and Northern Ireland data to ", EWDate, ".<br>Scotland data to ", ScotDate, "."),
                   paste0("Weekly deaths in <span style='color:red;'>2020/21</span> compared to <span style='color:Skyblue4;'>the range in 2010-19</span><br>England and Wales data to ",  EWDate, ".<br>Northern Ireland data to ", NIDate, ".<br>Scotland data to ", ScotDate, "."))


RegPlot <- ggplot(plot15)+
  geom_ribbon(aes(x=date, ymin=min, ymax=max), fill="Skyblue2")+
  geom_ribbon(aes(x=date, ymin=mean, ymax=deaths), fill="Red", alpha=0.2)+
  geom_line(aes(x=date, y=mean), colour="Grey50", linetype=2)+
  geom_line(aes(x=date, y=deaths), colour="Red")+
  scale_x_date(name="", date_labels="%b-%y")+
  scale_y_continuous(name="Weekly deaths registered", limits=c(0,NA))+
  facet_wrap(~region)+
  theme_classic()+
  theme(plot.title=element_text(face="bold", size=rel(1.2)), plot.subtitle=element_markdown(),
        strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)))+
  labs(title="Regional variation in all-cause mortality across the UK",
       subtitle=subtitle,
       caption="Data from ONS, NRS & NISRA | Plot by @VictimOfMaths")+
  geom_text(data=ann_text15, aes(x=date, y=deaths), label=c(paste0("+", round(UK.excess.region[1,2],0)," excess deaths in 2020/21\nvs. 2010-19 average (+",
                                                                   round(UK.excess.region[1,4]*100, 0),"%)"), 
                                                            paste0("+", round(UK.excess.region[2,2],0)," deaths (+",
                                                                   round(UK.excess.region[2,4]*100, 0),"%)"),
                                                            paste0("+", round(UK.excess.region[3,2],0)," deaths (+",
                                                                   round(UK.excess.region[3,4]*100, 0),"%)"),
                                                            paste0("+", round(UK.excess.region[4,2],0)," deaths (+",
                                                                   round(UK.excess.region[4,4]*100, 0),"%)"),
                                                            paste0("+", round(UK.excess.region[5,2],0)," deaths (+",
                                                                   round(UK.excess.region[5,4]*100, 0),"%)"),
                                                            paste0("+", round(UK.excess.region[6,2],0)," deaths (+",
                                                                   round(UK.excess.region[6,4]*100, 0),"%)"),
                                                            paste0("+", round(UK.excess.region[7,2],0)," deaths (+",
                                                                   round(UK.excess.region[7,4]*100, 0),"%)"),
                                                            paste0("+", round(UK.excess.region[8,2],0)," deaths (+",
                                                                   round(UK.excess.region[8,4]*100, 0),"%)"),
                                                            paste0("+", round(UK.excess.region[9,2],0)," deaths (+",
                                                                   round(UK.excess.region[9,4]*100, 0),"%)"),
                                                            paste0("+", round(UK.excess.region[10,2],0)," deaths (+",
                                                                   round(UK.excess.region[10,4]*100, 0),"%)"),
                                                            paste0("+", round(UK.excess.region[11,2],0)," deaths (+",
                                                                   round(UK.excess.region[11,4]*100, 0),"%)"),
                                                            paste0("+", round(UK.excess.region[12,2],0)," deaths (+",
                                                                   round(UK.excess.region[12,4]*100, 0),"%)")), 
            size=3, colour=rep("Red", 12), hjust=0)



agg_tiff("Outputs/ONSNRSNISRAWeeklyDeathsxReg.tiff", units="in", width=12, height=8, res=500)
RegPlot
dev.off()  

agg_png("Outputs/ONSNRSNISRAWeeklyDeathsxReg.png", units="in", width=12, height=8, res=500)
RegPlot
dev.off() 

RegPlot2 <- ggplot(plot15)+
  geom_ribbon(aes(x=date, ymin=min, ymax=max), fill="Skyblue2")+
  geom_ribbon(aes(x=date, ymin=mean, ymax=deaths), fill="Red", alpha=0.2)+
  geom_line(aes(x=date, y=mean), colour="Grey50", linetype=2)+
  geom_line(aes(x=date, y=deaths), colour="Red")+
  scale_x_date(name="", date_labels="%b-%y")+
  scale_y_continuous(name="Weekly deaths registered", limits=c(0,NA))+
  facet_wrap(~region, scales="free_y")+
  theme_classic()+
  theme(plot.title=element_text(face="bold", size=rel(1.2)), plot.subtitle=element_markdown(),
        strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)))+
  labs(title="Regional variation in all-cause mortality across the UK",
       subtitle=subtitle,
       caption="Data from ONS, NRS & NISRA | Plot by @VictimOfMaths")+
  geom_text(data=ann_text15, aes(x=date, y=deaths), label=c(paste0("+", round(UK.excess.region[1,2],0)," excess deaths in 2020/21\nvs. 2010-19 average (+",
                                                                   round(UK.excess.region[1,4]*100, 0),"%)"), 
                                                            paste0("+", round(UK.excess.region[2,2],0)," deaths (+",
                                                                   round(UK.excess.region[2,4]*100, 0),"%)"),
                                                            paste0("+", round(UK.excess.region[3,2],0)," deaths (+",
                                                                   round(UK.excess.region[3,4]*100, 0),"%)"),
                                                            paste0("+", round(UK.excess.region[4,2],0)," deaths (+",
                                                                   round(UK.excess.region[4,4]*100, 0),"%)"),
                                                            paste0("+", round(UK.excess.region[5,2],0)," deaths (+",
                                                                   round(UK.excess.region[5,4]*100, 0),"%)"),
                                                            paste0("+", round(UK.excess.region[6,2],0)," deaths (+",
                                                                   round(UK.excess.region[6,4]*100, 0),"%)"),
                                                            paste0("+", round(UK.excess.region[7,2],0)," deaths (+",
                                                                   round(UK.excess.region[7,4]*100, 0),"%)"),
                                                            paste0("+", round(UK.excess.region[8,2],0)," deaths (+",
                                                                   round(UK.excess.region[8,4]*100, 0),"%)"),
                                                            paste0("+", round(UK.excess.region[9,2],0)," deaths (+",
                                                                   round(UK.excess.region[9,4]*100, 0),"%)"),
                                                            paste0("+", round(UK.excess.region[10,2],0)," deaths (+",
                                                                   round(UK.excess.region[10,4]*100, 0),"%)"),
                                                            paste0("+", round(UK.excess.region[11,2],0)," deaths (+",
                                                                   round(UK.excess.region[11,4]*100, 0),"%)"),
                                                            paste0("+", round(UK.excess.region[12,2],0)," deaths (+",
                                                                   round(UK.excess.region[12,4]*100, 0),"%)")), 
            size=2.6, colour=rep("Red", 12), hjust=0)



agg_tiff("Outputs/ONSNRSNISRAWeeklyDeathsxReg2.tiff", units="in", width=12, height=8, res=500)
RegPlot2
dev.off()  

plot16 <- data.reg.UK %>%
  group_by(region, year) %>%
  arrange(region, year, date) %>% 
  mutate(cumul_deaths=cumsum(deaths),
         week=if_else(region=="Northern Ireland" & year==2021, week-53, week))

agg_tiff("Outputs/ONSNRSNISRAWeeklyCumulDeaths_reg.tiff", units="in", width=12, height=8, res=350)
ggplot()+
  geom_line(data=subset(plot16, year!=2020), aes(x=week, y=cumul_deaths, group=as.factor(year)), colour="Grey80")+
  geom_line(data=subset(plot16, year==2020), aes(x=week, y=cumul_deaths), colour="Tomato")+
  geom_line(data=subset(plot16, year==2021), aes(x=week, y=cumul_deaths), colour="darkorchid")+
  theme_classic()+
  facet_wrap(~region)+
  scale_x_continuous(name="Week number")+
  scale_y_continuous(name="Deaths registered")+
  theme(strip.background=element_blank(), strip.text=element_text(face="bold"),
        plot.subtitle =element_markdown(), plot.title=element_text(face="bold", size=rel(1.2)))+
  labs(title="Not 'just another year'",
       subtitle="Cumulative deaths from all causes in <span style='color:Tomato;'>2020</span> and <span style='color:darkorchid;'>2021</span> compared to <span style='color:Grey60;'>the range in 2010-19</span>",
       caption="Data from ONS, NRS & NISRA | Plot by @VictimOfMaths")
dev.off()
