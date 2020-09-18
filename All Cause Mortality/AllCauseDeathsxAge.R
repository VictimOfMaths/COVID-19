rm(list=ls())

library(tidyverse)
library(paletteer)
library(curl)
library(readxl)
library(lubridate)
library(forcats)
library(ggtext)
library(HMDHFDplus)

#Read in data for England & Wales (created by AllCauseMortality.R)
data.EW <- read.csv("Data/deaths_age_EW.csv")

#Tidy to match Scottish data
data.EW <- data.EW[,c(5,6,4,3,7)]
colnames(data.EW) <- c("year", "week", "age", "deaths", "country")
data.EW <- subset(data.EW, age!="Total")
data.EW$age <- if_else(data.EW$age=="Under 1 year", "0-14", as.character(data.EW$age))
data.EW$age <- if_else(data.EW$age=="01-14", "0-14", as.character(data.EW$age))

data.EW <- data.EW %>%
  group_by(age, year, week, country) %>%
  summarise(deaths=sum(deaths))

#Read in Scottish data
#Weekly age-specific data is published by NRS
temp <- tempfile()
temp <- curl_download(url="https://www.nrscotland.gov.uk/files//statistics/covid19/covid-deaths-data-week-37.xlsx", destfile=temp, quiet=FALSE, mode="wb")
data2020.S <- data.frame(t(read_excel(temp, sheet=3, range="C15:AM21", col_names=FALSE)))
date <- data.frame(date=format(seq.Date(from=as.Date("2019-12-30"), by="7 days", length.out=nrow(data2020.S)), "%d/%m/%y"))
data2020.S <- cbind(date, data2020.S)
colnames(data2020.S) <- c("date", "Under 1 year", "01-14", "15-44", "45-64", "65-74", "75-84", "85+")
data2020.S$date <- as.Date(data2020.S$date, "%d/%m/%y")
data2020.S$weekno <- week(data2020.S$date)

data2020.S_long <- gather(data2020.S, age, deaths, c(2:8))
data2020.S_long$year <- 2020
data2020.S_long$deaths <- as.numeric(ifelse(data2020.S_long$deaths==".", 0, data2020.S_long$deaths))

#Recalculate dates to align with ONS data (which uses week to, not w/c)
data2020.S_long$date <- data2020.S_long$date+days(6)
data2020.S_long$week <- week(data2020.S_long$date)

data2020.S_long$age <- if_else(data2020.S_long$age=="Under 1 year", "0-14", data2020.S_long$age)
data2020.S_long$age <- if_else(data2020.S_long$age=="01-14", "0-14", data2020.S_long$age)
data2020.S_long$age <- as.factor(data2020.S_long$age)

data2020.S_long <- data2020.S_long %>%
  group_by(week, year, age) %>%
  summarise(deaths=sum(deaths))

#Bring in 2010-2019 data
temp <- tempfile()
temp <- curl_download(url="https://www.nrscotland.gov.uk/files//statistics/covid19/weekly-deaths-by-sex-age-2000-2019.xlsx", destfile=temp, quiet=FALSE, mode="wb")
data2010.S <- read_excel(temp, sheet="2010", range="B4:BD44", col_names=TRUE)
data2010.S$sex <- rep(c("Female", "Male"), each=20)
data2010.S_long <- gather(data2010.S, week, deaths, c(3:55))[,-c(2)]
data2010.S_long$year <- 2010

data2011.S <- read_excel(temp, sheet="2011", range="B4:BD44", col_names=TRUE)
data2011.S$sex <- rep(c("Female", "Male"), each=20)
data2011.S_long <- gather(data2011.S, week, deaths, c(3:55))[,-c(2)]
data2011.S_long$year <- 2011

data2012.S <- read_excel(temp, sheet="2012", range="B4:BD44", col_names=TRUE)
data2012.S$sex <- rep(c("Female", "Male"), each=20)
data2012.S_long <- gather(data2012.S, week, deaths, c(3:55))[,-c(2)]
data2012.S_long$year <- 2012

data2013.S <- read_excel(temp, sheet="2013", range="B4:BD44", col_names=TRUE)
data2013.S$sex <- rep(c("Female", "Male"), each=20)
data2013.S_long <- gather(data2013.S, week, deaths, c(3:55))[,-c(2)]
data2013.S_long$year <- 2013

data2014.S <- read_excel(temp, sheet="2014", range="B4:BD44", col_names=TRUE)
data2014.S$sex <- rep(c("Female", "Male"), each=20)
data2014.S_long <- gather(data2014.S, week, deaths, c(3:55))[,-c(2)]
data2014.S_long$year <- 2014

data2015.S <- read_excel(temp, sheet="2015", range="B4:BD44", col_names=TRUE)
data2015.S$sex <- rep(c("Female", "Male"), each=20)
data2015.S_long <- gather(data2015.S, week, deaths, c(3:55))[,-c(2)]
data2015.S_long$year <- 2015

data2016.S <- read_excel(temp, sheet="2016", range="B4:BD44", col_names=TRUE)
data2016.S$sex <- rep(c("Female", "Male"), each=20)
data2016.S_long <- gather(data2016.S, week, deaths, c(3:55))[,-c(2)]
data2016.S_long$year <- 2016

data2017.S <- read_excel(temp, sheet="2017", range="B4:BD44", col_names=TRUE)
data2017.S$sex <- rep(c("Female", "Male"), each=20)
data2017.S_long <- gather(data2017.S, week, deaths, c(3:55))[,-c(2)]
data2017.S_long$year <- 2017

data2018.S <- read_excel(temp, sheet="2018", range="B4:BD44", col_names=TRUE)
data2018.S$sex <- rep(c("Female", "Male"), each=20)
data2018.S_long <- gather(data2018.S, week, deaths, c(3:55))[,-c(2)]
data2018.S_long$year <- 2018

data2019.S <- read_excel(temp, sheet="2019", range="B4:BD44", col_names=TRUE)
data2019.S$sex <- rep(c("Female", "Male"), each=20)
data2019.S_long <- gather(data2019.S, week, deaths, c(3:55))[,-c(2)]
data2019.S_long$year <- 2019

data1019.S_long <- bind_rows(data2010.S_long, data2011.S_long, data2012.S_long, data2013.S_long,
                             data2014.S_long, data2015.S_long, data2016.S_long, data2017.S_long,
                             data2018.S_long, data2019.S_long)

#Match age bands
data1019.S_long$age <- case_when(
  data1019.S_long$Age=="0" ~ "0-14",
  data1019.S_long$Age=="1-4" ~ "0-14",
  data1019.S_long$Age=="5-9" ~ "0-14",
  data1019.S_long$Age=="10-14" ~ "0-14",
  data1019.S_long$Age=="15-19" ~ "15-44",
  data1019.S_long$Age=="20-24" ~ "15-44",
  data1019.S_long$Age=="25-29" ~ "15-44",
  data1019.S_long$Age=="30-34" ~ "15-44",
  data1019.S_long$Age=="35-39" ~ "15-44",
  data1019.S_long$Age=="40-44" ~ "15-44",
  data1019.S_long$Age=="45-49" ~ "45-64",
  data1019.S_long$Age=="50-54" ~ "45-64",
  data1019.S_long$Age=="55-59" ~ "45-64",
  data1019.S_long$Age=="60-64" ~ "45-64",
  data1019.S_long$Age=="65-69" ~ "65-74",
  data1019.S_long$Age=="70-74" ~ "65-74",
  data1019.S_long$Age=="75-79" ~ "75-84",
  data1019.S_long$Age=="80-84" ~ "75-84",
  data1019.S_long$Age=="85-89" ~ "85+",
  data1019.S_long$Age=="90+" ~ "85+"
)

data1019.S <- data1019.S_long %>%
  group_by(year, week, age) %>%
  summarise(deaths=sum(deaths))

data1019.S$week <- as.integer(data1019.S$week)

data.S <- bind_rows(data1019.S, data2020.S_long)
data.S$country <- "Scotland"

data <- bind_rows(data.S, data.EW)

#Read in NI data (for 2020 only)
temp <- tempfile()
temp <- curl_download(url="https://www.nisra.gov.uk/sites/nisra.gov.uk/files/publications/Weekly_Deaths.xls", 
                      destfile=temp, quiet=FALSE, mode="wb")
data2020.NI <- read_excel(temp, sheet="Table 2", range="D7:AJ14", col_names=FALSE)
colnames(data2020.NI) <- c(1:ncol(data2020.NI))

data2020.NI$year <- 2020
data2020.NI$age <- c("0-14", "0-14", "0-14", "15-44", "45-64", "65-74", "75-84", "85+")

data2020.NI_long <- gather(data2020.NI, week, deaths, c(1:(ncol(data2020.NI)-2)))

data2020.NI_long <- data2020.NI_long %>%
  group_by(age, week, year) %>%
  summarise(deaths=sum(deaths))

data2020.NI_long$country <- "Northern Ireland"
data2020.NI_long$week <- as.integer(data2020.NI_long$week)

data <- bind_rows(data, data2020.NI_long)

#Bring in populations
temp <- tempfile()
temp2 <- tempfile()
source <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fpopulationandmigration%2fpopulationestimates%2fdatasets%2fpopulationestimatesforukenglandandwalesscotlandandnorthernireland%2fmid2001tomid2019detailedtimeseries/myeb1.zip"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)
popdata <- read.csv(file.path(temp2,"MYEB1_detailed_population_estimates_series_UK_(2019_geog20).csv"))

popdata$age <- case_when(
  popdata$age<15 ~ "0-14",
  popdata$age<45 ~ "15-44",
  popdata$age<65 ~ "45-64",
  popdata$age<75 ~ "65-74",
  popdata$age<85 ~ "75-84",
  TRUE ~ "85+")

popdata$country <- case_when(
  popdata$country=="S" ~ "Scotland",
  popdata$country=="N" ~ "Northern Ireland",
  TRUE ~ "England & Wales")

popdata <- popdata %>%
  group_by(age, country) %>%
  summarise(pop_2010=sum(population_2010), pop_2011=sum(population_2011), pop_2012=sum(population_2012),
            pop_2013=sum(population_2013), pop_2014=sum(population_2014), pop_2015=sum(population_2015),
            pop_2016=sum(population_2016), pop_2017=sum(population_2017), pop_2018=sum(population_2018),
            pop_2019=sum(population_2019), pop_2020=sum(population_2019))

popdata_long <- gather(popdata, year, pop, c(3:13))
popdata_long$year <- as.integer(substr(popdata_long$year, 5, 8))

#Merge into main data
data <- merge(data, popdata_long)

data$mortrate <- data$deaths*100000/data$pop

#take a copy for later
data.UK <- data

#Calculate 2010-19 average, min and max
hist.data <- data %>%
  filter(year!=2020) %>%
  group_by(age, country, week) %>%
  summarise(mean_d=mean(deaths), max_d=max(deaths), min_d=min(deaths),
            mean_r=mean(mortrate), max_r=max(mortrate), min_r=min(mortrate))

data <- merge(hist.data, subset(data, year==2020)[,c(2:5,7)], all.x=TRUE, all.y=TRUE)

#Tidy up before plotting
data$country <- factor(data$country, levels=c("England & Wales", "Scotland", "Northern Ireland"))
data$age <- ifelse(data$age=="01-14", "0-14", data$age)

#Calculate excess deaths in 2020 vs. historic mean
excess <- data %>%
  group_by(age, country) %>%
  filter(!is.na(deaths) & week<20) %>%
  summarise(deaths=sum(deaths), mean=sum(mean_d), place=mean(mean_r))

excess$excess <- excess$deaths-excess$mean
excess$prop <- excess$excess/excess$mean

ann_text <- data.frame(week=rep(21, times=18), mean_r=excess$place*1.3+0.4, 
                       country=rep(c("England & Wales", "Scotland", "Northern Ireland"), times=6),
                       age=rep(c("0-14", "15-44", "45-64", "65-74", "75-84", "85+"), each=3))

tiff("Outputs/ExcessDeathsxAgexCountry.tiff", units="in", width=10, height=10, res=300)
ggplot(data)+
  geom_ribbon(aes(x=week, ymin=min_r, ymax=max_r), fill="Skyblue2")+
  geom_ribbon(aes(x=week, ymin=mean_r, ymax=mortrate), fill="Red", alpha=0.2)+
  geom_line(aes(x=week, y=mean_r), colour="Grey50", linetype=2)+
  geom_line(aes(x=week, y=mortrate), colour="Red")+
  scale_x_continuous(name="Week number")+
  scale_y_continuous("Weekly deaths per 100,000")+
  facet_grid(age~country, scales="free_y")+
  geom_text(data=ann_text, aes(x=week, y=mean_r), label=c(paste0("", round(excess[1,7]*100,0), "% change in deaths in 2020\nvs. 2010-19 average"),
                                                          paste0("", round(excess[2,7]*100,0), "%"),
                                                          paste0("+", round(excess[3,7]*100,0), "%"),
                                                          paste0("+", round(excess[4,7]*100,0), "%"),
                                                          paste0("+", round(excess[5,7]*100,0), "%"),
                                                          paste0("+", round(excess[6,7]*100,0), "%"),
                                                          paste0("+", round(excess[7,7]*100,0), "%"),
                                                          paste0("+", round(excess[8,7]*100,0), "%"),
                                                          paste0("+", round(excess[9,7]*100,0), "%"),
                                                          paste0("+", round(excess[10,7]*100,0), "%"),
                                                          paste0("+", round(excess[11,7]*100,0), "%"),
                                                          paste0("+", round(excess[12,7]*100,0), "%"),
                                                          paste0("+", round(excess[13,7]*100,0), "%"),
                                                          paste0("+", round(excess[14,7]*100,0), "%"),
                                                          paste0("+", round(excess[15,7]*100,0), "%"),
                                                          paste0("+", round(excess[16,7]*100,0), "%"),
                                                          paste0("+", round(excess[17,7]*100,0), "%"),
                                                          paste0("+", round(excess[18,7]*100,0), "%")),
            size=3, colour="Red", hjust=0)+
  theme_classic()+
  theme(strip.background=element_blank(), strip.text=element_text(size=rel(1), face="bold"), 
        plot.subtitle =element_markdown())+
  labs(title="Excess deaths in England are higher in 45-84 year-olds than elsewhere in the UK",
       subtitle="Weekly deaths in <span style='color:red;'>2020</span> compared to <span style='color:Skyblue4;'>the range in 2010-19</span>.",
       caption="Date from ONS, NRS & NISRA | Plot by @VictimOfMaths")

dev.off()

#Bring in data from HMD
temp <- tempfile()
temp <- curl_download(url="https://www.mortality.org/Public/STMF/Outputs/stmf.csv", destfile=temp, quiet=FALSE, mode="wb")
HMDdata <- read_delim(temp, delim=",", comment="#")

#Keep only combined sex data and from years 2010 onwards
HMDdata <- subset(HMDdata, Year>=2010 & Sex=="b")

#Lengthen (there's probably (definitely) a more elegant pivot_longer() solution)
HMD_long <- gather(HMDdata, age, deaths,c(5:9))[,c(1:3, 15, 16)]
HMD_long$age <- substr(HMD_long$age, 2, 7)
temp <- gather(HMDdata, age, mortrate, c(11:15))[,c(1:3, 15, 16)]
temp$age <- substr(temp$age, 2, 7)

HMD_long <- merge(HMD_long, temp)

colnames(HMD_long) <- c("country", "year", "week", "age", "deaths", "mortrate")

#Tidy up years
HMD_long$age <- case_when(
  HMD_long$age=="0_14" ~ "0-14",
  HMD_long$age=="15_64" ~ "15-64",
  HMD_long$age=="65_74" ~ "65-74",
  HMD_long$age=="75_84" ~ "75-84",
  HMD_long$age=="85p" ~ "85+")

#Adjust HMD mortality rates from annualised per capita to weekly rates per 100,000
HMD_long$mortrate <- HMD_long$mortrate*100000/52

#Update UK data age groups to match HMD groups
data.UK$age <- case_when(
  data.UK$age %in% c("15-44", "45-64") ~ "15-64",
  TRUE ~ data.UK$age)

data.UK <- data.UK %>%
  group_by(age, year, week, country) %>%
  summarise(deaths=sum(deaths), pop=sum(pop))

data.UK$mortrate <- data.UK$deaths*100000/data.UK$pop

#Merge data excluding countries which are in HMD data, but which we've collected separately (with longer coverage)
mergeddata <- bind_rows(subset(HMD_long, !(country %in% c("GBRTENW", "GBR_SCO", "FRATNP", "ITA"))), data.UK[,-c(6)])

#Calculate 2010-19 average, min and max
hist.mergeddata <- mergeddata %>%
  filter(year!=2020) %>%
  group_by(age, country, week) %>%
  summarise(mean_d=mean(deaths), max_d=max(deaths), min_d=min(deaths),
            mean_r=mean(mortrate), max_r=max(mortrate), min_r=min(mortrate))

fulldata <- merge(hist.mergeddata, subset(mergeddata, year==2020), all.x=TRUE, all.y=TRUE)

#Bring in French data from Insee
#File created by All Cause Deaths France.R
data.FR <- read.csv("Data/deaths_age_France.csv")[,-c(1)]

#Bring in French population from HMD (need to register and put your details in here)
username <- "" 
password <- ""

FraPop <- readHMDweb(CNTRY="FRATNP", "Exposures_1x1", username, password)

FraPop <- subset(FraPop, Year>=2010)

FraPop$age <- case_when(
  FraPop$Age<15 ~ "0-14",
  FraPop$Age<65 ~ "15-64",
  FraPop$Age<75 ~ "65-74",
  FraPop$Age<85 ~ "75-84",
  TRUE ~ "85+"
)

FraPop <- FraPop %>%
  group_by(Year, age) %>%
  summarise(pop=sum(Total))

#Replicate 2017 populations for 2018+
temp <- subset(FraPop, Year==2017)
temp2 <- temp
temp3 <- temp
temp$Year <- 2018
temp2$Year <- 2019
temp3$Year <- 2020

FraPop <- bind_rows(FraPop, temp, temp2, temp3)

data.FR <- merge(data.FR, FraPop, by.x=c("year", "ageband"), by.y=c("Year", "age"))
colnames(data.FR) <- c("year", "age", "week", "deaths", "pop")
data.FR$mortrate=data.FR$deaths*100000/data.FR$pop

#Calculate 2010-19 average, min and max
hist.FR <- data.FR %>%
  filter(year!=2020) %>%
  group_by(age,week) %>%
  summarise(mean_d=mean(deaths), max_d=max(deaths), min_d=min(deaths),
            mean_r=mean(mortrate), max_r=max(mortrate), min_r=min(mortrate))

fulldata.FR <- merge(hist.FR, subset(data.FR, year==2020), all.x=TRUE, all.y=TRUE)

fulldata.FR <- fulldata.FR[,-c(11)]
fulldata.FR$country <- "France"

fulldata <- bind_rows(fulldata, fulldata.FR)

#Bring in Italian data from ISTAT
#File created by All Cause Deaths Italy.R
data.IT <- read.csv("Data/deaths_age_Italy.csv")[,-c(1)]

#Compress to match
data.IT$age <- case_when(
  data.IT$age=="0" ~ "0-14",
  data.IT$age=="1-4" ~ "0-14",
  data.IT$age=="5-9" ~ "0-14",
  data.IT$age=="10-14" ~ "0-14",
  data.IT$age=="15-19" ~ "15-64",
  data.IT$age=="20-24" ~ "15-64",
  data.IT$age=="25-29" ~ "15-64",
  data.IT$age=="30-34" ~ "15-64",
  data.IT$age=="35-39" ~ "15-64",
  data.IT$age=="40-44" ~ "15-64",
  data.IT$age=="45-49" ~ "15-64",
  data.IT$age=="50-54" ~ "15-64",
  data.IT$age=="55-59" ~ "15-64",
  data.IT$age=="60-64" ~ "15-64",
  data.IT$age=="65-69" ~ "65-74",
  data.IT$age=="70-74" ~ "65-74",
  data.IT$age=="75-79" ~ "75-84",
  data.IT$age=="80-84" ~ "75-84",
  data.IT$age=="85-89" ~ "85+",
  data.IT$age=="90-94" ~ "85+",
  data.IT$age=="95-99" ~ "85+",
  TRUE ~ "85+")

data.IT <- data.IT %>%
  group_by(age, year, week) %>%
  summarise(deaths=sum(deaths))

#Bring in Italian population data from HMD
ItaPop <- readHMDweb(CNTRY="ITA", "Exposures_1x1", username, password)

ItaPop <- subset(ItaPop, Year>=2010)

ItaPop$age <- case_when(
  ItaPop$Age<15 ~ "0-14",
  ItaPop$Age<65 ~ "15-64",
  ItaPop$Age<75 ~ "65-74",
  ItaPop$Age<85 ~ "75-84",
  TRUE ~ "85+"
)

ItaPop <- ItaPop %>%
  group_by(Year, age) %>%
  summarise(pop=sum(Total))

#Replicate 2017 populations for 2018+
temp <- subset(ItaPop, Year==2017)
temp2 <- temp
temp3 <- temp
temp$Year <- 2018
temp2$Year <- 2019
temp3$Year <- 2020

ItaPop <- bind_rows(ItaPop, temp, temp2, temp3)

data.IT <- merge(data.IT, ItaPop, by.x=c("year", "age"), by.y=c("Year", "age"))
data.IT$mortrate=data.IT$deaths*100000/data.IT$pop

#Calculate 2010-19 average, min and max
hist.IT <- data.IT %>%
  filter(year!=2020) %>%
  group_by(age,week) %>%
  summarise(mean_d=mean(deaths), max_d=max(deaths), min_d=min(deaths),
            mean_r=mean(mortrate), max_r=max(mortrate), min_r=min(mortrate))

fulldata.IT <- merge(hist.IT, subset(data.IT, year==2020), all.x=TRUE, all.y=TRUE)

fulldata.IT <- fulldata.IT[,-c(11)]
fulldata.IT$country <- "Italy"

fulldata <- bind_rows(fulldata, fulldata.IT)

#Use HMD data from Northern Ireland, rather than direct from NISRA - the 2020 data is slightly
#less complete, but it does include 2010-19 data

fulldata <- fulldata %>% filter(country!="Northern Ireland")

#Tidy up names
fulldata$country <- case_when(
  fulldata$country=="AUT" ~ "Austria",
  fulldata$country=="BEL" ~ "Belgium",
  fulldata$country=="BGR" ~ "Bulgaria",
  fulldata$country=="CHE" ~ "Switzerland",
  fulldata$country=="CZE" ~ "Czechia",
  fulldata$country=="DNK" ~ "Denmark",
  fulldata$country=="DEUTNP" ~ "Germany",
  fulldata$country=="ESP" ~ "Spain",
  fulldata$country=="EST" ~ "Estonia",
  fulldata$country=="FIN" ~ "Finland",
  fulldata$country=="GBR_NIR" ~ "Northern Ireland",
  fulldata$country=="GRC" ~ "Greece",
  fulldata$country=="HRV" ~ "Croatia",
  fulldata$country=="HUN" ~ "Hungary",
  fulldata$country=="ISL" ~ "Iceland",
  fulldata$country=="ISR" ~ "Israel",
  fulldata$country=="LTU" ~ "Lithuania",
  fulldata$country=="LUX" ~ "Luxembourg",
  fulldata$country=="LVA" ~ "Latvia",
  fulldata$country=="NLD" ~ "Netherlands",
  fulldata$country=="NOR" ~ "Norway",
  fulldata$country=="POL" ~ "Poland",
  fulldata$country=="PRT" ~ "Portugal",
  fulldata$country=="RUS" ~ "Russia",
  fulldata$country=="SWE" ~ "Sweden",
  fulldata$country=="SVK" ~ "Slovakia",
  fulldata$country=="SVN" ~ "Slovenia",
  TRUE ~ fulldata$country)

#Remove most recent week of data for FIN, FRA, NOR, SVK, SWE and USA which is wonky
fulldata <- fulldata %>%
  group_by(age, country, year) %>%
  mutate(last_week=max(week)) %>%
  ungroup() %>%
  filter(!(country %in% c("Finland", "USA", "Slovakia", "Norway", "Lithuania", "France") & year==2020 & week==last_week))

#Remove Russia which has no 2020 (or 2019) data
fulldata <- fulldata %>% 
  filter(!country %in% c("Russia"))

Excessplot <- ggplot(fulldata)+
  geom_ribbon(aes(x=week, ymin=min_r, ymax=max_r), fill="Skyblue2")+
  geom_ribbon(aes(x=week, ymin=mean_r, ymax=mortrate), fill="Red", alpha=0.2)+
  geom_line(aes(x=week, y=mean_r), colour="Grey50", linetype=2)+
  geom_line(aes(x=week, y=mortrate), colour="Red")+
  scale_x_continuous(name="Week number")+
  scale_y_continuous("Weekly deaths per 100,000")+
  facet_grid(age~country, scales="free_y")+
  theme_classic()+
  theme(strip.background=element_blank(), strip.text=element_text(size=rel(1), face="bold"),
        plot.subtitle =element_markdown())+
  labs(title="Excess mortality rates by age group across Europe & the US",
       subtitle="Registered weekly death rates in <span style='color:red;'>2020</span> compared to <span style='color:Skyblue4;'>the range for 2010-19",
       caption="Data from mortality.org, Insee, ISTAT, ONS, NRS and NISRA | Plot by @VictimOfMaths")

tiff("Outputs/ExcessEURUSxAge.tiff", units="in", width=35, height=10, res=300)
Excessplot
dev.off()

png("Outputs/ExcessEURUSxAge.png", units="in", width=35, height=10, res=300)
Excessplot
dev.off()

#15-64 year olds only
tiff("Outputs/ExcessEURUSxAge1564.tiff", units="in", width=12, height=10, res=300)
ggplot(subset(fulldata, age=="15-64"))+
  geom_ribbon(aes(x=week, ymin=min_r, ymax=max_r), fill="Skyblue2")+
  geom_ribbon(aes(x=week, ymin=mean_r, ymax=mortrate), fill="Red", alpha=0.2)+
  geom_line(aes(x=week, y=mean_r), colour="Grey50", linetype=2)+
  geom_line(aes(x=week, y=mortrate), colour="Red")+
  scale_x_continuous(name="Week number")+
  scale_y_continuous("Weekly deaths per 100,000")+
  facet_wrap(~country)+
  theme_classic()+
  theme(strip.background=element_blank(), strip.text=element_text(size=rel(1), face="bold"),
        plot.subtitle =element_markdown())+
  labs(title="15-64 year olds in England, Wales and the US appear to fared poorly compared to their peers elsewhere",
       subtitle="Registered weekly death rates among 15-64 year-olds in <span style='color:red;'>2020</span> compared to <span style='color:Skyblue4;'>the range for 2010-19",
       caption="Data from mortality.org, Insee, ISTAT, ONS and NRS | Plot by @VictimOfMaths")

dev.off()

#Calculate excess mortality rate
fulldata$excess_r <- fulldata$mortrate-fulldata$mean_r

tiff("Outputs/ExcessRatexAgeEUR.tiff", units="in", width=10, height=6, res=500)
ggplot(subset(fulldata, age=="15-64"))+
  geom_segment(aes(x=0, xend=29, y=0, yend=0))+
  geom_line(aes(x=week, y=excess_r, colour=country))+
  scale_x_continuous(name="Week number", breaks=c(0,10,20), limits=c(0,29))+
  scale_y_continuous("Excess weekly deaths per 100,000 vs. 2010-19 average")+
  theme_classic()+
  scale_colour_manual(values=c(rep("Grey90", times=6), "#011627", rep("Grey90", times=17), 
                               "#2ec4b6", "Grey90", "Grey90", "#e71d36", rep("Grey90", times=2), 
                               "#ff9f1c"), name="Country")+
  labs(title="Some of these countries are not like the others",
       subtitle="Excess mortality rates in 15-64 year-olds in 2020",
       caption="Data from mortality.org, ONS, NRS, Insee and ISTAT\nPlot by @VictimOfMaths")
dev.off()

#Heatmaps of country and age-specific excess deaths
#Calculate excess mortality counts and proportion
fulldata$excess_d <- fulldata$deaths-fulldata$mean_d

excess <- fulldata %>%
  filter(!is.na(excess_d)) %>%
  group_by(age, country) %>%
  summarise(excess=sum(excess_d), total=sum(deaths), excessprop=excess/total, maxweek=max(week))

excess$excessprop <- ifelse(excess$excessprop<=0, paste0(round(excess$excessprop*100, 0), "%"),
                            paste0("+", round(excess$excessprop*100, 0), "%"))

#Get order for plots (sort by overall % increase in excess deaths)
excessrank <- excess %>%
  group_by(country) %>%
  summarise(excess=sum(excess), total=sum(total), excessprop=excess/total)

excessrank$country <- fct_reorder(as.factor(excessrank$country), -excessrank$excessprop)

#plot of excess mortality in 2020
tiff("Outputs/ExcessDeathsBars.tiff", units="in", width=10, height=8, res=500)
ggplot(excessrank, aes(y=country, x=excessprop, fill=excessprop))+
  geom_col(show.legend=FALSE)+
  scale_fill_paletteer_c("ggthemes::Classic Red-White-Green", direction=-1, 
                         limit=c(-1,1)*max(abs(excessrank$excessprop)))+
  scale_x_continuous(name="Change in all-cause deaths in 2020 vs. average in 2010-19",
                     breaks=c(-0.1,0,0.1,0.2), labels=c("-10%", "0", "+10%", "+20%"))+
  scale_y_discrete(name="")+
  theme_classic()+
  theme(plot.title.position="plot")+
  labs(title="England & Wales, alongside Spain, have seen the biggest rise in mortality in 2020 in Europe",
       subtitle="All-cause deaths in 2020 vs. the average for 2010-19",
       caption="Data from mortality.org, Insee, ISTAT, ONS & NRS | Plot by @VictimOfMaths")
dev.off()

tiff("Outputs/ExcessDeathsBarsAbs.tiff", units="in", width=10, height=8, res=500)
ggplot(excessrank, aes(y=fct_reorder(country, -excess), x=excess, fill=excess))+
  geom_col(show.legend=FALSE)+
  scale_fill_paletteer_c("ggthemes::Classic Red-White-Green", direction=-1, 
                         limit=c(-1,1)*max(abs(excessrank$excess)))+
  scale_x_continuous(name="Change in all-cause deaths in 2020 vs. average in 2010-19")+
  scale_y_discrete(name="")+
  theme_classic()+
  theme(plot.title.position="plot")+
  labs(title="The US has seen far more deaths than anywhere in Europe during the pandemic",
       subtitle="All-cause deaths in 2020 vs. the average for 2010-19",
       caption="Data from mortality.org, Insee, ISTAT, ONS & NRS | Plot by @VictimOfMaths")
dev.off()

#Calculate aggregate national data
fulldata$pop <- fulldata$deaths*100000/fulldata$mortrate

#Fix issue in Slovenia as some weeks have 0 deaths, so pop is undefined using this method
#I bet there's an elegant dplyr solution to this
fulldata$pop <- if_else(fulldata$country %in% c("Slovenia", "Northern Ireland") & fulldata$year=="2020" & is.na(fulldata$pop) & fulldata$age=="0-14",
                        max(fulldata$pop[fulldata$country=="Slovenia" & fulldata$year=="2020" & !is.na(fulldata$pop) & fulldata$age=="0-14"]), 
                        fulldata$pop)
fulldata$pop <- if_else(fulldata$country %in% c("Slovenia", "Northern Ireland") & fulldata$year=="2020" & is.na(fulldata$pop) & fulldata$age=="15-64",
                        max(fulldata$pop[fulldata$country=="Slovenia" & fulldata$year=="2020" & !is.na(fulldata$pop) & fulldata$age=="15-64"]), 
                        fulldata$pop)
fulldata$pop <- if_else(fulldata$country %in% c("Slovenia", "Northern Ireland") & fulldata$year=="2020" & is.na(fulldata$pop) & fulldata$age=="65-74",
                        max(fulldata$pop[fulldata$country=="Slovenia" & fulldata$year=="2020" & !is.na(fulldata$pop) & fulldata$age=="65-74"]), 
                        fulldata$pop)
fulldata$pop <- if_else(fulldata$country %in% c("Slovenia", "Northern Ireland") & fulldata$year=="2020" & is.na(fulldata$pop) & fulldata$age=="75-84",
                        max(fulldata$pop[fulldata$country=="Slovenia" & fulldata$year=="2020" & !is.na(fulldata$pop) & fulldata$age=="75-84"]), 
                        fulldata$pop)
fulldata$pop <- if_else(fulldata$country %in% c("Slovenia", "Northern Ireland") & fulldata$year=="2020" & is.na(fulldata$pop) & fulldata$age=="85+",
                        max(fulldata$pop[fulldata$country=="Slovenia" & fulldata$year=="2020" & !is.na(fulldata$pop) & fulldata$age=="85+"]), 
                        fulldata$pop)

natdata <- fulldata %>% 
  group_by(country, week) %>% 
  summarise(mean_d=sum(mean_d), min_d=sum(min_d), max_d=sum(max_d), deaths=sum(deaths), 
            pop=sum(pop), excess_d=sum(excess_d))

pop <- natdata %>% 
  filter(week==1) %>% 
  select(country, pop)

natdata <- merge(natdata, pop, by="country")

natdata <- natdata %>% 
  mutate(mean_r=mean_d*100000/pop.y, min_r=min_d*100000/pop.y, max_r=max_d*100000/pop.y,
         mortrate=deaths*100000/pop.y, excess_r=mortrate-mean_r)

tiff("Outputs/ExcessEUROverall.tiff", units="in", width=10, height=8, res=500)
ggplot(subset(natdata, !country %in% c("Iceland") & week<53))+
  geom_ribbon(aes(x=week, ymax=max_r, ymin=min_r), fill="Skyblue2")+
  geom_line(aes(x=week, y=mortrate), colour="red")+
  geom_line(aes(x=week, y=mean_r), colour="Grey50", linetype=2)+
  geom_ribbon(aes(x=week, ymax=mortrate, ymin=mean_r), fill="red", alpha=0.2)+
  scale_x_continuous(name="Week")+
  scale_y_continuous(name="Deaths per 100,000")+
  facet_wrap(~country)+
  theme_classic()+
  theme(strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)),
        plot.subtitle=element_markdown())+
  labs(title="Excess mortality rates across Europe & the US",
       subtitle="Registered weekly death rates in <span style='color:red;'>2020</span> compared to <span style='color:Skyblue4;'>the range for 2010-19",
       caption="Data from mortality.org, Insee, ISTAT, ONS, NRS and NISRA | Plot by @VictimOfMaths")

dev.off()  

#Plots
plotage <- "0-14"
plotdata <- subset(fulldata, age==plotage & !is.na(excess_r))
plotexcess <- subset(excess, age==plotage)

plotdata$country <- factor(plotdata$country, levels=levels(excessrank$country))

plotdata <- plotdata%>%
  group_by(country) %>%
  mutate(maxweek=max(week))

tiff(paste0("Outputs/ExcessEURUSHeatmap", plotage, ".tiff"), units="in", width=10, height=8, res=300)
ggplot()+
  geom_tile(data=plotdata, aes(x=week, y=country, fill=excess_r))+
  scale_x_continuous(limits=c(0,35), breaks=c(0,5,10,15,20,25,30,35), name="Week")+
  scale_y_discrete(name="")+
  scale_fill_paletteer_c("pals::kovesi.diverging_gwr_55_95_c38", limit=c(-1,1)*max(abs(plotdata$excess_r)), 
                         name="Weekly excess deaths\nper 100,000")+
  geom_text(data=subset(excess, age==plotage), aes(x=maxweek+1, y=country, label=excessprop), hjust=0, size=rel(3), colour="White")+
  labs(title=paste0("International variation in mortality rates in ages ", plotage),
       subtitle=paste0("Excess weekly all-cause death rates in 2020 compared to 2010-19 average.\nCountries ordered by overall change in deaths across all ages."),
       caption="Data from mortality.org, Insee, ISTAT, ONS & NRS | Plot by @VictimOfMaths")+
  theme_classic()+
  theme(panel.background=element_rect(fill="Black"), plot.background=element_rect(fill="Black"),
        axis.line=element_line(colour="White"), text=element_text(colour="White"),
        axis.text=element_text(colour="White"), axis.ticks=element_line(colour="White"),
        legend.background=element_rect(fill="Black"),legend.text=element_text(colour="White"),
        plot.title.position="plot")
dev.off()

#############################################
tiff("Outputs/ExcessEURTiming.tiff", units="in", width=10, height=8, res=500)
fulldata %>% 
  select(age, country, week, excess_r) %>% 
  group_by(country, age) %>% 
  filter(!is.na(excess_r) & excess_r>0 & !age %in% c("0-14", "15-64") & 
           country %in% c("England & Wales", "France", "Italy", "Scotland", "Spain", 
                          "Sweden", "USA")) %>% 
  mutate(max_excess=max(excess_r), maxprop=excess_r/max_excess) %>% 
  ggplot()+
  geom_line(aes(x=week, y=maxprop, colour=age))+
  scale_x_continuous(name="Week")+
  scale_y_continuous(name="Proportion of peak number of deaths by age group",
                     labels = scales::percent_format(accuracy = 1))+
  scale_colour_paletteer_d("fishualize::Acanthurus_olivaceus", name="Age")+
  facet_wrap(~country)+
  theme_classic()+
  theme(strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)))+
  labs(title="Was the mortality peak later in older age groups?",
       subtitle="Weekly all-cause deaths by age in 2020 as a proportion of the maximum weekly count for selected countries",
       caption="Data from ONS, Insee, ISTAT, NRS and mortality.org | Plot by @VictimOfMaths")
dev.off()

