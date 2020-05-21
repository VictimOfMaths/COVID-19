rm(list=ls())
#Install the ggtext package from GitHub if you don't have it already
#remotes::install_github("wilkelab/ggtext")

library(tidyverse)
library(paletteer)
library(curl)
library(readxl)
library(lubridate)
library(forcats)
library(ggtext)

#Read in data for England & Wales (created by AllCauseMortality.R)
data.EW <- read.csv("Data/deaths_age_EW.csv")

#Tidy to match Scottish data
data.EW <- data.EW[,c(5,6,4,3,7)]
colnames(data.EW) <- c("year", "week", "age", "deaths", "country")
data.EW <- subset(data.EW, age!="Total")
levels(data.EW$age)[which(levels(data.EW$age)=="Under 1 year")] <- "0-14"
levels(data.EW$age)[which(levels(data.EW$age)=="01-14")] <- "0-14"

data.EW <- data.EW %>%
  group_by(age, year, week, country) %>%
  summarise(deaths=sum(deaths))

#Read in Scottish data
#Weekly age-specific data is published by NRS
temp <- tempfile()
temp <- curl_download(url="https://www.nrscotland.gov.uk/files//statistics/covid19/covid-deaths-data-week-20.xlsx", destfile=temp, quiet=FALSE, mode="wb")
data2020.S <- data.frame(t(read_excel(temp, sheet="Table 2 - All deaths", range="C15:V21", col_names=FALSE)))
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

data2020.S_long$age <- as.factor(data2020.S_long$age)
levels(data2020.S_long$age)[which(levels(data2020.S_long$age)=="Under 1 year")] <- "0-14"
levels(data2020.S_long$age)[which(levels(data2020.S_long$age)=="01-14")] <- "0-14"

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
temp <- curl_download(url="https://www.nisra.gov.uk/sites/nisra.gov.uk/files/publications/Weekly_Deaths.XLS", 
                      destfile=temp, quiet=FALSE, mode="wb")
data2020.NI <- read_excel(temp, sheet="Weekly_Deaths_Age by Sex", range="D6:U13", col_names=FALSE)
colnames(data2020.NI) <- c(1:ncol(data2020.NI))

data2020.NI$year <- 2020
data2020.NI$age <- c("0-14", "0-14", "01-14", "15-44", "45-64", "65-74", "75-84", "85+")

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

#Scotland only age plot
ann_text2 <- data.frame(week=rep(21, times=6), pos=c(75, 100, 240, 260, 400, 450),
                        age=c("0-14", "15-44", "45-64", "65-74", "75-84", "85+"))

tiff("Outputs/NRSWeeklyDeathsxAge.tiff", units="in", width=12, height=8, res=300)
ggplot(subset(data, country=="Scotland"))+
  geom_ribbon(aes(x=week, ymin=min_d, ymax=max_d), fill="Skyblue2")+
  geom_ribbon(aes(x=week, ymin=mean_d, ymax=deaths), fill="Red", alpha=0.2)+
  geom_line(aes(x=week, y=mean_d), colour="Grey50", linetype=2)+
  geom_line(aes(x=week, y=deaths), colour="Red")+
  theme_classic()+
  facet_wrap(~age)+
  scale_x_continuous(name="Week number", breaks=c(0,10,20,30,40,50))+
  scale_y_continuous(name="Deaths registered")+
  expand_limits(y=0)+
  labs(title="All-cause deaths in Scotland have stopped falling in under-65s and over-85s",
       subtitle="Weekly deaths in <span style='color:red;'>2020</span> compared to <span style='color:Skyblue4;'>the range in 2010-19</span>. Data up to 16th May",
       caption="Data from NRS | Plot by @VictimOfMaths")+
  theme(strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)),
        plot.subtitle =element_markdown())+
  geom_text(data=ann_text2, aes(x=week, y=pos), label=c(paste0(round(excess[2,6],0)," excess deaths in 2020\nvs. 2010-19 average (",
                                                                    round(excess[2,7]*100, 1),"%)"), 
                                                             paste0(round(excess[5,6],0)," deaths (+",
                                                                    round(excess[5,7]*100, 0),"%)"),
                                                             paste0(round(excess[8,6],0)," deaths (+",
                                                                    round(excess[8,7]*100, 0),"%)"),
                                                             paste0(round(excess[11,6],0)," deaths (+",
                                                                    round(excess[11,7]*100, 0),"%)"),
                                                             paste0(round(excess[14,6],0)," deaths (+",
                                                                    round(excess[14,7]*100, 0),"%)"),
                                                             paste0(round(excess[17,6],0)," deaths (+",
                                                                    round(excess[17,7]*100, 0),"%)")), 
            size=3, colour=rep("red", times=6), hjust=0)

dev.off()  

#Bring in date from HMD
temp <- tempfile()
temp <- curl_download(url="https://www.mortality.org/Public/STMF/Outputs/stmf.csv", destfile=temp, quiet=FALSE, mode="wb")
HMDdata <- read_delim(temp, delim=",", comment="#")

#Keep only combined sex data and from years 2010 onwards
HMDdata <- subset(HMDdata, Year>=2010 & Sex=="b")

#Lengthen (there's probably a more elegant pivot_longer() solution)
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

#Adjust HMD mortality rates from annualised per capite to weekly rates per 100,000
HMD_long$mortrate <- HMD_long$mortrate*100000/52

#Update UK data age groups to match HMD groups
data.UK$age <- case_when(
  data.UK$age %in% c("15-44", "45-64") ~ "15-64",
  TRUE ~ data.UK$age)

data.UK <- data.UK %>%
  group_by(age, year, week, country) %>%
  summarise(deaths=sum(deaths), pop=sum(pop))

data.UK$mortrate <- data.UK$deaths*100000/data.UK$pop

#Merge data
fulldata <- bind_rows(subset(HMD_long, country!="GBRTENW"), data.UK[,-c(6)])

#Calculate 2010-19 average, min and max
hist.fulldata <- fulldata %>%
  filter(year!=2020) %>%
  group_by(age, country, week) %>%
  summarise(mean_d=mean(deaths), max_d=max(deaths), min_d=min(deaths),
            mean_r=mean(mortrate), max_r=max(mortrate), min_r=min(mortrate))

fulldata <- merge(hist.fulldata, subset(fulldata, year==2020), all.x=TRUE, all.y=TRUE)

#Tidy up names
fulldata$country <- case_when(
  fulldata$country=="AUT" ~ "Austria",
  fulldata$country=="BEL" ~ "Belgium",
  fulldata$country=="DNK" ~ "Denmark",
  fulldata$country=="DEUTNP" ~ "Germany",
  fulldata$country=="ESP" ~ "Spain",
  fulldata$country=="FIN" ~ "Finland",
  fulldata$country=="ISL" ~ "Iceland",
  fulldata$country=="NLD" ~ "Netherlands",
  fulldata$country=="NOR" ~ "Norway",
  fulldata$country=="PRT" ~ "Portugal",
  fulldata$country=="SWE" ~ "Sweden",
  TRUE ~ fulldata$country)

#Remove most recent week of data for FIN, NOR and USA which is wonky
fulldata <- fulldata %>%
  group_by(age, country, year) %>%
  mutate(last_week=max(week)) %>%
  ungroup() %>%
  filter(!(country %in% c("Finland", "Norway", "USA") & year==2020 & week==last_week))

png("Outputs/ExcessEURUSxAge.png", units="in", width=24, height=10, res=300)
ggplot(fulldata)+
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
       subtitle="Registered weekly death rates in <span style='color:red;'>2020</span> compared to <span style='color:Skyblue4;'>the average for 2010-19",
       caption="Data from mortality.org, ONS, NRS and NISRA | Plot by @VictimOfMaths")
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
  labs(title="15-64 year olds in England & Wales appear to fared poorly compared to their peers elsewhere",
       subtitle="Registered weekly death rates among 15-64 year-olds in <span style='color:red;'>2020</span> compared to <span style='color:Skyblue4;'>the average for 2010-19",
       caption="Data from mortality.org, ONS, NRS and NISRA | Plot by @VictimOfMaths")
dev.off()

#Calculate excess mortality rate
fulldata$excess <- fulldata$mortrate-fulldata$mean_r

ggplot(subset(fulldata, age=="15-64"))+
  geom_segment(aes(x=0, xend=25, y=0, yend=0))+
  geom_line(aes(x=week, y=excess, colour=country))+
  scale_x_continuous(name="Week number", breaks=c(0,10,20), limits=c(0,25))+
  scale_y_continuous("Excess weekly deaths per 100,000 vs. 2010-19 average")+
  theme_classic()+
  scale_colour_manual(values=c(rep("Grey50", times=3), "Red", rep("Grey50", times=7), "Blue", rep("Grey50", times=3)))
