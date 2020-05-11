rm(list=ls())

library(tidyverse)
library(curl)
library(readxl)
library(RcppRoll)
library(geojsonio)
library(broom)
library(zoo)
library(gganimate)

#Get English UTLA-level case trajectories
temp <- tempfile()
source <- "https://coronavirus.data.gov.uk/downloads/csv/coronavirus-cases_latest.csv"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
data <- read.csv(temp)[,c(1,2,3,4,5,8)]
colnames(data) <- c("name", "code", "type", "date", "cases", "cumul_cases")
data$date <- as.Date(data$date)
data <- subset(data, type=="Upper tier local authority")

#Set up skeleton dataframe with dates
LAcodes <- unique(data$code)
min <- min(data$date)
max <- max(data$date)

skeleton <- data.frame(code=rep(LAcodes, each=(max-min+1), times=1), date=rep(seq.Date(from=min, to=max, by="day"), each=1, times=length(LAcodes)))

#Map data onto skeleton
fulldata <- merge(skeleton, data[,-c(1,3)], by=c("code", "date"), all.x=TRUE, all.y=TRUE)

#Bring in LA names
temp <- data %>%
  group_by(code) %>%
  slice(1L)
fulldata <- merge(fulldata, temp[,c(1,2)], by="code")

#Fill in blank days
fulldata$cases <- ifelse(is.na(fulldata$cases), 0, fulldata$cases)

#Calculate cumulative sums so far
fulldata <- fulldata %>%
  arrange(code, date) %>%
  group_by(code) %>%
  mutate(cumul_cases=cumsum(cases))

#Get Welsh data
#Read in data
temp <- tempfile()
source <- "http://www2.nphs.wales.nhs.uk:8080/CommunitySurveillanceDocs.nsf/61c1e930f9121fd080256f2a004937ed/77fdb9a33544aee88025855100300cab/$FILE/Rapid%20COVID-19%20surveillance%20data.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
data.w <- read_excel(temp, sheet=2)[,c(1:4)]

colnames(data.w) <- c("name", "date", "cases", "cumul_cases")
data.w$date <- as.Date(data.w$date)

#Read in UTLA/LA lookup
temp <- tempfile()
source <- "https://opendata.arcgis.com/datasets/3e4f4af826d343349c13fb7f0aa2a307_0.csv"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
lookup <- read.csv(temp)
colnames(lookup) <- c("no", "LTcode", "LTname", "code", "name")

#Sort out Isles of Scilly
lookup$code <- ifelse(lookup$LTcode=="E06000053", "E06000052", as.character(lookup$code))

#Lookup codes for Welsh LAs
data.w <- merge(data.w, subset(lookup, substr(code,1,1)=="W"), all.x=TRUE, by="name")[,c(1:4,8)]
data.w <- data.w[,c(5,2,3,4,1)]

#Merge with English data
fulldata <- bind_rows(fulldata, subset(data.w, !is.na(code)))

#Merge in LTLAs - cloning UTLA data
fulldata <- merge(lookup[,-c(1,5)], fulldata, by="code", all.x=T, all.y=T)

#Get Scottish data
temp <- tempfile()
source <- "https://raw.githubusercontent.com/DataScienceScotland/COVID-19-Management-Information/master/COVID19%20-%20Daily%20Management%20Information%20-%20Scottish%20Health%20Boards%20-%20Cumulative%20cases.csv"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
data.s <- read.csv(temp)

data.s$Date <- as.Date(data.s$Date)

data_long.s <- gather(data.s, HB, cumul_cases, c(2:15))

#Treat supressed numbers as 0
data_long.s$cumul_cases <- as.numeric(ifelse(data_long.s$cumul_cases=="*", 0, data_long.s$cumul_cases))

#Calculate daily cases
data_long.s <- data_long.s %>%
  arrange(HB, Date) %>%
  group_by(HB) %>%
  mutate(cases=cumul_cases-lag(cumul_cases,1))

data_long.s$cases <- ifelse(is.na(data_long.s$cases), 0, data_long.s$cases)

#Bring in HB codes
data_long.s$code <- case_when(
  data_long.s$HB=="Ayrshire.and.Arran" ~ "S08000015",
  data_long.s$HB=="Borders" ~ "S08000016",
  data_long.s$HB=="Dumfries.and.Galloway" ~ "S08000017",
  data_long.s$HB=="Fife" ~ "S08000029",
  data_long.s$HB=="Forth.Valley" ~ "S08000019",
  data_long.s$HB=="Grampian" ~ "S08000020",
  data_long.s$HB=="Greater.Glasgow.and.Clyde" ~ "S08000031",
  data_long.s$HB=="Highland" ~ "S08000022",
  data_long.s$HB=="Lanarkshire" ~ "S08000032",
  data_long.s$HB=="Lothian" ~ "S08000024",
  data_long.s$HB=="Orkney" ~ "S08000025",
  data_long.s$HB=="Shetland" ~ "S08000026",
  data_long.s$HB=="Tayside" ~ "S08000030",
  data_long.s$HB=="Western.Isles" ~ "S08000028")

#Get Health board to LA lookup
temp <- tempfile()
source <- "http://statistics.gov.scot/downloads/file?id=5a9bf61e-7571-45e8-a307-7c1218d5f6b5%2FDatazone2011Lookup.csv"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
lookup.s <- read.csv(temp)
lookup.s <- distinct(lookup.s, Council, .keep_all=TRUE)[,c(6,7)]
colnames(lookup.s) <- c("LTcode", "code")

#Merge in Health Boards to Councils, cloning data within HBs
data_long.s <- merge(lookup.s, data_long.s, by="code")
colnames(data_long.s) <- c("code", "LTcode", "date", "name", "cumul_cases", "cases")
data_long.s <- data_long.s[,c(1,2,3,6,5,4)]

#Merge into E&W data
fulldata <- bind_rows(fulldata, data_long.s)

#Grab NI data from Tom White's excellent resource
temp <- tempfile()
source <- "https://raw.githubusercontent.com/tomwhite/covid-19-uk-data/master/data/covid-19-cases-uk.csv"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
data.ni <- subset(read.csv(temp), Country=="Northern Ireland")[,-c(2)]
colnames(data.ni) <- c("date", "code", "name", "cumul_cases")
data.ni$date <- as.Date(as.character(data.ni$date))

#Remove missing location rows
data.ni$code <- as.character(data.ni$code)
data.ni <- subset(data.ni, code!="")

#Set up skeleton dataframe for missing dates
NILAs <- unique(data.ni$code)
min <- min(data.ni$date)
max <- max(data.ni$date)

skeleton <- data.frame(code=rep(NILAs, each=(max-min+1)), 
                       date=rep(seq.Date(from=min, to=max, by="day"), each=1, times=length(NILAs)))

#Map data onto skeleton
fulldata.ni <- merge(skeleton, data.ni[,-c(3)], by=c("code", "date"), all.x=TRUE, all.y=TRUE)

#Interpolate missing dates
fulldata.ni <- fulldata.ni %>%
  arrange(code, date) %>%
  group_by(code) %>%
  mutate(cumul_cases=na.approx(cumul_cases), cases=cumul_cases-lag(cumul_cases,1))

fulldata.ni$cases <- ifelse(is.na(fulldata.ni$cases), 0, fulldata.ni$cases)
fulldata.ni$cases <- ifelse(fulldata.ni$cases<0, 0, fulldata.ni$cases)

#bring back names
fulldata.ni <- merge(fulldata.ni, data.ni[,c(2,3)], by="code")

#Merge into E, W & S data
fulldata <- bind_rows(fulldata, fulldata.ni)

#tidy up
fulldata$LTcode <- ifelse(is.na(fulldata$LTcode), fulldata$code, fulldata$LTcode)
fulldata$LTname <- ifelse(is.na(fulldata$LTname), fulldata$name, fulldata$LTname)

#Calculate rolling averages of case numbers
fulldata <- fulldata %>%
  arrange(LTcode, date) %>%
  group_by(LTcode) %>%
  mutate(casesroll_avg=roll_mean(cases, 5, align="right", fill=0))

#Bring in hexmap
#Read in hex boundaries (adapted from from https://olihawkins.com/2018/02/1 and ODI Leeds)
hex <- geojson_read("Data/UKLA.geojson", what="sp")

# Fortify into a data frame format to be shown with ggplot2
hexes <- tidy(hex, region="id")

#data <- left_join(hexes, subset(fulldata, date==as.Date("2020-04-14")),by=c("id"="LTcode"))
data <- left_join(hexes, fulldata, by=c("id"="LTcode"), all.y=TRUE)
data$date <- as.Date(data$date)

HexAnim <- ggplot()+
  geom_polygon(data=subset(data, date>as.Date("2020-03-06")), aes(x=long, y=lat, group=id, fill=casesroll_avg))+
  coord_map()+
  scale_fill_distiller(palette="Spectral", name="Daily confirmed\ncases (5-day\nrolling avg.)")+
  theme_classic()+
  theme(axis.line=element_blank(), axis.ticks=element_blank(), axis.text=element_blank(),
        axis.title=element_blank(),  plot.title=element_text(face="bold"))+
  transition_time(date)+
  labs(title="Visualising the spread of COVID-19 across the UK",
       subtitle="Rolling 5-day average number of new confirmed cases.\nDate: {frame_time}",
       caption="Data from PHE, PHW, ScotGov & DoHNI\nVisualisation by @VictimOfMaths")

animate(HexAnim, duration=18, fps=10, width=2000, height=3000, res=300, renderer=gifski_renderer("Outputs/HexAnim.gif"), 
        end_pause=60)
