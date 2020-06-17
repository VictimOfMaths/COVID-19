rm(list=ls())

library(tidyverse)
library(paletteer)
library(curl)
library(lubridate)
library(readxl)
library(ggtext)

#Controls
ScotDate <- "13th June"
Scot2020 <- "https://www.nrscotland.gov.uk/files//statistics/covid19/covid-deaths-data-week-24.xlsx"
ScotRange <- "Z" #incrememnt by one letter each week

#Read in 2015-2019 location data
temp <- tempfile()
source <- "https://www.nrscotland.gov.uk/files//statistics/covid19/weekly-deaths-by-location-2015-2019.csv"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
data.loc <- na.omit(read.csv(temp))

data.loc <- data.loc[-c(1, 26:31),]
colnames(data.loc) <- c("year", c(1:53))

data.loc$location <- rep(c("Care Home", "Home / Non-institution", "Hospital", "Other instutition"),
                         each=6)

data.loc_long <- gather(data.loc, week, deaths, c(2:54))
data.loc_long$week <- as.integer(data.loc_long$week)

#Read in 2015-19 health board data
temp <- tempfile()
source <- "https://www.nrscotland.gov.uk/files//statistics/covid19/weekly-HB-and-CA-2015-2019.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
data.HB <- read_excel(temp, sheet=1, range="A5:BC74", col_names=FALSE)

colnames(data.HB) <- c("HB", "year", c(1:53))
data.HB$year <- as.character(data.HB$year)

HBlist <- unique(na.omit(data.HB$HB))

data.HB$HB <- rep(HBlist, each=5)

data.HB_long <- gather(data.HB, week, deaths, c(3:55))
data.HB_long$week <- as.integer(data.HB_long$week)

#calculate averages
temp <- data.HB_long %>%
  group_by(week, HB) %>%
  summarise(deaths=mean(deaths))

temp$year="average"

data.HB_long <- bind_rows(data.HB_long, temp)

#Bring in 2020 data
temp <- tempfile()
temp <- curl_download(url=Scot2020, destfile=temp, quiet=FALSE, mode="wb")

#Tidy location data
data.loc.2020 <- data.frame(t(read_excel(temp, sheet=3, range=c(paste0("C89:", ScotRange, "92")), col_names=FALSE)))
date <- data.frame(date=format(seq.Date(from=as.Date("2019-12-30"), by="7 days", length.out=nrow(data.loc.2020)), "%d/%m/%y"))
data.loc.2020 <- cbind(date, data.loc.2020)
colnames(data.loc.2020) <- c("date", "Care Home", "Home / Non-institution", "Hospital", "Other instutition")
data.loc.2020$date <- as.Date(data.loc.2020$date, "%d/%m/%y")
data.loc.2020$week <- week(data.loc.2020$date+days(6))
data.loc.2020$year <- "2020"

#Merge with older years
data.loc.2020_long <- gather(data.loc.2020, location, deaths, c(2:5))
data.loc <- bind_rows(data.loc_long, data.loc.2020_long)

#merge 'Other institution' deaths into 'home/other'
data.loc$loc <- case_when(
  data.loc$location=="Care Home" ~ "Care Home",
  data.loc$location=="Hospital" ~ "Hospital",
  TRUE ~ "Home/Other"
)

data.loc$loc <- factor(data.loc$loc, levels=c("Hospital", "Care Home", "Home/Other"))

data.loc <- data.loc %>%
  group_by(year, loc, week) %>%
  summarise(deaths=sum(deaths))

#Tidy HB data
data.HB.2020 <- data.frame(t(read_excel(temp, sheet=3, range=c(paste0("C39:", ScotRange,"52")), col_names=FALSE)))
data.HB.2020 <- cbind(date, data.HB.2020)
colnames(data.HB.2020) <- c("date", HBlist)
data.HB.2020$date <- as.Date(data.HB.2020$date, "%d/%m/%y")
data.HB.2020$week <- week(data.HB.2020$date+days(6))
data.HB.2020$year <- "2020"

#Merge with older years
data.HB.2020_long <- gather(data.HB.2020, HB, deaths, c(2:15))
data.HB <- bind_rows(data.HB_long, data.HB.2020_long)

#Plot death location data
data.loc.old <- data.loc %>%
  filter(!year %in% c("2020", "average")) %>%
  group_by(week, loc) %>%
  summarise(max=max(deaths), min=min(deaths), mean=mean(deaths))

tiff("Outputs/NRSWeeklyDeathsxLocation.tiff", units="in", width=12, height=8, res=300)
ggplot()+
  geom_ribbon(data=data.loc.old, aes(x=week, ymin=min, ymax=max), fill="Skyblue2")+
  geom_line(data=subset(data.loc, year=="average"), aes(x=week, y=deaths), colour="Grey50", linetype=2)+
  geom_line(data=subset(data.loc, year=="2020"), aes(x=week, y=deaths), colour="Red")+
  facet_wrap(~loc)+
  theme_classic()+
  scale_x_continuous(name="Week number", breaks=c(0,10,20,30,40,50))+
  scale_y_continuous(name="Deaths registered")+
  expand_limits(y=0)+
  labs(title="Excess deaths in Scotland are at historically low levels in hospitals and high levels at home",
       subtitle=paste0("Weekly deaths in <span style='color:red;'>2020</span> compared to <span style='color:Skyblue4;'>the range in 2015-19</span>. Data up to ", ScotDate, "."),
       caption="Data from NRS | Plot by @VictimOfMaths")+
  theme(strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)),
        plot.subtitle =element_markdown())
dev.off()

#Calculate excess deaths vs. average
data.loc.new <- merge(subset(data.loc, year=="2020"), data.loc.old)
data.loc.new$excess <- data.loc.new$deaths-data.loc.new$mean

#plot excess deaths over time
tiff("Outputs/NRSWeeklyDeathsExcessxLocation.tiff", units="in", width=12, height=8, res=300)
ggplot(data.loc.new, aes(x=week, y=excess))+
  geom_segment(aes(x=0, xend=23, y=0, yend=0), colour="Grey40")+
  geom_line(aes(colour=loc))+
  theme_classic()+
  scale_x_continuous(name="Week commencing", breaks=c(1:23), 
                     labels=c(format(seq.Date(from=as.Date("2019-12-30"), by="7 days", 
                                              length.out=23), "%d/%m/%y")))+
  scale_y_continuous(name="Excess deaths compared to 2015-19 average")+
  scale_colour_paletteer_d("ggsci::planetexpress_futurama", name="Place of death")+
  labs(title="The fall in excess deaths is happening slowest in deaths at home",
       subtitle="Weekly deaths in 2020 compared to the average in 2015-19",
       caption="Data from NRS | Plot by @VictimOfMaths")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()

#Plot HB data
data.HB.old <- data.HB %>%
  filter(!year %in% c("2020", "average")) %>%
  group_by(week, HB) %>%
  summarise(max=max(deaths), min=min(deaths), mean=mean(deaths))

#Bring in 2020 data & average for easier plotting
data.HB.old <- merge(data.HB.old, subset(data.HB, year=="2020")[,c(1,3,4)], by=c("week", "HB"), all.x=TRUE)
colnames(data.HB.old) <- c("week", "HB", "max", "min", "mean", "2020")

maxweek <- max(data.HB.2020$week)

#Calculate excess deaths by Health Board
data.HB.old$excess <- data.HB.old$`2020`-data.HB.old$mean
excess <- data.HB.old %>%
  filter(week<=maxweek) %>%
  group_by(HB) %>%
  summarise(totalexcess=sum(excess), totalmean=sum(mean), propexcess=totalexcess/totalmean)

#Order HBs by total excess deaths
excess$HB <- fct_reorder(as.factor(excess$HB), -excess$totalexcess)
excess <- arrange(excess, HB)
data.HB.old$HB <- factor(data.HB.old$HB, levels=levels(excess$HB))

data.HB.old <- arrange(data.HB.old, data.HB.old$HB)

#Extract label positions for excess deaths
labpos <-  data.HB.old %>%
  filter(!is.na(`2020`) & week==maxweek) %>%
  group_by(HB) %>%
  mutate(pos=max(`2020`*1.2, max+60))

ann_text <- data.frame(weekno=rep(maxweek+0.5, times=14), deaths=labpos$pos, 
                       HB=levels(data.HB.old$HB))


tiff("Outputs/NRSWeeklyDeathsxHB.tiff", units="in", width=12, height=8, res=300)
ggplot(data.HB.old)+
  geom_ribbon(aes(x=week, ymin=min, ymax=max), fill="Skyblue2")+
  geom_ribbon(aes(x=week, ymin=mean, ymax=`2020`), fill="Red", alpha=0.2)+
  geom_line(aes(x=week, y=mean), colour="Grey50", linetype=2)+
  geom_line(aes(x=week, y=`2020`), colour="Red")+
  facet_wrap(~HB)+
  theme_classic()+
  scale_x_continuous(name="Week number", breaks=c(0,10,20,30,40,50))+
  scale_y_continuous(name="Deaths registered")+
  expand_limits(y=0)+
  labs(title="Excess mortality in Scotland is at 'normal' levels everywhere",
       subtitle=paste0("Weekly deaths in <span style='color:red;'>2020</span> compared to <span style='color:Skyblue4;'>the range in 2015-19</span>. Data up to ", ScotDate, "."),
       caption="Data from NRS | Plot by @VictimOfMaths")+
  geom_text(data=ann_text, aes(x=weekno, y=deaths), label=c(paste0(round(excess[1,2],0)," excess deaths in 2020\nvs. 2010-19 average (+",
                                                                   round(excess[1,4]*100, 0),"%)"),
                                                            paste0(round(excess[2,2],0)," excess deaths (+",
                                                                   round(excess[2,4]*100, 0),"%)"),
                                                            paste0(round(excess[3,2],0)," excess deaths (+",
                                                                   round(excess[3,4]*100, 0),"%)"),
                                                            paste0(round(excess[4,2],0)," excess deaths (+",
                                                                   round(excess[4,4]*100, 0),"%)"),
                                                            paste0(round(excess[5,2],0)," excess deaths (+",
                                                                   round(excess[5,4]*100, 0),"%)"),
                                                            paste0(round(excess[6,2],0)," excess deaths (+",
                                                                   round(excess[6,4]*100, 0),"%)"),
                                                            paste0(round(excess[7,2],0)," excess deaths (+",
                                                                   round(excess[7,4]*100, 0),"%)"),
                                                            paste0(round(excess[8,2],0)," excess deaths (+",
                                                                   round(excess[8,4]*100, 0),"%)"),
                                                            paste0(round(excess[9,2],0)," excess deaths (+",
                                                                   round(excess[9,4]*100, 0),"%)"),
                                                            paste0(round(excess[10,2],0)," excess deaths (+",
                                                                   round(excess[10,4]*100, 0),"%)"),
                                                            paste0(round(excess[11,2],0)," excess deaths (+",
                                                                   round(excess[11,4]*100, 0),"%)"),
                                                            paste0(round(excess[12,2],0)," excess deaths (+",
                                                                   round(excess[12,4]*100, 0),"%)"),
                                                            paste0(round(excess[13,2],0)," excess deaths (+",
                                                                   round(excess[13,4]*100, 0),"%)"),
                                                            paste0(round(excess[14,2],0)," excess deaths (+",
                                                                   round(excess[14,4]*100, 0),"%)")),
            size=3, colour=rep("red", times=14), hjust=0)+
  theme(strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)),
        plot.subtitle =element_markdown())
dev.off()

#Read in age data
temp <- tempfile()
temp <- curl_download(url=Scot2020, destfile=temp, quiet=FALSE, mode="wb")
data.age.2020 <- data.frame(t(read_excel(temp, sheet="Table 2 - All deaths", range=paste0("C15:",ScotRange, "21"), col_names=FALSE)))
date <- data.frame(date=format(seq.Date(from=as.Date("2019-12-30"), by="7 days", length.out=nrow(data.age.2020)), "%d/%m/%y"))
data.age.2020 <- cbind(date, data.age.2020)
colnames(data.age.2020) <- c("date", "Under 1 year", "01-14", "15-44", "45-64", "65-74", "75-84", "85+")
data.age.2020$date <- as.Date(data.age.2020$date, "%d/%m/%y")
data.age.2020$weekno <- week(data.age.2020$date)

data.age.2020_long <- gather(data.age.2020, age, deaths, c(2:8))
data.age.2020_long$year <- 2020
data.age.2020_long$deaths <- as.numeric(ifelse(data.age.2020_long$deaths==".", 0, data.age.2020_long$deaths))

#Recalculate dates to align with ONS data (which uses week to, not w/c)
data.age.2020_long$date <- data.age.2020_long$date+days(6)
data.age.2020_long$week <- week(data.age.2020_long$date)

data.age.2020_long$age <- as.factor(data.age.2020_long$age)
levels(data.age.2020_long$age)[which(levels(data.age.2020_long$age)=="Under 1 year")] <- "0-14"
levels(data.age.2020_long$age)[which(levels(data.age.2020_long$age)=="01-14")] <- "0-14"

data.age.2020_long <- data.age.2020_long %>%
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

data.age <- bind_rows(data1019.S, data.age.2020_long)

#Calculate 2010-19 average, min and max
hist.data.age <- data.age %>%
  filter(year!=2020) %>%
  group_by(age, week) %>%
  summarise(mean=mean(deaths), max=max(deaths), min=min(deaths))

data.age <- merge(hist.data.age, subset(data.age, year==2020), all.x=TRUE, all.y=TRUE)

#Calculate excess deaths by Health Board
data.age$excess <- data.age$deaths-data.age$mean
excess.age <- data.age %>%
  filter(!is.na(data.age$excess)) %>%
  group_by(age) %>%
  summarise(totalexcess=sum(excess), totalmean=sum(mean), propexcess=totalexcess/totalmean)

#Scotland only age plot
ann_text2 <- data.frame(week=rep(maxweek+0.5, times=6), pos=c(75, 100, 240, 260, 400, 450),
                        age=c("0-14", "15-44", "45-64", "65-74", "75-84", "85+"))

tiff("Outputs/NRSWeeklyDeathsxAge.tiff", units="in", width=12, height=8, res=300)
ggplot(data.age)+
  geom_ribbon(aes(x=week, ymin=min, ymax=max), fill="Skyblue2")+
  geom_ribbon(aes(x=week, ymin=mean, ymax=deaths), fill="Red", alpha=0.2)+
  geom_line(aes(x=week, y=mean), colour="Grey50", linetype=2)+
  geom_line(aes(x=week, y=deaths), colour="Red")+
  theme_classic()+
  facet_wrap(~age)+
  scale_x_continuous(name="Week number", breaks=c(0,10,20,30,40,50))+
  scale_y_continuous(name="Deaths registered")+
  expand_limits(y=0)+
  labs(title="Deaths at all ages in Scotland are now within historical ranges",
       subtitle=paste0("Weekly deaths in <span style='color:red;'>2020</span> compared to <span style='color:Skyblue4;'>the range in 2010-19</span>. Data up to ", ScotDate, "."),
       caption="Data from NRS | Plot by @VictimOfMaths")+
  theme(strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)),
        plot.subtitle =element_markdown())+
  geom_text(data=ann_text2, aes(x=week, y=pos), label=c(paste0(round(excess.age[1,2],0)," excess deaths in 2020\nvs. 2010-19 average (",
                                                               round(excess.age[1,4]*100, 0),"%)"), 
                                                        paste0("+",round(excess.age[2,2],0)," deaths (+",
                                                               round(excess.age[2,4]*100, 0),"%)"),
                                                        paste0("+",round(excess.age[3,2],0)," deaths (+",
                                                               round(excess.age[3,4]*100, 0),"%)"),
                                                        paste0("+",round(excess.age[4,2],0)," deaths (+",
                                                               round(excess.age[4,4]*100, 0),"%)"),
                                                        paste0("+",round(excess.age[5,2],0)," deaths (+",
                                                               round(excess.age[5,4]*100, 0),"%)"),
                                                        paste0("+",round(excess.age[6,2],0)," deaths (+",
                                                               round(excess.age[6,4]*100, 0),"%)")), 
            size=3, colour=rep("red", times=6), hjust=0)

dev.off()
