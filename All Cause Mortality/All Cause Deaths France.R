 rm(list=ls())

library(tidyverse)
library(curl)
library(readxl)
library(lubridate)
library(forcats)
library(ggtext)
library(HMDHFDplus)
library(paletteer)
library(ggthemes)

#Read in historic French mortality data for 2010-18
#Source: https://www.insee.fr/fr/information/4190491
temp <- tempfile()
temp2 <- tempfile()
source <- "https://www.insee.fr/fr/statistiques/fichier/4190491/deces-2010-2018-csv.zip"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)
data10 <- read.csv(file.path(temp2, "deces-2010.csv"), sep=";")
data11 <- read.csv(file.path(temp2, "deces-2011.csv"), sep=";")
data12 <- read.csv(file.path(temp2, "deces-2012.csv"), sep=";")
data13 <- read.csv(file.path(temp2, "deces-2013.csv"), sep=";")
data14 <- read.csv(file.path(temp2, "deces-2014.csv"), sep=";")
data15 <- read.csv(file.path(temp2, "deces-2015.csv"), sep=";")
data16 <- read.csv(file.path(temp2, "deces-2016.csv"), sep=";")
data17 <- read.csv(file.path(temp2, "deces-2017.csv"), sep=";")
data <- bind_rows(data10, data11, data12, data13, data14, data15, data16, data17)[,c(2,3,7)]
colnames(data) <- c("sex", "dob", "dod")

#Some dates of Birth, particularly older ones are missing days and months. Allocate these randomly.
data$dob <- as.character(data$dob)
data$yob <- as.numeric(substr(data$dob, 1, 4))
data$dob <- as.Date(data$dob, format=c("%Y%m%d"))
data$temp <- as.Date(paste0(data$yob, "-01-01"))
data$temp2 <- data$temp+round(runif(nrow(data), min=-0.49, max=365.49))
data$dob <- if_else(is.na(data$dob) & data$yob!=0, data$temp2, data$dob)

data$dod <- as.character(data$dod)

#remove very small number of dates of death which are too short
data <- subset(data, nchar(data$dod, type="chars")==8)[,c(1:3)]

data$dod <- as.Date(data$dod, format=c("%Y%m%d"))
data$age <- floor(time_length(difftime(data$dod, data$dob), "years"))

#remove a few other weird cases
data <- subset(data, age>=-1 & age<=120)

#categorise age
data$ageband <- case_when(
  data$age<15 ~ "0-14",
  data$age<65 ~ "15-64",
  data$age<75 ~ "65-74",
  data$age<85 ~ "75-84",
  TRUE ~ "85+")

#Tidy up sex variable
data$sex <- if_else(data$sex==1, "Male", "Female")

#Bring in deaths data for 2020 from https://www.insee.fr/en/statistiques/4493808?sommaire=4493845 (updated on Fridays)
temp <- tempfile()
temp2 <- tempfile()
source <- "https://www.insee.fr/en/statistiques/fichier/4493808/2020-07-31_detail.zip"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)
data18 <- read.csv(file.path(temp2, "DC_2018_det.csv"), sep=";")
data19 <- read.csv(file.path(temp2, "DC_2019_det.csv"), sep=";")
data20 <- read.csv(file.path(temp2, "DC_2020_det.csv"), sep=";")

data1820 <- bind_rows(data18, data19, data20)

#Set up dates
data1820$MNAIS <- as.character(formatC(data1820$MNAIS, width=2, format="d", flag="0"))
data1820$JNAIS <- as.character(formatC(data1820$JNAIS, width=2, format="d", flag="0"))
data1820$dob <- as.Date(paste0(data1820$ANAIS, data1820$MNAIS, data1820$JNAIS), format=c("%Y%m%d"))

data1820$MDEC <- as.character(formatC(data1820$MDEC, width=2, format="d", flag="0"))
data1820$JDEC <- as.character(formatC(data1820$JDEC, width=2, format="d", flag="0"))
data1820$dod <- as.Date(paste0(data1820$ADEC, data1820$MDEC, data1820$JDEC), format=c("%Y%m%d"))

data1820$age <- floor(time_length(difftime(data1820$dod, data1820$dob), "years"))
data1820$sex <- if_else(data1820$SEXE=="M", "Male", "Female")

data1820 <- data1820[,c(12:15)]

#categorise age
data1820$ageband <- case_when(
  data1820$age<15 ~ "0-14",
  data1820$age<65 ~ "15-64",
  data1820$age<75 ~ "65-74",
  data1820$age<85 ~ "75-84",
  TRUE ~ "85+")

#Check latest date in deaths data aligns with the end of a week
maxdate <- max(data1820$dod)
maxweek <- week(maxdate)

#Merge all years
fulldata <- bind_rows(data, data1820)

fulldata$year <- year(fulldata$dod)
fulldata$week <- week(fulldata$dod+days(1))

#Aggregate to weekly data
aggdata <- fulldata %>%
  group_by(ageband, year, week, sex) %>%
  filter(year>=2010) %>%
  summarise(deaths=n())

#Combines sexes for saving later
data.FR <- aggdata %>%
  group_by(ageband, year, week) %>%
  summarise(deaths=sum(deaths))

#Save data
write.csv(data.FR, "Data/deaths_age_France.csv")

#Draw overall plot
data.full <- aggdata %>%
  group_by(year, week) %>%
  summarise(deaths=sum(deaths))

hist.full <- data.full %>%
  filter(year!=2020) %>%
  group_by(week) %>%
  summarise(mean_d=mean(deaths), max_d=max(deaths), min_d=min(deaths))

data.full <- merge(hist.full, subset(data.full, year==2020 & week<=maxweek), all.x=TRUE, all.y=TRUE)

#Calculate excess deaths in 2020 vs. historic mean
excess.full <- data.full %>%
  filter(!is.na(deaths)) %>%
  summarise(deaths=sum(deaths), mean=sum(mean_d))

excess.full$excess <- excess.full$deaths-excess.full$mean
excess.full$prop <- excess.full$excess/excess.full$mean

tiff("Outputs/ExcessDeathsFrance.tiff", units="in", width=8, height=6, res=300)
ggplot(subset(data.full, week<53))+
  geom_ribbon(aes(x=week, ymin=min_d, ymax=max_d), fill="Skyblue2")+
  geom_ribbon(aes(x=week, ymin=mean_d, ymax=deaths), fill="Red", alpha=0.2)+
  geom_line(aes(x=week, y=mean_d), colour="Grey50", linetype=2)+
  geom_line(aes(x=week, y=deaths), colour="Red")+
  scale_x_continuous(name="Week number")+
  scale_y_continuous("Weekly deaths recorded")+
  annotate(geom="text", x=19, y=15000, label=paste0("+", round(excess.full$excess, 0), 
                                                             " more deaths in 2020 than average (+", 
                                                             round(excess.full$prop*100, 0),"%)"), colour="Red", hjust=0)+
  annotate(geom="text", x=30, y=12000, label="Historic maximum", colour="Skyblue4")+
  annotate(geom="text", x=30, y=9200, label="Historic minimum", colour="Skyblue4")+
  annotate(geom="text", x=46, y=9200, label="Historic mean", colour="grey30")+
  geom_curve(aes(x=48, y=9400, xend=47, yend=11200), colour="grey30", curvature=0.15,
             arrow=arrow(length=unit(0.1, "cm"), type="closed"), lineend="round")+
  theme_classic()+
  theme(plot.subtitle =element_markdown())+
  labs(title="Deaths in France are sticking around 'usual' levels",
       subtitle="Weekly deaths in <span style='color:red;'>2020</span> compared to <span style='color:Skyblue4;'>the range in 2010-19</span>.",
       caption="Date from Insee | Plot by @VictimOfMaths")
dev.off()

#Calculate 2010-19 average, min and max
hist.data <- aggdata %>%
  filter(year!=2020) %>%
  group_by(ageband, sex, week) %>%
  summarise(mean_d=mean(deaths), max_d=max(deaths), min_d=min(deaths))

aggdata <- merge(hist.data, subset(aggdata, year==2020 & week<=maxweek), all.x=TRUE, all.y=TRUE)

#Calculate excess deaths in 2020 vs. historic mean
excess <- aggdata %>%
  group_by(ageband, sex) %>%
  filter(!is.na(deaths)) %>%
  summarise(deaths=sum(deaths), mean=sum(mean_d))

excess$excess <- excess$deaths-excess$mean
excess$prop <- excess$excess/excess$mean

ann_text <- data.frame(week=rep(20, times=10), 
                       position=c(600,300,1000,1700,1100,1400,1800,2000,4000,2500), 
                       sex=rep(c("Female", "Male"), times=5),
                       ageband=rep(c("0-14", "15-64", "65-74", "75-84", "85+"), each=2))

tiff("Outputs/ExcessDeathsFrancexAge.tiff", units="in", width=14, height=8, res=300)
ggplot(aggdata)+
  geom_ribbon(aes(x=week, ymin=min_d, ymax=max_d), fill="Skyblue2")+
  geom_ribbon(aes(x=week, ymin=mean_d, ymax=deaths), fill="Red", alpha=0.2)+
  geom_line(aes(x=week, y=mean_d), colour="Grey50", linetype=2)+
  geom_line(aes(x=week, y=deaths), colour="Red")+
  scale_x_continuous(name="Week number")+
  scale_y_continuous("Weekly deaths recorded")+
  facet_grid(sex~ageband, scales="free_y")+
  geom_text(data=ann_text, aes(x=week, y=position), label=c(
    paste0(round(excess[1,5],0)," excess deaths in 2020\nvs. 2010-19 mean (", round(excess[1,6]*100,0),"%)"),
    paste0(round(excess[2,5],0)," deaths (", round(excess[2,6]*100,0),"%)"),
    paste0(round(excess[3,5],0)," deaths (", round(excess[3,6]*100,0),"%)"),
    paste0(round(excess[4,5],0)," deaths (", round(excess[4,6]*100,0),"%)"),
    paste0("+",round(excess[5,5],0)," deaths (+", round(excess[5,6]*100,0),"%)"),
    paste0("+",round(excess[6,5],0)," deaths (+", round(excess[6,6]*100,0),"%)"),
    paste0(round(excess[7,5],0)," deaths (", round(excess[7,6]*100,0),"%)"),
    paste0(round(excess[8,5],0)," deaths (", round(excess[8,6]*100,0),"%)"),
    paste0("+",round(excess[9,5],0)," deaths (+", round(excess[9,6]*100,0),"%)"),
    paste0("+",round(excess[10,5],0)," deaths (+", round(excess[10,6]*100,0),"%)")),
    size=3.5, colour="Red", hjust=0)+
  theme_classic()+
  theme(strip.background=element_blank(), strip.text=element_text(size=rel(1), face="bold"), 
        plot.subtitle =element_markdown(), plot.title=element_markdown())+
  labs(title="All cause deaths in France are lower than 'usual' levels in many age groups",
       subtitle="Weekly deaths in <span style='color:red;'>2020</span> compared to <span style='color:Skyblue4;'>the range in 2010-19</span>.",
       caption="Date from Insee | Plot by @VictimOfMaths")
dev.off()

#Find latest year in 2020 data
temp <- fulldata %>%
  filter(year==2020) %>%
  summarise (maxweek2020=max(week))

maxweek20 <- temp[1,1]

#Collapse data for the pandemic period only
agedata <- fulldata %>%
  filter(year>=2010 & week>9 & week<=maxweek20 & age>=0 & age<100) %>%
  group_by(age, year, sex) %>%
  summarise(deaths=n())

#Import population size
#Bring in French population from HMD (need to register and put your details in here)
username <- "" 
password <- ""

FraPop <- readHMDweb(CNTRY="FRATNP", "Exposures_1x1", username, password)

FraPop <- subset(FraPop, Year>=2010)

#Replicate 2017 populations for 2018+
temp <- subset(FraPop, Year==2017)
temp2 <- temp
temp3 <- temp
temp$Year <- 2018
temp2$Year <- 2019
temp3$Year <- 2020

FraPop <- bind_rows(FraPop, temp, temp2, temp3)

FraPop_long <- gather(FraPop, sex, pop, c(3,4))

agedata <- merge(agedata, FraPop_long[,-c(3,4)], by.x=c("sex", "age", "year"), by.y=c("sex", "Age", "Year"))

agedata$mortrate <- agedata$deaths*100000/agedata$pop

#Look at 2010-19 data
olddata <- agedata %>%
  filter(year<2020) %>%
  group_by(age, sex) %>%
  summarise(mean_d=mean(deaths), min_d=min(deaths), max_d=max(deaths), mean_r=mean(mortrate),
            min_r=min(mortrate), max_r=max(mortrate))

newdata <- merge(olddata, subset(agedata, year==2020))

newdata$excess <- newdata$mortrate-newdata$mean_r
newdata$propexcess <- newdata$excess/newdata$mean_r

write.csv(newdata, "Data/FrenchDeathsByAge.csv")

newdata <- read.csv("Data/FrenchDeathsByAge.csv")

tiff("Outputs/AgeSpecificDeathsFr.tiff", units="in", width=12, height=7, res=300)
ggplot(newdata)+
  geom_ribbon(aes(x=age, ymin=log10(min_r), ymax=log10(max_r)), fill="Skyblue2")+
  geom_line(aes(x=age, y=log10(mean_r)), colour="Grey50", linetype=2)+
  geom_point(aes(x=age, y=log10(mortrate)), colour="red")+
  scale_x_continuous(name="Age", breaks=seq(0,100, by=10))+
  scale_y_continuous(name="Log mortality rate\n(deaths/100,000 | base 10)")+
  facet_wrap(~sex)+
  theme_classic()+
  theme(strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)),
        plot.subtitle=element_markdown())+
  labs(title="Age-specific mortality rates in France during the pandemic",
       subtitle=paste0("Deaths in weeks 9-", maxweek, " in <span style='color:red;'>2020</span> compared to <span style='color:Skyblue4;'>the range in 2010-19</span>."),
       caption="Data from Insee and Human Mortality Database | Plot by @VictimOfMaths")
dev.off()

tiff("Outputs/AgeSpecificDeathsPropFr.tiff", units="in", width=12, height=7, res=300)
ggplot(newdata)+
  geom_bar(stat="identity", aes(x=age, y=propexcess, fill=propexcess), show.legend=FALSE)+
  scale_fill_paletteer_c("ggthemes::Classic Red-White-Green", direction=-1, limit=c(-1,1)*max(abs(newdata$propexcess)))+
  scale_x_continuous(name="Age", breaks=seq(0,100, by=10))+
  scale_y_continuous(name="Proportional change in mortality rate in 2020 vs. 2010-19 average",
                     breaks=seq(-0.5, 1, by=0.5), labels=c("-50%", "0%", "+50%", "+100%"))+
  facet_wrap(~sex)+
  theme_classic()+
  theme(strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)),
        plot.subtitle=element_markdown())+
  labs(title="Age-specific variation in mortality rates in France during the pandemic",
       subtitle=paste0("Relative change in all-cause mortality rates in weeks 9-", maxweek, " of 2020 versus the average between 2010-19"),
       caption="Data from Insee and Human Mortality Database | Plot by @VictimOfMaths")
dev.off()
