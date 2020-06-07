rm(list=ls())

library(tidyverse)
library(curl)
library(paletteer)
library(lubridate)
library(ggtext)

#Download Italian deaths data from Istat (link helpfully provided by @MazzucoStefano)
temp <- tempfile()
temp2 <- tempfile()
source <- "https://t.co/LPX5KaLivx?amp=1"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)

rawdata <- read.csv(file.path(temp2, "comuni_giornaliero-decessi.csv"))

#Tidy up data
rawdata$age <- case_when(
  rawdata$CL_ETA==0 ~ "0",
  rawdata$CL_ETA==1 ~ "1-4",
  rawdata$CL_ETA==2 ~ "5-9",
  rawdata$CL_ETA==3 ~ "10-14",
  rawdata$CL_ETA==4 ~ "15-19",
  rawdata$CL_ETA==5 ~ "20-24",
  rawdata$CL_ETA==6 ~ "25-29",
  rawdata$CL_ETA==7 ~ "30-34",
  rawdata$CL_ETA==8 ~ "35-39",
  rawdata$CL_ETA==9 ~ "40-44",
  rawdata$CL_ETA==10 ~ "45-49",
  rawdata$CL_ETA==11 ~ "50-54",
  rawdata$CL_ETA==12 ~ "55-59",
  rawdata$CL_ETA==13 ~ "60-64",
  rawdata$CL_ETA==14 ~ "65-69",
  rawdata$CL_ETA==15 ~ "70-74",
  rawdata$CL_ETA==16 ~ "75-79",
  rawdata$CL_ETA==17 ~ "80-84",
  rawdata$CL_ETA==18 ~ "85-89",
  rawdata$CL_ETA==19 ~ "90-94",
  rawdata$CL_ETA==20 ~ "95-99",
  rawdata$CL_ETA==21 ~ "100+")

rawdata$day <- if_else(nchar(rawdata$GE)==3, as.integer(substr(rawdata$GE, 2,3)), as.integer(substr(rawdata$GE, 3,4)))
rawdata$month <- if_else(nchar(rawdata$GE)==3, as.integer(substr(rawdata$GE, 1,1)), as.integer(substr(rawdata$GE, 1,2)))

#Set missing values to NA
rawdata$M_20 <- as.numeric(as.character(rawdata$M_20))
rawdata$F_20 <- as.numeric(as.character(rawdata$F_20))
rawdata$T_20 <- as.numeric(as.character(rawdata$T_20))

data <- pivot_longer(rawdata, c(10:27), names_to=c("sex", "year"), names_sep="_", values_to="deaths")  
data$year <- as.numeric(paste0("20", data$year))
data$date <- as.Date(paste0(data$year,"-", data$month, "-", data$day), format="%Y-%m-%d")
#Remove missing dates (weirdly the data has a load of 29th of Februarys in non-leap years)
data <- subset(data, !is.na(data$date))
data$week <- week(data$date)

data$sex <- case_when(
  data$sex=="M" ~ "Male",
  data$sex=="F" ~ "Female",
  TRUE ~ "Total"
)

data$sex <- factor(data$sex, levels=c("Male", "Female", "Total"))
data$age <- factor(data$age, levels=c("0", "1-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", 
                                      "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89",
                                      "90-94", "95-99", "100+"))

#Explore missingness in the deaths data
test <- data %>%
  + filter(is.na(deaths)) %>%
  + group_by(date) %>%
  + summarise(test=n())

#Looking at this, a clear jump in missingness after 30th April 2020, so censor the data here
cut <- week(as.Date("2020-04-30"))-1

data_prov <- data %>%
  filter(sex!="Total" & !(year==2020 & week>cut)) %>%
  group_by(age, sex, year, week, NOME_REGIONE, NOME_PROVINCIA) %>%
  summarise(deaths=sum(deaths, na.rm=TRUE))

data_reg <- data_prov %>%
  group_by(age, sex, year, week, NOME_REGIONE) %>%
  summarise(deaths=sum(deaths, na.rm=TRUE))

data_nat <- data_prov %>%
  group_by(age, sex, year, week) %>%
  summarise(deaths=sum(deaths, na.rm=TRUE))

#Save national level data
write.csv(data_nat, "Data/deaths_age_Italy.csv")

#National plots
tempdata <- data_nat %>%
  group_by(year, week) %>%
  summarise(deaths=sum(deaths))

#Calculate 2010-19 average, min and max
hist.data <- tempdata %>%
  filter(year!=2020) %>%
  group_by(week) %>%
  summarise(mean_d=mean(deaths), max_d=max(deaths), min_d=min(deaths))

tempdata <- merge(hist.data, subset(tempdata, year==2020), all.x=TRUE, all.y=TRUE)

#Calculate excess deaths in 2020 vs. historic mean
excess <- tempdata %>%
  filter(!is.na(deaths)) %>%
  summarise(deaths=sum(deaths), mean=sum(mean_d))

excess$excess <- excess$deaths-excess$mean
excess$prop <- excess$excess/excess$mean

#Overall plot
tiff("Outputs/ExcessDeathsItaly.tiff", units="in", width=10, height=8, res=300)
ggplot(subset(tempdata, week<53))+
  geom_ribbon(aes(x=week, ymin=min_d, ymax=max_d), fill="Skyblue2")+
  geom_ribbon(aes(x=week, ymin=mean_d, ymax=deaths), fill="Red", alpha=0.2)+
  geom_line(aes(x=week, y=mean_d), colour="Grey50", linetype=2)+
  geom_line(aes(x=week, y=deaths), colour="Red")+
  scale_x_continuous(name="Week number")+
  scale_y_continuous("Weekly deaths recorded")+
  theme_classic()+
  theme(plot.subtitle =element_markdown())+
  expand_limits(y=0)+
  labs(title="All-cause mortality in Italy during the pandemic",
       subtitle="Weekly deaths in <span style='color:red;'>2020</span> compared to <span style='color:Skyblue4;'>the range in 2010-19</span>.<br>Data up to 28th April 2020.",
       caption="Date from ISTAT | Plot by @VictimOfMaths")+
  annotate(geom="text", x=cut-2, y=18000, label=paste0("+", round(excess$excess, 0), 
                                                                " more deaths in 2020 than average (+", 
                                                                round(excess$prop*100, 0),"%)"), colour="Red", hjust=0)+
  annotate(geom="text", x=30, y=15000, label="Historic maximum", colour="Skyblue4")+
  annotate(geom="text", x=30, y=10000, label="Historic minimum", colour="Skyblue4")+
  annotate(geom="text", x=48, y=11000, label="Historic mean", colour="grey30")+
  geom_curve(aes(x=50, y=11300, xend=49, yend=12500), colour="grey30", curvature=0.15,
             arrow=arrow(length=unit(0.1, "cm"), type="closed"), lineend="round")
dev.off()

#Plot by sex
tempdata.sex <- data_nat %>%
  group_by(year, week, sex) %>%
  summarise(deaths=sum(deaths))

#Calculate 2010-19 average, min and max
hist.data.sex <- tempdata.sex %>%
  filter(year!=2020) %>%
  group_by(week, sex) %>%
  summarise(mean_d=mean(deaths), max_d=max(deaths), min_d=min(deaths))

tempdata.sex <- merge(hist.data.sex, subset(tempdata.sex, year==2020), all.x=TRUE, all.y=TRUE)

#Calculate excess deaths in 2020 vs. historic mean
excess.sex <- tempdata.sex %>%
  filter(!is.na(deaths)) %>%
  group_by(sex) %>%
  summarise(deaths=sum(deaths), mean=sum(mean_d))

excess.sex$excess <- excess.sex$deaths-excess.sex$mean
excess.sex$prop <- excess.sex$excess/excess.sex$mean

ann_text <- data.frame(week=rep(cut-0.5, times=2), sex=c("Male", "Female"), position=c(9000,9000))

tiff("Outputs/ExcessDeathsItalyxSex.tiff", units="in", width=10, height=8, res=300)
ggplot(subset(tempdata.sex, week<53))+
  geom_ribbon(aes(x=week, ymin=min_d, ymax=max_d), fill="Skyblue2")+
  geom_ribbon(aes(x=week, ymin=mean_d, ymax=deaths), fill="Red", alpha=0.2)+
  geom_line(aes(x=week, y=mean_d), colour="Grey50", linetype=2)+
  geom_line(aes(x=week, y=deaths), colour="Red")+
  scale_x_continuous(name="Week number")+
  scale_y_continuous("Weekly deaths recorded")+
  facet_wrap(~sex)+
  theme_classic()+
  theme(plot.subtitle =element_markdown(), strip.background=element_blank(),
        strip.text=element_text(face="bold", size=rel(1)))+
  expand_limits(y=0)+
  labs(title="Excess deaths are almost twice as high in Italian men compared to women",
       subtitle="Weekly deaths in <span style='color:red;'>2020</span> compared to <span style='color:Skyblue4;'>the range in 2010-19</span>.<br>Data up to 28th April 2020.",
       caption="Date from ISTAT | Plot by @VictimOfMaths")+
  geom_text(data=ann_text, aes(x=week, y=position), 
            label=c(paste0(round(excess.sex[1,4],0)," excess deaths in 2020\nvs. 2010-19 mean (+", round(excess.sex[1,5]*100,0),"%)"),
            paste0(round(excess.sex[2,4],0)," deaths (+", round(excess.sex[2,5]*100,0),"%)")), colour="Red", size=3.5, hjust=0)
 
dev.off()

#Plot by region
tempdata.reg <- data_reg %>%
  group_by(year, week, NOME_REGIONE) %>%
  summarise(deaths=sum(deaths))

#Calculate 2010-19 average, min and max
hist.data.reg <- tempdata.reg %>%
  filter(year!=2020) %>%
  group_by(week, NOME_REGIONE) %>%
  summarise(mean_d=mean(deaths), max_d=max(deaths), min_d=min(deaths))

tempdata.reg <- merge(hist.data.reg, subset(tempdata.reg, year==2020), all.x=TRUE, all.y=TRUE)

#Calculate excess deaths in 2020 vs. historic mean
excess.reg <- tempdata.reg %>%
  filter(!is.na(deaths)) %>%
  group_by(NOME_REGIONE) %>%
  summarise(deaths=sum(deaths), mean=sum(mean_d))

excess.reg$excess <- excess.reg$deaths-excess.reg$mean
excess.reg$prop <- excess.reg$excess/excess.reg$mean

labpos <- tempdata.reg$mean_d[tempdata.reg$week==cut]+1000

ann_text2 <- data.frame(week=rep(cut+0.5, times=20), NOME_REGIONE=levels(data$NOME_REGIONE), position=labpos)

tiff("Outputs/ExcessDeathsItalyxReg.tiff", units="in", width=12, height=9, res=300)
ggplot(subset(tempdata.reg, week<53))+
  geom_ribbon(aes(x=week, ymin=min_d, ymax=max_d), fill="Skyblue2")+
  geom_ribbon(aes(x=week, ymin=mean_d, ymax=deaths), fill="Red", alpha=0.2)+
  geom_line(aes(x=week, y=mean_d), colour="Grey50", linetype=2)+
  geom_line(aes(x=week, y=deaths), colour="Red")+
  scale_x_continuous(name="Week number")+
  scale_y_continuous("Weekly deaths recorded")+
  facet_wrap(~NOME_REGIONE)+
  theme_classic()+
  theme(plot.subtitle =element_markdown(), strip.background=element_blank(),
        strip.text=element_text(face="bold", size=rel(1)))+
  expand_limits(y=0)+
  labs(title="Excess deaths in Italy are very heavily concentrated in the Northern regions",
       subtitle="Weekly deaths in <span style='color:red;'>2020</span> compared to <span style='color:Skyblue4;'>the range in 2010-19</span>.<br>Data up to 28th April 2020.",
       caption="Date from ISTAT | Plot by @VictimOfMaths")+
  geom_text(data=ann_text2, aes(x=week, y=position), 
            label=c(paste0(round(excess.reg[1,4],0)," excess deaths in 2020\nvs. 2010-19 mean (", round(excess.reg[1,5]*100,0),"%)"),
                    paste0(round(excess.reg[2,4],0)," deaths (", round(excess.reg[2,5]*100,0),"%)"), 
                    paste0(round(excess.reg[3,4],0)," deaths (", round(excess.reg[3,5]*100,0),"%)"), 
                    paste0(round(excess.reg[4,4],0)," deaths (", round(excess.reg[4,5]*100,0),"%)"), 
                    paste0(round(excess.reg[5,4],0)," deaths (+", round(excess.reg[5,5]*100,0),"%)"), 
                    paste0(round(excess.reg[6,4],0)," deaths (", round(excess.reg[6,5]*100,0),"%)"), 
                    paste0(round(excess.reg[7,4],0)," deaths (", round(excess.reg[7,5]*100,0),"%)"), 
                    paste0(round(excess.reg[8,4],0)," deaths (+", round(excess.reg[8,5]*100,0),"%)"), 
                    paste0(round(excess.reg[9,4],0)," deaths (+", round(excess.reg[9,5]*100,0),"%)"), 
                    paste0(round(excess.reg[10,4],0)," deaths (+", round(excess.reg[10,5]*100,0),"%)"), 
                    paste0(round(excess.reg[11,4],0)," deaths (", round(excess.reg[11,5]*100,0),"%)"), 
                    paste0(round(excess.reg[12,4],0)," deaths (+", round(excess.reg[12,5]*100,0),"%)"), 
                    paste0(round(excess.reg[13,4],0)," deaths (", round(excess.reg[13,5]*100,0),"%)"), 
                    paste0(round(excess.reg[14,4],0)," deaths (", round(excess.reg[14,5]*100,0),"%)"), 
                    paste0(round(excess.reg[15,4],0)," deaths (", round(excess.reg[15,5]*100,0),"%)"), 
                    paste0(round(excess.reg[16,4],0)," deaths (", round(excess.reg[16,5]*100,0),"%)"), 
                    paste0(round(excess.reg[17,4],0)," deaths (+", round(excess.reg[17,5]*100,0),"%)"), 
                    paste0(round(excess.reg[18,4],0)," deaths (", round(excess.reg[18,5]*100,0),"%)"), 
                    paste0(round(excess.reg[19,4],0)," deaths (+", round(excess.reg[19,5]*100,0),"%)"), 
                    paste0(round(excess.reg[20,4],0)," deaths (+", round(excess.reg[20,5]*100,0),"%)")), 
            colour="Red", size=3, hjust=0)

dev.off()

#Extract Lazio only
tiff("Outputs/ExcessDeathsLazio.tiff", units="in", width=10, height=8, res=300)
ggplot(subset(tempdata.reg, week<53 & NOME_REGIONE=="Lazio"))+
  geom_ribbon(aes(x=week, ymin=min_d, ymax=max_d), fill="Skyblue2")+
  geom_ribbon(aes(x=week, ymin=mean_d, ymax=deaths), fill="Red", alpha=0.2)+
  geom_line(aes(x=week, y=mean_d), colour="Grey50", linetype=2)+
  geom_line(aes(x=week, y=deaths), colour="Red")+
  scale_x_continuous(name="Week number")+
  scale_y_continuous("Weekly deaths recorded")+
  theme_classic()+
  theme(plot.subtitle=element_markdown(), plot.title=element_markdown())+
  expand_limits(y=0)+
  labs(title="All-cause deaths in the Italian capital are <i style='color:black'>lower</i> than in previous years",
       subtitle="Weekly deaths in the Lazio region in <span style='color:red;'>2020</span> compared to <span style='color:Skyblue4;'>the range in 2010-19</span>.<br>Data up to 28th April 2020.",
       caption="Date from ISTAT | Plot by @VictimOfMaths")+
  annotate(geom="text", x=cut+0.5, y=850, label=paste0(abs(round(excess.reg[7,4], 0)), 
                                                       " fewer deaths in 2020 than average (", 
                                                       round(excess.reg[7,5]*100, 0),"%)"), colour="Red", hjust=0)

dev.off()
