rm(list=ls())

library(tidyverse)
library(curl)
library(readxl)
library(lubridate)
library(paletteer)
library(ggtext)
library(broom)

#2005-2018 deaths registed from https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/adhocs/10929weeklydeathsregistrationsbyimdsexandagegroupenglandandwales2005to2018
temp <- tempfile()
source <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/adhocs/10929weeklydeathsregistrationsbyimdsexandagegroupenglandandwales2005to2018/wklydthsimdsexage2005to2018final.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

#Read in data
deaths2005 <- read_excel(temp, sheet="Table 1", range="A4:BC144")
deaths2006 <- read_excel(temp, sheet="Table 3", range="A4:BC144")
deaths2007 <- read_excel(temp, sheet="Table 5", range="A4:BC144")
deaths2008 <- read_excel(temp, sheet="Table 7", range="A4:BC144")
deaths2009 <- read_excel(temp, sheet="Table 9", range="A4:BD144")
colnames(deaths2009)[56] <- "Week 53"
deaths2010 <- read_excel(temp, sheet="Table 11", range="A4:BC144")
deaths2011 <- read_excel(temp, sheet="Table 13", range="A4:BC144")
deaths2012 <- read_excel(temp, sheet="Table 15", range="A4:BC144")
deaths2013 <- read_excel(temp, sheet="Table 17", range="A4:BC144")
deaths2014 <- read_excel(temp, sheet="Table 19", range="A4:BC144")
deaths2015 <- read_excel(temp, sheet="Table 21", range="A4:BD144")
deaths2016 <- read_excel(temp, sheet="Table 23", range="A4:BC144")
deaths2017 <- read_excel(temp, sheet="Table 25", range="A4:BC144")
deaths2018 <- read_excel(temp, sheet="Table 27", range="A4:BC144")

#Tidy up and merge
deaths2005$year <- 2005
deaths2006$year <- 2006
deaths2007$year <- 2007
deaths2008$year <- 2008
deaths2009$year <- 2009
deaths2010$year <- 2010
deaths2011$year <- 2011
deaths2012$year <- 2012
deaths2013$year <- 2013
deaths2014$year <- 2014
deaths2015$year <- 2015
deaths2016$year <- 2016
deaths2017$year <- 2017
deaths2018$year <- 2018

popwt <- data.frame(Age=c("Under 1 year", "01-14", "15-44", "45-64", "65-74", "75-84", "85+"), 
                    wt=c(0.01, 0.04+0.055+0.055, 0.055+0.06+0.06+0.065+0.07+0.07,
                             0.07+0.07+0.065+0.06, 0.055+0.05, 0.04+0.025,
                             0.015+0.008+0.002))

#Bring in populations
#2005-17 data from https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/adhocs/009316populationsbysexsingleyearofageandindexofmultipledeprivationimdengland2001to2017
temp <- tempfile()
source <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/adhocs/009316populationsbysexsingleyearofageandindexofmultipledeprivationimdengland2001to2017/populationbyageimdengland20012017.xls"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
engpop0117 <- read_excel(temp, sheet="Table 1", range="A14:E30953", col_names=FALSE)
colnames(engpop0117) <- c("year", "Sex", "Age", "IMD", "exposure")

engpop0117$Age <- case_when(
  engpop0117$Age=="0" ~ "Under 1 year",
  as.numeric(engpop0117$Age)<15 ~ "01-14",
  as.numeric(engpop0117$Age)<45 ~ "15-44",
  as.numeric(engpop0117$Age)<65 ~ "45-64",
  as.numeric(engpop0117$Age)<75 ~ "65-74",
  as.numeric(engpop0117$Age)<85 ~ "75-84",
  TRUE~ "85+")

#collapse ages
engpop0117 <- engpop0117 %>% 
  group_by(year, Sex, IMD, Age) %>% 
  summarise(exposure=sum(exposure)) %>% 
  ungroup()

engpop0117$Sex <- if_else(engpop0117$Sex==1, "Male", "Female")

#2018 data from https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/adhocs/10832deathsandpopulationsbyindexofmultipledeprivationimdenglandandwales2018registrations
temp <- tempfile()
source <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/adhocs/10832deathsandpopulationsbyindexofmultipledeprivationimdenglandandwales2018registrations/deathspopsimdengwal2018.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
engpop18.m <- read_excel(temp, sheet=4, range="B4:CO13", col_names=FALSE)
engpop18.f <- read_excel(temp, sheet=4, range="B15:CO24", col_names=FALSE)
engpop18.m$Sex <- "Male"
engpop18.f$Sex <- "Female"
engpop18 <- bind_rows(engpop18.m, engpop18.f)

engpop18 <- gather(engpop18, Age, exposure, c(2:92))
colnames(engpop18) <- c("IMD", "Sex", "Age", "exposure")

engpop18$Age <- as.numeric(substr(engpop18$Age,4,5))-2

engpop18$Age <- case_when(
  engpop18$Age==0 ~ "Under 1 year",
  engpop18$Age<15 ~ "01-14",
  engpop18$Age<45 ~ "15-44",
  engpop18$Age<65 ~ "45-64",
  engpop18$Age<75 ~ "65-74",
  engpop18$Age<85 ~ "75-84",
  TRUE~ "85+")

#Collapse ages and merge into 01-17 data
engpop <- engpop18 %>% 
  group_by(Sex, IMD, Age) %>% 
  summarise(exposure=sum(exposure)) %>% 
  ungroup() %>% 
  mutate(year=2018) %>% 
  bind_rows(engpop0117)

#Merge and collapse age groups
deaths0518 <- bind_rows(deaths2005, deaths2006, deaths2007, deaths2008, deaths2009, deaths2010,
                        deaths2011, deaths2012, deaths2013, deaths2014, deaths2015, deaths2016,
                        deaths2017, deaths2018) %>% 
  select(year, everything()) %>% 
  gather(week, deaths, `Week 1`:`Week 53`) %>%   
  merge(popwt) %>% 
  merge(engpop) %>% 
  mutate(mortrate=deaths*100000/exposure, wtrate=mortrate*wt) %>% 
  group_by(IMD, Sex, year, week) %>% 
  summarise(deaths=sum(deaths, na.rm=TRUE), ASMR=sum(wtrate, na.rm=TRUE)) %>% 
  ungroup() %>% 
  filter(!(week=="Week 53" & !year %in% c(2009, 2015)))


deaths0518$week <- as.numeric(substr(deaths0518$week,6,7))

#Collapse populations over age groups

engpop <- engpop %>% 
  group_by(Sex, IMD, year) %>% 
  summarise(exposure=sum(exposure)) %>% 
  ungroup()

engpop$time <- case_when(
  engpop$year==2001 ~ -26-2*52-53,
  engpop$year==2002 ~ -26-52-53,
  engpop$year==2003 ~ -26-53,
  engpop$year==2004 ~ -26,
  engpop$year==2005 ~ 26,
  engpop$year==2006 ~ 26+52,
  engpop$year==2007 ~ 26+52*2,
  engpop$year==2008 ~ 26+52*3,
  engpop$year==2009 ~ 26+52*3+53,
  engpop$year==2010 ~ 26+52*4+53,
  engpop$year==2011 ~ 26+52*5+53,
  engpop$year==2012 ~ 26+52*6+53,
  engpop$year==2013 ~ 26+52*7+53,
  engpop$year==2014 ~ 26+52*8+53,
  engpop$year==2015 ~ 26+52*8+2*53,
  engpop$year==2016 ~ 26+52*9+2*53,
  engpop$year==2017 ~ 26+52*10+2*53,
  engpop$year==2018 ~ 26+52*11+2*53
)

#Interpolate missing weeks between mid-year figures (take to be week 26)
#At the same time extrapolate out to mid-year 2020
exposures.splines <- function(data){
  interpolation <- spline(data$time, data$exposure, xout=seq(from=1, to=26+52*13+2*53+5, 1), method="natural")
  #Set up week structure
  weeks <- c(unlist(lapply(c(52,52,52,52,53,52,52,52,52,52,53,52,52,52,52,31), function(x){1:x})))
  years <- c(rep(2005:2020, c(52,52,52,52,53,52,52,52,52,52,53,52,52,52,52,31)))
  results <- data.frame(cbind(year=years, week=weeks, exposures=interpolation$y))
  
  return(results)
}

engpop.interpolated <- engpop %>% 
  arrange(year) %>% 
  group_by(IMD, Sex) %>% 
  group_modify(~ exposures.splines(.x)) %>% 
  mutate(index=seq(1:n()), date=as.Date("2005-01-03")+weeks(index-1)) %>% 
  ungroup()

#Check interpolation visually
ggplot()+
  geom_line(data=engpop.interpolated, aes(x=date, y=exposures))+
  geom_point(data=engpop, aes(x=as.Date(paste0(year, "-06-01")), y=exposure))+
  facet_grid(Sex~IMD)+
  theme_classic()

#Merge interpolated populations into 2005-2018 deaths data
data <- merge(engpop.interpolated, deaths0518, all.x=TRUE)[,-c(6)]

#Bring in 2020 deaths
temp <- tempfile()
source <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fbirthsdeathsandmarriages%2fdeaths%2fdatasets%2fdeathsinvolvingcovid19bylocalareaanddeprivation%2f1marchand31july2020/referencetables1.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
newdata <- read_excel(temp, sheet="Table 3", range="A6:AD95", col_names=FALSE)[,c(1,2,3,5,6,11,12,17,18,23,24,29,30)]
colnames(newdata) <- c("cause", "Sex", "IMD", "March" , "March_AS", "April", "April_AS",
                       "May", "May_AS", "June", "June_AS", "July", "July_AS")
newdata$IMD <- rep(c(1:10),9)
newdata <- subset(newdata, Sex!="Persons")

temp1 <- newdata %>% 
  gather(month, deaths, c(4,6,8,10,12)) %>% 
  select(cause, Sex, IMD, month, deaths) %>% 
  spread(cause, deaths)

temp2 <- newdata %>% 
  gather(month, ASMR, c(5,7,9,11,13)) %>% 
  select(cause, Sex, IMD, month, ASMR) %>% 
  spread(cause, ASMR)

temp2$month <- case_when(
  temp2$month=="March_AS" ~ "March",
  temp2$month=="April_AS" ~ "April",
  temp2$month=="May_AS" ~ "May",
  temp2$month=="June_AS" ~ "June",
  temp2$month=="July_AS" ~ "July"
)

colnames(temp1) <- c("Sex", "IMD", "month", "deaths", "COVID_deaths", "Other_deaths")
colnames(temp2) <- c("Sex", "IMD", "month", "ASMR", "COVID_ASMR", "Other_ASMR")


newdata <- merge(temp1, temp2)


newdata$Sex <- if_else(newdata$Sex=="Males", "Male", "Female")
newdata$year <- 2020

#Set up for merging
data$month <- case_when(
  data$year==2020 & data$week %in% c(10:13) ~ "March",
  data$year==2020 & data$week %in% c(14:18) ~ "April",
  data$year==2020 & data$week %in% c(19:22) ~ "May",
  data$year==2020 & data$week %in% c(23:27) ~ "June",
  data$year==2020 & data$week %in% c(28:31) ~ "July"
)

data <- merge(data, newdata, by=c("Sex", "IMD", "year", "month"), all.x=TRUE)

#Converte ASMRs to Age-Standardised deaths
data$ASdeaths.x <- data$ASMR.x*data$exposures/100000
data$ASdeaths.y <- data$ASMR.y*data$exposures/100000
data$ASCOVID_deaths <- data$COVID_ASMR*data$exposures/100000
data$ASOther_deaths <- data$Other_ASMR*data$exposures/100000

#Adjust 2020 deaths to weekly estimates - assume within-month distribution of deaths follows
#the same patterns as the distribution of deaths in the overall population in England

#Bring in overall deaths
temp <- tempfile()
source <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fbirthsdeathsandmarriages%2fdeaths%2fdatasets%2fweeklyprovisionalfiguresondeathsregisteredinenglandandwales%2f2020/publishedweek332020.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
alldeaths <- as.data.frame(t(read_excel(temp, sheet="Weekly figures 2020", range="L87:AI95", col_names=FALSE)))
alldeaths <- alldeaths %>% 
  mutate(totdeaths=V1+V2+V3+V4+V5+V6+V7+V8+V9, week=c(10:33), year=2020)

#calculate within-month proportions
alldeaths$month <- case_when(
  alldeaths$week %in% c(10:13) ~ "March",
  alldeaths$week %in% c(14:18) ~ "April",
  alldeaths$week %in% c(19:22) ~ "May",
  alldeaths$week %in% c(23:27) ~ "June",
  alldeaths$week %in% c(28:31) ~ "July"
)

alldeaths <- alldeaths %>% 
  group_by(month) %>% 
  mutate(total=sum(totdeaths), prop=totdeaths/total) %>% 
  ungroup()

data <- merge(data, alldeaths[,c(11,12,15)], all.x=TRUE)

#Apportion 2020 deaths within month
data$deaths.y <- data$deaths.y*data$prop
data$ASdeaths.y <- data$ASdeaths.y*data$prop
data$COVID_deaths <- data$COVID_deaths*data$prop
data$Other_deaths <- data$Other_deaths*data$prop
data$ASCOVID_deaths <- data$ASCOVID_deaths*data$prop
data$ASOther_deaths <- data$ASOther_deaths*data$prop

data$deaths <- coalesce(data$deaths.x, data$deaths.y)
data$ASdeaths <- coalesce(data$ASdeaths.x, data$ASdeaths.y)

data <- data[,-c(5,8,9,10,13,14,15,16,17,20)]

write.csv(data, "IMDDataForJM.csv")

#Plot mortality rates over time before GAM modelling
tiff("Outputs/AllCauseDeathsxIMD.tiff", units="in", width=12, height=8, res=500)
data %>% 
  mutate(mortrate=deaths*100000/exposures) %>% 
  ggplot(aes(x=date, y=mortrate, colour=as.factor(IMD)))+
  geom_line(size=0.3)+
  scale_colour_paletteer_d("dichromat::BluetoOrange_10", name="IMD decile", 
                           labels=c("1 (most deprived)","2","3","4","5","6","7","8","9","10 (least deprived)"))+
  scale_x_date(name="Date")+
  scale_y_continuous(name="Weekly deaths per 100,000")+
  facet_grid(Sex~.)+
  theme_classic()+
  theme(strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)))
dev.off()

tiff("Outputs/AllCauseDeathsxIMDASMR.tiff", units="in", width=12, height=8, res=500)
data %>% 
  mutate(ASMR=ASdeaths*100000/exposures) %>% 
  ggplot(aes(x=date, y=ASMR, colour=as.factor(IMD)))+
  geom_line(size=0.3)+
  scale_colour_paletteer_d("dichromat::BluetoOrange_10", name="IMD decile", 
                           labels=c("1 (most deprived)","2","3","4","5","6","7","8","9","10 (least deprived)"))+
  scale_x_date(name="Date")+
  scale_y_continuous(name="Weekly age-standardised deaths per 100,000")+
  facet_grid(Sex~.)+
  theme_classic()+
  theme(strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)))
dev.off()

#Replicate graphs
plotdata <- data %>% 
  filter(week %in% c(10:31) & year>=2014 & year!=2019) %>% 
  group_by(year, Sex, IMD) %>% 
  summarise(COVID_deaths=sum(COVID_deaths, na.rm=TRUE), Other_deaths=sum(Other_deaths, na.rm=TRUE),
            ASCOVID_deaths=sum(ASCOVID_deaths, na.rm=TRUE), ASOther_deaths=sum(ASOther_deaths, na.rm=TRUE),
            deaths=sum(deaths), ASdeaths=sum(ASdeaths)) %>% 
  gather(cause, deaths, c(4:9))

plotdata$Sex <- factor(plotdata$Sex, levels=c("Male", "Female"))
plotdata$cause <- factor(plotdata$cause, levels=c("Other_deaths", "COVID_deaths", "deaths", "ASOther_deaths",
                                                  "ASCOVID_deaths", "ASdeaths" ))

tiff("Outputs/COVIDIneqDeathHist.tiff", units="in", width=12, height=8, res=500)
ggplot()+
  geom_col(data=subset(plotdata, year==2020 & cause %in% c("COVID_deaths", "Other_deaths")),
           aes(x=as.factor(IMD), y=deaths, fill=cause))+
  geom_jitter(data=subset(plotdata, year!=2020 & cause=="deaths"), 
              aes(x=IMD, y=deaths), colour="midnightblue")+
  scale_fill_paletteer_d("palettetown::porygon", name="Cause", labels=c("Other", "COVID-19"))+
  scale_x_discrete(name="Deprivation decile", labels=c("1 (most deprived)", "2", "3", "4", "5",
                                                       "6", "7", "8", "9", "10 (least deprived)"))+
  scale_y_continuous(name="Deaths between 1st March & 31st July")+
  facet_wrap(~Sex)+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), strip.background=element_blank(),
        strip.text=element_text(face="bold", size=rel(1)), plot.title.position="plot",
        axis.text=element_text(colour="black"), plot.subtitle=element_markdown())+
  labs(title="Inequality in crude mortality rates in England & Wales in 2020 is similar to previous years",
       subtitle="<span style='color:grey40;'>Deaths in England & Wales between March 1st and 31st July by deprivation decile from <span style='color:midnightblue;'>all causes in 2014-18<span style='color:grey40;'> and from <span style='color:#f89088;'>COVID-19<span style='color:grey40;'> and <span style='color:#40a0d8;'>other causes<span style='color:grey40;'> in 2020",
       caption="Data from ONS | Plot by @VictimOfMaths")
dev.off()

tiff("Outputs/COVIDIneqRateHist.tiff", units="in", width=12, height=8, res=500)
ggplot()+
  geom_col(data=subset(plotdata, year==2020 & cause %in% c("ASCOVID_deaths", "ASOther_deaths")),
           aes(x=as.factor(IMD), y=deaths, fill=cause))+
  geom_jitter(data=subset(plotdata, year!=2020 & cause=="ASdeaths"), 
              aes(x=IMD, y=deaths), colour="midnightblue")+
  scale_fill_paletteer_d("palettetown::porygon", name="Cause", labels=c("Other", "COVID-19"))+
  scale_x_discrete(name="Deprivation decile", labels=c("1 (most deprived)", "2", "3", "4", "5",
                                                       "6", "7", "8", "9", "10 (least deprived)"))+
  scale_y_continuous(name="Age-standardised deaths between 1st March & 31st July")+
  facet_wrap(~Sex)+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), strip.background=element_blank(),
        strip.text=element_text(face="bold", size=rel(1)), plot.title.position="plot",
        axis.text=element_text(colour="black"), plot.subtitle=element_markdown())+
  labs(title="After age-standardising, deaths in 2020 are slightly more unequal than usual",
       subtitle="<span style='color:grey40;'>Age-standardised deaths in England & Wales between March 1st and 31st July by deprivation decile from <span style='color:midnightblue;'>all causes in 2014-18<span style='color:grey40;'> and from <span style='color:#f89088;'>COVID-19<span style='color:grey40;'> and <span style='color:#40a0d8;'>other causes<span style='color:grey40;'> in 2020",
       caption="Data from ONS | Plot by @VictimOfMaths")
dev.off()

###########################
#Generate labels
tempA <- (plotdata$deaths[plotdata$Sex=="Male" & plotdata$cause=="Other_deaths" & plotdata$IMD==1 & plotdata$year==2020]-
            plotdata$deaths[plotdata$Sex=="Male" & plotdata$cause=="Other_deaths" & plotdata$IMD==10 & plotdata$year==2020])/
  plotdata$deaths[plotdata$Sex=="Male" & plotdata$cause=="Other_deaths" & plotdata$IMD==10 & plotdata$year==2020]

labA <- paste0(round(tempA*100,0), "% more non-COVID-19 deaths\nin the most vs. least\ndeprived areas")

tempB <- (plotdata$deaths[plotdata$Sex=="Male" & plotdata$cause=="COVID_deaths" & plotdata$IMD==1 & plotdata$year==2020]-
            plotdata$deaths[plotdata$Sex=="Male" & plotdata$cause=="COVID_deaths" & plotdata$IMD==10 & plotdata$year==2020])/
  plotdata$deaths[plotdata$Sex=="Male" & plotdata$cause=="COVID_deaths" & plotdata$IMD==10 & plotdata$year==2020]

labB <- paste0(round(tempB*100,0), "% more COVID-19 deaths\nin the most vs. least\ndeprived areas")

tempC <- (plotdata$deaths[plotdata$Sex=="Female" & plotdata$cause=="Other_deaths" & plotdata$IMD==1 & plotdata$year==2020]-
            plotdata$deaths[plotdata$Sex=="Female" & plotdata$cause=="Other_deaths" & plotdata$IMD==10 & plotdata$year==2020])/
  plotdata$deaths[plotdata$Sex=="Female" & plotdata$cause=="Other_deaths" & plotdata$IMD==10 & plotdata$year==2020]

labC <- paste0(round(tempC*100,0), "% more non-COVID-19 deaths\nin the most vs. least\ndeprived areas")

tempD <- (plotdata$deaths[plotdata$Sex=="Female" & plotdata$cause=="COVID_deaths" & plotdata$IMD==1 & plotdata$year==2020]-
            plotdata$deaths[plotdata$Sex=="Female" & plotdata$cause=="COVID_deaths" & plotdata$IMD==10 & plotdata$year==2020])/
  plotdata$deaths[plotdata$Sex=="Female" & plotdata$cause=="COVID_deaths" & plotdata$IMD==10 & plotdata$year==2020]

labD <- paste0(round(tempD*100,0), "% more COVID-19 deaths\nin the most vs. least\ndeprived areas")

#Set up annotations
ann_text1 <- data.frame(IMD=rep(5, times=4), deaths=c(14000, -3000, 14000, -3000), Sex=c("Male", "Male", "Female", "Female"),
                        cause=rep("Other_deaths", times=4))

ann_arrows1a <- data.frame(x=rep(c(2.5,7.5), times=4), xend=rep(c(1,10), times=4), 
                           y=c(14000, -3000, 14000,-3000), yend=c(12000, 1500, 11000, 800),
                           Sex=rep(c("Male", "Female"), each=2), cause=rep("Other_deaths", times=4))

ann_arrows1b <- data.frame(x=rep(c(2.5,7.5), times=4), xend=rep(c(1,10), times=4), 
                           y=c(-3000, 14000, -3000, 14000), yend=c(1100, 9900, 900, 9500),
                           Sex=rep(c("Male", "Female"), each=2), cause=rep("Other_deaths", times=4))

tiff("Outputs/COVIDIneqDeath.tiff", units="in", width=12, height=8, res=500)
plotdata %>% 
  filter(cause %in% c("COVID_deaths", "Other_deaths") & year==2020) %>% 
  ggplot(aes(x=as.factor(IMD), y=deaths, fill=cause))+
  geom_col()+
  scale_fill_paletteer_d("palettetown::porygon", name="Cause", labels=c("Other", "COVID-19"))+
  scale_x_discrete(name="Deprivation decile", labels=c("1 (most deprived)",
                                                                         "2", "3", "4", "5",
                                                                         "6", "7", "8", "9",
                                                                         "10 (least deprived)"))+
  scale_y_continuous(name="Deaths between 1st March & 31st July 2020")+
  coord_cartesian(ylim=c(0,15000), clip="off")+
  facet_wrap(~Sex)+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), strip.background=element_blank(),
        strip.text=element_text(face="bold", size=rel(1)), plot.title.position="plot",
        axis.text=element_text(colour="black"), plot.subtitle=element_markdown())+
  geom_text(data=ann_text1, aes(x=IMD, y=deaths), 
            label=c(labA, labB, labC, labD), colour="Grey40")+
  geom_curve(data=ann_arrows1a, aes(x=x, xend=xend, y=y, yend=yend), colour="Grey40", curvature=0.21, 
            arrow=arrow(length=unit(0.2, "cm"), type="closed"))+
  geom_curve(data=ann_arrows1b, aes(x=x, xend=xend, y=y, yend=yend), colour="Grey40", curvature=-0.21, 
             arrow=arrow(length=unit(0.2, "cm"), type="closed"))+
  labs(title="Inequalities in all-cause deaths in England & Wales in 2020 are relatively small",
       subtitle="<span style='color:grey40;'>Total deaths in England & Wales in 2020 by deprivation decile from <span style='color:#f89088;'>COVID-19 <span style='color:grey40;'>and <span style='color:#40a0d8;'>all other causes.",
       caption="Data from ONS | Plot by @VictimOfMaths")
dev.off()

#######
#Rates#
#######

#Generate labels
tempE <- (plotdata$deaths[plotdata$Sex=="Male" & plotdata$cause=="ASOther_deaths" & plotdata$IMD==1 & plotdata$year==2020]-
            plotdata$deaths[plotdata$Sex=="Male" & plotdata$cause=="ASOther_deaths" & plotdata$IMD==10 & plotdata$year==2020])/
  plotdata$deaths[plotdata$Sex=="Male" & plotdata$cause=="ASOther_deaths" & plotdata$IMD==10 & plotdata$year==2020]

labE <- paste0(round(tempE*100,0), "% more non-COVID-19 deaths\nin the most vs. least\ndeprived areas")

tempF <- (plotdata$deaths[plotdata$Sex=="Male" & plotdata$cause=="ASCOVID_deaths" & plotdata$IMD==1 & plotdata$year==2020]-
            plotdata$deaths[plotdata$Sex=="Male" & plotdata$cause=="ASCOVID_deaths" & plotdata$IMD==10 & plotdata$year==2020])/
  plotdata$deaths[plotdata$Sex=="Male" & plotdata$cause=="ASCOVID_deaths" & plotdata$IMD==10 & plotdata$year==2020]

labF <- paste0(round(tempF*100,0), "% more COVID-19 deaths\nin the most vs. least\ndeprived areas")

tempG <- (plotdata$deaths[plotdata$Sex=="Female" & plotdata$cause=="ASOther_deaths" & plotdata$IMD==1 & plotdata$year==2020]-
            plotdata$deaths[plotdata$Sex=="Female" & plotdata$cause=="ASOther_deaths" & plotdata$IMD==10 & plotdata$year==2020])/
  plotdata$deaths[plotdata$Sex=="Female" & plotdata$cause=="ASOther_deaths" & plotdata$IMD==10 & plotdata$year==2020]

labG <- paste0(round(tempG*100,0), "% more non-COVID-19 deaths\nin the most vs. least\ndeprived areas")

tempH <- (plotdata$deaths[plotdata$Sex=="Female" & plotdata$cause=="ASCOVID_deaths" & plotdata$IMD==1 & plotdata$year==2020]-
            plotdata$deaths[plotdata$Sex=="Female" & plotdata$cause=="ASCOVID_deaths" & plotdata$IMD==10 & plotdata$year==2020])/
  plotdata$deaths[plotdata$Sex=="Female" & plotdata$cause=="ASCOVID_deaths" & plotdata$IMD==10 & plotdata$year==2020]

labH <- paste0(round(tempH*100,0), "% more COVID-19 deaths\nin the most vs. least\ndeprived areas")

#Set up annotations
ann_text2 <- data.frame(IMD=rep(5, times=4), deaths=c(23000, -4500, 19000, -4500), Sex=c("Male", "Male", "Female", "Female"),
                        cause=rep("ASOther_deaths", times=4))

ann_arrows2a <- data.frame(x=rep(c(2.5,7.5), times=4), xend=rep(c(1,10), times=4), 
                           y=c(23000, -4500, 19000,-4500), yend=c(20000, 1000, 13000, 700),
                           Sex=rep(c("Male", "Female"), each=2), cause=rep("ASOther_deaths", times=4))

ann_arrows2b <- data.frame(x=rep(c(2.5,7.5), times=4), xend=rep(c(1,10), times=4), 
                           y=c(-4500, 23000, -4500, 19000), yend=c(1500, 9000, 1100, 7500),
                           Sex=rep(c("Male", "Female"), each=2), cause=rep("ASOther_deaths", times=4))


COVIDIneqRate <- plotdata %>% 
  filter(cause %in% c("ASCOVID_deaths", "ASOther_deaths") & year==2020) %>% 
  ggplot(aes(x=as.factor(IMD), y=deaths, fill=cause))+
  geom_col()+
  scale_fill_paletteer_d("palettetown::porygon", name="Cause", labels=c("Other", "COVID-19"))+
  scale_x_discrete(name="Deprivation decile", labels=c("1 (most deprived)",
                                                       "2", "3", "4", "5",
                                                       "6", "7", "8", "9",
                                                       "10 (least deprived)"))+
  scale_y_continuous(name="Deaths between 1st March & 31st July 2020")+
  coord_cartesian(ylim=c(0,25000), clip="off")+
  facet_wrap(~Sex)+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), strip.background=element_blank(),
        strip.text=element_text(face="bold", size=rel(1)), plot.title.position="plot",
        axis.text=element_text(colour="black"), plot.subtitle=element_markdown())+
  geom_text(data=ann_text2, aes(x=IMD, y=deaths), 
            label=c(labE, labF, labG, labH), colour="Grey40")+
  geom_curve(data=ann_arrows2a, aes(x=x, xend=xend, y=y, yend=yend), colour="Grey40", curvature=0.21, 
             arrow=arrow(length=unit(0.2, "cm"), type="closed"))+
  geom_curve(data=ann_arrows2b, aes(x=x, xend=xend, y=y, yend=yend), colour="Grey40", curvature=-0.21, 
             arrow=arrow(length=unit(0.2, "cm"), type="closed"))+
  labs(title="After age-standardising there are significant inequalities in deaths in 2020",
       subtitle="<span style='color:grey40;'>Age-standardised deaths in England & Wales in 2020 by deprivation decile from <span style='color:#f89088;'>COVID-19 <span style='color:grey40;'>and <span style='color:#40a0d8;'>all other causes.",
       caption="Data from ONS | Plot by @VictimOfMaths")

tiff("Outputs/COVIDIneqRate.tiff", units="in", width=12, height=8, res=500)
COVIDIneqRate
dev.off()

png("Outputs/COVIDIneqRate.png", units="in", width=12, height=8, res=500)
COVIDIneqRate
dev.off()

#Calculate SII & RII
plotdata$cumproppop <- -(0.05+0.1*(plotdata$IMD-1))

SII <- plotdata %>% 
  group_by(year, Sex, cause) %>% 
  do(tidy(lm(deaths ~ cumproppop, data=.))) %>% 
  pivot_wider(id_cols=c("year", "Sex", "cause"), names_from=term, 
              values_from=c("estimate", "std.error"))

colnames(SII) <- c("year", "Sex", "cause", "intercept", "SII", "int.SE", "SII.SE")
SII$RII <- (SII$intercept+SII$SII)/SII$intercept

ggplot()+
  geom_line(data=subset(SII, cause=="deaths"), aes(x=year, y=SII), colour="midnightblue")+
  geom_point(data=subset(SII, cause %in% c("COVID_deaths", "Other_deaths") & year==2020), 
            aes(x=year, y=SII, colour=cause), show.legend=FALSE)+
  scale_x_continuous(name="Year")+
  scale_y_continuous(name="Slope Index of Inequality (SII)")+
  scale_colour_paletteer_d("palettetown::porygon")+
  coord_cartesian(ylim=c(0,NA))+
  facet_wrap(~Sex)+
  theme_classic()+
  theme(strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)),
        plot.subtitle=element_markdown())+
  labs(title="Absolute inequality in deaths has risen slightly in 2020",
       subtitle="<span style='color:grey40;'>Slope Index of Inequality for deaths in England & Wales from <span style='color:midnightblue;'>all causes<span style='color:grey40;'>, <span style='color:#f89088;'>COVID-19 <span style='color:grey40;'>and <span style='color:#40a0d8;'>all other causes",
       caption="Data from ONS | Plot by @VictimOfMaths")

tiff("Outputs/COVIDSIIEng.tiff", units="in", width=10, height=8, res=500)
ggplot()+
  geom_line(data=subset(SII, cause=="ASdeaths"), aes(x=year, y=SII), colour="midnightblue")+
  geom_point(data=subset(SII, cause %in% c("ASCOVID_deaths", "ASOther_deaths") & year==2020), 
             aes(x=year, y=SII, colour=cause), show.legend=FALSE)+
  scale_x_continuous(name="Year")+
  scale_y_continuous(name="Slope Index of Inequality (SII)")+
  scale_colour_paletteer_d("palettetown::porygon")+
  coord_cartesian(ylim=c(0,NA))+
  facet_wrap(~Sex)+
  theme_classic()+
  theme(strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)),
        plot.subtitle=element_markdown())+
  labs(title="Absolute inequality in deaths has risen sharply in 2020",
       subtitle="<span style='color:grey40;'>Slope Index of Inequality in age-standardised deaths in England & Wales from <span style='color:midnightblue;'>all causes<span style='color:grey40;'>, <span style='color:#f89088;'>COVID-19 <span style='color:grey40;'>and <span style='color:#40a0d8;'>all other causes",
       caption="Data from ONS | Plot by @VictimOfMaths")
dev.off()

tiff("Outputs/COVIDRIIEng.tiff", units="in", width=10, height=8, res=500)
ggplot()+
  geom_line(data=subset(SII, cause=="ASdeaths"), aes(x=year, y=RII), colour="midnightblue")+
  geom_point(data=subset(SII, cause %in% c("ASCOVID_deaths", "ASOther_deaths") & year==2020), 
             aes(x=year, y=RII, colour=cause), show.legend=FALSE)+
  scale_x_continuous(name="Year")+
  scale_y_continuous(name="Relative Index of Inequality (RII)")+
  scale_colour_paletteer_d("palettetown::porygon")+
  coord_cartesian(ylim=c(0,NA))+
  facet_wrap(~Sex)+
  theme_classic()+
  theme(strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)),
        plot.subtitle=element_markdown())+
  labs(title="Relative inequality in deaths is in line with recent years",
       subtitle="<span style='color:grey40;'>Relative Index of Inequality in age-standardised deaths in England & Wales from <span style='color:midnightblue;'>all causes<span style='color:grey40;'>, <span style='color:#f89088;'>COVID-19 <span style='color:grey40;'>and <span style='color:#40a0d8;'>all other causes",
       caption="Data from ONS | Plot by @VictimOfMaths")
dev.off()
