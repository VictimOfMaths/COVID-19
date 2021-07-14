rm(list=ls())

library(tidyverse)
library(curl)
library(arrow)
library(readxl)
library(RcppRoll)
library(paletteer)
library(lubridate)
library(geofacet)
library(scales)
library(extrafont)
library(ragg)

#Select start data for analysis
startdate <- as.Date("2021-05-01")

#Pull in deaths data
temp <- tempfile()
source1 <- "https://api.coronavirus.data.gov.uk/v2/data?areaType=ltla&metric=newDeaths28DaysByDeathDate&format=csv"
temp <- curl_download(url=source1, destfile=temp, quiet=FALSE, mode="wb")
deaths1 <- read.csv(temp)

source2 <- "https://api.coronavirus.data.gov.uk/v2/data?areaType=region&metric=newDeaths28DaysByDeathDate&format=csv"
temp <- curl_download(url=source2, destfile=temp, quiet=FALSE, mode="wb")
deaths2 <- read.csv(temp)

source3 <- "https://api.coronavirus.data.gov.uk/v2/data?areaType=nation&metric=newDeaths28DaysByDeathDate&format=csv"
temp <- curl_download(url=source3, destfile=temp, quiet=FALSE, mode="wb")
deaths3 <- read.csv(temp)

deaths <- bind_rows(deaths1, deaths2, deaths3) %>% 
  mutate(date=as.Date(date)) %>% 
  filter(date>startdate) %>%
  group_by(areaCode, areaName) %>% 
  rename(deaths=newDeaths28DaysByDeathDate) %>% 
  #calculate rolling average
  mutate(deathsroll=roll_mean(deaths, 7, align="center", fill=NA_real_)) %>% 
  ungroup()

#Get age-specific data
temp1 <- tempfile()
source1 <- "https://api.coronavirus.data.gov.uk/v2/data?areaType=ltla&metric=newCasesBySpecimenDateAgeDemographics&format=csv"
temp1 <- curl_download(url=source1, destfile=temp1, quiet=FALSE, mode="wb")

temp2 <- tempfile()
source2 <- "https://api.coronavirus.data.gov.uk/v2/data?areaType=region&metric=newCasesBySpecimenDateAgeDemographics&format=csv"
temp2 <- curl_download(url=source2, destfile=temp2, quiet=FALSE, mode="wb")

temp3 <- tempfile()
source3 <- "https://api.coronavirus.data.gov.uk/v2/data?areaType=nation&areaCode=E92000001&metric=newCasesBySpecimenDateAgeDemographics&format=csv"
temp3 <- curl_download(url=source3, destfile=temp3, quiet=FALSE, mode="wb")

data <- read_csv_arrow(temp1) %>% 
  bind_rows(read_csv_arrow(temp2), read_csv_arrow(temp3)) %>% 
  select(c(1:6)) %>% 
  filter(age!="unassigned") %>% 
  mutate(age=case_when(
    age=="00_04" ~ "0_4",
    age=="05_09" ~ "5-9",
    TRUE ~ age),
    age = age %>% str_replace("_", "-") %>%
      factor(levels=c("0-4", "5-9", "10-14", "15-19",
                      "20-24", "25-29", "30-34", "35-39", 
                      "40-44", "45-49", "50-54", "55-59", 
                      "60-64", "65-69", "70-74", "75-79", 
                      "80-84", "85-89", "90+"))) %>% 
  #Remove two bonus age categories that we don't need (0-59 and 60+)
  filter(!is.na(age)) 

n.areas=length(unique(data$areaCode))
n.ages=length(unique(data$age))
max.date.1=max(deaths$date[!is.na(deaths$deathsroll)], na.rm=TRUE)
max.date.2=max(data$date)
max.date=min(max.date.1, max.date.2)

#Create future dataframe
future <- data.frame(date=rep(seq.Date(from=as.Date(max.date.2+days(1)), 
                                   to=as.Date(max.date.2+days(71)), by="days"), 
                              times=n.areas*n.ages),
                     areaCode=rep(unique(data$areaCode), each=n.ages*71),
                     age=rep(unique(data$age), times=n.areas*71),
                     cases=0)

#merge in areaName and areaType
future <- data %>% 
  select(areaCode, areaName, areaType) %>%
  unique() %>% 
  merge(future, by="areaCode")

#Read in CFR data provided by Dan Howdon
#These estimates are not mine to share, sorry, contact Dan for more info.
#https://medicinehealth.leeds.ac.uk/medicine/staff/447/dr-dan-howdon
CFRdata <- read.csv("Data/cfrs_2021_07_07.csv") %>% 
  mutate(age=case_when(
    agegroup==0 ~ "0-4", agegroup==5 ~ "5-9", agegroup==10 ~ "10-14", agegroup==15 ~ "15-19",
    agegroup==20 ~ "20-24", agegroup==25 ~ "25-29", agegroup==30 ~ "30-34", 
    agegroup==35 ~ "35-39", agegroup==40 ~ "40-44", agegroup==45 ~ "45-49", 
    agegroup==50 ~ "50-54", agegroup==55 ~ "55-59", agegroup==60 ~ "60-64", 
    agegroup==65 ~ "65-69", agegroup==70 ~ "70-74", agegroup==75 ~ "75-79",
    agegroup==80 ~ "80-84", agegroup==85 ~ "85-89", TRUE ~ "90+"),
    date=as.Date(date, format="%d/%m/%Y"),
    age = age %>% str_replace("_", "-") %>%
      factor(levels=c("0-4", "5-9", "10-14", "15-19",
                      "20-24", "25-29", "30-34", "35-39", 
                      "40-44", "45-49", "50-54", "55-59", 
                      "60-64", "65-69", "70-74", "75-79", 
                      "80-84", "85-89", "90+")))

maxCFRdate <- max(CFRdata$date[!is.na(CFRdata$cfr_month)])

#Just extract the most recent monthly CFRs
CFRs <- CFRdata %>% 
  filter(date==maxCFRdate) %>% 
  mutate(CFR=cfr_month*100) %>% 
  select(age, CFR)

#Calculate expected deaths by age group based on case numbers and CFRs,
#assuming an infection to death distribution that is lognormal with location 2.71 and
#shape 0.56 from Wood 2020 https://arxiv.org/pdf/2005.02090.pdf
#As used by https://www.cebm.net/covid-19/the-declining-case-fatality-ratio-in-england/
lognorm <- dlnorm(1:70, meanlog=2.71, sdlog=0.56)

#Start calculations from 1st September, so need to lag back at least 70 days 
#(Over 99.2% of deaths are within this time frame)
pred.data <- data %>% 
  filter(date>startdate-days(75)) %>% 
  bind_rows(future) %>% 
  merge(CFRs) %>% 
  mutate(tot.deaths=cases*CFR/100) %>% 
  #Calculate 7-day rolling average of cases
  group_by(areaCode, areaName, areaType, age) %>% 
  arrange(date) %>% 
  #Distribute the deaths for each days cases based using the assumed distribution
  #(I bet there is a sexy vectorised approach to this that can be 
  #written in a line or two)
  mutate(exp.deaths=lag(tot.deaths,1)*lognorm[1]+
           lag(tot.deaths,2)*lognorm[2]+
           lag(tot.deaths,3)*lognorm[3]+
           lag(tot.deaths,4)*lognorm[4]+
           lag(tot.deaths,5)*lognorm[5]+
           lag(tot.deaths,6)*lognorm[6]+
           lag(tot.deaths,7)*lognorm[7]+
           lag(tot.deaths,8)*lognorm[8]+
           lag(tot.deaths,9)*lognorm[9]+
           lag(tot.deaths,10)*lognorm[10]+
           lag(tot.deaths,11)*lognorm[11]+
           lag(tot.deaths,12)*lognorm[12]+
           lag(tot.deaths,13)*lognorm[13]+
           lag(tot.deaths,14)*lognorm[14]+
           lag(tot.deaths,15)*lognorm[15]+
           lag(tot.deaths,16)*lognorm[16]+
           lag(tot.deaths,17)*lognorm[17]+
           lag(tot.deaths,18)*lognorm[18]+
           lag(tot.deaths,19)*lognorm[19]+
           lag(tot.deaths,20)*lognorm[20]+
           lag(tot.deaths,21)*lognorm[21]+
           lag(tot.deaths,22)*lognorm[22]+
           lag(tot.deaths,23)*lognorm[23]+
           lag(tot.deaths,24)*lognorm[24]+
           lag(tot.deaths,25)*lognorm[25]+
           lag(tot.deaths,26)*lognorm[26]+
           lag(tot.deaths,27)*lognorm[27]+
           lag(tot.deaths,28)*lognorm[28]+
           lag(tot.deaths,29)*lognorm[29]+
           lag(tot.deaths,30)*lognorm[30]+
           lag(tot.deaths,31)*lognorm[31]+
           lag(tot.deaths,32)*lognorm[32]+
           lag(tot.deaths,33)*lognorm[33]+
           lag(tot.deaths,34)*lognorm[34]+
           lag(tot.deaths,35)*lognorm[35]+
           lag(tot.deaths,36)*lognorm[36]+
           lag(tot.deaths,37)*lognorm[37]+
           lag(tot.deaths,38)*lognorm[38]+
           lag(tot.deaths,39)*lognorm[39]+
           lag(tot.deaths,40)*lognorm[40]+
           lag(tot.deaths,41)*lognorm[41]+
           lag(tot.deaths,42)*lognorm[42]+
           lag(tot.deaths,43)*lognorm[43]+
           lag(tot.deaths,44)*lognorm[44]+
           lag(tot.deaths,45)*lognorm[45]+
           lag(tot.deaths,46)*lognorm[46]+
           lag(tot.deaths,47)*lognorm[47]+
           lag(tot.deaths,48)*lognorm[48]+
           lag(tot.deaths,49)*lognorm[49]+
           lag(tot.deaths,50)*lognorm[50]+
           lag(tot.deaths,51)*lognorm[51]+
           lag(tot.deaths,52)*lognorm[52]+
           lag(tot.deaths,53)*lognorm[53]+
           lag(tot.deaths,54)*lognorm[54]+
           lag(tot.deaths,55)*lognorm[55]+
           lag(tot.deaths,56)*lognorm[56]+
           lag(tot.deaths,57)*lognorm[57]+
           lag(tot.deaths,58)*lognorm[58]+
           lag(tot.deaths,59)*lognorm[59]+
           lag(tot.deaths,60)*lognorm[60]+
           lag(tot.deaths,61)*lognorm[61]+
           lag(tot.deaths,62)*lognorm[62]+
           lag(tot.deaths,63)*lognorm[63]+
           lag(tot.deaths,64)*lognorm[64]+
           lag(tot.deaths,65)*lognorm[65]+
           lag(tot.deaths,66)*lognorm[66]+
           lag(tot.deaths,67)*lognorm[67]+
           lag(tot.deaths,68)*lognorm[68]+
           lag(tot.deaths,69)*lognorm[69]+
           lag(tot.deaths,70)*lognorm[70]+
           lag(tot.deaths,71)*(1-plnorm(70, meanlog=2.71, sdlog=0.56)))

#Compare cases to deaths to sense check  
pred.data %>% filter(areaName=="England" & age=="90+") %>% 
  ggplot()+
  geom_line(aes(x=date, y=cases, group=areaName), colour="Blue")+
  geom_line(aes(x=date, y=exp.deaths, group=areaName), colour="tomato")

#Calculate total deaths by age group that haven't yet happened
area.pred.deathsxage <- pred.data %>% 
  filter(date>max.date.2) %>% 
  group_by(areaName, areaType, areaCode, age) %>% 
  summarise(exp.deaths=sum(exp.deaths))

area.pred.deaths.total <- pred.data %>% 
  filter(date>max.date.2) %>% 
  group_by(areaName, areaType, areaCode) %>% 
  summarise(exp.deaths=sum(exp.deaths))

#################
#Compare CFR estimation period with subsequent observed data
ggplot()+
  #geom_rect(aes(xmin=as.Date("2021-05-04")-days(28), xmax=as.Date("2021-05-04"), ymin=0, ymax=500), 
  #          fill="Grey80")+
  geom_col(data=subset(pred.data, areaName=="England" & date>=as.Date("2021-05-04")-days(28)),
         aes(x=as.Date(date), y=exp.deaths), fill="Skyblue")+
  geom_line(data=subset(deaths, date>as.Date("2021-05-04")-days(28) & 
                          date<=as.Date(max.date.1)-days(3) & areaName=="England"), 
            aes(x=as.Date(date), y=deathsroll), colour="Red")+
  scale_fill_paletteer_d("pals::stepped", name="Age")+
  scale_x_date(name="")+
  scale_y_continuous(name="Expected daily deaths from COVID-19")+
  theme_classic()+
  #annotate("text", x=as.Date("2020-09-25"), y=450, label="CFRs estimated\nusing this data")+
  #annotate("text", x=as.Date("2020-11-25"), y=450, label="Deaths modelled\nfrom estimated CFRs")+
  #annotate("text", x=as.Date("2020-10-25"), y=320, label="Actual deaths")+
  #geom_curve(aes(x=as.Date("2020-11-26"), y=430, 
  #               xend=as.Date("2020-11-30"), yend=300), curvature=-0.30, 
  #           arrow=arrow(length=unit(0.1, "cm"), type="closed"))+
  #geom_curve(aes(x=as.Date("2020-10-25"), y=300, 
  #               xend=as.Date("2020-10-28"), yend=250), curvature=0.25, 
  #           arrow=arrow(length=unit(0.1, "cm"), type="closed"))+
  labs(title="Case Fatality Rates estimated in October have stood up pretty well",
       subtitle="Actual vs modelled deaths based on age-specific CFRs fitted to data from 05/09 - 16/10",
       caption="Data from PHE | CFRs from Daniel Howden | Time to death distribution from Wood 2020 | Analysis and plot by @VictimOfMaths")

#Plot age-specific forecasts for England
Englabel <- round(area.pred.deaths.total$exp.deaths[area.pred.deaths.total$areaName=="England"],0)

EngPlot <- ggplot()+
  geom_col(data=subset(pred.data, areaName=="England" & date>max.date-days(28)),
           aes(x=as.Date(date), y=exp.deaths, fill=age))+
  geom_line(data=subset(deaths, date>max.date-days(28) &
                          areaName=="England"), 
            aes(x=as.Date(date), y=deathsroll))+
  geom_vline(xintercept=as.Date(max.date.1), linetype=2)+
  scale_x_date(name="",
               breaks=pretty_breaks(n=interval(as.Date("2020-09-01"), as.Date(max.date.1+days(71)))%/% months(1)))+
  scale_y_continuous(name="Expected daily deaths from COVID-19")+
  scale_fill_paletteer_d("pals::stepped", name="Age")+
  annotate("text", x=as.Date("2021-06-26"), y=23, label="Actual deaths")+
  annotate("text", x=as.Date("2021-08-03"), y=20, label="Modelled deaths")+
  geom_curve(aes(x=as.Date("2021-06-28"), y=22, 
                 xend=as.Date("2021-07-01"), yend=19.5), curvature=0.15, 
             arrow=arrow(length=unit(0.1, "cm"), type="closed"))+
  geom_curve(aes(x=as.Date("2021-08-01"), y=19, 
                 xend=as.Date("2021-07-20"), yend=12), curvature=-0.25, 
             arrow=arrow(length=unit(0.1, "cm"), type="closed"))+
  theme_classic()+
  theme(plot.title=element_text(face="bold", size=rel(1.5)),
        plot.title.position="plot", text=element_text(family="Lato"),
        plot.caption.position = "plot")+
  labs(title=paste0("Even if COVID-19 disappeared today, we'd still expect ", Englabel, 
                    " more COVID-19 deaths over the coming months"),
       subtitle="Modelled COVID-19 deaths in England based on confirmed cases and the latest age-specific Case Fatality Rates",
       caption="Data from PHE | CFRs from Daniel Howden | Time to death distribution from Wood 2020 | Analysis and plot by @VictimOfMaths")

tiff("Outputs/COVIDDeathForecastEng.tiff", units="in", width=12, height=7, res=500)
EngPlot
dev.off()

png("Outputs/COVIDDeathForecastEng.png", units="in", width=12, height=7, res=500)
EngPlot
dev.off()

#Plot age distribution of forecasted deaths for England

tiff("Outputs/COVIDDeathForecastEngxAge.tiff", units="in", width=10, height=6, res=500)
area.pred.deathsxage %>% 
  filter(areaName=="England") %>% 
  ggplot()+
  geom_col(aes(x=exp.deaths, y=fct_rev(age), fill=age), show.legend=FALSE)+
  geom_text(aes(x=exp.deaths, y=fct_rev(age), label=round(exp.deaths,0)),
            hjust=-0.2)+
  scale_x_continuous(name="Expected future COVID-19 deaths")+
  scale_y_discrete(name="Age")+
  scale_fill_paletteer_d("pals::stepped")+
  theme_classic()+
  labs(title="The highest number of COVID-19 deaths is expected to be in 80-84 year-olds",
       subtitle="Modelled future COVID-19 deaths in England",
       caption="Data from PHE | CFRs from Daniel Howden | Time to death distribution from Wood 2020 | Analysis and plot by @VictimOfMaths")
dev.off()

#Plot age-specific forecasts for anywhere you like
area <- "Bolton"
Plotlabel <- unique(round(area.pred.deaths.total$exp.deaths[area.pred.deaths.total$areaName==area],0))

agg_tiff(paste0("Outputs/COVIDDeathForecast",area,".tiff"), units="in", width=10, height=8, res=500)
ggplot()+
  geom_col(data=subset(pred.data, areaName==area),
           aes(x=as.Date(date), y=exp.deaths, fill=age))+
  geom_line(data=subset(deaths, areaName==area & date<=as.Date(max.date.2)),
            aes(x=as.Date(date), y=deathsroll))+
  geom_vline(xintercept=as.Date(max.date.1), linetype=2)+
  scale_x_date(name="",
               breaks=pretty_breaks(n=interval(as.Date("2020-09-01"), 
                                               as.Date(max.date.1+days(71)))%/% months(1)),
               limits=c(as.Date("2021-05-01"), NA))+
  scale_y_continuous(name="Expected daily deaths from COVID-19", limits=c(0,0.75))+
  scale_fill_paletteer_d("pals::stepped", name="Age")+
  theme_classic()+
  theme(plot.title.position="plot", text=element_text(family="Lato"),
        plot.title=element_text(face="bold", size=rel(1.4)))+
  labs(title=paste0("Based on cases to date, we'd expect ", Plotlabel, 
                    " more COVID-19 deaths in ", area),
       subtitle="Modelled COVID-19 deaths based on confirmed cases and the latest age-specific Case Fatality Rates\nThe black line shows observed deaths to date",
       caption="Data from PHE | CFRs from Daniel Howden | Time to death distribution from Wood 2020 | Analysis and plot by @VictimOfMaths")

dev.off()

#Regional faceted plots
mygrid <- data.frame(name=c("North East", "North West", "Yorkshire and The Humber",
                            "West Midlands", "East Midlands", "East of England",
                            "South West", "London", "South East"),
                     row=c(1,2,2,3,3,3,4,4,4), col=c(2,1,2,1,2,3,1,2,3),
                     code=c(1:9))
pred.data <- arrange(pred.data, areaName)

reglabs <- data.frame(areaName=unique(pred.data$areaName[pred.data$areaType=="region"]),
                      total=round(area.pred.deaths.total$exp.deaths[area.pred.deaths.total$areaType=="region"],0))
reglabs$label <- if_else(reglabs$areaName=="North East", paste0(reglabs$total, " Expected\nfuture deaths"), 
                         as.character(reglabs$total))


tiff("Outputs/COVIDDeathForecastReg.tiff", units="in", width=12, height=7, res=500)
pred.data %>% 
  filter(areaType=="region") %>% 
  ggplot()+
  geom_col(aes(x=as.Date(date), y=exp.deaths, fill=age))+
  geom_line(data=subset(deaths, areaName %in% c("North East", "North West", "Yorkshire and The Humber",
                                            "West Midlands", "East Midlands", "East of England",
                                            "South West", "London", "South East") & date<=as.Date(max.date.2)),
            aes(x=as.Date(date), y=deathsroll, group=areaName))+
  geom_vline(xintercept=as.Date(max.date.1), linetype=2)+
  geom_text(data=reglabs, aes(x=as.Date("2021-05-22"), y=7, label=label))+
  scale_x_date(name="",
               breaks=pretty_breaks(n=interval(as.Date("2020-09-01"), as.Date(max.date.1+days(71)))%/% months(1)))+
  scale_y_continuous(name="Expected daily deaths from COVID-19")+
  scale_fill_paletteer_d("pals::stepped", name="Age")+
  facet_geo(~areaName, grid=mygrid)+
  theme_classic()+
  theme(plot.title=element_text(face="bold", size=rel(1.4)), strip.background=element_blank(),
        strip.text=element_text(face="bold", size=rel(1)),
        text=element_text(family="Lato"),
        axis.text=element_text(size=rel(0.7)))+
  labs(title="All regions in England can expect to see a rise in COVID deaths",
       subtitle="Modelled COVID-19 deaths in English regions based on confirmed cases and the latest age-specific Case Fatality Rates\nBlack lines show the rolling 7-day average of observed deaths for each region",
       caption="Data from PHE | CFRs from Daniel Howden | Time to death distribution from Wood 2020 | Analysis and plot by @VictimOfMaths")

dev.off()
