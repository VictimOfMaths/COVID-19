rm(list=ls())

library(tidyverse)
library(ukcovid19) #remotes::install_github("publichealthengland/coronavirus-dashboard-api-R-sdk")
library(curl)
library(arrow)
library(readxl)
library(paletteer)
library(lubridate)
library(geofacet)

#Pull in national level deaths data
deaths <- get_data(filters="areaType=nation", structure=list(date="date",
                                                             name="areaName",
                                                             deaths="newDeaths28DaysByDeathDate"))

deaths <- deaths %>% 
  mutate(date=as.Date(date)) %>% 
  filter(date>as.Date("2020-03-19") & name=="England")

#Get age-specific data
temp <- tempfile()
source <- "https://coronavirus.data.gov.uk/downloads/demographic/cases/specimenDate_ageDemographic-stacked.csv"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

data <- read_csv_arrow(temp) %>% 
  select(c(1:6)) %>% 
  filter(age!="unassigned") %>% 
  rename(cases=newCasesBySpecimenDate)

#Tidy up
data <- data %>% 
  mutate(age = age %>% str_replace("_", "-") %>%
           factor(levels=c("0-4", "5-9", "10-14", "15-19",
                           "20-24", "25-29", "30-34", "35-39", 
                           "40-44", "45-49", "50-54", "55-59", 
                           "60-64", "65-69", "70-74", "75-79", 
                           "80-84", "85-89", "90+"))) %>% 
  #Remove two bonus age categories that we don't need (0-59 and 60+)
  filter(!is.na(age))

n.areas=length(unique(data$areaCode))
n.ages=length(unique(data$age))
max.date=max(data$date)

#Create future dataframe
future <- data.frame(date=rep(seq.Date(from=as.Date(max.date+days(1)), 
                                   to=as.Date(max.date+days(71)), by="days"), 
                              times=n.areas*n.ages),
                     areaCode=rep(unique(data$areaCode), each=n.ages*71),
                     age=rep(unique(data$age), times=n.areas*71),
                     cases=0)

#merge in areaName and areaType
future <- data %>% 
  select(areaCode, areaName, areaType) %>%
  unique() %>% 
  merge(future, by="areaCode")

#Calculate expected deaths by age group based on case numbers and CFRs,
#assuming an infection to death distribution that is lognormal with location 2.71 and
#shape 0.56 from Wood 2020 https://arxiv.org/pdf/2005.02090.pdf
#As used by https://www.cebm.net/covid-19/the-declining-case-fatality-ratio-in-england/
lognorm <- dlnorm(1:70, meanlog=2.71, sdlog=0.56)

#Start calculations from 1st September, so need to lag back at least 70 days 
#(Over 99.2% of deaths are within this time frame)
pred.data <- data %>% 
  filter(date>as.Date("2020-09-01")-days(75)) %>% 
  bind_rows(future) %>% 
  #Bring in latest CFR estimates from Daniel Howden
  #https://twitter.com/danc00ks0n/status/1323057097815240706
  mutate(CFR=case_when(
    age %in% c("0-4", "5-9", "10-14", "15-19") ~ 0,
    age=="20-24" ~ 0.004,
    age=="25-29" ~ 0.006,
    age=="30-34" ~ 0.029,
    age=="35-39" ~ 0.034,
    age=="40-44" ~ 0.115,
    age=="45-49" ~ 0.142,
    age=="50-54" ~ 0.256,
    age=="55-59" ~ 0.463,
    age=="60-64" ~ 1.289,
    age=="65-69" ~ 3.358,
    age=="70-74" ~ 7.736,
    age=="75-79" ~ 14.509,
    age=="80-84" ~ 21.438,
    age=="85-89" ~ 25.081,
    age=="90+" ~ 27.516),
    tot.deaths=cases*CFR/100) %>% 
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
  filter(date>max.date) %>% 
  group_by(areaName, areaType, areaCode, age) %>% 
  summarise(exp.deaths=sum(exp.deaths))

area.pred.deaths.total <- pred.data %>% 
  filter(date>max.date) %>% 
  group_by(areaName, areaType, areaCode) %>% 
  summarise(exp.deaths=sum(exp.deaths))

#Plot age-specific forecasts for England
Englabel <- round(area.pred.deaths.total$exp.deaths[area.pred.deaths.total$areaName=="England"],0)

tiff("Outputs/COVIDDeathForecastEng.tiff", units="in", width=10, height=8, res=500)
ggplot()+
  geom_col(data=subset(pred.data, areaName=="England"),
           aes(x=as.Date(date), y=exp.deaths, fill=age))+
  geom_line(data=subset(deaths, date>as.Date("2020-09-01") & 
                          date<=as.Date(max.date)), 
            aes(x=as.Date(date), y=deaths))+
  geom_vline(xintercept=as.Date(max.date), linetype=2)+
  scale_x_date(name="")+
  scale_y_continuous(name="Expected daily deaths from COVID-19")+
  scale_fill_paletteer_d("pals::stepped", name="Age")+
  annotate("text", x=as.Date("2020-10-01"), y=120, label="Actual deaths")+
  annotate("text", x=as.Date("2020-12-15"), y=140, label="Modelled deaths")+
  geom_curve(aes(x=as.Date("2020-10-01"), y=117, 
                 xend=as.Date("2020-10-10"), yend=102), curvature=0.15, 
             arrow=arrow(length=unit(0.1, "cm"), type="closed"))+
  geom_curve(aes(x=as.Date("2020-12-15"), y=143, 
                 xend=as.Date("2020-11-20"), yend=180), curvature=0.25, 
             arrow=arrow(length=unit(0.1, "cm"), type="closed"))+
  theme_classic()+
  labs(title=paste0("Even if COVID-19 disappeared today, we'd still expect ", Englabel, 
                    " more COVID-19 deaths over the coming months"),
       subtitle="Modelled COVID-19 deaths in England based on confirmed cases and the latest age-specific Case Fatality Rates",
       caption="Data from PHE | CFRs from Daniel Howden | Time to death distribution from Wood 2020 | Analysis and plot by @VictimOfMaths")

dev.off()

#Plot age distribution of forecasted deaths for England

tiff("Outputs/COVIDDeathForecastEngxAge.tiff", units="in", width=8, height=6, res=500)
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
area <- "Sheffield"
Plotlabel <- unique(round(area.pred.deaths.total$exp.deaths[area.pred.deaths.total$areaName==area],0))

tiff(paste0("Outputs/COVIDDeathForecast",area,".tiff"), units="in", width=10, height=8, res=500)
ggplot()+
  geom_col(data=subset(pred.data, areaName==area & areaType=="ltla"),
           aes(x=as.Date(date), y=exp.deaths, fill=age))+
  geom_vline(xintercept=as.Date(max.date), linetype=2)+
  scale_x_date(name="")+
  scale_y_continuous(name="Expected daily deaths from COVID-19")+
  scale_fill_paletteer_d("pals::stepped", name="Age")+
  theme_classic()+
  labs(title=paste0("Even if COVID-19 disappeared today, we'd still expect ", Plotlabel, 
                    " more COVID-19 deaths in ", area),
       subtitle="Modelled COVID-19 deaths based on confirmed cases and the latest age-specific Case Fatality Rates",
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


tiff("Outputs/COVIDDeathForecastReg.tiff", units="in", width=10, height=10, res=500)
pred.data %>% 
  filter(areaType=="region") %>% 
  ggplot()+
  geom_col(aes(x=as.Date(date), y=exp.deaths, fill=age))+
  geom_vline(xintercept=as.Date(max.date), linetype=2)+
  geom_text(data=reglabs, aes(x=as.Date("2020-12-20"), y=40, label=label))+
  scale_x_date(name="")+
  scale_y_continuous(name="Expected daily deaths from COVID-19")+
  scale_fill_paletteer_d("pals::stepped", name="Age")+
  facet_geo(~areaName, grid=mygrid)+
  theme_classic()+
  theme(plot.title=element_text(face="bold"), strip.background=element_blank(),
        strip.text=element_text(face="bold", size=rel(1)))+
  labs(title="The North is expected to bear the brunt of COVID-19 deaths in the next month",
       subtitle="Modelled COVID-19 deaths in English regions based on confirmed cases and the latest age-specific Case Fatality Rates",
       caption="Data from PHE | CFRs from Daniel Howden | Time to death distribution from Wood 2020 | Analysis and plot by @VictimOfMaths")

dev.off()
