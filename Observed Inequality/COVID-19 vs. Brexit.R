rm(list=ls())

library(tidyverse)
library(curl)
library(sf)
library(paletteer)
library(readxl)

#Read in Brexit voting data by LA
temp <- tempfile()
source <- "http://researchbriefings.files.parliament.uk/documents/CBP-7639/EU-referendum-results-and-characteristics-data.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
Brexitdata <- read_excel(temp, sheet="Results", range="A1:U383")[,c(4,5,19,20)]
Brexitdata$Result <- if_else(Brexitdata$Pct_Leave>Brexitdata$Pct_Remain, "Leave", "Remain")

#Read in COVID19 data
#Deaths
temp <- tempfile()
source <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fbirthsdeathsandmarriages%2fdeaths%2fdatasets%2fdeathsinvolvingcovid19bylocalareaanddeprivation%2f1march2020to31may2020/referencetablesworkbook1.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
Deathsdata <- read_excel(temp, sheet="Table 2", range="A6:Z3488", col_names=FALSE)[,c(1,2,4,5,16,17)]
colnames(Deathsdata) <- c("cause", "sex", "Area_Code", "Area", "Deaths", "Mortrate")

Deathsdata <- Deathsdata %>% 
  filter(cause=="COVID-19" & sex=="Persons")

#Merge
data <- merge(Brexitdata, Deathsdata, by="Area_Code")

tiff("Outputs/COVIDvsBrexit.tiff", units="in", width=8, height=6, res=500)
ggplot(data, aes(x=Pct_Leave, y=as.numeric(Mortrate)))+
  geom_point(colour="tomato4", alpha=0.6)+
  geom_smooth(method="lm", formula=y~x)+
  scale_x_continuous(name="% leave vote", breaks=c(0.2,0.4,0.6), labels=c("20%", "40%", "60%"))+
  scale_y_continuous(name="Age-standardised COVID-19 deaths per 100,000")+
  theme_classic()+
  labs(title="Correlation between Brexit vote and COVID-19 mortality",
       subtitle="Based on Local Authorities in England & Wales",
       caption="Data from House of Commons Library & ONS | Plot by @VictimOfMaths")
dev.off()

#Bring in unadjusted case trajectories
temp <- tempfile()
source <- "https://coronavirus.data.gov.uk/downloads/csv/coronavirus-cases_latest.csv"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
Casesdata <- read.csv(temp)[,c(2,1,4,5)]
colnames(Casesdata) <- c("Area_Code", "Area", "Date", "NewCases")

Casesdata$Date <- as.Date(Casesdata$Date)

#Merge in Brexit vote
Casesdata <- merge(Casesdata, Brexitdata, by="Area_Code")

#Calculate trajectories
casetraj <- Casesdata %>%
  group_by(Date, Result) %>%
  summarise(cases=sum(NewCases))

tiff("Outputs/COVIDvsBrexitTrajectories.tiff", units="in", width=8, height=6, res=500)
ggplot(casetraj, aes(x=Date, y=cases, colour=Result))+
  geom_line()+
  theme_classic()+
  scale_colour_manual(values=c("#003399", "#FFCC00"))+
  scale_y_continuous(name="Daily confirmed COVID-19 cases")+
  labs(title="New COVID-19 cases by Local Authority Brexit vote",
       caption="Data from House of Commons Library & ONS | Plot by @VictimOfMaths")
dev.off()
