rm(list=ls())

library(tidyverse)
library(curl)
library(paletteer)
library(geofacet)
library(ggtext)
library(ragg)

options(scipen=999)

#Get US state level COVID-19 admissions data from CDC/COVID-NET
#https://covidtracking.com/data/download
temp <- tempfile()
source <- "https://covidtracking.com/data/download/all-states-history.csv"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb") 

data <- read.csv(temp) %>% 
  select(date, state, deathIncrease, hospitalizedIncrease, hospitalizedCurrently) %>% 
  mutate(date=as.Date(date))

#Bring in populations from Johns Hopkins data
#https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data
temp <- tempfile()
source <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/UID_ISO_FIPS_LookUp_Table.csv"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb") 

pop <- read.csv(temp) %>% 
  filter(iso3=="USA" & FIPS %in% c(1:56)) %>% 
  select(Province_State, Population)

#Get state abbreviations
st_crosswalk <- tibble(Province_State = state.name) %>%
  bind_cols(tibble(state = state.abb)) %>% 
  bind_rows(tibble(Province_State = "District of Columbia", state = "DC"))

#Merge them in
pop <- merge(pop, st_crosswalk)

data <- merge(data, pop)

#Calculate rates
data <- data %>% 
  arrange(state, date) %>% 
  mutate(hosprate=hospitalizedIncrease*100000/Population,
         deathrate=deathIncrease*100000/Population,
         inhosprate=hospitalizedCurrently*100000/Population,
         week=if_else(year(date)==2020, week(date), week(date)+52))

#National version
natdata <- data %>% 
  group_by(date) %>% 
  summarise(hospitalizedIncrease=sum(hospitalizedIncrease),
            deathIncrease=sum(deathIncrease),
            hospitalizedCurrently=sum(hospitalizedCurrently),
            Population=sum(Population)) %>% 
  mutate(hosprate=hospitalizedIncrease*100000/(Population),
         deathrate=deathIncrease*100000/(Population),
         inhosprate=hospitalizedCurrently*100000/Population,
         week=if_else(year(date)==2020, week(date), week(date)+52))

#generate weekly time series for both
weekdata <- data %>% 
  group_by(state, week) %>% 
  summarise(hospitalizedIncrease=sum(hospitalizedIncrease),
            deathIncrease=sum(deathIncrease),
            hospitalizedCurrently=sum(hospitalizedCurrently),
            Population=unique(Population)) %>% 
  mutate(hosprate=hospitalizedIncrease*100000/(7*Population),
         deathrate=deathIncrease*100000/(7*Population),
         inhosprate=hospitalizedCurrently*100000/(7*Population),
         label=as.Date("2020-03-03")+days(week-10)*7, 
         label=format(label, "%d %b"))

weeknatdata <- natdata %>% 
  group_by(week) %>% 
  summarise(hospitalizedIncrease=sum(hospitalizedIncrease),
            deathIncrease=sum(deathIncrease),
            hospitalizedCurrently=sum(hospitalizedCurrently),
            Population=sum(Population)/7) %>% 
  mutate(hosprate=hospitalizedIncrease*100000/(7*Population),
         deathrate=deathIncrease*100000/(7*Population),
         inhosprate=hospitalizedCurrently*100000/(7*Population),
         label=as.Date("2020-03-03")+days(week-10)*7, 
         label=format(label, "%d %b"))

#Plot
agg_tiff("Outputs/COVIDCycleUSA.tiff", units="in", width=12, height=8, res=500)
ggplot()+
  geom_path(data=subset(natdata, week>=12), 
            aes(x=hosprate, y=deathrate), colour="Grey40", alpha=0.1,
            arrow = arrow(type = "closed", angle = 30, length = unit(0.05, "inches")))+
  geom_path(data=subset(weeknatdata, week>=12), 
            aes(x=hosprate, y=deathrate, colour=week), show.legend=FALSE,# colour="tomato", 
            arrow = arrow(type = "closed", angle = 30, length = unit(0.05, "inches")))+
  geom_text(data=subset(weeknatdata, week>=12), 
            aes(x=hosprate, y=deathrate, label=label), size=rel(2), 
            colour="Grey40", vjust=-0.4)+
  scale_x_continuous(trans="log10", name="Number of COVID-19 patients in hospital per 100,000 (log scale)")+
  scale_y_continuous(trans="log10", name="Daily COVID-19 deaths per 100,000 (log scale)")+
  scale_colour_paletteer_c("pals::ocean.amp", limits=c(0.75*min(data$week),NA))+
  theme_classic()+
  theme(plot.subtitle=element_markdown(), plot.title=element_text(face="bold", size=rel(2)))+
  labs(title="Things in the US are slowly getting better",
       subtitle="COVID-19 hospitalisations and deaths in the United States <span style='color:Grey60;'>by day</span> and <span style='color:tomato;'>the weekly average",
       caption="Inspired by @maartenzam | Data from The COVID Tracking Project (CC BY 4.0) | Plot by @VictimOfMaths")

dev.off()

tiff("Outputs/COVIDCycleUSStates.tiff", units="in", width=14, height=10, res=500)
ggplot(data)+
  #geom_path(data=subset(data, week>=12), 
  #          aes(x=inhosprate, y=deathrate), colour="Grey40", alpha=0.1,
  #          arrow = arrow(type = "closed", angle = 30, length = unit(0.05, "inches")))+
  geom_path(data=subset(weekdata, week>=12), 
            aes(x=inhosprate, y=deathrate, colour=week), show.legend=FALSE)+
  scale_x_continuous(trans="log10", name="Number of COVID-19 patients in hospital per 100,000 (log scale)")+
  scale_y_continuous(trans="log10", name="Daily COVID-19 deaths per 100,000 (log scale)")+
  scale_colour_paletteer_c("pals::ocean.amp")+
  facet_geo(~state)+
  theme_classic()+
  theme(strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)),
        plot.title=element_text(face="bold", size=rel(2)))+
  labs(title="Breaking the cycles",
       subtitle="COVID-19 hospitalisations and deaths in the United States. Darker colours reflect more recent data",
       caption="Inspired by @maartenzam | Data from The COVID Tracking Project (CC BY 4.0) | Plot by @VictimOfMaths")

dev.off()
