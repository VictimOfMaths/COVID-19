rm(list=ls())

library(curl)
library(readxl)
library(tidyverse)
library(paletteer)
library(ragg)
library(extrafont)
library(patchwork)
library(scales)
library(ggtext)
library(RcppRoll)
library(keyring)
library(HMDHFDplus)
library(Hmisc)

#Define window as number of weeks across which to take rolling average
Window <- 10

#Define value to replace suppressed death counts with in the US data 
#(all weeks <9 are suppressed)
Suppressed <- 5

#Set common font for all plots
font <- "Lato"

theme_custom <- function() {
  theme_classic() %+replace%
    theme(plot.title.position="plot", plot.caption.position="plot",
          strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)),
          strip.clip="off",
          plot.title=element_text(face="bold", size=rel(1.5), hjust=0,
                                  margin=margin(0,0,5.5,0)),
          text=element_text(family="Lato"),
          plot.subtitle=element_text(colour="Grey40", hjust=0, vjust=1),
          plot.caption=element_text(colour="Grey40", hjust=1, vjust=1, size=rel(0.8)),
          axis.text=element_text(colour="Grey40"),
          axis.title=element_text(colour="Grey20"),
          legend.text=element_text(colour="Grey40"),
          legend.title=element_text(colour="Grey20"))
}

options(scipen=10000)

#Read in COVID deaths by age from ONS
#2022 & 2023
temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/datasets/weeklyprovisionalfiguresondeathsregisteredinenglandandwales/2023/publicationfileweek412023.xlsx"
rawfile <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

EWraw2223 <- read_excel(rawfile, sheet="5", range="A7:W100") %>% 
  select(-c(`All ages`, `Week number`)) %>% 
  gather(Age, Deaths, c(2:ncol(.)))

#2020 & 2021 
temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/datasets/weeklyprovisionalfiguresondeathsregisteredinenglandandwales/2021/publishedweek522021.xlsx"
rawfile <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

EWraw2021 <- read_excel(rawfile, sheet="Covid-19 - Weekly occurrences", range="B12:DC31", col_names = FALSE) %>% 
  set_names("Age", as.character(seq.Date(from=as.Date("2020-01-03"), to=as.Date("2021-12-31"),
                                         length.out=ncol(.)-1))) %>% 
  gather(`Week ending`, Deaths, c(2:ncol(.))) %>% 
  mutate(`Week ending`=as.Date(`Week ending`))

#Merge
EWraw <- bind_rows(EWraw2223, EWraw2021) %>% 
  #align dates with US data, selecting date in the middle of each reporting week
  #for convenience
  mutate(Date=`Week ending`+days(3)) %>% 
  select(-`Week ending`) %>% 
  group_by(Age) %>% 
  arrange(Date) %>% 
  mutate(DeathsRoll=roll_mean(Deaths, n=Window, align="center", fill=NA)) %>% 
  ungroup()

#Plot to sense check
ggplot(EWraw, aes(x=Date))+
  geom_point(aes(y=Deaths), shape=21)+
  geom_line(aes(y=DeathsRoll))+
  facet_wrap(~Age, scales="free_y")+
  theme_custom()

#Deaths in under 15s are very small, also need to align age groups with US data
#(also, I didn't quite align the age group names in the two ONS datasets, oops)
EWdata <- EWraw %>% dplyr::filter(!Age %in% c("<1", "01-04", "05-09", "1-4", "5-9", "10-14")) %>%
  mutate(Age=case_when(
    Age %in% c("15-19", "20-24") ~ "15-24",
    Age %in% c("25-29", "30-34") ~ "25-34",
    Age %in% c("35-39", "40-44") ~ "35-44",
    Age %in% c("45-49", "50-54") ~ "45-54",
    Age %in% c("55-59", "60-64") ~ "55-64",
    Age %in% c("65-69", "70-74") ~ "65-74",
    Age %in% c("75-79", "80-84") ~ "75-84",
    TRUE ~ "85+")) %>% 
  group_by(Date, Age) %>% 
  summarise(Deaths=sum(Deaths), .groups="drop") %>% 
  group_by(Age) %>% 
  arrange(Date) %>% 
  mutate(DeathsRoll=roll_mean(Deaths, n=Window, align="center", fill=NA)) %>% 
  ungroup()

#Read in data extracted from CDC WONDER database
USraw <- read.csv("CDC Data/CDCCOVIDWeeklyDataxAge.txt", sep="\t") %>% 
  select(Ten.Year.Age.Groups.Code, MMWR.Week.Code, Deaths) %>% 
  set_names("Age", "Week", "Deaths") %>% 
  dplyr::filter(Age!="") %>% 
  mutate(Year=as.numeric(substr(Week, 1, 4)),
         WeekNo=as.numeric(substr(Week, 6, 7))) %>% 
  #Almost no deaths allocated to week 99 (presumably an unknown value), so remove
  dplyr::filter(WeekNo!=99) %>% 
  #Convert to actual date format (using a date in the middle of the week for convenience
  #and to align with the E&W data)
  mutate(AddWeek=case_when(Year==2018 ~ 0, Year==2019 ~ 52, Year==2020 ~ 52+52,
                           Year==2021 ~ 52+52+53, Year==2022 ~ 52+52+53+52,
                           Year==2023 ~ 52+52+53+52+52),
    Date=as.Date("2018-01-01")+weeks(WeekNo+AddWeek),
    Deaths=if_else(Deaths=="Suppressed", Suppressed, as.numeric(Deaths))) %>% 
  group_by(Age) %>% 
  arrange(Date) %>% 
  mutate(DeathsRoll=roll_mean(Deaths, n=Window, align="center", fill=NA)) %>% 
  ungroup()

#Plot to sense check
ggplot(USraw %>% dplyr::filter(Date>as.Date("2020-01-01")), aes(x=Date))+
  geom_point(aes(y=Deaths), shape=21)+
  geom_line(aes(y=DeathsRoll))+
  facet_wrap(~Age, scales="free_y")+
  theme_custom()

#Visual inspection suggests all ages under 15 are mostly suppressed values, so remove
#these ages.
USdata <- USraw %>% dplyr::filter(!Age %in% c("1", "1-4", "5-14")) %>% 
  select(Date, Age, Deaths, DeathsRoll) %>% 
  dplyr::filter(Date>as.Date("2020-01-01"))

######################
#Bring in population data from HMD
#Note that you will need to register with mortality.org and set this 
#username and password up with {keyring} for this to work

#Start with England & Wales
ewpop <- readHMDweb(CNTRY="GBRTENW", "Population", key_list("mortality.org")[1,2], 
                    key_get("mortality.org", key_list("mortality.org")[1,2]), fixup=TRUE) %>% 
  mutate(Age=as.numeric(Age), Age=if_else(is.na(Age), 110, Age)) %>% 
  dplyr::filter(Year>=2020) 

ewpop <- bind_rows(ewpop %>% dplyr::filter(Year==2020) %>% 
                     select("Year", "Age", "Total2") %>% 
                     mutate(Year=2021) %>% 
                     set_names(c("Year", "Age", "Pop")),
                   ewpop %>% select(c("Year", "Age", "Total1")) %>% 
                     set_names(c("Year", "Age", "Pop"))) 

#This gives us the mid-year population estimates for 2020 and 2021 only.
#There's definitely a clever approach here, but for now, let's linearly interpolate/extrapolate
#to get estimated weekly populations across the time period
#(which will be close enough to make no difference, I imagine)

#Set up framework for inter/extrapolation
popframe <- ewpop %>% #dplyr::filter(Age==15) %>% 
  mutate(weeksince=if_else(Year==2020, 26, 78)) %>% 
  arrange(weeksince) 

#Define inter/extrapolation function based on {signal}'s interp1 function
interpolate <- function(x){
  interp1(x=popframe$weeksince[popframe$Age==x], 
          y=popframe$Pop[popframe$Age==x], 
          xi=c(1:length(unique(EWdata$Date))), method="linear", extrap=TRUE)
}

#Do inter/extrapolation
InterpolatedPop <- data.frame(Date=as.Date(unique(EWdata$Date))) 

for(i in c(0:110)){
  InterpolatedPop <- bind_cols(InterpolatedPop, interpolate(i))
}

InterpolatedPop <- InterpolatedPop %>% 
  set_names("Date", c(0:110)) %>% 
  gather(Age, Pop, c(2:ncol(.))) %>% 
  mutate(Age=as.numeric(Age))

#Visualise to sense check
ggplot(InterpolatedPop, aes(x=Date, y=Age, fill=Pop))+
  geom_tile()+
  theme_custom()

#Looks ok, so group up and merge into deaths data
EWfull <- EWdata %>% 
  merge(InterpolatedPop %>% 
          dplyr::filter(Age>=15) %>% 
          mutate(Age=case_when(
            Age<25 ~ "15-24", Age<35 ~ "25-34", Age<45 ~ "35-44", Age<55 ~ "45-54",
            Age<65 ~ "55-64", Age<75 ~ "65-74", Age<85 ~ "75-84", Age>=85 ~ "85+")) %>% 
          group_by(Date, Age) %>% 
          summarise(Pop=sum(Pop), .groups="drop")) %>% 
  group_by(Age) %>% 
  mutate(mx=Deaths*100000/Pop,
         mxroll=roll_mean(mx, n=Window, align="center", fill=NA)) %>% 
  ungroup()

#Move on to USA
USpop <- readHMDweb(CNTRY="USA", "Population", key_list("mortality.org")[1,2], 
                    key_get("mortality.org", key_list("mortality.org")[1,2]), fixup=TRUE) %>% 
  mutate(Age=as.numeric(Age), Age=if_else(is.na(Age), 110, Age)) %>% 
  dplyr::filter(Year>=2020) 

USpop <- bind_rows(USpop %>% dplyr::filter(Year==2021) %>% 
                     select("Year", "Age", "Total2") %>% 
                     mutate(Year=2022) %>% 
                     set_names(c("Year", "Age", "Pop")),
                   USpop %>% select(c("Year", "Age", "Total1")) %>% 
                     set_names(c("Year", "Age", "Pop"))) 

#Set up framework for inter/extrapolation
popframe2 <- USpop %>% 
  mutate(weeksince=case_when(Year==2020 ~ 26, 
                             Year==2021 ~ 78,
                             Year==2022 ~ 130)) %>% 
  arrange(weeksince) 

#Define inter/extrapolation function based on {signal}'s interp1 function
interpolate2 <- function(x){
  interp1(x=popframe2$weeksince[popframe2$Age==x], 
          y=popframe2$Pop[popframe2$Age==x], 
          xi=c(1:length(unique(EWdata$Date))), method="linear", extrap=TRUE)
}

#Do inter/extrapolation
InterpolatedPop2 <- data.frame(Date=as.Date(unique(USdata$Date))) 

for(i in c(0:110)){
  InterpolatedPop2 <- bind_cols(InterpolatedPop2, interpolate2(i))
}

InterpolatedPop2 <- InterpolatedPop2 %>% 
  set_names("Date", c(0:110)) %>% 
  gather(Age, Pop, c(2:ncol(.))) %>% 
  mutate(Age=as.numeric(Age))

#Visualise to sense check
ggplot(InterpolatedPop2, aes(x=Date, y=Age, fill=Pop))+
  geom_tile()+
  theme_custom()

#Looks ok, so group up and merge into deaths data
USfull <- USdata %>% 
  merge(InterpolatedPop2 %>% 
          dplyr::filter(Age>=15) %>% 
          mutate(Age=case_when(
            Age<25 ~ "15-24", Age<35 ~ "25-34", Age<45 ~ "35-44", Age<55 ~ "45-54",
            Age<65 ~ "55-64", Age<75 ~ "65-74", Age<85 ~ "75-84", Age>=85 ~ "85+")) %>% 
          group_by(Date, Age) %>% 
          summarise(Pop=sum(Pop), .groups="drop")) %>% 
  group_by(Age) %>% 
  mutate(mx=Deaths*100000/Pop,
         mxroll=roll_mean(mx, n=Window, align="center", fill=NA)) %>% 
  ungroup()

Fulldata <- EWfull %>% mutate(Country="England & Wales") %>% 
  bind_rows(USfull %>% mutate(Country="USA"))

agg_png("Outputs/EWUSACOVIDDeathsxAgeRates.png", units="in", width=9, height=7, res=500)
ggplot(Fulldata, aes(x=Date, y=mxroll, colour=Country))+
  geom_hline(yintercept=0, colour="grey30")+
  geom_line()+
  scale_y_continuous(limits=c(0,NA), name="Weekly deaths per 100,000")+
  scale_colour_manual(values=c("#C11432", "#009ADA"))+
  facet_wrap(~Age, scales="free_y")+
  theme_custom()+
  theme(axis.line.x=element_blank(), 
        panel.grid.major.y=element_line(colour="grey95"))+
  labs(title="Younger adults in the US had much higher COVID deaths rates than England & Wales",
       subtitle="Rolling 10-week average rates of death occurrences where COVID was listed as a contributory cause",
       caption="Data from CDC Wonder and ONS | Populations estimated from mortality.org data\nPlot by @VictimOfMaths")
dev.off()

Ratios <- Fulldata %>% 
  group_by(Date, Age) %>% 
  mutate(Ratio=mxroll[Country=="USA"]/mxroll[Country=="England & Wales"]) %>% 
  ungroup() %>% 
  dplyr::filter(!is.na(Ratio)) %>% 
  mutate(Date=as.Date(Date))

agg_png("Outputs/EWUSACOVIDDeathsxAgeRatio.png", units="in", width=8, height=6, res=500)
ggplot(Ratios %>% dplyr::filter(Age!="15-24"), aes(x=Date, y=Ratio, colour=Age))+
  geom_hline(yintercept=1, colour="grey30")+
  geom_line()+
  scale_x_date(name="")+
  scale_y_continuous(trans="log10", name="Ratio of COVID-19 death occurrences\nin the USA to England & Wales",
                     breaks=c(1/4, 1/2, 1, 2, 4, 8), 
                     labels=c("x 1/4", "x 1/2", "Equal", "x2", "x4", "x8"))+
  scale_colour_manual(values=c("#B83326FF", "#C8570DFF", "#EDB144FF", "#8CC8BCFF", "#7DA7EAFF", "#5773C0FF", "#1D4497FF"))+
  theme_custom()+
  theme(panel.grid.major.y=element_line(colour="grey95"))+
  labs(title="The pandemic hit young people much harder in the US than England& Wales",
       subtitle="Ratio of rolling 10-week average rates of death occurrences where COVID was listed as a contributory cause\nin the USA compared to England & Wales.",
       caption="Data from CDC Wonder and ONS | Populations estimated from mortality.org data\nPlot by @VictimOfMaths")+
  annotate("text", x=as.Date("2023-01-01"), y=0.3, colour="Grey30",
           family="Lato", label="More deaths in England & Wales")+
  annotate("text", x=as.Date("2023-01-01"), y=7, colour="Grey30",
           family="Lato", label="More deaths in the USA")

dev.off()


