rm(list=ls())

library(tidyverse)
library(curl)
library(arrow)
library(readxl)
library(RcppRoll)
library(scales)
library(lubridate)
library(extrafont)
library(ragg)
library(paletteer)
library(ggrepel)

temp=tempfile()
url <- "https://api.coronavirus.data.gov.uk/v2/data?areaType=ltla&metric=newCasesBySpecimenDateAgeDemographics&format=csv"
temp <- temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

rawdata <- read_csv_arrow(temp) %>% 
  filter(!age %in% c("unassigned", "00_59", "60+")) %>% 
  mutate(date=as.Date(date),
         age=case_when(
           age=="00_04" ~ "0_4",
           age=="05_09" ~ "5_9",
           TRUE ~ age),
         age = age %>% str_replace("_", "-") %>%
           factor(levels=c("0-4", "5-9", "10-14", "15-19",
                           "20-24", "25-29", "30-34", "35-39", 
                           "40-44", "45-49", "50-54", "55-59", 
                           "60-64", "65-69", "70-74", "75-79", 
                           "80-84", "85-89", "90+"))) 

#Read in CFR data provided by Dan Howdon
CFRdata <- read_csv_arrow("Data/cfrs_2021_06_01.csv") %>% 
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

agg_tiff("Outputs/COVIDCFRs.tiff", units="in", width=12, height=7, res=800)
ggplot(CFRdata %>% filter(date>as.Date("2020-06-01")), aes(x=date, y=cfr_month, colour=age))+
  geom_line(show.legend=FALSE)+
  scale_x_date(labels=label_date(format="%b-%Y"), name="")+
  scale_y_continuous(labels=label_percent(accuracy=0.01), name="Case Fatality Rate",
                     limits=c(0,NA))+
  scale_colour_paletteer_d("pals::stepped")+
  facet_wrap(~age, scales="free_y", ncol=4)+
  theme_classic()+
  theme(strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)),
        text=element_text(family="Lato"), plot.title=element_text(face="bold", size=rel(1.8)),
        plot.title.position="plot")+
  labs(title="Case Fatality Rates for COVID have varied a lot over the past year",
       subtitle="Proportion of people testing positive for COVID who die (of any cause) within 28 days, by age group",
       caption="CFRs calculated by Daniel Howdon\nPlot by @VictimOfMaths")
dev.off()

maxCFRdate <- max(CFRdata$date[!is.na(CFRdata$cfr_month)])

#Just extract the most recent monthly CFRs
CFRs <- CFRdata %>% 
  filter(date==maxCFRdate) %>% 
  mutate(CFR=cfr_month*100) %>% 
  select(age, CFR)

#Here are the latest age-specific CFRs:
#CFRs <- tibble::tribble(
#  ~age, ~CFR,
#  "0-4",   0,
#  "5-9",   0,
#  "10-14", 0,
#  "15-19", 0.02164,
#  "20-24", 0,
#  "25-29", 0,
#  "30-34", 0,
#  "35-39", 0.0242269,
#  "40-44", 0.0289471,
#  "45-49", 0.2123436,
#  "50-54", 0.2208057,
#  "55-59", 0.5648288,
#  "60-64", 0.4692304,
#  "65-69", 3.5336073,
#  "70-74", 4.1047055,
#  "75-79", 6.2152125,
#  "80-84", 11.6088927,
#  "85-89", 16.7702883,
#  "90+",   22.6581156)

data <- merge(rawdata, CFRs) %>% 
  mutate(ex_deaths=cases*CFR/100) %>% 
  group_by(areaName, areaCode, date) %>% 
  summarise(ex_deaths=sum(ex_deaths), cases=sum(cases)) %>% 
  ungroup()

#Bring in populations
#Bring in LA populations
temp <- tempfile()
source <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fpopulationandmigration%2fpopulationestimates%2fdatasets%2fpopulationestimatesforukenglandandwalesscotlandandnorthernireland%2fmid20182019laboundaries/ukmidyearestimates20182019ladcodes.xls"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
LApop <- read_excel(temp, sheet="MYE2-All", range="A5:D435", col_names=TRUE)
colnames(LApop) <- c("areaCode", "name", "geography", "pop")

#Merge isles of Scilly in with Cornwall
LApop$code <- if_else(LApop$areaCode=="E06000053", "E06000052", LApop$code)
LApop$name <- if_else(LApop$name=="Isles of Scilly", "Cornwall", LApop$name)

#Merge City of London & Hackney
LApop$code <- if_else(LApop$areaCode=="E09000001", "E09000012", LApop$code)
LApop$name <- if_else(LApop$name=="City of London", "Hackney and City of London", LApop$name)
LApop$name <- if_else(LApop$name=="Hackney", "Hackney and City of London", LApop$name)

LApop <- LApop %>% 
  group_by(name, areaCode) %>% 
  summarise(pop=sum(pop)) %>% 
  ungroup()

#Bring in region
temp <- tempfile()
source <- "https://opendata.arcgis.com/datasets/0c3a9643cc7c4015bb80751aad1d2594_0.csv"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
LADtoRegion <- read.csv(temp)[,c(1,4)]
colnames(LADtoRegion) <- c("LTLA", "Region")

data <- data %>% 
  merge(LADtoRegion, all.x=TRUE, by.x="areaCode", by.y="LTLA") %>% 
  mutate(Region=case_when(
    areaCode %in% c("E07000244", "E07000245") ~ "East of England",
    areaCode %in% c("E06000058", "E06000059", "E07000246") ~ "South West",
    TRUE ~ Region))

plotdata <- data %>% 
  merge(LApop) %>% 
  mutate(caserate=cases*100000/pop,
         deathspercase=ex_deaths/cases) %>% 
  #take rolling averages
  group_by(areaCode, areaName) %>% 
  arrange(date) %>% 
  mutate(deathsroll=roll_mean(deathspercase, 7, align="center", fill=NA),
         casesroll=roll_mean(caserate, 7, align="center", fill=NA),
         flag=if_else(date %in% c(as.Date("2020-10-23"), as.Date("2020-12-08"),
                                  as.Date("2020-04-10"), as.Date("2021-01-06"),
                                  as.Date("2020-07-10"), max(date)-days(3)), 1, 0),
         ex_deathsroll=deathsroll*casesroll) %>% 
  ungroup()

Boltonpop <- unique(plotdata$pop[plotdata$areaName=="Bolton"])

agg_tiff("Outputs/COVIDCasevsMortPropBolton.tiff", units="in", width=8, height=8, res=800)
ggplot()+
  geom_path(data=plotdata %>% filter(areaName=="Bolton"), 
            aes(x=deathsroll, y=casesroll, alpha=date), show.legend=FALSE, colour="Red4")+
  geom_point(data=plotdata %>% filter(areaName=="Bolton" & flag==1), 
            aes(x=deathsroll, y=casesroll), colour="Red4")+
  geom_text(data=plotdata %>% filter(areaName=="Bolton" & flag==1), 
             aes(x=deathsroll, y=casesroll, label=date %>% format("%d-%b-%y")), colour="Grey30",
            hjust=-0.1, size=3)+
  geom_hline(yintercept=0)+
  geom_vline(xintercept=0)+
  scale_x_continuous(name="Proportion of cases expected to lead to death",
                     labels=label_percent(accuracy=1), 
                     limits=c(0,max(plotdata$deathsroll[plotdata$areaName=="Bolton"], 
                                    na.rm=TRUE)*1.05))+
  scale_y_continuous(name="Daily cases per 100,000")+
  theme_classic()+
  theme(axis.line=element_blank(), text=element_text(family="Lato"), 
        plot.title=element_text(face="bold", size=rel(1.4)), plot.title.position = "plot")+
  labs(title="Case rates in Bolton are high, but the younger age profile means we should\nexpect far fewer deaths than in previous waves",
       subtitle="Rolling 7-day average daily case rates compared to the expected proportion of cases which lead to a death\nwithin 28 days, based on age-specific Case Fatality Rates.",
       caption="Case data from coronavirus.data.gov.uk\nCFRs estimated by Daniel Howden\nPlot by @VictimOfMaths")

dev.off()

#version with countours
agg_tiff("Outputs/COVIDCasevsMortPropBoltonIso.tiff", units="in", width=8, height=8, res=800)
ggplot()+
  geom_line(data=plotdata, aes(x=deathsroll, y=0.25*100000/(Boltonpop*deathsroll)), colour="SkyBlue")+
  geom_line(data=plotdata, aes(x=deathsroll, y=1*100000/(Boltonpop*deathsroll)), colour="SkyBlue")+
  geom_line(data=plotdata, aes(x=deathsroll, y=2.5*100000/(Boltonpop*deathsroll)), colour="SkyBlue")+
  geom_path(data=plotdata %>% filter(areaName=="Bolton"), 
            aes(x=deathsroll, y=casesroll, colour=date), show.legend=FALSE)+
  geom_point(data=plotdata %>% filter(areaName=="Bolton" & flag==1), 
             aes(x=deathsroll, y=casesroll), colour="Red4")+
  geom_text(data=plotdata %>% filter(areaName=="Bolton" & flag==1), 
            aes(x=deathsroll, y=casesroll, label=date %>% format("%d-%b-%y")), colour="Grey30",
            hjust=-0.1, size=3)+
  geom_hline(yintercept=0)+
  geom_vline(xintercept=0)+
  scale_x_continuous(name="Proportion of cases expected to lead to death",
                     labels=label_percent(accuracy=1), 
                     limits=c(0,max(plotdata$deathsroll[plotdata$areaName=="Bolton"], 
                                    na.rm=TRUE)*1.05))+
  scale_y_continuous(name="Daily cases per 100,000",
                     limits=c(0,max(plotdata$casesroll[plotdata$areaName=="Bolton"], 
                                    na.rm=TRUE)*1.05))+
  scale_colour_viridis_c(option="rocket", direction=-1)+
  annotate("text", x=0.045, y=3.5, angle=-2, label="0.25 deaths/day", colour="SkyBlue", family="Lato",
           size=3)+
  annotate("text", x=0.035, y=12, angle=-14, label="1 deaths/day", colour="SkyBlue", family="Lato",
           size=3)+
  annotate("text", x=0.035, y=27, angle=-32, label="2.5 death/day", colour="SkyBlue", family="Lato",
           size=3)+
  theme_classic()+
  theme(axis.line=element_blank(), text=element_text(family="Lato"), 
        plot.title=element_text(face="bold", size=rel(1.4)), plot.title.position = "plot")+
  labs(title="Case rates in Bolton are high, but the younger age profile means we should\nexpect far fewer deaths than in previous waves",
       subtitle="Centered rolling 7-day average daily case rates compared to the expected proportion of cases which lead to a death\nwithin 28 days, based on age-specific Case Fatality Rates.",
       caption="Case data from coronavirus.data.gov.uk\nCFRs estimated by Daniel Howden\nPlot by @VictimOfMaths")

dev.off()

LA <- "Blackburn with Darwen"
LApop <- unique(plotdata$pop[plotdata$areaName==LA])

view(plotdata %>% filter(areaName==LA))

plotdata <- plotdata %>% 
  mutate(flag=if_else(date %in% c(as.Date("2021-01-05"), as.Date("2020-10-05"),
                          #as.Date("2020-10-04"), as.Date("2020-12-09"),
                          as.Date("2020-03-25"), max(date)-days(4)), 1, 0))

agg_tiff(paste0("Outputs/COVIDCasevsMortProp", LA, "Iso.tiff"), units="in", width=8, height=8, res=800)
ggplot()+
  geom_line(data=plotdata, aes(x=deathsroll, y=0.25*100000/(LApop*deathsroll)), colour="SkyBlue")+
  geom_line(data=plotdata, aes(x=deathsroll, y=1*100000/(LApop*deathsroll)), colour="SkyBlue")+
  geom_line(data=plotdata, aes(x=deathsroll, y=2.5*100000/(LApop*deathsroll)), colour="SkyBlue")+
  geom_line(data=plotdata, aes(x=deathsroll, y=5*100000/(LApop*deathsroll)), colour="SkyBlue")+
  geom_path(data=plotdata %>% filter(areaName==LA), 
            aes(x=deathsroll, y=casesroll, colour=date), show.legend=FALSE)+
  geom_point(data=plotdata %>% filter(areaName==LA & flag==1), 
             aes(x=deathsroll, y=casesroll), colour="Red4")+
  geom_text(data=plotdata %>% filter(areaName==LA & flag==1), 
            aes(x=deathsroll, y=casesroll, label=date %>% format("%d-%b-%y")), colour="Grey30",
            hjust=-0.1, size=3)+
  geom_hline(yintercept=0)+
  geom_vline(xintercept=0)+
  scale_x_continuous(name="Proportion of cases expected to lead to death",
                     labels=label_percent(accuracy=1), 
                     limits=c(0,max(plotdata$deathsroll[plotdata$areaName==LA], 
                                    na.rm=TRUE)*1.05))+
  scale_y_continuous(name="Daily cases per 100,000",
                     limits=c(0,max(plotdata$casesroll[plotdata$areaName==LA], 
                                    na.rm=TRUE)*1.05))+
  scale_colour_viridis_c(option="rocket", direction=-1)+
  annotate("text", x=0.015, y=5, angle=-5, label="0.25 deaths/day", colour="SkyBlue", family="Lato",
           size=3)+
  annotate("text", x=0.022, y=11, angle=-8, label="1 death/day", colour="SkyBlue", family="Lato",
           size=3)+
  annotate("text", x=0.025, y=23, angle=-16, label="2.5 deaths/day", colour="SkyBlue", family="Lato",
           size=3)+
  annotate("text", x=0.031, y=35, angle=-20, label="5 deaths/day", colour="SkyBlue", family="Lato",
           size=3)+
  theme_classic()+
  theme(axis.line=element_blank(), text=element_text(family="Lato"), 
        plot.title=element_text(face="bold", size=rel(1.4)), plot.title.position = "plot")+
  labs(title=paste0("Evolution of COVID cases and associated mortality risk in ", LA),
       subtitle="Centered rolling 7-day average daily case rates compared to the expected proportion of cases which lead to a death\nwithin 28 days, based on age-specific Case Fatality Rates.",
       caption="Case data from coronavirus.data.gov.uk\nCFRs estimated by Daniel Howden\nPlot by @VictimOfMaths")

dev.off()

#newmax=max(plotdata$date[!is.na(plotdata$casesroll)])
newmax=as.Date("2020-11-10")
#xmax=max(plotdata$deathsroll[plotdata$date==newmax], na.rm=TRUE)*1.05
#ymax=max(plotdata$casesroll[plotdata$date==newmax], na.rm=TRUE)*1.05

view(plotdata %>% filter(date==newmax))

#ggplot(plotdata %>% filter(date==max(date)-days(3)), 
agg_tiff("Outputs/COVIDCasevsMortPropIsoAllLTLAs1.tiff", units="in", width=8, height=7, res=800)
ggplot()+
  geom_line(data=plotdata, 
            aes(x=deathsroll, y=0.25/deathsroll), colour="SkyBlue")+
  geom_line(data=plotdata, 
            aes(x=deathsroll, y=1/deathsroll), colour="SkyBlue")+
  geom_line(data=plotdata, 
            aes(x=deathsroll, y=2.5/deathsroll), colour="SkyBlue")+
  geom_line(data=plotdata, 
            aes(x=deathsroll, y=5/deathsroll), colour="SkyBlue")+
  geom_point(data=plotdata %>% filter(date==newmax),
             aes(x=deathsroll, y=casesroll, size=pop, fill=Region), shape=21, alpha=0.7)+
  geom_hline(yintercept=0)+
  geom_vline(xintercept=0)+
  geom_text_repel(data=plotdata %>% filter(date==newmax),
                  aes(x=deathsroll, y=casesroll,label=areaName), size=rel(2.5))+
  scale_x_continuous(name="Proportion of cases expected to lead to death",
                     labels=label_percent(accuracy=1), 
                     limits=c(0,0.05))+
  scale_y_continuous(name="Daily cases per 100,000",
                     limits=c(0,250))+
  scale_fill_paletteer_d("LaCroixColoR::paired")+
  scale_size(guide=FALSE)+
  annotate("text", x=0.039, y=10, angle=-2, label="0.25 deaths/100,000/day", colour="SkyBlue", family="Lato",
           size=3)+
  annotate("text", x=0.041, y=28, angle=-6, label="1 death/100,000/day", colour="SkyBlue", family="Lato",
           size=3)+
  annotate("text", x=0.043, y=63, angle=-15, label="2.5 deaths/100,000/day", colour="SkyBlue", family="Lato",
           size=3)+
  annotate("text", x=0.045, y=117, angle=-26, label="5 deaths/100,000/day", colour="SkyBlue", family="Lato",
           size=3)+
  theme_classic()+
  theme(axis.line=element_blank(), text=element_text(family="Lato"), 
        plot.title=element_text(face="bold", size=rel(1.4)), plot.title.position = "plot",
        plot.caption.position="plot")+
  labs(title="In November case rates were lower, but CFRs were higher in some areas",
       subtitle=paste0("Centered rolling 7-day average daily case rates compared to the expected proportion of cases which lead to a death\nwithin 28 days, based on age-specific Case Fatality Rates. Data from ", format(newmax,"%d-%b-%Y")),
       caption="Data from coronavirus.data.gov.uk\nCFRs estimated by Daniel Howdon\nPlot by @VictimOfMaths")

dev.off()

agg_tiff("Outputs/COVIDCasevsMortPropIsoAllLTLAs.tiff", units="in", width=8, height=7, res=800)
ggplot()+
  geom_line(data=plotdata, 
            aes(x=deathsroll, y=0.25/deathsroll), colour="SkyBlue")+
  geom_line(data=plotdata, 
            aes(x=deathsroll, y=1/deathsroll), colour="SkyBlue")+
  #geom_line(data=plotdata, 
  #          aes(x=deathsroll, y=2.5/deathsroll), colour="SkyBlue")+
  geom_point(data=plotdata %>% filter(date==newmax),
             aes(x=deathsroll, y=casesroll, size=pop, fill=Region), shape=21, alpha=0.7)+
  geom_hline(yintercept=0)+
  geom_vline(xintercept=0)+
  geom_text_repel(data=plotdata %>% filter(date==newmax),
                  aes(x=deathsroll, y=casesroll,label=areaName), size=rel(2.5))+
  scale_x_continuous(name="Proportion of cases expected to lead to death",
                     labels=label_percent(accuracy=1), #trans="log")+ 
                     limits=c(0,0.03))+
  scale_y_continuous(name="Daily cases per 100,000", #trans="log")+ 
                     limits=c(0,75))+
  scale_fill_paletteer_d("LaCroixColoR::paired")+
  scale_size(guide=FALSE)+
  annotate("text", x=0.02, y=14, angle=-14, label="0.25 deaths/100,000/day", colour="SkyBlue", family="Lato",
           size=3)+
  annotate("text", x=0.023, y=46, angle=-38, label="1 death/100,000/day", colour="SkyBlue", family="Lato",
           size=3)+
  #annotate("text", x=0.026, y=67, angle=-44, label="2.5 deaths/day", colour="SkyBlue", family="Lato",
  #         size=3)+
  theme_classic()+
  theme(axis.line=element_blank(), text=element_text(family="Lato"), 
        plot.title=element_text(face="bold", size=rel(1.4)), plot.title.position = "plot",
        plot.caption.position="plot")+
  labs(title="Even in areas with high case rates, we can expect relatively low COVID death rates",
       subtitle=paste0("Centered rolling 7-day average daily case rates compared to the expected proportion of cases which lead to a death\nwithin 28 days, based on age-specific Case Fatality Rates. Data from ", format(newmax,"%d-%b-%Y")),
       caption="Data from coronavirus.data.gov.uk\nCFRs estimated by Daniel Howdon\nPlot by @VictimOfMaths")

dev.off()

agg_tiff("Outputs/COVIDCasevsMortPropIsoAllLTLAsLog.tiff", units="in", width=8, height=7, res=800)
ggplot()+
  geom_line(data=plotdata, 
            aes(x=deathsroll, y=0.25/deathsroll), colour="SkyBlue")+
  geom_line(data=plotdata, 
            aes(x=deathsroll, y=1/deathsroll), colour="SkyBlue")+
  #geom_line(data=plotdata, 
  #          aes(x=deathsroll, y=2.5/deathsroll), colour="SkyBlue")+
  geom_point(data=plotdata %>% filter(date==newmax),
             aes(x=deathsroll, y=casesroll, size=pop, fill=Region), shape=21, alpha=0.7)+
  geom_text_repel(data=plotdata %>% filter(date==newmax),
                  aes(x=deathsroll, y=casesroll,label=areaName), size=rel(2.5))+
  scale_x_continuous(name="Proportion of cases expected to lead to death\n(log scale)",
                     labels=label_percent(accuracy=0.01), trans="log", 
                     limits=c(NA,0.3), breaks=c(0.0001, 0.0025, 0.05))+
  scale_y_continuous(name="Daily cases per 100,000\n(log scale)", trans="log", 
                     limits=c(NA,75), labels=label_number(accuracy=1),
                     breaks=c(1, 10, 50))+
  scale_fill_paletteer_d("LaCroixColoR::paired")+
  scale_size(guide=FALSE)+
  annotate("text", x=0.022, y=14, angle=-60, label="0.25 deaths/100,000/day", colour="SkyBlue", family="Lato",
           size=3)+
  annotate("text", x=0.03, y=40, angle=-60, label="1 death/100,000/day", colour="SkyBlue", family="Lato",
           size=3)+
  #annotate("text", x=0.026, y=67, angle=-44, label="2.5 deaths/day", colour="SkyBlue", family="Lato",
  #         size=3)+
  theme_classic()+
  theme(text=element_text(family="Lato"), 
        plot.title=element_text(face="bold", size=rel(1.4)), plot.title.position = "plot",
        plot.caption.position="plot")+
  labs(title="Even in areas with high case rates, we can expect relatively low COVID death rates",
       subtitle=paste0("Centered rolling 7-day average daily case rates compared to the expected proportion of cases which lead to a death\nwithin 28 days, based on age-specific Case Fatality Rates. Data from ", format(newmax,"%d-%b-%Y")),
       caption="Data from coronavirus.data.gov.uk\nCFRs estimated by Daniel Howdon\nPlot by @VictimOfMaths")

dev.off()
