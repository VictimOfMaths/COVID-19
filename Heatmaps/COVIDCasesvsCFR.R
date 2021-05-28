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

CFRs <- tibble::tribble(
  ~age, ~CFR,
  "0-4",   0,
  "5-9",   0,
  "10-14", 0,
  "15-19", 0,
  "20-24", 0,
  "25-29", 0,
  "30-34", 0,
  "35-39", 0.0219879511860199,
  "40-44", 0.0262285,
  "45-49", 0.1247561,
  "50-54", 0.2722577,
  "55-59", 0.458359,
  "60-64", 0.8949931,
  "65-69", 4.0467128,
  "70-74", 4.6385467,
  "75-79", 5.9203394,
  "80-84", 13.0488619,
  "85-89", 18.1130067,
  "90+",   23.080945)

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
LApop$code <- if_else(LApop$code=="E06000053", "E06000052", LApop$code)
LApop$name <- if_else(LApop$name=="Isles of Scilly", "Cornwall", LApop$name)

#Merge City of London & Hackney
LApop$code <- if_else(LApop$code=="E09000001", "E09000012", LApop$code)
LApop$name <- if_else(LApop$name=="City of London", "Hackney and City of London", LApop$name)
LApop$name <- if_else(LApop$name=="Hackney", "Hackney and City of London", LApop$name)

LApop <- LApop %>% 
  group_by(name, areaCode) %>% 
  summarise(pop=sum(pop)) %>% 
  ungroup()

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
                                  as.Date("2020-07-10"), max(date)-days(4)), 1, 0)) %>% 
  ungroup()

LA <- "Liverpool"
LApop <- unique(plotdata$pop[plotdata$areaName==LA])

agg_tiff("Outputs/COVIDCasevsMortPropBolton.tiff", units="in", width=8, height=8, res=800)
ggplot()+
  geom_path(data=plotdata %>% filter(areaName==LA), 
            aes(x=deathsroll, y=casesroll, alpha=date), show.legend=FALSE, colour="Red4")+
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
  scale_y_continuous(name="Daily cases per 100,000")+
  theme_classic()+
  theme(axis.line=element_blank(), text=element_text(family="Lato"), 
        plot.title=element_text(face="bold", size=rel(1.4)), plot.title.position = "plot")+
  labs(title=paste0("Case rates in ", LA, " are high, but the younger age profile means we should\nexpect far fewer deaths than in previous waves"),
       subtitle="Rolling 7-day average daily case rates compared to the expected proportion of cases which lead to a death\nwithin 28 days, based on age-specific Case Fatality Rates.",
       caption="Case data from coronavirus.data.gov.uk\nCFRs estimated by Daniel Howden\nPlot by @VictimOfMaths")

dev.off()

#version with countours
agg_tiff("Outputs/COVIDCasevsMortPropBoltonIso.tiff", units="in", width=8, height=8, res=800)
ggplot()+
  geom_line(data=plotdata, aes(x=deathsroll, y=0.25*100000/(LApop*deathsroll)), colour="SkyBlue")+
  geom_line(data=plotdata, aes(x=deathsroll, y=1*100000/(LApop*deathsroll)), colour="SkyBlue")+
  geom_line(data=plotdata, aes(x=deathsroll, y=2.5*100000/(LApop*deathsroll)), colour="SkyBlue")+
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
  annotate("text", x=0.045, y=3.5, angle=-2, label="0.25 deaths/day", colour="SkyBlue", font="Lato",
           size=3)+
  annotate("text", x=0.035, y=12, angle=-14, label="1 deaths/day", colour="SkyBlue", font="Lato",
           size=3)+
  annotate("text", x=0.035, y=27, angle=-32, label="2.5 death/day", colour="SkyBlue", font="Lato",
           size=3)+
  theme_classic()+
  theme(axis.line=element_blank(), text=element_text(family="Lato"), 
        plot.title=element_text(face="bold", size=rel(1.4)), plot.title.position = "plot")+
  labs(title=paste0("Case rates in ", LA, " are high, but the younger age profile means we should\nexpect far fewer deaths than in previous waves"),
  #labs(title=paste0("Evolution of COVID cases and associated mortality risk in ", LA),
       subtitle="Centered rolling 7-day average daily case rates compared to the expected proportion of cases which lead to a death\nwithin 28 days, based on age-specific Case Fatality Rates.",
       caption="Case data from coronavirus.data.gov.uk\nCFRs estimated by Daniel Howden\nPlot by @VictimOfMaths")

dev.off()

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
  annotate("text", x=0.015, y=5, angle=-5, label="0.25 deaths/day", colour="SkyBlue", font="Lato",
           size=3)+
  annotate("text", x=0.022, y=11, angle=-8, label="1 death/day", colour="SkyBlue", font="Lato",
           size=3)+
  annotate("text", x=0.025, y=23, angle=-16, label="2.5 deaths/day", colour="SkyBlue", font="Lato",
           size=3)+
  annotate("text", x=0.031, y=35, angle=-20, label="5 deaths/day", colour="SkyBlue", font="Lato",
           size=3)+
  theme_classic()+
  theme(axis.line=element_blank(), text=element_text(family="Lato"), 
        plot.title=element_text(face="bold", size=rel(1.4)), plot.title.position = "plot")+
  labs(title=paste0("Evolution of COVID cases and associated mortality risk in ", LA),
       subtitle="Centered rolling 7-day average daily case rates compared to the expected proportion of cases which lead to a death\nwithin 28 days, based on age-specific Case Fatality Rates.",
       caption="Case data from coronavirus.data.gov.uk\nCFRs estimated by Daniel Howden\nPlot by @VictimOfMaths")

dev.off()
