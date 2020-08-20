rm(list=ls())

library(tidyverse)
library(curl)
library(forcats)
library(readxl)
library(RcppRoll)
library(cowplot)

#Read in data
temp <- tempfile()
source <- "https://raw.githubusercontent.com/DataScienceScotland/COVID-19-Management-Information/master/COVID19%20-%20Daily%20Management%20Information%20-%20Scottish%20Health%20Boards%20-%20Cumulative%20cases.csv"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
data <- read.csv(temp)[-c(136),-c(16,17)]

#Remove blank rows
data <- data %>% filter_all(all_vars(complete.cases(.)))  

data$Date <- as.Date(data$Date)

data_long <- gather(data, HB, cumul_cases, c(2:15))
data_long$HB <- gsub("[.]", " ", data_long$HB)

#Treat supressed numbers as 0
data_long$cumul_cases <- as.numeric(ifelse(data_long$cumul_cases=="*", 0, data_long$cumul_cases))

#Calculate daily cases
data_long <- data_long %>%
  arrange(HB, Date) %>%
  group_by(HB) %>%
  mutate(cases=cumul_cases-lag(cumul_cases,1))

data_long$cases <- ifelse(is.na(data_long$cases), 0, data_long$cases)

#Cases data is weirdly missing for 20th July, so assume there were 0 new cases in every HB on that day
temp <- data.frame(Date=rep(as.Date("2020-07-20", times=14)),
                   HB=unique(data_long$HB), cases=rep(0, times=14))

data_long <- bind_rows(data_long, temp)

heatmap <- data_long %>%
  arrange(HB, Date) %>%
  group_by(HB) %>%
  mutate(casesroll_avg=roll_mean(cases, 7, align="right", fill=0)) 

#Since 15th June 2020, Pillar 2 cases are now included in the Scottish total,
#this means that all Pillar 2 cases *prior* to this date all show up on 15th June.
#To fix this for the time series we *could* redistribute these back across the time
#series, but easier just to leave them out and allocate the moving average from 
#14th June as the number of new cases on 15th.

heatmap$cases <- if_else(heatmap$Date=="2020-06-15", lag(heatmap$casesroll_avg, 1),
                         heatmap$cases)

#Recalculate rolling average
heatmap <-  heatmap %>%
  group_by(HB) %>% 
  mutate(casesroll_avg=roll_mean(cases, 7, align="right", fill=0)) %>% 
  mutate(maxcaserate=max(casesroll_avg), maxcaseday=Date[which(casesroll_avg==maxcaserate)][1],
         cumul_cases=cumsum(cases), totalcases=max(cumul_cases))

heatmap$maxcaseprop <- heatmap$casesroll_avg/heatmap$maxcaserate

#Enter dates to plot from and to
plotfrom <- "2020-03-14"
plotto <- max(heatmap$Date)

#Plot case trajectories
casetiles <- ggplot(heatmap, aes(x=Date, y=fct_reorder(HB, maxcaseday), fill=maxcaseprop))+
  geom_tile(colour="White", show.legend=FALSE)+
  geom_segment(aes(x=as.Date("2020-06-15"), xend=as.Date("2020-06-15"), y=0.5, yend=14.5),
               colour="grey20")+
  annotate("text", x=as.Date("2020-06-15"), y=14.6, label="*", size=5)+
  theme_classic()+
  scale_fill_distiller(palette="Spectral")+
  scale_y_discrete(name="", expand=c(0,0))+
  scale_x_date(name="Date", limits=as.Date(c(plotfrom, plotto)), expand=c(0,0))+
  coord_cartesian(clip = 'off')+
  labs(title="Timelines for COVID-19 cases in Scottish Health Boards",
       subtitle=paste0("The heatmap represents the 7-day rolling average of the number of new confirmed cases, normalised to the maximum value within the Health Board.\nBoards are ordered by the date at which they reached their peak number of new cases. Bars on the right represent the absolute number of cases in each Health Board.\nData since 15th June (denoted with an asterisk) has included additional tests conducted under the UK Government testing programme (Pillar 2).\nAs a result, data for the 15th June itself is estimated. Data updated to ", plotto,". Data for most recent days is provisional and may be revised upwards as additional tests are processed."),
       caption="Data from Scottish Government | Plot by @VictimOfMaths")+
  theme(axis.line.y=element_blank(), plot.subtitle=element_text(size=rel(0.78)), plot.title.position="plot",
        axis.text.y=element_text(colour="Black"))

casebars <- ggplot(subset(heatmap, Date==maxcaseday), aes(x=totalcases, y=fct_reorder(HB, maxcaseday), fill=totalcases))+
  geom_col(show.legend=FALSE)+
  theme_classic()+
  scale_fill_distiller(palette="Spectral")+
  scale_x_continuous(name="Total confirmed cases", breaks=c(0,1000, 2000, 3000, 4000))+
  theme(axis.title.y=element_blank(), axis.line.y=element_blank(), axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), axis.text.x=element_text(colour="Black"))

tiff("Outputs/COVIDScottishLACasesHeatmap.tiff", units="in", width=12, height=5, res=500)
plot_grid(casetiles, casebars, align="h", rel_widths=c(1,0.2))
dev.off()

library(ggridges)

tiff("Outputs/COVIDScottishHBCaseRidges.tiff", units="in", width=12, height=5, res=500)
ggplot(heatmap, aes(x=Date, y=fct_reorder(HB, totalcases), height=casesroll_avg, fill=casesroll_avg))+
  geom_density_ridges_gradient(stat="identity", rel_min_height=0.001)+
  theme_classic()+
  scale_fill_distiller(palette="Spectral", name="Cases per day\n7-day rolling avg.")+
  scale_x_date(name="Date", limits=as.Date(c(plotfrom, plotto)), expand=c(0,0))+
  scale_y_discrete(name="")+
  labs(title="Timelines of confirmed COVID-19 cases in Scottish Health Boards",
       caption="Data from Scottish Government | Plot by @VictimOfMaths")
dev.off()

ggplot(heatmap)+
  geom_col(aes(x=Date, y=cases, fill=cases))+
  facet_wrap(~HB)+
  scale_fill_distiller(palette="Spectral", name="Cases per day\n7-day rolling avg.")+
  theme_classic()

#Download ICU data from https://www.gov.scot/publications/coronavirus-covid-19-trends-in-daily-data/
temp <- tempfile()
source <- "https://www.gov.scot/binaries/content/documents/govscot/publications/statistics/2020/04/coronavirus-covid-19-trends-in-daily-data/documents/covid-19-data-by-nhs-board/covid-19-data-by-nhs-board/govscot%3Adocument/COVID-19%2Bdata%2Bby%2BNHS%2BBoard%2B07%2BJuly%2B2020.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

#Need to manually increment the numbers at the end of this range: 79 = 1st June
ICUdata <- read_excel(temp, sheet=4, range="A3:O115")

ICUdata_long <- gather(ICUdata, HB, cases, c(2:15))
ICUdata_long$cases <- as.numeric(ifelse(ICUdata_long$cases=="*", 0, ICUdata_long$cases))
ICUdata_long$Date <- as.Date(ICUdata_long$Date)
ICUdata_long$HB <- substr(ICUdata_long$HB, 5, 100)

ICUheatmap <- ICUdata_long %>%
  arrange(HB, Date) %>%
  group_by(HB) %>%
  mutate(casesroll_avg=roll_mean(cases, 5, align="right", fill=0)) %>%
  mutate(maxcaserate=max(casesroll_avg), maxcaseday=Date[which(casesroll_avg==maxcaserate)][1],
         totalcases=sum(cases))

ICUheatmap$maxcaseprop <- ICUheatmap$casesroll_avg/ICUheatmap$maxcaserate

#Enter dates to plot from and to
ICUplotfrom <- "2020-03-14"
ICUplotto <- max(ICUheatmap$Date)

#Plot case trajectories
ICUcasetiles <- ggplot(ICUheatmap, aes(x=Date, y=fct_reorder(HB, maxcaseday), fill=maxcaseprop))+
  geom_tile(colour="White", show.legend=FALSE)+
  theme_classic()+
  scale_fill_distiller(palette="Spectral", na.value="White")+
  scale_y_discrete(name="", expand=c(0,0))+
  scale_x_date(name="Date", limits=as.Date(c(ICUplotfrom, ICUplotto)), expand=c(0,0))+
  labs(title="Timelines for COVID-19 ICU patients in Scottish Health Boards",
       subtitle=paste0("The heatmap represents the 5-day rolling average of the number of ICU inpatients with confirmed or suspected COVID-19, normalised to the maximum value within the Health Board.\nBoards are ordered by the date at which they reached their peak number of ICU cases. Bars on the right represent the absolute number of ICU cases in each Health Board.\nData updated to ", ICUplotto,". Data for most recent days is provisional and may be revised upwards as additional tests are processed."),
       caption="Data from Scottish Government | Plot by @VictimOfMaths")+
  theme(axis.line.y=element_blank(), plot.subtitle=element_text(size=rel(0.78)), plot.title.position="plot",
        axis.text.y=element_text(colour="Black"))

ICUcasebars <- ggplot(subset(ICUheatmap, Date==maxcaseday), aes(x=totalcases, y=fct_reorder(HB, maxcaseday), fill=totalcases))+
  geom_col(show.legend=FALSE)+
  theme_classic()+
  scale_fill_distiller(palette="Spectral")+
  scale_x_continuous(name="Total confirmed cases", breaks=c(0,1000, 2000, 3000))+
  theme(axis.title.y=element_blank(), axis.line.y=element_blank(), axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), axis.text.x=element_text(colour="Black"))

tiff("Outputs/COVIDScottishLAICUHeatmap.tiff", units="in", width=12, height=5, res=500)
plot_grid(ICUcasetiles, ICUcasebars, align="h", rel_widths=c(1,0.2))
dev.off()
