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
data <- read.csv(temp)

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

heatmap <- data_long %>%
  arrange(HB, Date) %>%
  group_by(HB) %>%
  mutate(casesroll_avg=roll_mean(cases, 5, align="right", fill=0)) %>%
  mutate(maxcaserate=max(casesroll_avg), maxcaseday=Date[which(casesroll_avg==maxcaserate)][1],
         totalcases=max(cumul_cases))

heatmap$maxcaseprop <- heatmap$casesroll_avg/heatmap$maxcaserate

#Enter dates to plot from and to
plotfrom <- "2020-03-14"
plotto <- max(heatmap$Date)

#Plot case trajectories
casetiles <- ggplot(heatmap, aes(x=Date, y=fct_reorder(HB, maxcaseday), fill=maxcaseprop))+
  geom_tile(colour="White", show.legend=FALSE)+
  theme_classic()+
  scale_fill_distiller(palette="Spectral")+
  scale_y_discrete(name="", expand=c(0,0))+
  scale_x_date(name="Date", limits=as.Date(c(plotfrom, plotto)), expand=c(0,0))+
  labs(title="Timelines for COVID-19 cases in Scottish Health Boards",
       subtitle=paste0("The heatmap represents the 5-day rolling average of the number of new confirmed cases, normalised to the maximum value within the Health Board.\nBoards are ordered by the date at which they reached their peak number of new cases. Bars on the right represent the absolute number of cases in each Health Board.\nData updated to ", plotto,". Data for most recent days is provisional and may be revised upwards as additional tests are processed."),
       caption="Data from Scottish Government | Plot by @VictimOfMaths")+
  theme(axis.line.y=element_blank(), plot.subtitle=element_text(size=rel(0.78)), plot.title.position="plot",
        axis.text.y=element_text(colour="Black"))

casebars <- ggplot(subset(heatmap, Date==maxcaseday), aes(x=totalcases, y=fct_reorder(HB, maxcaseday), fill=totalcases))+
  geom_col(show.legend=FALSE)+
  theme_classic()+
  scale_fill_distiller(palette="Spectral")+
  scale_x_continuous(name="Total confirmed cases", breaks=c(0,1000, 2000, 3000))+
  theme(axis.title.y=element_blank(), axis.line.y=element_blank(), axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), axis.text.x=element_text(colour="Black"))

tiff("Outputs/COVIDScottishLACasesHeatmap.tiff", units="in", width=12, height=5, res=500)
plot_grid(casetiles, casebars, align="h", rel_widths=c(1,0.2))
dev.off()
