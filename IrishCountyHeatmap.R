rm(list=ls())

library(tidyverse)
library(curl)
library(forcats)
library(readxl)
library(RcppRoll)
library(cowplot)

#Read in data
temp <- tempfile()
source <- "http://opendata-geohive.hub.arcgis.com/datasets/4779c505c43c40da9101ce53f34bb923_0.csv?outSR={%22latestWkid%22:3857,%22wkid%22:102100}"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
data <- read_csv(temp)

#Strip out geographical information
data <- data[,c(1,3,4,10,11)]

colnames(data) <- c("TimeStamp", "county", "pop", "cumul_cases", "caseprop")

#Convert timestamp to date
data$date <- as.Date(substr(data$TimeStamp, 1, 10))

#Calculate daily cases
data <- data %>%
  arrange(county, date) %>%
  group_by(county) %>%
  mutate(cases=cumul_cases-lag(cumul_cases,1))

#For 3 counties (Leitrim, Limerick and Sligo) the case count goes *down* in early May. Ignore these for now
data$cases <- ifelse(is.na(data$cases), 0, data$cases)
data$cases <- ifelse(data$cases<0, 0, data$cases)
data$cumul_cases <- ifelse(is.na(data$cumul_cases), 0, data$cumul_cases)

heatmap <- data %>%
  group_by(county) %>%
  mutate(casesroll_avg=roll_mean(cases, 5, align="right", fill=0)) %>%
  mutate(maxcaserate=max(casesroll_avg), maxcaseday=date[which(casesroll_avg==maxcaserate)][1],
         totalcases=max(cumul_cases))

heatmap$maxcaseprop <- heatmap$casesroll_avg/heatmap$maxcaserate

#Enter dates to plot from and to
plotfrom <- "2020-03-21"
plotto <- max(heatmap$date)

#Plot case trajectories
casetiles <- ggplot(heatmap, aes(x=date, y=fct_reorder(county, maxcaseday), fill=maxcaseprop))+
  geom_tile(colour="White", show.legend=FALSE)+
  theme_classic()+
  scale_fill_distiller(palette="Spectral")+
  scale_y_discrete(name="", expand=c(0,0))+
  scale_x_date(name="Date", limits=as.Date(c(plotfrom, plotto)), expand=c(0,0))+
  labs(title="Timelines for COVID-19 cases in Irish Counties",
       subtitle="The heatmap represents the 5-day rolling average of the number of new confirmed cases, normalised to the maximum value within the county.\nCounties are ordered by the date at which they reached their peak number of new cases. Bars on the right represent the absolute number of cases in each county.\nData updated to 11th May. Data for most recent days is provisional and may be revised upwards as additional tests are processed.",
       caption="Data from data.gov.ie | Plot by @VictimOfMaths")+
  theme(axis.line.y=element_blank(), plot.subtitle=element_text(size=rel(0.78)), plot.title.position="plot",
        axis.text.y=element_text(colour="Black"))

casebars <- ggplot(subset(heatmap, date==maxcaseday), aes(x=totalcases, y=fct_reorder(county, maxcaseday), fill=totalcases))+
  geom_col(show.legend=FALSE)+
  theme_classic()+
  scale_fill_distiller(palette="Spectral")+
  scale_x_continuous(name="Total confirmed cases", breaks=c(0,5000,10000))+
  theme(axis.title.y=element_blank(), axis.line.y=element_blank(), axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), axis.text.x=element_text(colour="Black"))

tiff("Outputs/COVIDIrishLACasesHeatmap.tiff", units="in", width=11, height=6, res=500)
plot_grid(casetiles, casebars, align="h", rel_widths=c(1,0.2))
dev.off()
