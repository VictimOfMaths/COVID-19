rm(list=ls())

library(tidyverse)
library(curl)
library(forcats)
library(readxl)
library(RcppRoll)

#Read in data
temp <- tempfile()
source <- "http://www2.nphs.wales.nhs.uk:8080/CommunitySurveillanceDocs.nsf/61c1e930f9121fd080256f2a004937ed/77fdb9a33544aee88025855100300cab/$FILE/Rapid%20COVID-19%20surveillance%20data.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
data <- read_excel(temp, sheet=2)

colnames(data) <- c("LA", "date", "cases", "totalcases", "totalcaserate", "tests", "totaltests")
data$date <- as.Date(data$date)

heatmap <- data %>%
  group_by(LA) %>%
  mutate(casesroll_avg=roll_mean(cases, 5, align="left", fill=0)) %>%
  mutate(maxcaserate=max(casesroll_avg), maxcaseday=date[which(casesroll_avg==maxcaserate)][1])

heatmap$maxcaseprop <- heatmap$casesroll_avg/heatmap$maxcaserate

#Enter dates to plot from and to
plotfrom <- "2020-03-01"
plotto <- "2020-05-08"

#Plot case trajectories
casetiles <- ggplot(heatmap, aes(x=date, y=fct_reorder(LA, maxcaseday), fill=maxcaseprop))+
  geom_tile(colour="White", show.legend=FALSE)+
  theme_classic()+
  scale_fill_distiller(palette="Spectral")+
  scale_y_discrete(name="", expand=c(0,0))+
  scale_x_date(name="Date", limits=as.Date(c(plotfrom, plotto)), expand=c(0,0))+
  labs(title="Timelines for COVID-19 cases in Welsh Local Authorities",
       subtitle="The heatmap represents the 5-day rolling average of the number of new confirmed cases, normalised to the maximum value within the Local Authority.\nLAs are ordered by the date at which they reached their peak number of new cases. Bars on the right represent the absolute number of cases in each LA.\nData updated to 8th May. Data for most recent days is provisional and may be revised upwards as additional tests are processed.",
       caption="Data from Public Health Wales | Plot by @VictimOfMaths")+
  theme(axis.line.y=element_blank(), plot.subtitle=element_text(size=rel(0.78)), plot.title.position="plot",
        axis.text.y=element_text(colour="Black"))

casebars <- ggplot(subset(heatmap, date==maxcaseday), aes(x=totalcases, y=fct_reorder(LA, maxcaseday), fill=totalcases))+
  geom_col(show.legend=FALSE)+
  theme_classic()+
  scale_fill_distiller(palette="Spectral")+
  scale_x_continuous(name="Total confirmed cases", breaks=c(0,500,1000))+
  theme(axis.title.y=element_blank(), axis.line.y=element_blank(), axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), axis.text.x=element_text(colour="Black"))

tiff("Outputs/COVIDWelshLACasesHeatmap.tiff", units="in", width=10, height=6, res=500)
plot_grid(casetiles, casebars, align="h", rel_widths=c(1,0.2))
dev.off()
