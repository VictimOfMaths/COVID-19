rm(list=ls())

library(tidyverse)
library(curl)
library(forcats)
library(readxl)
library(RcppRoll)
library(cowplot)
library(ggtext)
library(extrafont)
library(ggridges)

#Read in data
temp <- tempfile()
source <- "https://www.opendata.nhs.scot/dataset/b318bddf-a4dc-4262-971f-0ba329e09b87/resource/2dd8534b-0a6f-4744-9253-9565d62f96c2/download/trend_hb_20210621.csv"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

data <- read.csv(temp) %>% 
  mutate(date=as.Date(as.character(Date), format="%Y%m%d")) %>% 
  group_by(HB, HBName) %>% 
  arrange(date) %>% 
  mutate(casesroll_avg=roll_mean(DailyPositive, 7, align="right", fill=NA),
         maxcaserate=max(casesroll_avg, na.rm=TRUE), 
         maxcaseday=Date[which(casesroll_avg==maxcaserate)][1],
         totalcases=max(CumulativePositive, na.rm=TRUE),
         maxcaseprop=casesroll_avg/maxcaserate) %>% 
  ungroup() %>% 
  mutate(HBName=gsub("NHS ", "", HBName))

heatmap <- data %>% filter(HBName!="Scotland")

#Enter dates to plot from and to
plotfrom <- "2020-03-04"
plotto <- max(heatmap$date)

#Plot case trajectories
casetiles <- ggplot(heatmap, aes(x=date, y=fct_reorder(HBName, maxcaseday), 
                              fill=maxcaseprop))+
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
       subtitle=paste0("The heatmap represents the 7-day rolling average of the number of new confirmed cases, normalised to the maximum value within the Health Board.\nBoards are ordered by the date at which they reached their peak number of new cases. Bars on the right represent the absolute number of cases in each Health Board.\nData prior to 15th June (denoted with an asterisk) excluded community (Pillar 2) testing.\nData updated to ", plotto,". Data for most recent days is provisional and may be revised upwards as additional tests are processed."),
       caption="Data from Scottish Government | Plot by @VictimOfMaths")+
  theme(axis.line.y=element_blank(), plot.subtitle=element_text(size=rel(0.78)), 
        plot.title.position="plot",
        axis.text.y=element_text(colour="Black"), 
        plot.title=element_text(face="bold", size=rel(1.6)),
        text=element_text(family="Lato"))

casebars <- ggplot(subset(heatmap, Date==maxcaseday), 
                   aes(x=totalcases, y=fct_reorder(HB, maxcaseday), fill=totalcases))+
  geom_col(show.legend=FALSE)+
  theme_classic()+
  scale_fill_distiller(palette="Spectral")+
  scale_x_continuous(name="Total confirmed cases")+
  theme(axis.title.y=element_blank(), axis.line.y=element_blank(), 
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), axis.text.x=element_text(colour="Black"),
        text=element_text(family="Lato"))

tiff("Outputs/COVIDScottishLACasesHeatmap.tiff", units="in", width=12, height=5, res=500)
plot_grid(casetiles, casebars, align="h", rel_widths=c(1,0.2))
dev.off()


tiff("Outputs/COVIDScottishHBCaseRidges.tiff", units="in", width=9, height=6, res=500)
ggplot(heatmap, aes(x=date, y=fct_reorder(HBName, totalcases), 
                 height=casesroll_avg, fill=casesroll_avg))+
  geom_density_ridges_gradient(stat="identity", rel_min_height=0.001)+
  theme_classic()+
  theme(plot.title=element_text(face="bold", size=rel(1.6)),
        text=element_text(family="Lato"), plot.caption.position="plot", 
        plot.title.position="plot", legend.position="top")+
  scale_fill_distiller(palette="Spectral", name="Cases per day\n7-day rolling avg.")+
  scale_x_date(name="Date", limits=as.Date(c(plotfrom, plotto)), expand=c(0,0))+
  scale_y_discrete(name="")+
  labs(title="Timelines of confirmed COVID-19 cases in Scottish Health Boards",
       caption="Data from Scottish Government | Plot by @VictimOfMaths")+
  guides(fill = guide_colorbar(title.position = 'top', title.hjust = .5,
                                barwidth = unit(20, 'lines'), 
                               barheight = unit(.5, 'lines')))
dev.off()

ggplot(heatmap)+
  geom_col(aes(x=date , y=DailyPositive, fill=DailyPositive))+
  facet_wrap(~HBName)+
  scale_fill_distiller(palette="Spectral", name="Cases per day\n7-day rolling avg.")+
  theme_classic()

#Download ICU data from https://www.gov.scot/publications/coronavirus-covid-19-trends-in-daily-data/
temp <- tempfile()
source <- "https://www.gov.scot/binaries/content/documents/govscot/publications/statistics/2020/04/coronavirus-covid-19-trends-in-daily-data/documents/covid-19-data-by-nhs-board/covid-19-data-by-nhs-board/govscot%3Adocument/COVID-19%2Bdaily%2Bdata%2B-%2Bby%2BNHS%2BBoard%2B-%2B28%2BJune%2B2021.xlsx?forceDownload=true"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

#Historic ICU data (using a slightly different definition)
ICUdata.hist <- read_excel(temp, sheet=6, range="A12:Q180", col_names=FALSE)
colnames(ICUdata.hist) <- c("Date", "Ayrshire & Arran", "Borders", "Dumfries & Galloway",
                            "Fife", "Forth Valley", "Grampian", "Greater Glasgow & Clyde",
                            "Highland", "Lanarkshire", "Lothian", "Orkney", "Shetland",
                            "Tayside", "Western Isles", "Golden Jubilee National Hospital",
                            "Scotland")
ICUdata.hist_long <- gather(ICUdata.hist, HB, cases, c(2:17))
ICUdata.hist_long$cases <- as.numeric(ifelse(ICUdata.hist_long$cases=="*", NA, ICUdata.hist_long$cases))
ICUdata.hist_long$Date <- as.Date(ICUdata.hist_long$Date)

#Recent ICU data
#Need to manually increment the numbers at the end of this range: 24 = 1st October
ICUdata <- read_excel(temp, sheet=4, range="A3:Q287")

ICUdata_long <- gather(ICUdata, HB, cases, c(2:17))
ICUdata_long$cases <- as.numeric(ifelse(ICUdata_long$cases=="*", NA, ICUdata_long$cases))
ICUdata_long$Date <- as.Date(ICUdata_long$`Reporting date`)
ICUdata_long$HB <- if_else(substr(ICUdata_long$HB, 1,3)=="NHS", substr(ICUdata_long$HB, 5, 100),
                           ICUdata_long$HB)
ICUdata_long$HB <- if_else(ICUdata_long$HB=="Scotland total", "Scotland", ICUdata_long$HB)

#need to use a custom max function as some HBs are all 0s
my.max <- function(x) ifelse( !all(is.na(x)), max(x, na.rm=T), NA)

ICUheatmap <- ICUdata_long %>%
  bind_rows(., ICUdata.hist_long) %>% 
  arrange(HB, Date) %>%
  group_by(HB) %>%
  mutate(maxcases=my.max(cases), maxcaseday=Date[which(cases==maxcases)][1],
         totalcases=sum(cases))

#Enter dates to plot from and to
ICUplotfrom <- "2020-03-26"
ICUplotto <- max(ICUheatmap$Date)

#Plot case trajectories
ICUcasetiles <- ggplot(subset(ICUheatmap, HB!="Scotland" & !is.na(maxcases)), 
                       aes(x=Date, y=fct_reorder(HB, maxcaseday), fill=cases))+
  geom_tile(colour="White")+
  geom_vline(aes(xintercept=as.Date("2020-09-10")))+
  theme_classic()+
  scale_fill_distiller(palette="Spectral", na.value="White", name="Total patients",
                       limits=c(0,NA))+
  scale_y_discrete(name="", expand=c(0,0))+
  scale_x_date(name="Date", limits=as.Date(c(ICUplotfrom, ICUplotto)), expand=c(0,0))+
  labs(title="We haven't (yet) seen a rise in COVID patients in Scottish Intensive Care Units this wave",
       subtitle=paste0("The heatmap represents the number of ICU inpatients with confirmed COVID-19 in each Health Board. Numbers below 5 are censored and appear white. Health Boards with no non-missing days are excluded\nBoards are ordered by the date at which they reached their peak number of ICU cases. The definition of a COVID-19 patient was revised to a stricter definition after 10th September (denoted by the vertical line).\nData updated to ", ICUplotto,". Data for most recent days is provisional and may be revised upwards as additional tests are processed."),
       caption="Data from Scottish Government | Plot by @VictimOfMaths")+
  theme(axis.line.y=element_blank(), plot.subtitle=element_text(size=rel(0.78)), 
        plot.title.position="plot",
        axis.text.y=element_text(colour="Black"), 
        plot.title=element_text(face="bold", size=rel(1.6)),
        text=element_text(family="Lato"), plot.caption.position="plot")


tiff("Outputs/COVIDScottishHBICUHeatmap.tiff", units="in", width=12, height=5, res=500)
ICUcasetiles
dev.off()

#Repeat for all hospital patients
Hospdata.hist <- read_excel(temp, sheet=7, range="A3:Q172")
Hospdata.hist_long <- gather(Hospdata.hist, HB, cases, c(2:17))
Hospdata.hist_long$cases <- as.numeric(ifelse(Hospdata.hist_long$cases=="*", NA, Hospdata.hist_long$cases))
Hospdata.hist_long$Date <- as.Date(Hospdata.hist_long$Date)
Hospdata.hist_long$HB <- if_else(substr(Hospdata.hist_long$HB, 1,3)=="NHS", substr(Hospdata.hist_long$HB, 5, 100),
                                 Hospdata.hist_long$HB)

#Recent ICU data
#Need to manually increment the numbers at the end of this range: 24 = 1st October
Hospdata <- read_excel(temp, sheet=5, range="A3:Q287")

Hospdata_long <- gather(Hospdata, HB, cases, c(2:17))
Hospdata_long$cases <- as.numeric(ifelse(Hospdata_long$cases=="*", NA, Hospdata_long$cases))
Hospdata_long$Date <- as.Date(Hospdata_long$`Reporting date`)
Hospdata_long$HB <- if_else(substr(Hospdata_long$HB, 1,3)=="NHS", substr(Hospdata_long$HB, 5, 100),
                            Hospdata_long$HB)


Hospheatmap <- Hospdata_long %>%
  bind_rows(., Hospdata.hist_long) %>% 
  arrange(HB, Date) %>%
  group_by(HB) %>%
  mutate(maxcases=my.max(cases), maxcaseday=Date[which(cases==maxcases)][1],
         totalcases=sum(cases))

#Enter dates to plot from and to
Hospplotfrom <- "2020-03-26"
Hospplotto <- max(Hospheatmap$Date)

#Plot case trajectories
Hospcasetiles <- ggplot(subset(Hospheatmap, HB!="Scotland" & !is.na(maxcases)), 
                       aes(x=Date, y=fct_reorder(HB, maxcaseday), fill=cases))+
  geom_tile(colour="White")+
  geom_vline(aes(xintercept=as.Date("2020-09-10")))+
  theme_classic()+
  scale_fill_distiller(palette="Spectral", na.value="White", name="Total patients",
                       limits=c(0,NA))+
  scale_y_discrete(name="", expand=c(0,0))+
  scale_x_date(name="Date", limits=as.Date(c(ICUplotfrom, ICUplotto)), expand=c(0,0))+
  labs(title="The number of COVID-19 patients in Scottish hospitals is still low",
       subtitle=paste0("The heatmap represents the number of hospital patients with confirmed COVID-19 in each Health Board. Numbers below 5 are censored and appear white. Health Boards with no non-missing days are excluded\nBoards are ordered by the date at which they reached their peak number of hospital patients. The definition of a COVID-19 patient was revised to a stricter definition after 10th September (denoted by the vertical line).\nData updated to ", ICUplotto,". Data for most recent days is provisional and may be revised upwards as additional tests are processed."),
       caption="Data from Scottish Government | Plot by @VictimOfMaths")+
  theme(axis.line.y=element_blank(), plot.subtitle=element_text(size=rel(0.78)), 
        plot.title.position="plot",
        axis.text.y=element_text(colour="Black"), 
        plot.title=element_text(face="bold", size=rel(1.6)),
        text=element_text(family="Lato"), plot.caption.position="plot")


tiff("Outputs/COVIDScottishHBHospHeatmap.tiff", units="in", width=12, height=5, res=500)
Hospcasetiles
dev.off()

#Plot cases, admissions and ICU occupancy for the whole country
natdata <- data %>% 
  filter(HBName=="Scotland") %>% 
  merge(., subset(Hospheatmap, HB=="Scotland"), by.x="date", by.y="Date") %>% 
  merge(., subset(ICUheatmap, HB=="Scotland"), by.x="date", by.y="Date") %>% 
  select(date, DailyPositive, cases.x, cases.y) 

colnames(natdata) <- c("Date", "New cases", "Hospital patients", "ICU patients")

#Remove case data from 15th June as that's when the Pillar 2 data is added
natdata$`New cases` <- if_else(natdata$date==as.Date("2020-06-15"), 0, natdata$`New cases`)

#Calculate rolling averages
natdata <- natdata %>% 
  mutate(casesroll=roll_mean(`New cases`, 7, align="center", fill=NA),
         hosproll=roll_mean(`Hospital patients`, 7, align="center", fill=NA),
         ICUroll=roll_mean(`ICU patients`, 7, align="center", fill=NA))

limit <- abs(max(c(natdata$`New cases`, natdata$`Hospital patients`, natdata$`ICU patients`)))

tiff("Outputs/COVIDScottishHospBars.tiff", units="in", width=9, height=6, res=500)
ggplot(subset(natdata,Date>=as.Date("2020-09-11")))+
  geom_col(aes(x=Date, y=`New cases`), fill="#47d4ae")+
  geom_col(aes(x=Date, y=-`Hospital patients`), fill="#ff9f55")+
  geom_col(aes(x=Date, y=-`ICU patients`), fill="#ff1437")+
  geom_line(aes(x=Date, y=casesroll), colour="#09614a")+
  geom_hline(yintercept=0)+
  scale_x_date(name="")+
  scale_y_continuous(name="", limits=c(-limit, limit), labels=abs,
                     position = "right")+
  theme_classic()+
  annotate(geom="text", x=as.Date("2020-09-12"), y=1800, 
           label="New cases in the population", hjust=0, family="Lato")+
  annotate(geom="text", x=as.Date("2020-09-12"), y=-1600, 
           label="Total patients in hospital", hjust=0, family="Lato")+
  #annotate(geom="text", x=as.Date("2020-10-06"), y=-70, label="Patients in ICU", colour="#a80b20")+  
  labs(title="New COVID-19 cases in Scotland are rising, but hospital beds have yet to follow suit",
       subtitle="Daily confirmed <span style='color:#47d4ae;'>new COVID-19 cases</span> and patients with recently confirmed COVID-19<br>in <span style='color:#ff9f55;'>Scottish hospitals </span>and <span style='color:#ff1437;'>Intensive Care Units",
       caption="Data from Scottish Government | Plot by @VictimOfMaths")+
  theme(plot.subtitle=element_markdown(), 
        plot.title=element_text(face="bold", size=rel(1.5)),
        plot.caption.position = "plot",
        text=element_text(family="Lato"))

dev.off()

tiff("Outputs/COVIDScottishICUBars.tiff", units="in", width=10, height=8, res=500)
ggplot(natdata)+
  geom_col(aes(x=Date, y=`ICU patients`, fill=`ICU patients`), show.legend = FALSE)+
  geom_vline(xintercept=as.Date("2020-09-10"))+
  scale_x_date(name="")+
  scale_y_continuous(name="Total patients in ICU")+
  scale_fill_distiller(palette="Spectral")+
  theme_classic()+
  theme(plot.title=element_text(face="bold", size=rel(1.4)),
        plot.title.position = "plot",
        text=element_text(family="Lato"))+
  labs(title="The number of COVID-19 patients in Intensive Care in Scotland is holding fairly steady",
       subtitle=paste0("Hospital patients in Intensive Care Units with confirmed COVID-19.\nThe definition of a COVID-19 patient was revised to a stricter definition after 10th September (denoted by the vertical line).\nData for most recent days is provisional and may be revised upwards as additional tests are processed."),
       caption="Data from Scottish Government | Plot by @VictimOfMaths")

dev.off()
  
