rm(list=ls())

library(tidyverse)
library(curl)
library(RcppRoll)
library(padr)
library(lubridate)
library(scales)
library(grDevices)
library(paletteer)
library(extrafont)
library(ragg)

#Read in Canadian data
temp <- tempfile()
source <- "https://health-infobase.canada.ca/src/data/covidLive/covid19-download.csv"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
data <- read.csv(temp)

data <- data %>% 
  select(prname, date, numtoday, numdeathstoday) %>% 
  filter(!prname %in% c("Canada", "Repatriated travellers")) %>% 
  rename(cases=numtoday, deaths=numdeathstoday) %>% 
  mutate(date=as.Date(date)) %>% 
  group_by(prname) %>% 
  pad()

#Bring in populations
temp <- tempfile()
temp2 <- tempfile()
source <- "https://www150.statcan.gc.ca/n1/tbl/csv/17100005-eng.zip"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)
pop <- read.csv(file.path(temp2, "17100005.csv"))

pop <- pop %>% 
  filter(ï..REF_DATE==2020 & GEO!="Canada" & Sex=="Both sexes" &
            Age.group=="All ages") %>% 
  rename(prname=GEO, pop=VALUE) %>% 
  select(prname, pop)
  
data <- merge(data, pop, all.x=TRUE) %>%
  group_by(prname) %>% 
  #Set up data for heatmap
  mutate(caserate=cases*100000/pop, deathrate=deaths*100000/pop,
         caserate_avg=roll_mean(caserate, 7, align="right", fill=NA),
         deathrate_avg=roll_mean(deathrate, 7, align="right", fill=NA),
         maxcaserate=max(caserate_avg, na.rm=TRUE),
         maxcaseday=date[which(caserate_avg==maxcaserate)][1],
         maxcaseprop=caserate_avg/maxcaserate,
         maxdeathrate=max(deathrate_avg, na.rm=TRUE),
         maxdeathday=date[which(deathrate_avg==maxdeathrate)][1],
         maxdeathprop=deathrate_avg/maxdeathrate,
         totalcases=sum(cases, na.rm=TRUE),
         totaldeaths=sum(deaths, na.rm=TRUE)) %>% 
  filter(!is.na(caserate_avg)) %>% 
  ungroup

plotfrom <- "2020-03-11"
plotto <- max(data$date)

#Plot case trajectories
casetiles.all <- ggplot(data, aes(x=date, y=fct_reorder(prname, maxcaseday), fill=maxcaseprop))+
  geom_tile(colour="White", show.legend=FALSE)+
  theme_classic()+
  scale_fill_distiller(palette="Spectral")+
  scale_y_discrete(name="", expand=c(0,0))+
  scale_x_date(name="", limits=as.Date(c(plotfrom, plotto)), expand=c(0,0), 
               breaks=pretty_breaks(n=interval(as.Date(plotfrom), plotto)%/% months(1)))+
  labs(title="Timelines for COVID-19 cases in Canadian provinces",
       subtitle=paste0("The heatmap represents the 7-day rolling average of the number of new confirmed cases, normalised to the maximum value within the province.\nProvinces are ordered by the date at which they reached their peak number of new cases. Bars on the right represent the absolute number of cases in each province.\nData updated to ", plotto, ". Data for most recent days is provisional and may be revised upwards as additional tests are processed."),
       caption="Data from Public Health Agency of Canada | Plot by @VictimOfMaths")+
  theme(axis.line.y=element_blank(), plot.subtitle=element_text(size=rel(0.78)), plot.title.position="plot",
        axis.text.y=element_text(colour="black"), plot.title=element_text(face="bold", size=rel(1.4)),
        text=element_text(family="Lato"))

casebars.all <- ggplot(subset(data, date==maxcaseday), aes(x=totalcases, y=fct_reorder(prname, maxcaseday), fill=totalcases))+
  geom_col(show.legend=FALSE)+
  theme_classic()+
  scale_fill_distiller(palette="Spectral")+
  scale_x_continuous(name="Total confirmed cases")+
  theme(axis.title.y=element_blank(), axis.line.y=element_blank(), axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), axis.text.x=element_text(colour="Black", angle=45, hjust=1, 
                                                               vjust=1),
        text=element_text(family="Lato"))

agg_tiff("Outputs/COVIDCanadaCasesHeatmap.tiff", units="in", width=15, height=5, res=500)
plot_grid(casetiles.all, casebars.all, align="h", rel_widths=c(1,0.2))
dev.off()

#Plot case rate trajectories
caseratetiles.all <- ggplot(data, aes(x=date, y=fct_reorder(prname, maxcaseday), fill=caserate_avg))+
  geom_tile(colour="White", show.legend=FALSE)+
  theme_classic()+
  scale_fill_distiller(palette="Spectral")+
  scale_y_discrete(name="", expand=c(0,0))+
  scale_x_date(name="", limits=as.Date(c(plotfrom, plotto)), expand=c(0,0), 
               breaks=pretty_breaks(n=interval(as.Date(plotfrom), plotto)%/% months(1)))+
  labs(title="Timelines for COVID-19 case rates in Canadian provinces",
       subtitle=paste0("The heatmap represents the 7-day rolling average of the rate of new confirmed cases per 100,000.\nProvinces are ordered by the date at which they reached their peak number of new cases. Bars on the right represent the population of each province.\nData updated to ", plotto, ". Data for most recent days is provisional and may be revised upwards as additional tests are processed."),
       caption="Data from Public Health Agency of Canada | Plot by @VictimOfMaths")+
  theme(axis.line.y=element_blank(), plot.subtitle=element_text(size=rel(0.78)), plot.title.position="plot",
        axis.text.y=element_text(colour="Black"), plot.title=element_text(face="bold", size=rel(1.4)),
        text=element_text(family="Lato"))

caseratebars.all <- ggplot(subset(data, date==maxcaseday), aes(x=pop/1000000, y=fct_reorder(prname, maxcaseday), fill=pop))+
  geom_col(show.legend=FALSE)+
  theme_classic()+
  scale_fill_distiller(palette="Spectral")+
  scale_x_continuous(name="Population (millions)")+
  theme(axis.title.y=element_blank(), axis.line.y=element_blank(), axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), axis.text.x=element_text(colour="Black"),
        text=element_text(family="Lato"))

agg_tiff("Outputs/COVIDCanadaCaseRatesHeatmap.tiff", units="in", width=15, height=5, res=500)
plot_grid(caseratetiles.all, caseratebars.all, align="h", rel_widths=c(1,0.2))
dev.off() 

#Plot death trajectories
deathtiles.all <- ggplot(data, aes(x=date, y=fct_reorder(prname, maxdeathday), fill=maxdeathprop))+
  geom_tile(colour="White", show.legend=FALSE)+
  theme_classic()+
  scale_fill_distiller(palette="Spectral", na.value="white")+
  scale_y_discrete(name="", expand=c(0,0))+
  scale_x_date(name="", limits=as.Date(c(plotfrom, plotto)), expand=c(0,0), 
               breaks=pretty_breaks(n=interval(as.Date(plotfrom), plotto)%/% months(1)))+
  labs(title="Timelines for COVID-19 deaths in Canadian provinces",
       subtitle=paste0("The heatmap represents the 7-day rolling average of the number of new confirmed deaths from COVID-19, normalised to the maximum value within the province. Provinces with no confirmed deaths are shown as white.\nProvinces are ordered by the date at which they reached their peak number of new cases. Bars on the right represent the absolute number of cases in each province.\nData updated to ", plotto, ". Data for most recent days is provisional and may be revised upwards as additional tests are processed."),
       caption="Data from Public Health Agency of Canada | Plot by @VictimOfMaths")+
  theme(axis.line.y=element_blank(), plot.subtitle=element_text(size=rel(0.78)), plot.title.position="plot",
        axis.text.y=element_text(colour="Black"), plot.title=element_text(face="bold", size=rel(1.4)),
        text=element_text(family="Lato"))

deathbars.all <- ggplot(subset(data, date==maxdeathday), aes(x=totaldeaths, y=fct_reorder(prname, maxdeathday), fill=totaldeaths))+
  geom_col(show.legend=FALSE)+
  theme_classic()+
  scale_fill_distiller(palette="Spectral")+
  scale_x_continuous(name="Total confirmed deaths")+
  theme(axis.title.y=element_blank(), axis.line.y=element_blank(), axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), axis.text.x=element_text(colour="Black", angle=45, hjust=1, 
                                                               vjust=1),
        text=element_text(family="Lato"))

tiff("Outputs/COVIDCanadaDeathsHeatmap.tiff", units="in", width=15, height=5, res=500)
plot_grid(deathtiles.all, deathbars.all, align="h", rel_widths=c(1,0.2))
dev.off()

#Plot death rate trajectories
deathratetiles.all <- ggplot(data, aes(x=date, y=fct_reorder(prname, maxdeathday), fill=deathrate_avg))+
  geom_tile(colour="White", show.legend=FALSE)+
  theme_classic()+
  scale_fill_distiller(palette="Spectral")+
  scale_y_discrete(name="", expand=c(0,0))+
  scale_x_date(name="", limits=as.Date(c(plotfrom, plotto)), expand=c(0,0), 
               breaks=pretty_breaks(n=interval(as.Date(plotfrom), plotto)%/% months(1)))+
  labs(title="Timelines for COVID-19 death rates in Canadian provinces",
       subtitle=paste0("The heatmap represents the 7-day rolling average of the rate of new confirmed deaths from COVID-19 per 100,000. Provinces with no confirmed deaths are shown as white.\nProvinces are ordered by the date at which they reached their peak number of deaths. Bars on the right represent the population of each province.\nData updated to ", plotto, ". Data for most recent days is provisional and may be revised upwards as additional tests are processed."),
       caption="Data from Public Health Agency of Canada | Plot by @VictimOfMaths")+
  theme(axis.line.y=element_blank(), plot.subtitle=element_text(size=rel(0.78)), plot.title.position="plot",
        axis.text.y=element_text(colour="Black"), plot.title=element_text(face="bold", size=rel(1.4)),
        text=element_text(family="Lato"))

deathratebars.all <- ggplot(subset(data, date==maxdeathday), aes(x=pop/1000000, y=fct_reorder(prname, maxdeathday), fill=pop))+
  geom_col(show.legend=FALSE)+
  theme_classic()+
  scale_fill_distiller(palette="Spectral")+
  scale_x_continuous(name="Population (millions)")+
  theme(axis.title.y=element_blank(), axis.line.y=element_blank(), axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), axis.text.x=element_text(colour="Black"),
        text=element_text(family="Lato"))

tiff("Outputs/COVIDCanadaDeathRatesHeatmap.tiff", units="in", width=15, height=5, res=500)
plot_grid(deathratetiles.all, deathratebars.all, align="h", rel_widths=c(1,0.2))
dev.off() 
