rm(list=ls())

library(tidyverse)
library(forcats)
library(cowplot)
library(ggridges)
library(geojsonio)
library(broom)
library(sf)
library(scales)
library(curl)
library(rmapshaper)
library(gganimate)
library(paletteer)
library(lubridate)
library(ragg)
library(extrafont)

options(scipen=999)

theme_custom <- function() {
  theme_classic() %+replace%
    theme(plot.title.position="plot", plot.caption.position="plot",
          strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)),
          plot.title=element_text(face="bold", size=rel(1.5), hjust=0,
                                  margin=margin(0,0,5.5,0)),
          text=element_text(family="Lato"))
}

#Read in data created by COVID_LA_Plots/UnderlyingCode.R, which lives here:
#https://github.com/VictimOfMaths/COVID_LA_Plots/blob/master/UnderlyingCode.R

#data <- read.csv("COVID_LA_Plots/LACases.csv")[,-c(1)]
load("COVID_LA_Plots/Alldata.RData")

mortdata <- data
data <- daydata

rm(daydata)

#EVERYWHERE
data.all <- data %>% 
  group_by(name) %>% 
  filter(!Region %in% c("Nation", "Region") & !is.na(casesroll_avg)) %>% 
  mutate(date=as.Date(date), maxcaserate=max(caserate_avg),
         maxcaseday=date[which(caserate_avg==maxcaserate)][1],
         maxcaseprop=caserate_avg/maxcaserate,
         totalcases=sum(cases), recentpeak=maxcaseprop[date==as.Date("2020-09-15")],
         maxadmrate=max(admrate_avg, na.rm=TRUE),
         maxadmday=date[which(admrate_avg==maxadmrate)][1],
         maxadmprop=admrate_avg/maxadmrate, totaladm=sum(admissions, na.rm=TRUE))

plotfrom <- "2020-03-01"
plotto <- max(data.all$date)

#Plot case trajectories
casetiles.all <- ggplot(data.all, aes(x=date, y=fct_reorder(name, maxcaseday), fill=maxcaseprop))+
  geom_tile(colour="White", show.legend=FALSE)+
  theme_classic()+
  scale_fill_distiller(palette="Spectral")+
  scale_y_discrete(name="", expand=c(0,0))+
  scale_x_date(name="Date", limits=as.Date(c(plotfrom, plotto)), expand=c(0,0), 
               breaks=pretty_breaks(n=interval(as.Date(plotfrom), plotto)%/% months(1)))+
  labs(title="Timelines for COVID-19 cases in Local Authorities/Council Areas across the UK",
       subtitle=paste0("The heatmap represents the 7-day rolling average of the number of new confirmed cases, normalised to the maximum value within the Local Authority.\nLAs are ordered by the date at which they reached their peak number of new cases. Bars on the right represent the absolute number of cases in each LA.\nData updated to ", plotto, ". Data for most recent days is provisional and may be revised upwards as additional tests are processed."),
       caption="Data from PHE, PHW, PHS & DoHNI | Plot by @VictimOfMaths")+
  theme_custom()+
  theme(axis.line.y=element_blank(), plot.subtitle=element_text(size=rel(0.78)), 
        axis.text.y=element_text(colour="Black"), plot.title=element_text(size=rel(1.8)))

casebars.all <- ggplot(subset(data.all, date==maxcaseday), aes(x=totalcases, y=fct_reorder(name, maxcaseday), fill=totalcases))+
  geom_col(show.legend=FALSE)+
  theme_classic()+
  scale_fill_distiller(palette="Spectral")+
  scale_x_continuous(name="Total confirmed cases")+
  theme_custom()+
  theme(axis.title.y=element_blank(), axis.line.y=element_blank(), axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), axis.text.x=element_text(colour="Black"))

agg_tiff("Outputs/COVIDLTLACasesHeatmapUK.tiff", units="in", width=16, height=30, res=500)
plot_grid(casetiles.all, casebars.all, align="h", rel_widths=c(1,0.2))
dev.off()

agg_png("Outputs/COVIDLTLACasesHeatmapUK.png", units="in", width=16, height=30, res=500)
plot_grid(casetiles.all, casebars.all, align="h", rel_widths=c(1,0.2))
dev.off()

#Rate trajectories

ratetiles.all <- ggplot(data.all, aes(x=date, y=fct_reorder(name, maxcaseday), fill=caserate_avg))+
  geom_tile(colour="White", show.legend=FALSE)+
  theme_custom()+
  scale_fill_distiller(palette="Spectral")+
  scale_y_discrete(name="", expand=c(0,0))+
  scale_x_date(name="Date", limits=as.Date(c(plotfrom, plotto)), expand=c(0,0), 
               breaks=pretty_breaks(n=interval(as.Date(plotfrom), plotto)%/% months(1)))+
  labs(title="Timelines for COVID-19 case rates in Local Authorities/Council Areas across the UK",
       subtitle=paste0("The heatmap represents the 7-day rolling average of the number of new confirmed cases per 100,000 population.\nLAs are ordered by the date at which they reached their peak number of new cases. Bars on the right represent the population of the LA.\nData updated to ", plotto, ". Data for most recent days is provisional and may be revised upwards as additional tests are processed."),
       caption="Data from PHE, PHW, PHS & DoHNI | Plot by @VictimOfMaths")+
  theme(axis.line.y=element_blank(), plot.subtitle=element_text(size=rel(0.78)), plot.title.position="plot",
        axis.text.y=element_text(colour="Black"), plot.title=element_text(face="bold", size=rel(1.2)))

ratebars.all <- ggplot(subset(data.all, date==maxcaseday), aes(x=pop, y=fct_reorder(name, maxcaseday), fill=pop))+
  geom_col(show.legend=FALSE)+
  theme_custom()+
  scale_fill_distiller(palette="Spectral")+
  scale_x_continuous(name="Population")+
  theme(axis.title.y=element_blank(), axis.line.y=element_blank(), axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), axis.text.x=element_text(colour="Black"))

agg_tiff("Outputs/COVIDLTLARatesHeatmapUK.tiff", units="in", width=16, height=30, res=500)
plot_grid(ratetiles.all, ratebars.all, align="h", rel_widths=c(1,0.2))
dev.off()

agg_png("Outputs/COVIDLTLARatesHeatmapUK.png", units="in", width=16, height=30, res=500)
plot_grid(ratetiles.all, ratebars.all, align="h", rel_widths=c(1,0.2))
dev.off()

#EVERYWHERE - SINCE 1ST JULY ONLY
data.all.recent <- data %>% 
  group_by(name) %>% 
  mutate(date=as.Date(date)) %>% 
  filter(!Region %in% c("Nation", "Region") & !is.na(casesroll_avg) & date>as.Date("2020-07-01")) %>% 
  mutate(maxcaserate=max(caserate_avg),
         maxcaseday=date[which(caserate_avg==maxcaserate)][1],
         maxcaseprop=caserate_avg/maxcaserate,
         totalcases=sum(cases), recentpeak=maxcaseprop[date==as.Date("2020-09-15")])

plotfrom <- "2020-07-01"
plotto <- max(data.all.recent$date)

#Plot case trajectories
casetiles.all.recent <- ggplot(data.all.recent, aes(x=date, y=fct_reorder(name, maxcaseday), fill=maxcaseprop))+
  geom_tile(colour="White", show.legend=FALSE)+
  theme_custom()+
  scale_fill_distiller(palette="Spectral")+
  scale_y_discrete(name="", expand=c(0,0))+
  scale_x_date(name="Date", limits=as.Date(c(plotfrom, plotto)), expand=c(0,0), 
               breaks=pretty_breaks(n=interval(as.Date(plotfrom), plotto)%/% months(1)))+
  labs(title="Timelines for COVID-19 cases in Local Authorities/Council Areas across the UK",
       subtitle=paste0("The heatmap represents the 7-day rolling average of the number of new confirmed cases, normalised to the maximum value within the Local Authority.\nLAs are ordered by the date at which they reached their peak number of new cases. Bars on the right represent the absolute number of cases in each LA.\nData updated to ", plotto, ". Data for most recent days is provisional and may be revised upwards as additional tests are processed."),
       caption="Data from PHE, PHW, PHS & DoHNI | Plot by @VictimOfMaths")+
  theme(axis.line.y=element_blank(), plot.subtitle=element_text(size=rel(0.78)), plot.title.position="plot",
        axis.text.y=element_text(colour="Black"), plot.title=element_text(face="bold", size=rel(1.2)))

casebars.all.recent <- ggplot(subset(data.all.recent, date==maxcaseday), aes(x=totalcases, y=fct_reorder(name, maxcaseday), fill=totalcases))+
  geom_col(show.legend=FALSE)+
  theme_custom()+
  scale_fill_distiller(palette="Spectral")+
  scale_x_continuous(name="Total confirmed cases", breaks=c(0,5000,10000))+
  theme(axis.title.y=element_blank(), axis.line.y=element_blank(), axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), axis.text.x=element_text(colour="Black"))

agg_tiff("Outputs/COVIDLTLACasesHeatmapUK_Recent.tiff", units="in", width=10, height=30, res=500)
plot_grid(casetiles.all.recent, casebars.all.recent, align="h", rel_widths=c(1,0.2))
dev.off()

agg_png("Outputs/COVIDLTLACasesHeatmapUK_Recent.png", units="in", width=16, height=30, res=500)
plot_grid(casetiles.all.recent, casebars.all.recent, align="h", rel_widths=c(1,0.2))
dev.off()

#########
#ENGLAND#
#########
data.e <- data %>% 
  group_by(name) %>% 
  filter(country=="England" & name!="England" & !is.na(casesroll_avg)) %>% 
  mutate(date=as.Date(date), maxcaserate=max(caserate_avg),
         maxcaseday=date[which(caserate_avg==maxcaserate)][1],
         maxcaseprop=caserate_avg/maxcaserate,
         totalcases=sum(cases), maxadmrate=max(admrate_avg, na.rm=TRUE),
         maxadmday=date[which(admrate_avg==maxadmrate)][1],
         maxadmprop=admrate_avg/maxadmrate, totaladm=sum(admissions, na.rm=TRUE))

#Enter dates to plot from and to
plotfrom <- "2020-03-01"
plotto <- max(data.e$date)
plotadmto <- max(data.e$date[!is.na(data.e$admrate_avg)])

#Plot case trajectories
casetiles.e <- ggplot(data.e, aes(x=date, y=fct_reorder(name, maxcaseday), fill=maxcaseprop))+
  geom_tile(colour="White", show.legend=FALSE)+
  theme_custom()+
  scale_fill_distiller(palette="Spectral")+
  scale_y_discrete(name="", expand=c(0,0))+
  scale_x_date(name="Date", limits=as.Date(c(plotfrom, plotto)), expand=c(0,0), 
               breaks=pretty_breaks(n=interval(as.Date(plotfrom), plotto)%/% months(1)))+
  labs(title="Timelines for COVID-19 cases in English Local Authorities",
       subtitle=paste0("The heatmap represents the 7-day rolling average of the number of new confirmed cases, normalised to the maximum value within the Local Authority.\nLAs are ordered by the date at which they reached their peak number of new cases. Bars on the right represent the absolute number of cases in each LA.\nData updated to ", plotto, ". Data for most recent days is provisional and may be revised upwards as additional tests are processed."),
       caption="Data from Public Health England | Plot by @VictimOfMaths")+
  theme(axis.line.y=element_blank(), plot.subtitle=element_text(size=rel(0.78)), plot.title.position="plot",
        axis.text.y=element_text(colour="Black"), plot.title=element_text(face="bold", size=rel(1.2)))

casebars.e <- ggplot(subset(data.e, date==maxcaseday), aes(x=totalcases, y=fct_reorder(name, maxcaseday), fill=totalcases))+
  geom_col(show.legend=FALSE)+
  theme_custom()+
  scale_fill_distiller(palette="Spectral")+
  scale_x_continuous(name="Total confirmed cases")+
  theme(axis.title.y=element_blank(), axis.line.y=element_blank(), axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), axis.text.x=element_text(colour="Black"))

agg_tiff("Outputs/COVIDLTLACasesHeatmap.tiff", units="in", width=16, height=26, res=500)
plot_grid(casetiles.e, casebars.e, align="h", rel_widths=c(1,0.2))
dev.off()

agg_png("Outputs/COVIDLTLACasesHeatmap.png", units="in", width=16, height=26, res=500)
plot_grid(casetiles.e, casebars.e, align="h", rel_widths=c(1,0.2))
dev.off()

ratetiles.e <- ggplot(data.e, aes(x=date, y=fct_reorder(name, maxcaseday), fill=caserate_avg))+
  geom_tile(colour="White", show.legend=FALSE)+
  theme_custom()+
  scale_fill_distiller(palette="Spectral")+
  scale_y_discrete(name="", expand=c(0,0))+
  scale_x_date(name="Date", limits=as.Date(c(plotfrom, plotto)), expand=c(0,0), 
               breaks=pretty_breaks(n=interval(as.Date(plotfrom), plotto)%/% months(1)))+
  labs(title="Timelines for COVID-19 case rates in Local Authorities in England",
       subtitle=paste0("The heatmap represents the 7-day rolling average of the number of new confirmed cases per 100,000 population.\nLAs are ordered by the date at which they reached their peak number of new cases. Bars on the right represent the population of the LA.\nData updated to ", plotto, ". Data for most recent days is provisional and may be revised upwards as additional tests are processed."),
       caption="Data from PHE | Plot by @VictimOfMaths")+
  theme(axis.line.y=element_blank(), plot.subtitle=element_text(size=rel(0.78)), plot.title.position="plot",
        axis.text.y=element_text(colour="Black"), plot.title=element_text(face="bold", size=rel(1.2)))

ratebars.e <- ggplot(subset(data.e, date==maxcaseday), aes(x=pop, y=fct_reorder(name, maxcaseday), fill=pop))+
  geom_col(show.legend=FALSE)+
  theme_custom()+
  scale_fill_distiller(palette="Spectral")+
  scale_x_continuous(name="Population")+
  theme(axis.title.y=element_blank(), axis.line.y=element_blank(), axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), axis.text.x=element_text(colour="Black"))

agg_tiff("Outputs/COVIDLTLARatesHeatmapEng.tiff", units="in", width=16, height=24, res=500)
plot_grid(ratetiles.e, ratebars.e, align="h", rel_widths=c(1,0.2))
dev.off()

agg_png("Outputs/COVIDLTLARatesHeatmapEng.png", units="in", width=16, height=24, res=500)
plot_grid(ratetiles.e, ratebars.e, align="h", rel_widths=c(1,0.2))
dev.off()

admtiles.e <- ggplot(data.e, aes(x=date, y=fct_reorder(name, maxadmday), fill=maxadmprop))+
  geom_tile(colour="White", show.legend=FALSE)+
  theme_custom()+
  scale_fill_distiller(palette="Spectral")+
  scale_y_discrete(name="", expand=c(0,0))+
  scale_x_date(name="Date", limits=as.Date(c(as.Date("2020-08-03"), plotadmto)), expand=c(0,0), 
               breaks=pretty_breaks(n=interval(as.Date(plotfrom), plotadmto)%/% months(1)))+
  labs(title="Timelines for COVID-19 admissions in English Local Authorities",
       subtitle=paste0("The heatmap represents the 7-day rolling average of the number of new confirmed hospital admissions, normalised to the maximum value within the Local Authority.\nLAs are ordered by the date at which they reached their peak number of new admissions Bars on the right represent the absolute number of admissions in each LA.\nData updated to ", 
                       plotto, ". Data for most recent days is provisional and may be revised upwards as additional tests are processed.\nAdmissions are defined as patients admitted with a positive COVID-19 diagnosis, or those diagnosed in hospital"),
       caption="Data from NHS England | Plot by @VictimOfMaths")+
  theme(axis.line.y=element_blank(), plot.subtitle=element_text(size=rel(0.78)), plot.title.position="plot",
        axis.text.y=element_text(colour="Black"), plot.title=element_text(face="bold", size=rel(1.2)))

admbars.e <- ggplot(subset(data.e, date==maxadmday), aes(x=totaladm, y=fct_reorder(name, maxadmday), 
                                                         fill=totaladm))+
  geom_col(show.legend=FALSE)+
  theme_custom()+
  scale_fill_distiller(palette="Spectral")+
  scale_x_continuous(name="Total confirmed admissions")+
  theme(axis.title.y=element_blank(), axis.line.y=element_blank(), axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), axis.text.x=element_text(colour="Black"))

agg_tiff("Outputs/COVIDLTLAAdmissionsHeatmap.tiff", units="in", width=10, height=26, res=500)
plot_grid(admtiles.e, admbars.e, align="h", rel_widths=c(1,0.2))
dev.off()

agg_png("Outputs/COVIDLTLAAdmissionsHeatmap.png", units="in", width=10, height=26, res=500)
plot_grid(admtiles.e, admbars.e, align="h", rel_widths=c(1,0.2))
dev.off()

admratetiles.e <- ggplot(data.e, aes(x=date, y=fct_reorder(name, maxadmday), fill=admrate_avg))+
  geom_tile(colour="White", show.legend=FALSE)+
  theme_custom()+
  scale_fill_distiller(palette="Spectral")+
  scale_y_discrete(name="", expand=c(0,0))+
  scale_x_date(name="Date", limits=as.Date(c(as.Date("2020-08-03"), plotadmto)), expand=c(0,0), 
               breaks=pretty_breaks(n=interval(as.Date(plotfrom), plotadmto)%/% months(1)))+
  labs(title="Timelines for COVID-19 admission rates in Local Authorities in England",
       subtitle=paste0("The heatmap represents the 7-day rolling average of the number of new confirmed hospital admissions per 100,000 population.\nLAs are ordered by the date at which they reached their peak number of new cases. Bars on the right represent the population of the LA.\nData updated to ", 
                       plotto, ". Data for most recent days is provisional and may be revised upwards as additional tests are processed.\nAdmissions are defined as patients admitted with a positive COVID-19 diagnosis, or those diagnosed in hospital"),
       caption="Data from NHS England | Plot by @VictimOfMaths")+
  theme(axis.line.y=element_blank(), plot.subtitle=element_text(size=rel(0.78)), plot.title.position="plot",
        axis.text.y=element_text(colour="Black"), plot.title=element_text(face="bold", size=rel(1.2)))

admratebars.e <- ggplot(subset(data.e, date==maxadmday), aes(x=pop, y=fct_reorder(name, maxadmday), fill=pop))+
  geom_col(show.legend=FALSE)+
  theme_custom()+
  scale_fill_distiller(palette="Spectral")+
  scale_x_continuous(name="Population")+
  theme(axis.title.y=element_blank(), axis.line.y=element_blank(), axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), axis.text.x=element_text(colour="Black"))

agg_tiff("Outputs/COVIDLTLAAdmRatesHeatmapEng.tiff", units="in", width=10, height=24, res=500)
plot_grid(admratetiles.e, admratebars.e, align="h", rel_widths=c(1,0.2))
dev.off()

agg_png("Outputs/COVIDLTLAAdmRatesHeatmapEng.png", units="in", width=10, height=24, res=500)
plot_grid(admratetiles.e, admratebars.e, align="h", rel_widths=c(1,0.2))
dev.off()

#######
#WALES#
#######
data.w <- data %>% 
  group_by(name) %>% 
  filter(country=="Wales" & name!="Wales" & !is.na(casesroll_avg)) %>% 
  mutate(date=as.Date(date), maxcaserate=max(caserate_avg),
         maxcaseday=date[which(caserate_avg==maxcaserate)][1],
         maxcaseprop=caserate_avg/maxcaserate,
         totalcases=sum(cases))

#Enter dates to plot from and to
plotfrom <- "2020-03-01"
plotto <- max(data.w$date)

#Plot case trajectories
casetiles.w <- ggplot(data.w, aes(x=date, y=fct_reorder(name, maxcaseday), fill=maxcaseprop))+
  geom_tile(colour="White", show.legend=FALSE)+
  theme_custom()+
  scale_fill_distiller(palette="Spectral")+
  scale_y_discrete(name="", expand=c(0,0))+
  scale_x_date(name="Date", limits=as.Date(c(plotfrom, plotto)), expand=c(0,0), 
               breaks=pretty_breaks(n=interval(as.Date(plotfrom), plotto)%/% months(1)))+
  labs(title="Timelines for COVID-19 cases in Welsh Local Authorities",
       subtitle=paste0("The heatmap represents the 7-day rolling average of the number of new confirmed cases, normalised to the maximum value within the Local Authority.\nLAs are ordered by the date at which they reached their peak number of new cases. Bars on the right represent the absolute number of cases in each LA.\nData updated to ", plotto, ". Data for most recent days is provisional and may be revised upwards as additional tests are processed."),
       caption="Data from Public Health Wales | Plot by @VictimOfMaths")+
  theme(axis.line.y=element_blank(), plot.subtitle=element_text(size=rel(0.78)), plot.title.position="plot",
        axis.text.y=element_text(colour="Black"), plot.title=element_text(face="bold", size=rel(1.2)))

casebars.w <- ggplot(subset(data.w, date==maxcaseday), aes(x=totalcases, y=fct_reorder(name, maxcaseday), fill=totalcases))+
  geom_col(show.legend=FALSE)+
  theme_custom()+
  scale_fill_distiller(palette="Spectral")+
  scale_x_continuous(name="Total confirmed cases")+
  theme(axis.title.y=element_blank(), axis.line.y=element_blank(), axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), axis.text.x=element_text(colour="Black"))

agg_tiff("Outputs/COVIDWelshLACasesHeatmap.tiff", units="in", width=12, height=6, res=500)
plot_grid(casetiles.w, casebars.w, align="h", rel_widths=c(1,0.2))
dev.off()

agg_tiff("Outputs/COVIDWelshLACaseRidges.tiff", units="in", width=10, height=6, res=500)
ggplot(data.w, aes(x=date, y=fct_reorder(name, totalcases), height=casesroll_avg, fill=casesroll_avg))+
  geom_density_ridges_gradient(stat="identity")+
  theme_custom()+
  scale_fill_distiller(palette="Spectral", name="Cases per day\n7-day rolling avg.")+
  scale_x_date(name="Date", limits=as.Date(c(plotfrom, plotto)), expand=c(0,0), 
               breaks=pretty_breaks(n=interval(as.Date(plotfrom), plotto)%/% months(1)))+
  scale_y_discrete(name="")+
  labs(title="Timelines of confirmed COVID-19 cases in Welsh Local Authorities",
       caption="Data from Public Health Wales | Plot by @VictimOfMaths")
dev.off()

ratetiles.w <- ggplot(data.w, aes(x=date, y=fct_reorder(name, maxcaseday), fill=caserate_avg))+
  geom_tile(colour="White", show.legend=FALSE)+
  theme_custom()+
  scale_fill_distiller(palette="Spectral")+
  scale_y_discrete(name="", expand=c(0,0))+
  scale_x_date(name="Date", limits=as.Date(c(plotfrom, plotto)), expand=c(0,0), 
               breaks=pretty_breaks(n=interval(as.Date(plotfrom), plotto)%/% months(1)))+
  labs(title="Timelines for COVID-19 case rates in Local Authorities in Wales",
       subtitle=paste0("The heatmap represents the 7-day rolling average of the number of new confirmed cases per 100,000 population.\nLAs are ordered by the date at which they reached their peak number of new cases. Bars on the right represent the population of the LA.\nData updated to ", plotto, ". Data for most recent days is provisional and may be revised upwards as additional tests are processed."),
       caption="Data from PHW | Plot by @VictimOfMaths")+
  theme(axis.line.y=element_blank(), plot.subtitle=element_text(size=rel(0.78)), plot.title.position="plot",
        axis.text.y=element_text(colour="Black"))

ratebars.w <- ggplot(subset(data.w, date==maxcaseday), aes(x=pop, y=fct_reorder(name, maxcaseday), fill=pop))+
  geom_col(show.legend=FALSE)+
  theme_custom()+
  scale_fill_distiller(palette="Spectral")+
  scale_x_continuous(name="Population")+
  theme(axis.title.y=element_blank(), axis.line.y=element_blank(), axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), axis.text.x=element_text(colour="Black"))

agg_tiff("Outputs/COVIDLTLARatesHeatmapWal.tiff", units="in", width=12, height=6, res=500)
plot_grid(ratetiles.w, ratebars.w, align="h", rel_widths=c(1,0.2))
dev.off()

##########
#Scotland#
##########
data.s <- data %>% 
  group_by(name) %>% 
  filter(country=="Scotland" & name!="Scotland" & !is.na(casesroll_avg)) %>% 
  mutate(date=as.Date(date), maxcaserate=max(caserate_avg),
         maxcaseday=date[which(caserate_avg==maxcaserate)][1],
         maxcaseprop=caserate_avg/maxcaserate,
         totalcases=sum(cases))

#Enter dates to plot from and to
plotfrom <- "2020-03-01"
plotto <- max(data.s$date)

#Plot case trajectories
casetiles.s <- ggplot(data.s, aes(x=date, y=fct_reorder(name, maxcaseday), fill=maxcaseprop))+
  geom_tile(colour="White", show.legend=FALSE)+
  theme_custom()+
  scale_fill_distiller(palette="Spectral")+
  scale_y_discrete(name="", expand=c(0,0))+
  scale_x_date(name="Date", limits=as.Date(c(plotfrom, plotto)), expand=c(0,0), 
               breaks=pretty_breaks(n=interval(as.Date(plotfrom), plotto)%/% months(1)))+
  labs(title="Timelines for COVID-19 cases in Scottish Coucils",
       subtitle=paste0("The heatmap represents the 7-day rolling average of the number of new confirmed cases, normalised to the maximum value within the Council area.\nCouncils are ordered by the date at which they reached their peak number of new cases. Bars on the right represent the absolute number of cases in each Council area.\nData updated to ", plotto, ". Data for most recent days is provisional and may be revised upwards as additional tests are processed."),
       caption="Data from Public Health Scotland | Plot by @VictimOfMaths")+
  theme(axis.line.y=element_blank(), plot.subtitle=element_text(size=rel(0.78)), plot.title.position="plot",
        axis.text.y=element_text(colour="Black"), plot.title=element_text(face="bold", size=rel(1.2)))

casebars.s <- ggplot(subset(data.s, date==maxcaseday), aes(x=totalcases, y=fct_reorder(name, maxcaseday), fill=totalcases))+
  geom_col(show.legend=FALSE)+
  theme_custom()+
  scale_fill_distiller(palette="Spectral")+
  scale_x_continuous(name="Total confirmed cases")+
  theme(axis.title.y=element_blank(), axis.line.y=element_blank(), axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), axis.text.x=element_text(colour="Black"))

agg_tiff("Outputs/COVIDScottishCouncilCasesHeatmap.tiff", units="in", width=12, height=6, res=500)
plot_grid(casetiles.s, casebars.s, align="h", rel_widths=c(1,0.2))
dev.off()

agg_tiff("Outputs/COVIDScottishCouncilCaseRidges.tiff", units="in", width=10, height=6, res=500)
ggplot(data.s, aes(x=date, y=fct_reorder(name, totalcases), height=casesroll_avg, fill=casesroll_avg))+
  geom_density_ridges_gradient(stat="identity")+
  theme_custom()+
  scale_fill_distiller(palette="Spectral", name="Cases per day\n7-day rolling avg.")+
  scale_x_date(name="Date", limits=as.Date(c(plotfrom, plotto)), expand=c(0,0), 
               breaks=pretty_breaks(n=interval(as.Date(plotfrom), plotto)%/% months(1)))+
  scale_y_discrete(name="")+
  labs(title="Timelines of confirmed COVID-19 cases in Scottish Council areas",
       caption="Data from Public Health Scotland | Plot by @VictimOfMaths")
dev.off()

ratetiles.s <- ggplot(data.s, aes(x=date, y=fct_reorder(name, maxcaseday), fill=caserate_avg))+
  geom_tile(colour="White", show.legend=FALSE)+
  theme_custom()+
  scale_fill_distiller(palette="Spectral")+
  scale_y_discrete(name="", expand=c(0,0))+
  scale_x_date(name="Date", limits=as.Date(c(plotfrom, plotto)), expand=c(0,0), 
               breaks=pretty_breaks(n=interval(as.Date(plotfrom), plotto)%/% months(1)))+
  labs(title="Timelines for COVID-19 case rates in Council Areas in Scotland",
       subtitle=paste0("The heatmap represents the 7-day rolling average of the number of new confirmed cases per 100,000 population.\nLAs are ordered by the date at which they reached their peak number of new cases. Bars on the right represent the population of the LA.\nData updated to ", plotto, ". Data for most recent days is provisional and may be revised upwards as additional tests are processed."),
       caption="Data from PHS | Plot by @VictimOfMaths")+
  theme(axis.line.y=element_blank(), plot.subtitle=element_text(size=rel(0.78)), plot.title.position="plot",
        axis.text.y=element_text(colour="Black"))

ratebars.s <- ggplot(subset(data.s, date==maxcaseday), aes(x=pop, y=fct_reorder(name, maxcaseday), fill=pop))+
  geom_col(show.legend=FALSE)+
  theme_custom()+
  scale_fill_distiller(palette="Spectral")+
  scale_x_continuous(name="Population")+
  theme(axis.title.y=element_blank(), axis.line.y=element_blank(), axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), axis.text.x=element_text(colour="Black"))

agg_tiff("Outputs/COVIDLTLARatesHeatmapSco.tiff", units="in", width=12, height=6, res=500)
plot_grid(ratetiles.s, ratebars.s, align="h", rel_widths=c(1,0.2))
dev.off()

##########
#Northern Ireland#
##########
data.ni <- data %>% 
  group_by(name) %>% 
  filter(country=="Northern Ireland" & name!="Northern Ireland" & !is.na(casesroll_avg)) %>% 
  mutate(date=as.Date(date), maxcaserate=max(caserate_avg),
         maxcaseday=date[which(caserate_avg==maxcaserate)][1],
         maxcaseprop=caserate_avg/maxcaserate,
         totalcases=sum(cases))

#Enter dates to plot from and to
plotfrom <- "2020-03-01"
plotto <- max(data.ni$date)

#Plot case trajectories
casetiles.ni <- ggplot(data.ni, aes(x=date, y=fct_reorder(name, maxcaseday), fill=maxcaseprop))+
  geom_tile(colour="White", show.legend=FALSE)+
  theme_custom()+
  scale_fill_distiller(palette="Spectral")+
  scale_y_discrete(name="", expand=c(0,0))+
  scale_x_date(name="Date", limits=as.Date(c(plotfrom, plotto)), expand=c(0,0), 
               breaks=pretty_breaks(n=interval(as.Date(plotfrom), plotto)%/% months(2)))+
  labs(title="Timelines for COVID-19 cases in Northern Irish Local Authoritiess",
       subtitle=paste0("The heatmap represents the 7-day rolling average of the number of new confirmed cases, normalised to the maximum value within the Local Authority.\nAuthorities are ordered by the date at which they reached their peak number of new cases. Bars on the right represent the absolute number of cases in each Local Authority.\nData updated to ", plotto, ". Data for most recent days is provisional and may be revised upwards as additional tests are processed."),
       caption="Data from coronavirus.data.gov.uk | Plot by @VictimOfMaths")+
  theme(axis.line.y=element_blank(), plot.subtitle=element_text(size=rel(0.78)), plot.title.position="plot",
        axis.text.y=element_text(colour="Black"), plot.title=element_text(face="bold", size=rel(1.2)),
        axis.text.x=element_text(colour="Black"))

casebars.ni <- ggplot(subset(data.ni, date==maxcaseday), aes(x=totalcases, y=fct_reorder(name, maxcaseday), fill=totalcases))+
  geom_col(show.legend=FALSE)+
  theme_custom()+
  scale_fill_distiller(palette="Spectral")+
  scale_x_continuous(name="Total confirmed cases")+
  theme(axis.title.y=element_blank(), axis.line.y=element_blank(), axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), axis.text.x=element_text(colour="Black"))

agg_tiff("Outputs/COVIDNILACasesHeatmap.tiff", units="in", width=12, height=6, res=500)
plot_grid(casetiles.ni, casebars.ni, align="h", rel_widths=c(1,0.2))
dev.off()

agg_tiff("Outputs/COVIDNILACaseRidges.tiff", units="in", width=10, height=6, res=500)
ggplot(data.ni, aes(x=date, y=fct_reorder(name, totalcases), height=casesroll_avg, fill=casesroll_avg))+
  geom_density_ridges_gradient(stat="identity")+
  theme_custom()+
  scale_fill_distiller(palette="Spectral", name="Cases per day\n7-day rolling avg.")+
  scale_x_date(name="Date", limits=as.Date(c(plotfrom, plotto)), expand=c(0,0), 
               breaks=pretty_breaks(n=interval(as.Date(plotfrom), plotto)%/% months(2)))+
  scale_y_discrete(name="")+
  labs(title="Timelines of confirmed COVID-19 cases in Northern Irish Local Authorities",
       caption="Data from coronavirus.data.gov.uk | Plot by @VictimOfMaths")
dev.off()

ratetiles.ni <- ggplot(data.ni, aes(x=date, y=fct_reorder(name, maxcaseday), fill=caserate_avg))+
  geom_tile(colour="White", show.legend=FALSE)+
  theme_custom()+
  scale_fill_distiller(palette="Spectral")+
  scale_y_discrete(name="", expand=c(0,0))+
  scale_x_date(name="Date", limits=as.Date(c(plotfrom, plotto)), expand=c(0,0), 
               breaks=pretty_breaks(n=interval(as.Date(plotfrom), plotto)%/% months(1)))+
  labs(title="Timelines for COVID-19 case rates in Local Authorities in Northern Ireland",
       subtitle=paste0("The heatmap represents the 7-day rolling average of the number of new confirmed cases per 100,000 population.\nLAs are ordered by the date at which they reached their peak number of new cases. Bars on the right represent the population of the LA.\nData updated to ", plotto, ". Data for most recent days is provisional and may be revised upwards as additional tests are processed."),
       caption="Data from DoHNI | Plot by @VictimOfMaths")+
  theme(axis.line.y=element_blank(), plot.subtitle=element_text(size=rel(0.78)), plot.title.position="plot",
        axis.text.y=element_text(colour="Black"), plot.title=element_text(face="bold", size=rel(1.2)))

ratebars.ni <- ggplot(subset(data.ni, date==maxcaseday), aes(x=pop, y=fct_reorder(name, maxcaseday), fill=pop))+
  geom_col(show.legend=FALSE)+
  theme_custom()+
  scale_fill_distiller(palette="Spectral")+
  scale_x_continuous(name="Population")+
  theme(axis.title.y=element_blank(), axis.line.y=element_blank(), axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), axis.text.x=element_text(colour="Black"))

agg_tiff("Outputs/COVIDLTLARatesHeatmapNI.tiff", units="in", width=12, height=4, res=500)
plot_grid(ratetiles.ni, ratebars.ni, align="h", rel_widths=c(1,0.2))
dev.off()

############
#Animations#
############

#Hex map
data.hex <- data

#Sort out Buckinghamshire to match hex template
temp <- subset(data, code=="E06000060")

data.hex$code <- if_else(data$code=="E06000060", "E07000004", as.character(data$code))
data.hex$name <- if_else(data$name=="Buckinghamshire", "Aylesbury Vale", as.character(data$name))

temp1 <- temp
temp1$code <- "E07000005"
temp1$name <- "Chiltern"

temp2 <- temp
temp2$code <- "E07000006"
temp2$name <- "South Bucks"

temp$code <- "E07000007"
temp$name <- "Wycombe"

data.hex <- bind_rows(data.hex, temp, temp1, temp2)

#Bring in hexmap
#Read in hex boundaries (adapted from from https://olihawkins.com/2018/02/1 and ODI Leeds)
hex <- geojson_read("Data/UKLA.geojson", what="sp")

# Fortify into a data frame format to be shown with ggplot2
hexes <- tidy(hex, region="id")

hexes$id <- if_else(hexes$id=="E09000001", "E09000012", hexes$id)

data.hex <- left_join(hexes, data.hex, by=c("id"="code"), all.y=TRUE)
data.hex$date <- as.Date(data.hex$date)

#Remove Isles of Scilly which are too small to have their own data
data.hex <- subset(data.hex, id!="E06000053")

#extract latest date with full UK data
data.hex <- data.hex %>%
  group_by(country) %>%
  filter(country!="Republic of Ireland" & !is.na(casesroll_avg)) %>% 
  mutate(min=min(date), max=max(date))

completefrom <- max(data.hex$min, na.rm=TRUE)
completeto <- min(data.hex$max, na.rm=TRUE)

HexAnimUK <- ggplot()+
  geom_polygon(data=subset(data.hex, date>as.Date("2020-03-06") & date<=completeto), 
               aes(x=long, y=lat, group=id, fill=casesroll_avg))+
  coord_fixed()+
  scale_fill_distiller(palette="Spectral", name="Daily confirmed\ncases (7-day\nrolling avg.)", na.value="white")+
  theme_custom()+
  theme(axis.line=element_blank(), axis.ticks=element_blank(), axis.text=element_blank(),
        axis.title=element_blank(),  plot.title=element_text(face="bold"))+
  transition_time(date)+
  labs(title="Visualising the spread of COVID-19 across the UK",
       subtitle="Rolling 7-day average number of new confirmed cases.\nDate: {frame_time}",
       caption="Data from PHE, PHW, PHS & DoHNI\nVisualisation by @VictimOfMaths")

animate(HexAnimUK, duration=18, fps=10, width=2000, height=3000, res=300, renderer=gifski_renderer("Outputs/HexAnimUK.gif"), 
        device="ragg_png", end_pause=60)

#Rates version
HexAnimUKrate <- ggplot()+
  geom_polygon(data=subset(data.hex, date>as.Date("2020-03-06") & date<=completeto), 
               aes(x=long, y=lat, group=id, fill=caserate_avg))+
  coord_fixed()+
  scale_fill_distiller(palette="Spectral", name="Daily confirmed\ncases/100,000\n(7-day rolling avg.)", na.value="white")+
  theme_custom()+
  theme(axis.line=element_blank(), axis.ticks=element_blank(), axis.text=element_blank(),
        axis.title=element_blank(),  plot.title=element_text(face="bold"))+
  transition_time(date)+
  labs(title="Visualising the spread of COVID-19 across the UK",
       subtitle="Rolling 7-day average number of new confirmed cases per 100,000.\nDate: {frame_time}",
       caption="Data from PHE, PHW, PHS & DoHNI\nVisualisation by @VictimOfMaths")

animate(HexAnimUKrate, duration=18, fps=10, width=2000, height=3000, res=300, 
        renderer=gifski_renderer("Outputs/HexAnimUKrate.gif"), 
        device="ragg_png", end_pause=60)

#Chloropeth map
data.map <- subset(data, as.Date(date) %within% interval(completefrom,completeto)) %>% 
  filter(!Region %in% c("Nation", "Region"))

#Sort out Buckinghamshire to match hex template
temp <- subset(data.map, code=="E06000060")

data.map$code <- if_else(data.map$code=="E06000060", "E07000004", as.character(data.map$code))
data.map$name <- if_else(data.map$name=="Buckinghamshire", "Aylesbury Vale", as.character(data.map$name))

temp1 <- temp
temp1$code <- "E07000005"
temp1$name <- "Chiltern"

temp2 <- temp
temp2$code <- "E07000006"
temp2$name <- "South Bucks"

temp$code <- "E07000007"
temp$name <- "Wycombe"

data.map <- bind_rows(data.map, temp, temp1, temp2)

temp <- tempfile()
temp2 <- tempfile()
source <- "https://opendata.arcgis.com/datasets/1d78d47c87df4212b79fe2323aae8e08_0.zip?outSR=%7B%22latestWkid%22%3A27700%2C%22wkid%22%3A27700%7D"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)

#The actual shapefile has a different name each time you download it, so need to fish the name out of the unzipped file
name <- list.files(temp2, pattern=".shp")
shapefile <- st_read(file.path(temp2, name))

names(shapefile)[names(shapefile) == "lad19cd"] <- "code"

simplemap <- ms_simplify(shapefile, keep=0.2, keep_shapes = TRUE)

#TODO fix Northamptonshire in these maps

map.cases <- full_join(simplemap, data.map, by="code", all.y=TRUE)
map.cases$date <- as.Date(map.cases$date)

#Map of current cases
agg_tiff("Outputs/COVIDCaseMapUK.tiff", units="in", width=8, height=12, res=500)
map.cases %>% 
  filter(date==completeto-days(3) & !name %in% c("England", "Wales", "Northern Ireland", "Scotland")) %>% 
  ggplot()+
  geom_sf(aes(geometry=geometry, fill=casesroll_avg), colour=NA)+
  scale_fill_distiller(palette="Spectral", name="Daily cases\n(rolling 7-day avg.)")+
  theme_custom()+
  theme(axis.line=element_blank(), axis.ticks=element_blank(), axis.text=element_blank(),
        axis.title=element_blank(), plot.title=element_text(face="bold", size=rel(1.2)))+
  labs(title="Confirmed new COVID-19 cases in the UK",
       subtitle=paste0("Rolling 7-day average of confirmed new cases at Local Authority/Council Area level\nData up to ", completeto-days(3)),
       caption="Data from PHE, PHW, PHS & DoHNI | Plot by @VictimOfMaths")
dev.off()

agg_tiff("Outputs/COVIDCaserateMapUK.tiff", units="in", width=8, height=12, res=500)
map.cases %>% 
  filter(date==completeto-days(3) & !name %in% c("England", "Wales", "Northern Ireland", "Scotland")) %>% 
  ggplot()+
  geom_sf(aes(geometry=geometry, fill=caserate_avg), colour=NA)+
  scale_fill_distiller(palette="Spectral", name="Daily cases\nper 100,000\n(rolling 7-day avg.)")+
  theme_custom()+
  theme(axis.line=element_blank(), axis.ticks=element_blank(), axis.text=element_blank(),
        axis.title=element_blank(), plot.title=element_text(face="bold", size=rel(1.2)))+
  labs(title="Rates of confirmed new COVID-19 cases in the UK",
       subtitle=paste0("Rolling 7-day average of confirmed new cases per 100,000 at Local Authority/Council Area level\nData up to ", completeto-days(3)),
       caption="Data from PHE, PHW, PHS & DoHNI | Plot by @VictimOfMaths")
dev.off()

#Map of admission rates
admmaxdate <- max(map.cases$date[!is.na(map.cases$admrate_avg)])

agg_tiff("Outputs/COVIDAdmrateMapUK.tiff", units="in", width=8, height=7.5, res=500)
map.cases %>% 
  filter(date==admmaxdate & !name %in% c("England", "Wales", "Northern Ireland", "Scotland")) %>% 
  ggplot()+
  geom_sf(aes(geometry=geometry, fill=admrate_avg), colour=NA)+
  scale_fill_distiller(palette="Spectral", name="Daily admissions\nper 100,000",
                       na.value="transparent")+
  xlim(-116,655644)+
  ylim(5337,620000)+
  theme_classic()+
  theme(axis.line=element_blank(), axis.ticks=element_blank(), axis.text=element_blank(),
        axis.title=element_blank(), plot.title=element_text(face="bold", size=rel(1.2)))+
  labs(title="Rates of confirmed new COVID-19 admissions in England",
       subtitle=paste0("Rolling 7-day average of confirmed new hospital admissions with COVID-19 per 100,000 at Local Authority level\nData up to ", admmaxdate),
       caption="Data from NHS England | Plot by @VictimOfMaths")
dev.off()

###########################
#UKLTLA heatmap ordered from S->N
lat.data <- map.cases %>% 
  select(code, lat) %>% 
  as.data.frame() %>% 
  select(code, lat) %>% 
  distinct() %>% 
  filter(!is.na(code) & !code %in% c("E06000062", "E06000061")) %>% 
  #Fix Northamptonshire LA changes (use Northampton and Kettering as the latitudes for the new counties)
  bind_rows(map.cases %>% filter(code %in% c("E07000154", "E07000153")) %>% 
              as.data.frame() %>% 
              select(code, lat) %>% 
              mutate(code=if_else(code=="E07000154", "E06000062", "E06000061")))

data.all2 <- merge(data.all, lat.data)

#Plot case trajectories
casetiles.all2 <- ggplot(data.all2, aes(x=date, y=fct_reorder(name, lat), fill=maxcaseprop))+
  geom_tile(colour="White", show.legend=FALSE)+
  theme_custom()+
  scale_fill_distiller(palette="Spectral")+
  scale_y_discrete(name="", expand=c(0,0))+
  scale_x_date(name="Date", limits=as.Date(c(plotfrom, plotto)), expand=c(0,0), 
               breaks=pretty_breaks(n=interval(as.Date(plotfrom), plotto)%/% months(1)))+
  labs(title="Timelines for COVID-19 cases in Local Authorities/Council Areas across the UK",
       subtitle=paste0("The heatmap represents the 7-day rolling average of the number of new confirmed cases, normalised to the maximum value within the Local Authority.\nLAs are ordered from North to South. Bars on the right represent the absolute number of cases in each LA.\nData updated to ", plotto, ". Data for most recent days is provisional and may be revised upwards as additional tests are processed."),
       caption="Data from PHE, PHW, PHS & DoHNI | Plot by @VictimOfMaths")+
  theme(axis.line.y=element_blank(), plot.subtitle=element_text(size=rel(1.6)), plot.title.position="plot",
        axis.text.y=element_text(colour="Black"), plot.title=element_text(face="bold", size=rel(2.5)))

casebars.all2 <- ggplot(subset(data.all2, date==maxcaseday), aes(x=totalcases, y=fct_reorder(name, lat), fill=totalcases))+
  geom_col(show.legend=FALSE)+
  theme_custom()+
  scale_fill_distiller(palette="Spectral")+
  scale_x_continuous(name="Total confirmed cases")+
  theme(axis.title.y=element_blank(), axis.line.y=element_blank(), axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), axis.text.x=element_text(colour="Black"))

agg_tiff("Outputs/COVIDLTLACasesHeatmapUKOrdered.tiff", units="in", width=25, height=30, res=500)
plot_grid(casetiles.all2, casebars.all2, align="h", rel_widths=c(1,0.2))
dev.off()

agg_png("Outputs/COVIDLTLACasesHeatmapUKOrdered.png", units="in", width=25, height=30, res=500)
plot_grid(casetiles.all2, casebars.all2, align="h", rel_widths=c(1,0.2))
dev.off()

ratetiles.all2 <- ggplot(data.all2, aes(x=date, y=fct_reorder(name, lat), fill=caserate_avg))+
  geom_tile(colour="White", show.legend=FALSE)+
  theme_custom()+
  scale_fill_distiller(palette="Spectral")+
  scale_y_discrete(name="", expand=c(0,0))+
  scale_x_date(name="Date", limits=as.Date(c(plotfrom, plotto)), expand=c(0,0), 
               breaks=pretty_breaks(n=interval(as.Date(plotfrom), plotto)%/% months(1)))+
  labs(title="Timelines for COVID-19 cases in Local Authorities/Council Areas across the UK",
       subtitle=paste0("The heatmap represents the 7-day rolling average of the number of new confirmed cases per 100,000.\nLAs are ordered from North to South. Bars on the right represent the population each LA.\nData updated to ", plotto, ". Data for most recent days is provisional and may be revised upwards as additional tests are processed."),
       caption="Data from PHE, PHW, PHS & DoHNI | Plot by @VictimOfMaths")+
  theme(axis.line.y=element_blank(), plot.subtitle=element_text(size=rel(0.78)), plot.title.position="plot",
        axis.text.y=element_text(colour="Black"), plot.title=element_text(face="bold", size=rel(1.2)))

ratebars.all2 <- ggplot(subset(data.all2, date==maxcaseday), aes(x=pop, y=fct_reorder(name, lat), fill=pop))+
  geom_col(show.legend=FALSE)+
  theme_custom()+
  scale_fill_distiller(palette="Spectral")+
  scale_x_continuous(name="Total population")+
  theme(axis.title.y=element_blank(), axis.line.y=element_blank(), axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), axis.text.x=element_text(colour="Black"))

agg_tiff("Outputs/COVIDLTLARatesHeatmapUKOrdered.tiff", units="in", width=16, height=30, res=500)
plot_grid(ratetiles.all2, ratebars.all2, align="h", rel_widths=c(1,0.2))
dev.off()

agg_png("Outputs/COVIDLTLARatesHeatmapUKOrdered.png", units="in", width=16, height=30, res=500)
plot_grid(ratetiles.all2, ratebars.all2, align="h", rel_widths=c(1,0.2))
dev.off()

#Order admissions map for England only
admtiles.e2 <- ggplot(subset(data.all2, !is.na(admrate_avg)), aes(x=date, y=fct_reorder(name, lat), 
                                                                  fill=admrate_avg))+
  geom_tile(colour="White", show.legend=FALSE)+
  theme_custom()+
  scale_fill_distiller(palette="Spectral")+
  scale_y_discrete(name="", expand=c(0,0))+
  scale_x_date(name="Date", limits=as.Date(c(as.Date("2020-08-03"), plotadmto)), expand=c(0,0), 
               breaks=pretty_breaks(n=interval(as.Date(plotfrom), plotadmto)%/% months(1)))+
  labs(title="Timelines for COVID-19 admissions in Local Authorities in England",
       subtitle=paste0("The heatmap represents the 7-day rolling average of the number of new confirmed hospital admissions per 100,000 population.\nLAs are ordered from North to South. Bars on the right represent the population of the LA.\nData updated to ", 
                       plotto, ". Data for most recent days is provisional and may be revised upwards as additional tests are processed.\nAdmissions are defined as patients admitted with a positive COVID-19 diagnosis, or those diagnosed in hospital"),
       caption="Data from NHS England | Plot by @VictimOfMaths")+
  theme(axis.line.y=element_blank(), plot.subtitle=element_text(size=rel(0.78)), plot.title.position="plot",
        axis.text.y=element_text(colour="Black"), plot.title=element_text(face="bold", size=rel(1.2)))

admbars.e2 <- ggplot(subset(data.all2, !is.na(admrate_avg) & date==maxadmday), 
                     aes(x=pop, y=fct_reorder(name, lat), fill=pop))+
  geom_col(show.legend=FALSE)+
  theme_custom()+
  scale_fill_distiller(palette="Spectral")+
  scale_x_continuous(name="Total population")+
  theme(axis.title.y=element_blank(), axis.line.y=element_blank(), axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), axis.text.x=element_text(colour="Black"))

agg_tiff("Outputs/COVIDLTLAAdmRatesHeatmapEngOrdered.tiff", units="in", width=10, height=26, res=500)
plot_grid(admtiles.e2, admbars.e2, align="h", rel_widths=c(1,0.2))
dev.off()

agg_png("Outputs/COVIDLTLAAdmRatesHeatmapEngOrdered.png", units="in", width=10, height=26, res=500)
plot_grid(admtiles.e2, admbars.e2, align="h", rel_widths=c(1,0.2))
dev.off()


#Map of changes in admission and case rates in the last 7 days:
data.map2 <- data.map %>% 
  filter(country=="England") %>% 
  mutate(date=as.Date(date)) %>% 
  arrange(code, date) %>% 
  group_by(code) %>% 
  mutate(admchange=admrate_avg-lag(admrate_avg,7)) %>% 
  filter(date==plotadmto)

data.map3 <- data.map %>% 
  mutate(date=as.Date(date)) %>% 
  arrange(code, date) %>% 
  group_by(code) %>% 
  mutate(casechange=caserate_avg-lag(caserate_avg,7)) %>% 
  filter(date==plotto-days(3))

map.admchange <- full_join(simplemap, data.map2, by="code", all.y=TRUE) %>% 
  filter(!is.na(admchange))
map.admchange$date <- as.Date(map.admchange$date)


map.casechange <- full_join(simplemap, data.map3, by="code", all.y=TRUE) %>% 
  filter(!is.na(casechange)) 
map.casechange$date <- as.Date(map.casechange$date)

#Map of week-on-week change in cases
agg_tiff("Outputs/COVIDCasesChangeMapUK.tiff", units="in", width=8, height=10, res=500)
map.casechange %>% 
  ggplot()+
  geom_sf(aes(geometry=geometry, fill=casechange), colour=NA)+
  scale_fill_paletteer_c("scico::roma", limit=c(-1,1)*max(abs(map.casechange$casechange)), 
                         name="Change in cases\nper day per 100,000\nin the past week", direction=-1,
                         na.value="transparent")+
  theme_custom()+
  theme(axis.line=element_blank(), axis.ticks=element_blank(), axis.text=element_blank(),
        axis.title=element_blank(), plot.title=element_text(face="bold", size=rel(1.5)))+
  labs(title="Changes in COVID-19 cases across the UK",
       subtitle=paste0("Change in the rolling 7-day average rate of new confirmed COVID-19\nbetween ", plotto-days(10) , " and ", plotto-days(3)),
       caption="Data from coronavirus.data.gov.uk | Plot by @VictimOfMaths")
dev.off()

#Map of week-on-week change in admissions
agg_tiff("Outputs/COVIDAdmChangeMap.tiff", units="in", width=8, height=8, res=500)
map.admchange %>% 
  ggplot()+
  geom_sf(aes(geometry=geometry, fill=admchange), colour=NA)+
  scale_fill_paletteer_c("scico::roma", limit=c(-1,1)*max(abs(map.admchange$admchange)), 
                         name="Change in admissions\nper day per 100,000\nin the past week", direction=-1,
                         na.value="transparent")+
  theme_custom()+
  theme(axis.line=element_blank(), axis.ticks=element_blank(), axis.text=element_blank(),
        axis.title=element_blank(), plot.title=element_text(face="bold", size=rel(1.5)))+
  labs(title="Changes in COVID-19 hospital admissions across England",
       subtitle=paste0("Change in the rolling 7-day average rate of new admissions between ", plotadmto-days(7), " and ", plotadmto),
       caption="Data from NHS England | Plot by @VictimOfMaths")
dev.off()

#Line chart of cases by country
agg_tiff("Outputs/COVIDCaserateUK.tiff", units="in", width=12, height=6, res=500)
ggplot(subset(data, Region=="Nation" & as.Date(date)>as.Date("2021-01-01") & as.Date(date)<plotto-days(1)))+
  geom_line(aes(x=as.Date(date), y=caserate_avg, colour=country, group=country))+
  scale_x_date(name="", date_breaks="1 week", date_labels="%d %b")+
  scale_y_continuous(name="Daily cases per 100,000")+
  scale_colour_paletteer_d("fishualize::Scarus_quoyi", name="")+
  theme_custom()+
  labs(title="Scotland's rise in cases is, quite something",
       subtitle="Rolling 7-day average of daily confirmed new cases per 100,000",
       caption="Data from coronavirus.data.gov.uk | Plot by @VictimOfMaths")
dev.off()

#Line chart of cases by region
agg_tiff("Outputs/COVIDCaseRateReg.tiff", units="in", width=12, height=6, res=500)
ggplot(subset(data, Region=="Region" & as.Date(date)>as.Date("2021-01-01") & as.Date(date)<plotto-days(1)))+
  geom_line(aes(x=as.Date(date), y=caserate_avg, colour=name, group=name))+
  scale_x_date(name="", date_breaks="1 week", date_labels="%d %b")+
  scale_y_continuous(name="Daily cases per 100,000")+
  scale_colour_paletteer_d("LaCroixColoR::paired", name="")+
  theme_custom()+
  theme(plot.title=element_text(face="bold", size=rel(1.2)),
        axis.text.x=element_text(angle=45, hjust=1),
        text=element_text(family="Roboto"))+
  labs(title="COVID case rates in England have separated into regional groups. Again.",
       subtitle="Rolling 7-day average of daily confirmed new cases per 100,000",
       caption="Data from coronavirus.data.gov.uk | Plot by @VictimOfMaths")
dev.off()

agg_tiff("Outputs/COVIDCaseRateRegxRestrictions.tiff", units="in", width=12, height=6, res=500)
ggplot(subset(data, Region=="Region" & as.Date(date)>as.Date("2020-10-01") & as.Date(date)<plotto-days(1)))+
  geom_rect(aes(xmin=as.Date("2021-01-06"), xmax=plotto,
                ymin=-Inf, ymax=Inf), fill="Grey80")+
  geom_rect(aes(xmin=as.Date("2020-11-05"), xmax=as.Date("2020-12-02"),
                ymin=-Inf, ymax=Inf), fill="Grey80")+
  geom_vline(xintercept=as.Date("2020-12-25"), colour="Red", linetype=2)+
  geom_line(aes(x=as.Date(date), y=caserate_avg, colour=name, group=name))+
  scale_x_date(name="", date_breaks="1 week", date_labels="%d %b")+
  scale_y_continuous(name="Daily cases per 100,000")+
  scale_colour_paletteer_d("LaCroixColoR::paired", name="")+
  theme_custom()+
  theme(plot.title=element_text(face="bold", size=rel(1.2)),
        text=element_text(family="Roboto"))+
  labs(title="COVID-19 case rates and lockdown restrictions in England",
       subtitle="Rolling 7-day average of daily confirmed new cases per 100,000",
       caption="Data from coronavirus.data.gov.uk | Plot by @VictimOfMaths")+
  annotate("text", x=as.Date("2020-11-18"), y=160, label="2nd national lockdown")+
  annotate("text", x=as.Date("2021-01-20"), y=160, label="3rd national lockdown")+
  annotate("text", x=as.Date("2020-12-24"), y=160, label="Christmas day",
           colour="Red", hjust=1)
dev.off()

agg_tiff("Outputs/COVIDCaseRateRegLog.tiff", units="in", width=12, height=6, res=500)
ggplot(subset(data, Region=="Region" & as.Date(date)>as.Date("2021-01-01") & as.Date(date)<plotto-days(1)))+
  geom_line(aes(x=as.Date(date), y=caserate_avg, colour=name, group=name))+
  scale_x_date(name="", date_breaks="1 week", date_labels="%d %b")+
  scale_y_continuous(name="Daily cases per 100,000 (log scale)", trans="log")+
  scale_colour_paletteer_d("LaCroixColoR::paired", name="")+
  theme_custom()+
  theme(plot.title=element_text(face="bold", size=rel(1.2)),
        text=element_text(family="Roboto"))+
  labs(title="COVID case rates are falling at different speeds in different parts of England",
       subtitle="Rolling 7-day average of daily confirmed new cases per 100,000",
       caption="Data from coronavirus.data.gov.uk | Plot by @VictimOfMaths")
dev.off()

#Whole UK bar chart
bardata <- data %>% 
  filter(Region=="Nation") %>% 
  mutate(date=as.Date(date)) %>% 
  group_by(date) %>% 
  summarise(cases=sum(cases), casesroll_avg=sum(casesroll_avg))

agg_tiff("Outputs/COVIDCaseNumbersUK.tiff", units="in", width=8, height=6, res=500)
ggplot()+
  geom_col(data=subset(bardata, date<=plotto),aes(x=date, y=cases), fill="skyblue2")+
  geom_line(data=subset(bardata, date<=plotto-days(3)), aes(x=date, y=casesroll_avg), colour="red")+
  scale_x_date(name="", 
               breaks=pretty_breaks(n=interval(as.Date(plotfrom), plotto)%/% months(1)))+
  scale_y_continuous(name="Daily new cases")+
  theme_custom()+
  theme(plot.title=element_text(face="bold", size=rel(1.2)),
        plot.subtitle=element_markdown(),
        text=element_text(family="Roboto"), axis.text.x=element_text(angle=45, hjust=1, vjust=1))+
  labs(title="The fall in new COVID cases has stalled",
       subtitle="Daily <span style='color:SkyBlue4;'>confirmed new cases </span>and the <span style='color:red;'>rolling 7-day average</span> across the UK",
       caption="Date from coronavirus.gov.uk | Plot by @VictimOfMaths")
dev.off()

#Map of COVID deaths rates and changes in COVID death rates
maxweek <- 56

#Collapse to LA level
mortdata <- mortdata %>% 
  group_by(week, name, code) %>% 
  filter(measure=="Registrations") %>% 
  summarise(deaths=sum(COVID.20), pop=unique(pop)) %>% 
  group_by(name, code) %>% 
  mutate(mortrate=deaths*100000/pop,
         mortratechange=mortrate-lag(mortrate, 1, order_by=week)) %>% 
  ungroup() %>% 
  filter(week==maxweek)

#Sort out Buckinghamshire to match map template
temp <- subset(mortdata, code=="E06000060")

mortdata$code <- if_else(mortdata$code=="E06000060", "E07000004", as.character(mortdata$code))
mortdata$name <- if_else(mortdata$name=="Buckinghamshire", "Aylesbury Vale", as.character(mortdata$name))

temp1 <- temp
temp1$code <- "E07000005"
temp1$name <- "Chiltern"

temp2 <- temp
temp2$code <- "E07000006"
temp2$name <- "South Bucks"

temp$code <- "E07000007"
temp$name <- "Wycombe"

mortdata <- bind_rows(mortdata, temp, temp1, temp2)

mortmap.data <- full_join(simplemap, mortdata, by="code", all.y=TRUE)

#Map of death rates
agg_tiff("Outputs/COVIDMortrateMapUK.tiff", units="in", width=8, height=7.5, res=500)
mortmap.data %>% 
  ggplot()+
  geom_sf(aes(geometry=geometry, fill=mortrate), colour=NA)+
  scale_fill_distiller(palette="Spectral", name="COVID-19 deaths\nper 100,000",
                       na.value="transparent")+
  theme_custom()+
  theme(axis.line=element_blank(), axis.ticks=element_blank(), axis.text=element_blank(),
        axis.title=element_blank(), plot.title=element_text(face="bold", size=rel(1.2)))+
  labs(title="Rates of confirmed COVID-19 deaths in Great Britain",
       subtitle=paste0("Weekly rates of deaths recorded as due to COVID-19 on the death certificate\nData up to ", as.Date("2020-01-03")+weeks(maxweek-1)),
       caption="Data from ONS & NRS | Plot by @VictimOfMaths")

dev.off()

#Map of week-on-week change in death rates
agg_tiff("Outputs/COVIDMortChangeMap.tiff", units="in", width=8, height=8, res=500)
mortmap.data %>% 
  ggplot()+
  geom_sf(aes(geometry=geometry, fill=mortratechange), colour=NA)+
  scale_fill_paletteer_c("scico::roma", limit=c(-1,1)*max(abs(mortmap.data$mortratechange)), 
                         name="Change in deaths\nper week per 100,000\nvs. the previous week", direction=-1,
                         na.value="transparent")+
  theme_custom()+
  theme(axis.line=element_blank(), axis.ticks=element_blank(), axis.text=element_blank(),
        axis.title=element_blank(), plot.title=element_text(face="bold", size=rel(1.5)))+
  labs(title="Changes in confirmed COVID-19 deaths in Great Britain",
       subtitle=paste0("Change in the rates of deaths recorded as due to COVID-19 between weeks ", maxweek, " & ",maxweek-1,"\nData up to ", as.Date("2020-01-03")+weeks(maxweek-1)),
       caption="Data from ONS & NRS | Plot by @VictimOfMaths")
dev.off()

####################################################################################################




#These last 2 animations require a more powerful computer/more patience than I have, so I'm
#not 100% certain they actually work...

CaseAnimAbs <- map.cases %>% 
  filter(!name %in% c("England", "Wales", "Northern Ireland", "Scotland") & date>as.Date("2020-02-25")) %>% 
  ggplot(aes(geometry=geometry, fill=casesroll_avg))+
  geom_sf(colour=NA)+
  theme_classic()+
  scale_fill_distiller(palette="Spectral", name="Daily cases\n(rolling 7-day avg.)")+
  theme(axis.line=element_blank(), axis.ticks=element_blank(), axis.text=element_blank(),
        axis.title=element_blank(),  plot.title=element_text(face="bold"))+
  transition_time(date)+
  labs(title="Visualising the spread of the pandemic across England",
       subtitle="Rolling 7-day average number of new confirmed cases in each Local Authority/Council area\nDate: {frame_time}",
       caption="Data from PHE, PHW, PHS & DoHNI | Visualisation by @VictimOfMaths")

animate(CaseAnimAbs, duration=25, fps=2, width=2000, height=3000, res=100, renderer=gifski_renderer("Outputs/CaseAnimAbs.gif"), end_pause=60)

CaseAnimRate <- map.cases %>% 
  filter(!name %in% c("England", "Wales", "Northern Ireland", "Scotland") & !is.na(date)) %>% 
  ggplot(aes(geometry=geometry, fill=caserate_avg))+
  geom_sf(colour=NA)+
  theme_classic()+
  scale_fill_distiller(palette="Spectral", name="Daily cases\nper 100,000\n(rolling 7-day avg.)")+
  theme(axis.line=element_blank(), axis.ticks=element_blank(), axis.text=element_blank(),
        axis.title=element_blank(),  plot.title=element_text(face="bold"))+
  transition_time(date)+
  labs(title="Visualising the spread of the pandemic across England",
       subtitle="Rolling 7-day average rate of new confirmed cases per 100,000 in each Local Authority/Council area\nDate: {frame_time}",
       caption="Data from PHE, PHW, PHS & DoHNI | Visualisation by @VictimOfMaths")

animate(CaseAnimRate, duration=25, fps=2, width=2000, height=3000, res=100, renderer=gifski_renderer("Outputs/CaseAnimRate.gif"), end_pause=60)
