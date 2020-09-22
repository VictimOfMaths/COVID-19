rm(list=ls())

library(tidyverse)
library(forcats)
library(cowplot)
library(ggridges)
library(geojsonio)
library(broom)
library(sf)
library(curl)
library(rmapshaper)
library(gganimate)

#Read in data created by COVID_LA_Plots/UnderlyingCode.R, which lives here:
#https://github.com/VictimOfMaths/COVID_LA_Plots/blob/master/UnderlyingCode.R

data <- read.csv("COVID_LA_Plots/LACases.csv")[,-c(1,7,8,9)]

#EVERYWHERE
data.all <- data %>% 
  group_by(name) %>% 
  filter(!name %in% c("England", "Wales", "Scotland", "Northern Ireland")) %>% 
  mutate(date=as.Date(date), maxcaserate=max(caserate_avg),
         maxcaseday=date[which(caserate_avg==maxcaserate)][1],
         maxcaseprop=caserate_avg/maxcaserate,
         totalcases=sum(cases))

plotfrom <- "2020-03-01"
plotto <- max(data.all$date)

#Plot case trajectories
casetiles.all <- ggplot(data.all, aes(x=date, y=fct_reorder(name, maxcaseday), fill=maxcaseprop))+
  geom_tile(colour="White", show.legend=FALSE)+
  theme_classic()+
  scale_fill_distiller(palette="Spectral")+
  scale_y_discrete(name="", expand=c(0,0))+
  scale_x_date(name="Date", limits=as.Date(c(plotfrom, plotto)), expand=c(0,0))+
  labs(title="Timelines for COVID-19 cases in Local Authorities/Council Areas across the UK",
       subtitle=paste0("The heatmap represents the 7-day rolling average of the number of new confirmed cases, normalised to the maximum value within the Local Authority.\nLAs are ordered by the date at which they reached their peak number of new cases. Bars on the right represent the absolute number of cases in each LA.\nData updated to ", plotto, ". Data for most recent days is provisional and may be revised upwards as additional tests are processed."),
       caption="Data from PHE, PHW, PHS & DoHNI | Plot by @VictimOfMaths")+
  theme(axis.line.y=element_blank(), plot.subtitle=element_text(size=rel(0.78)), plot.title.position="plot",
        axis.text.y=element_text(colour="Black"))

casebars.all <- ggplot(subset(data.all, date==maxcaseday), aes(x=totalcases, y=fct_reorder(name, maxcaseday), fill=totalcases))+
  geom_col(show.legend=FALSE)+
  theme_classic()+
  scale_fill_distiller(palette="Spectral")+
  scale_x_continuous(name="Total confirmed cases")+
  theme(axis.title.y=element_blank(), axis.line.y=element_blank(), axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), axis.text.x=element_text(colour="Black"))

tiff("Outputs/COVIDLTLACasesHeatmapUK.tiff", units="in", width=16, height=30, res=500)
plot_grid(casetiles.all, casebars.all, align="h", rel_widths=c(1,0.2))
dev.off()

png("Outputs/COVIDLTLACasesHeatmapUK.png", units="in", width=16, height=30, res=500)
plot_grid(casetiles.all, casebars.all, align="h", rel_widths=c(1,0.2))
dev.off()

#Rate trajectories

ratetiles.all <- ggplot(data.all, aes(x=date, y=fct_reorder(name, maxcaseday), fill=casesroll_avg))+
  geom_tile(colour="White", show.legend=FALSE)+
  theme_classic()+
  scale_fill_distiller(palette="Spectral")+
  scale_y_discrete(name="", expand=c(0,0))+
  scale_x_date(name="Date", limits=as.Date(c(plotfrom, plotto)), expand=c(0,0))+
  labs(title="Timelines for COVID-19 case rates in Local Authorities/Council Areas across the UK",
       subtitle=paste0("The heatmap represents the 7-day rolling average of the number of new confirmed cases per 100,000 population.\nLAs are ordered by the date at which they reached their peak number of new cases. Bars on the right represent the population of the LA.\nData updated to ", plotto, ". Data for most recent days is provisional and may be revised upwards as additional tests are processed."),
       caption="Data from PHE, PHW, PHS & DoHNI | Plot by @VictimOfMaths")+
  theme(axis.line.y=element_blank(), plot.subtitle=element_text(size=rel(0.78)), plot.title.position="plot",
        axis.text.y=element_text(colour="Black"))

ratebars.all <- ggplot(subset(data.all, date==maxcaseday), aes(x=pop, y=fct_reorder(name, maxcaseday), fill=pop))+
  geom_col(show.legend=FALSE)+
  theme_classic()+
  scale_fill_distiller(palette="Spectral")+
  scale_x_continuous(name="Population")+
  theme(axis.title.y=element_blank(), axis.line.y=element_blank(), axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), axis.text.x=element_text(colour="Black"))

tiff("Outputs/COVIDLTLARatesHeatmapUK.tiff", units="in", width=16, height=30, res=500)
plot_grid(ratetiles.all, ratebars.all, align="h", rel_widths=c(1,0.2))
dev.off()

png("Outputs/COVIDLTLARatesHeatmapUK.png", units="in", width=16, height=30, res=500)
plot_grid(ratetiles.all, ratebars.all, align="h", rel_widths=c(1,0.2))
dev.off()

#########
#ENGLAND#
#########
data.e <- data %>% 
  group_by(name) %>% 
  filter(country=="England" & name!="England") %>% 
  mutate(date=as.Date(date), maxcaserate=max(caserate_avg),
         maxcaseday=date[which(caserate_avg==maxcaserate)][1],
         maxcaseprop=caserate_avg/maxcaserate,
         totalcases=sum(cases))

#Enter dates to plot from and to
plotfrom <- "2020-03-01"
plotto <- max(data.e$date)

#Plot case trajectories
casetiles.e <- ggplot(data.e, aes(x=date, y=fct_reorder(name, maxcaseday), fill=maxcaseprop))+
  geom_tile(colour="White", show.legend=FALSE)+
  theme_classic()+
  scale_fill_distiller(palette="Spectral")+
  scale_y_discrete(name="", expand=c(0,0))+
  scale_x_date(name="Date", limits=as.Date(c(plotfrom, plotto)), expand=c(0,0))+
  labs(title="Timelines for COVID-19 cases in English Local Authorities",
       subtitle=paste0("The heatmap represents the 7-day rolling average of the number of new confirmed cases, normalised to the maximum value within the Local Authority.\nLAs are ordered by the date at which they reached their peak number of new cases. Bars on the right represent the absolute number of cases in each LA.\nData updated to ", plotto, ". Data for most recent days is provisional and may be revised upwards as additional tests are processed."),
       caption="Data from Public Health England | Plot by @VictimOfMaths")+
  theme(axis.line.y=element_blank(), plot.subtitle=element_text(size=rel(0.78)), plot.title.position="plot",
        axis.text.y=element_text(colour="Black"))

casebars.e <- ggplot(subset(data.e, date==maxcaseday), aes(x=totalcases, y=fct_reorder(name, maxcaseday), fill=totalcases))+
  geom_col(show.legend=FALSE)+
  theme_classic()+
  scale_fill_distiller(palette="Spectral")+
  scale_x_continuous(name="Total confirmed cases")+
  theme(axis.title.y=element_blank(), axis.line.y=element_blank(), axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), axis.text.x=element_text(colour="Black"))

tiff("Outputs/COVIDLTLACasesHeatmap.tiff", units="in", width=16, height=30, res=500)
plot_grid(casetiles.e, casebars.e, align="h", rel_widths=c(1,0.2))
dev.off()

png("Outputs/COVIDLTLACasesHeatmap.png", units="in", width=16, height=30, res=500)
plot_grid(casetiles.e, casebars.e, align="h", rel_widths=c(1,0.2))
dev.off()

ratetiles.e <- ggplot(data.e, aes(x=date, y=fct_reorder(name, maxcaseday), fill=casesroll_avg))+
  geom_tile(colour="White", show.legend=FALSE)+
  theme_classic()+
  scale_fill_distiller(palette="Spectral")+
  scale_y_discrete(name="", expand=c(0,0))+
  scale_x_date(name="Date", limits=as.Date(c(plotfrom, plotto)), expand=c(0,0))+
  labs(title="Timelines for COVID-19 case rates in Local Authorities in England",
       subtitle=paste0("The heatmap represents the 7-day rolling average of the number of new confirmed cases per 100,000 population.\nLAs are ordered by the date at which they reached their peak number of new cases. Bars on the right represent the population of the LA.\nData updated to ", plotto, ". Data for most recent days is provisional and may be revised upwards as additional tests are processed."),
       caption="Data from PHE | Plot by @VictimOfMaths")+
  theme(axis.line.y=element_blank(), plot.subtitle=element_text(size=rel(0.78)), plot.title.position="plot",
        axis.text.y=element_text(colour="Black"))

ratebars.e <- ggplot(subset(data.e, date==maxcaseday), aes(x=pop, y=fct_reorder(name, maxcaseday), fill=pop))+
  geom_col(show.legend=FALSE)+
  theme_classic()+
  scale_fill_distiller(palette="Spectral")+
  scale_x_continuous(name="Population")+
  theme(axis.title.y=element_blank(), axis.line.y=element_blank(), axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), axis.text.x=element_text(colour="Black"))

tiff("Outputs/COVIDLTLARatesHeatmapEng.tiff", units="in", width=16, height=24, res=500)
plot_grid(ratetiles.e, ratebars.e, align="h", rel_widths=c(1,0.2))
dev.off()

png("Outputs/COVIDLTLARatesHeatmapEng.png", units="in", width=16, height=24, res=500)
plot_grid(ratetiles.e, ratebars.e, align="h", rel_widths=c(1,0.2))
dev.off()

#######
#WALES#
#######
data.w <- data %>% 
  group_by(name) %>% 
  filter(country=="Wales" & name!="Wales") %>% 
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
  theme_classic()+
  scale_fill_distiller(palette="Spectral")+
  scale_y_discrete(name="", expand=c(0,0))+
  scale_x_date(name="Date", limits=as.Date(c(plotfrom, plotto)), expand=c(0,0))+
  labs(title="Timelines for COVID-19 cases in Welsh Local Authorities",
       subtitle=paste0("The heatmap represents the 7-day rolling average of the number of new confirmed cases, normalised to the maximum value within the Local Authority.\nLAs are ordered by the date at which they reached their peak number of new cases. Bars on the right represent the absolute number of cases in each LA.\nData updated to ", plotto, ". Data for most recent days is provisional and may be revised upwards as additional tests are processed."),
       caption="Data from Public Health Wales | Plot by @VictimOfMaths")+
  theme(axis.line.y=element_blank(), plot.subtitle=element_text(size=rel(0.78)), plot.title.position="plot",
        axis.text.y=element_text(colour="Black"))

casebars.w <- ggplot(subset(data.w, date==maxcaseday), aes(x=totalcases, y=fct_reorder(name, maxcaseday), fill=totalcases))+
  geom_col(show.legend=FALSE)+
  theme_classic()+
  scale_fill_distiller(palette="Spectral")+
  scale_x_continuous(name="Total confirmed cases", breaks=c(0,1000,2000))+
  theme(axis.title.y=element_blank(), axis.line.y=element_blank(), axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), axis.text.x=element_text(colour="Black"))

tiff("Outputs/COVIDWelshLACasesHeatmap.tiff", units="in", width=12, height=6, res=500)
plot_grid(casetiles.w, casebars.w, align="h", rel_widths=c(1,0.2))
dev.off()

tiff("Outputs/COVIDWelshLACaseRidges.tiff", units="in", width=10, height=6, res=500)
ggplot(data.w, aes(x=date, y=fct_reorder(name, totalcases), height=casesroll_avg, fill=casesroll_avg))+
  geom_density_ridges_gradient(stat="identity")+
  theme_classic()+
  scale_fill_distiller(palette="Spectral", name="Cases per day\n7-day rolling avg.")+
  scale_x_date(name="Date", limits=as.Date(c(plotfrom, plotto)), expand=c(0,0))+
  scale_y_discrete(name="")+
  labs(title="Timelines of confirmed COVID-19 cases in Welsh Local Authorities",
       caption="Data from Public Health Wales | Plot by @VictimOfMaths")
dev.off()

ratetiles.w <- ggplot(data.w, aes(x=date, y=fct_reorder(name, maxcaseday), fill=casesroll_avg))+
  geom_tile(colour="White", show.legend=FALSE)+
  theme_classic()+
  scale_fill_distiller(palette="Spectral")+
  scale_y_discrete(name="", expand=c(0,0))+
  scale_x_date(name="Date", limits=as.Date(c(plotfrom, plotto)), expand=c(0,0))+
  labs(title="Timelines for COVID-19 case rates in Local Authorities in Wales",
       subtitle=paste0("The heatmap represents the 7-day rolling average of the number of new confirmed cases per 100,000 population.\nLAs are ordered by the date at which they reached their peak number of new cases. Bars on the right represent the population of the LA.\nData updated to ", plotto, ". Data for most recent days is provisional and may be revised upwards as additional tests are processed."),
       caption="Data from PHW | Plot by @VictimOfMaths")+
  theme(axis.line.y=element_blank(), plot.subtitle=element_text(size=rel(0.78)), plot.title.position="plot",
        axis.text.y=element_text(colour="Black"))

ratebars.w <- ggplot(subset(data.w, date==maxcaseday), aes(x=pop, y=fct_reorder(name, maxcaseday), fill=pop))+
  geom_col(show.legend=FALSE)+
  theme_classic()+
  scale_fill_distiller(palette="Spectral")+
  scale_x_continuous(name="Population")+
  theme(axis.title.y=element_blank(), axis.line.y=element_blank(), axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), axis.text.x=element_text(colour="Black"))

tiff("Outputs/COVIDLTLARatesHeatmapWal.tiff", units="in", width=12, height=6, res=500)
plot_grid(ratetiles.w, ratebars.w, align="h", rel_widths=c(1,0.2))
dev.off()

##########
#Scotland#
##########
data.s <- data %>% 
  group_by(name) %>% 
  filter(country=="Scotland" & name!="Scotland") %>% 
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
  theme_classic()+
  scale_fill_distiller(palette="Spectral")+
  scale_y_discrete(name="", expand=c(0,0))+
  scale_x_date(name="Date", limits=as.Date(c(plotfrom, plotto)), expand=c(0,0))+
  labs(title="Timelines for COVID-19 cases in Scottish Coucils",
       subtitle=paste0("The heatmap represents the 7-day rolling average of the number of new confirmed cases, normalised to the maximum value within the Council area.\nCouncils are ordered by the date at which they reached their peak number of new cases. Bars on the right represent the absolute number of cases in each Council area.\nData updated to ", plotto, ". Data for most recent days is provisional and may be revised upwards as additional tests are processed."),
       caption="Data from Public Health Scotland | Plot by @VictimOfMaths")+
  theme(axis.line.y=element_blank(), plot.subtitle=element_text(size=rel(0.78)), plot.title.position="plot",
        axis.text.y=element_text(colour="Black"))

casebars.s <- ggplot(subset(data.s, date==maxcaseday), aes(x=totalcases, y=fct_reorder(name, maxcaseday), fill=totalcases))+
  geom_col(show.legend=FALSE)+
  theme_classic()+
  scale_fill_distiller(palette="Spectral")+
  scale_x_continuous(name="Total confirmed cases", breaks=c(0,1000,2000,3000))+
  theme(axis.title.y=element_blank(), axis.line.y=element_blank(), axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), axis.text.x=element_text(colour="Black"))

tiff("Outputs/COVIDScottishCouncilCasesHeatmap.tiff", units="in", width=12, height=6, res=500)
plot_grid(casetiles.s, casebars.s, align="h", rel_widths=c(1,0.2))
dev.off()

tiff("Outputs/COVIDScottishCouncilCaseRidges.tiff", units="in", width=10, height=6, res=500)
ggplot(data.s, aes(x=date, y=fct_reorder(name, totalcases), height=casesroll_avg, fill=casesroll_avg))+
  geom_density_ridges_gradient(stat="identity")+
  theme_classic()+
  scale_fill_distiller(palette="Spectral", name="Cases per day\n7-day rolling avg.")+
  scale_x_date(name="Date", limits=as.Date(c(plotfrom, plotto)), expand=c(0,0))+
  scale_y_discrete(name="")+
  labs(title="Timelines of confirmed COVID-19 cases in Scottish Council areas",
       caption="Data from Public Health Scotland | Plot by @VictimOfMaths")
dev.off()

ratetiles.s <- ggplot(data.s, aes(x=date, y=fct_reorder(name, maxcaseday), fill=casesroll_avg))+
  geom_tile(colour="White", show.legend=FALSE)+
  theme_classic()+
  scale_fill_distiller(palette="Spectral")+
  scale_y_discrete(name="", expand=c(0,0))+
  scale_x_date(name="Date", limits=as.Date(c(plotfrom, plotto)), expand=c(0,0))+
  labs(title="Timelines for COVID-19 case rates in Council Areas in Scotland",
       subtitle=paste0("The heatmap represents the 7-day rolling average of the number of new confirmed cases per 100,000 population.\nLAs are ordered by the date at which they reached their peak number of new cases. Bars on the right represent the population of the LA.\nData updated to ", plotto, ". Data for most recent days is provisional and may be revised upwards as additional tests are processed."),
       caption="Data from PHS | Plot by @VictimOfMaths")+
  theme(axis.line.y=element_blank(), plot.subtitle=element_text(size=rel(0.78)), plot.title.position="plot",
        axis.text.y=element_text(colour="Black"))

ratebars.s <- ggplot(subset(data.s, date==maxcaseday), aes(x=pop, y=fct_reorder(name, maxcaseday), fill=pop))+
  geom_col(show.legend=FALSE)+
  theme_classic()+
  scale_fill_distiller(palette="Spectral")+
  scale_x_continuous(name="Population")+
  theme(axis.title.y=element_blank(), axis.line.y=element_blank(), axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), axis.text.x=element_text(colour="Black"))

tiff("Outputs/COVIDLTLARatesHeatmapSco.tiff", units="in", width=12, height=6, res=500)
plot_grid(ratetiles.s, ratebars.s, align="h", rel_widths=c(1,0.2))
dev.off()

##########
#Northern Ireland#
##########
data.ni <- data %>% 
  group_by(name) %>% 
  filter(country=="Northern Ireland" & name!="Northern Ireland") %>% 
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
  theme_classic()+
  scale_fill_distiller(palette="Spectral")+
  scale_y_discrete(name="", expand=c(0,0))+
  scale_x_date(name="Date", limits=as.Date(c(plotfrom, plotto)), expand=c(0,0))+
  labs(title="Timelines for COVID-19 cases in Northern Irish Local Authoritiess",
       subtitle=paste0("The heatmap represents the 7-day rolling average of the number of new confirmed cases, normalised to the maximum value within the Local Authority.\nAuthorities are ordered by the date at which they reached their peak number of new cases. Bars on the right represent the absolute number of cases in each Local Authority.\nData updated to ", plotto, ". Data for most recent days is provisional and may be revised upwards as additional tests are processed."),
       caption="Data from Department of Health NI | Plot by @VictimOfMaths")+
  theme(axis.line.y=element_blank(), plot.subtitle=element_text(size=rel(0.78)), plot.title.position="plot",
        axis.text.y=element_text(colour="Black"))

casebars.ni <- ggplot(subset(data.ni, date==maxcaseday), aes(x=totalcases, y=fct_reorder(name, maxcaseday), fill=totalcases))+
  geom_col(show.legend=FALSE)+
  theme_classic()+
  scale_fill_distiller(palette="Spectral")+
  scale_x_continuous(name="Total confirmed cases", breaks=c(0,1000,2000))+
  theme(axis.title.y=element_blank(), axis.line.y=element_blank(), axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), axis.text.x=element_text(colour="Black"))

tiff("Outputs/COVIDNILACasesHeatmap.tiff", units="in", width=12, height=6, res=500)
plot_grid(casetiles.ni, casebars.ni, align="h", rel_widths=c(1,0.2))
dev.off()

tiff("Outputs/COVIDNILACaseRidges.tiff", units="in", width=10, height=6, res=500)
ggplot(data.ni, aes(x=date, y=fct_reorder(name, totalcases), height=casesroll_avg, fill=casesroll_avg))+
  geom_density_ridges_gradient(stat="identity")+
  theme_classic()+
  scale_fill_distiller(palette="Spectral", name="Cases per day\n7-day rolling avg.")+
  scale_x_date(name="Date", limits=as.Date(c(plotfrom, plotto)), expand=c(0,0))+
  scale_y_discrete(name="")+
  labs(title="Timelines of confirmed COVID-19 cases in Northern Irish Local Authorities",
       caption="Data from Department of Health NI | Plot by @VictimOfMaths")
dev.off()

ratetiles.ni <- ggplot(data.ni, aes(x=date, y=fct_reorder(name, maxcaseday), fill=casesroll_avg))+
  geom_tile(colour="White", show.legend=FALSE)+
  theme_classic()+
  scale_fill_distiller(palette="Spectral")+
  scale_y_discrete(name="", expand=c(0,0))+
  scale_x_date(name="Date", limits=as.Date(c(plotfrom, plotto)), expand=c(0,0))+
  labs(title="Timelines for COVID-19 case rates in Local Authorities in Northern Ireland",
       subtitle=paste0("The heatmap represents the 7-day rolling average of the number of new confirmed cases per 100,000 population.\nLAs are ordered by the date at which they reached their peak number of new cases. Bars on the right represent the population of the LA.\nData updated to ", plotto, ". Data for most recent days is provisional and may be revised upwards as additional tests are processed."),
       caption="Data from DoHNI | Plot by @VictimOfMaths")+
  theme(axis.line.y=element_blank(), plot.subtitle=element_text(size=rel(0.78)), plot.title.position="plot",
        axis.text.y=element_text(colour="Black"))

ratebars.ni <- ggplot(subset(data.ni, date==maxcaseday), aes(x=pop, y=fct_reorder(name, maxcaseday), fill=pop))+
  geom_col(show.legend=FALSE)+
  theme_classic()+
  scale_fill_distiller(palette="Spectral")+
  scale_x_continuous(name="Population")+
  theme(axis.title.y=element_blank(), axis.line.y=element_blank(), axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), axis.text.x=element_text(colour="Black"))

tiff("Outputs/COVIDLTLARatesHeatmapNI.tiff", units="in", width=12, height=4, res=500)
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
  filter(country!="Republic of Ireland") %>% 
  mutate(min=min(date), max=max(date))

completefrom <- max(data.hex$min, na.rm=TRUE)
completeto <- min(data.hex$max, na.rm=TRUE)

HexAnimUK <- ggplot()+
  geom_polygon(data=subset(data.hex, date>as.Date("2020-03-06") & date<=completeto), 
               aes(x=long, y=lat, group=id, fill=casesroll_avg))+
  coord_fixed()+
  scale_fill_distiller(palette="Spectral", name="Daily confirmed\ncases (7-day\nrolling avg.)", na.value="white")+
  theme_classic()+
  theme(axis.line=element_blank(), axis.ticks=element_blank(), axis.text=element_blank(),
        axis.title=element_blank(),  plot.title=element_text(face="bold"))+
  transition_time(date)+
  labs(title="Visualising the spread of COVID-19 across the UK",
       subtitle="Rolling 7-day average number of new confirmed cases.\nDate: {frame_time}",
       caption="Data from PHE, PHW, PHS & DoHNI\nVisualisation by @VictimOfMaths")

animate(HexAnimUK, duration=18, fps=10, width=2000, height=3000, res=300, renderer=gifski_renderer("Outputs/HexAnimUK.gif"), 
        end_pause=60)

#Rates version
HexAnimUKrate <- ggplot()+
  geom_polygon(data=subset(data.hex, date>as.Date("2020-03-06") & date<=completeto), 
               aes(x=long, y=lat, group=id, fill=caserate_avg))+
  coord_fixed()+
  scale_fill_distiller(palette="Spectral", name="Daily confirmed\ncases/100,000\n(7-day rolling avg.)", na.value="white")+
  theme_classic()+
  theme(axis.line=element_blank(), axis.ticks=element_blank(), axis.text=element_blank(),
        axis.title=element_blank(),  plot.title=element_text(face="bold"))+
  transition_time(date)+
  labs(title="Visualising the spread of COVID-19 across the UK",
       subtitle="Rolling 7-day average number of new confirmed cases per 100,000.\nDate: {frame_time}",
       caption="Data from PHE, PHW, PHS & DoHNI\nVisualisation by @VictimOfMaths")

animate(HexAnimUKrate, duration=18, fps=10, width=2000, height=3000, res=300, 
        renderer=gifski_renderer("Outputs/HexAnimUKrate.gif"), 
        end_pause=60)

#Chloropeth map
data.map <- data

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

map.cases <- full_join(simplemap, data.map, by="code", all.y=TRUE)
map.cases$date <- as.Date(map.cases$date)

#Map of current cases
tiff("Outputs/COVIDCaseMapUK.tiff", units="in", width=8, height=12, res=500)
map.cases %>% 
filter(date==completeto & !name %in% c("England", "Wales", "Northern Ireland", "Scotland")) %>% 
  ggplot()+
  geom_sf(aes(geometry=geometry, fill=casesroll_avg), colour=NA)+
  scale_fill_distiller(palette="Spectral", name="Daily cases\n(rolling 7-day avg.)")+
  theme_classic()+
  theme(axis.line=element_blank(), axis.ticks=element_blank(), axis.text=element_blank(),
        axis.title=element_blank())+
  labs(title="Confirmed new COVID-19 cases in the UK",
       subtitle=paste0("Rolling 7-day average of confirmed new cases at Local Authority/Council Area level\nData up to ", completeto),
       caption="Data from PHE, PHW, PHS & DoHNI | Plot by @VictimOfMaths")
dev.off()
  
tiff("Outputs/COVIDCaserateMapUK.tiff", units="in", width=8, height=12, res=500)
map.cases %>% 
  filter(date==completeto & !name %in% c("England", "Wales", "Northern Ireland", "Scotland")) %>% 
  ggplot()+
  geom_sf(aes(geometry=geometry, fill=caserate_avg), colour=NA)+
  scale_fill_distiller(palette="Spectral", name="Daily cases\nper 100,000\n(rolling 7-day avg.)")+
  theme_classic()+
    theme(axis.line=element_blank(), axis.ticks=element_blank(), axis.text=element_blank(),
          axis.title=element_blank())+
  labs(title="Rates of confirmed new COVID-19 cases in the UK",
       subtitle=paste0("Rolling 7-day average of confirmed new cases per 100,000 at Local Authority/Council Area level\nData up to ", completeto),
       caption="Data from PHE, PHW, PHS & DoHNI | Plot by @VictimOfMaths")
dev.off()

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

animate(CaseAnimAbs, duration=25, fps=10, width=2000, height=3000, res=300, renderer=gifski_renderer("Outputs/CaseAnimAbs.gif"), end_pause=60)

CaseAnimRate <- ggplot(subset(map.cases, date>as.Date("2020-02-25")), aes(geometry=geometry, fill=caserate_avg))+
  geom_sf(colour=NA)+
  theme_classic()+
  scale_fill_distiller(palette="Spectral", name="Daily cases\nper 100,000\n(rolling 7-day avg.)")+
  theme(axis.line=element_blank(), axis.ticks=element_blank(), axis.text=element_blank(),
        axis.title=element_blank(),  plot.title=element_text(face="bold"))+
  transition_time(date)+
  labs(title="Visualising the spread of the pandemic across England",
       subtitle="Rolling 7-day average rate of new confirmed cases per 100,000 in each Local Authority/Council area\nDate: {frame_time}",
       caption="Data from PHE, PHW, PHS & DoHNI | Visualisation by @VictimOfMaths")

animate(CaseAnimRate, duration=25, fps=10, width=2000, height=3000, res=300, renderer=gifski_renderer("Outputs/CaseAnimRate.gif"), end_pause=60)
