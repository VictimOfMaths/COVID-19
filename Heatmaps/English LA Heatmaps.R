rm(list=ls())

library(tidyverse)
library(curl)
library(forcats)
library(RcppRoll)
library(data.table)
library(readxl)
library(cowplot)
library(sf)
library(rmapshaper)
library(gganimate)
library(paletteer)
library(ggtext)

options(scipen = 999)

#Read in COVID case data
temp <- tempfile()
source <- "https://coronavirus.data.gov.uk/downloads/csv/coronavirus-cases_latest.csv"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
data <- fread(temp)[,c(1:6)]
colnames(data) <- c("name", "code", "type", "date", "cases", "cumul_cases")
data$date <- as.Date(data$date)
data <- subset(data, type=="utla")

#Set up skeleton dataframe with dates
LAcodes <- unique(data$code)
min <- min(data$date)
max <- max(data$date)

skeleton <- data.frame(code=rep(LAcodes, each=(max-min+1), times=1), date=rep(seq.Date(from=min, to=max, by="day"), each=1, times=length(LAcodes)))

#Map data onto skeleton
fulldata <- merge(skeleton, data[,-c(1,3)], by=c("code", "date"), all.x=TRUE, all.y=TRUE)

#Bring in LA names
temp <- data %>%
  group_by(code) %>%
  slice(1L)
fulldata <- merge(fulldata, temp[,c(1,2)], by="code")

#Fill in blank days
fulldata$cases <- ifelse(is.na(fulldata$cases), 0, fulldata$cases)

#Calculate cumulative sums so far
fulldata <- fulldata %>%
  group_by(code) %>%
  mutate(cumul_cases=cumsum(cases))

#this is the deaths for each NHS trust (only deaths in hospital) - we dont have daily deaths by LA yet 
#so need to map this to LA. Approach to do this developed by and code adapted from @Benj_barr.

#Need to manually update the link to the latest total announced deaths file here: 
#https://www.england.nhs.uk/statistics/statistical-work-areas/covid-19-daily-deaths/
#and extend the final number value in rows 78 & 80 by 1 to capture additional days (67=1st May announcement date)

temp <- tempfile()
source <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2020/10/COVID-19-total-announced-deaths-2-October-2020.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

deaths<-as.data.table(read_excel(temp, sheet=6, col_names = F))

deaths<-deaths[18:.N, c(1:223)]

deaths<- melt.data.table(deaths, id=1:4, measure.vars = 5:223)

deaths[, 2:=NULL]
names(deaths)<-c("region", "procode3","trust","variable","deaths")
deaths$procode3 <- substr(deaths$procode3, 1, 3)

deaths[order(variable), date:=1:.N, by=.(procode3)]

deaths[, date:=as.Date("2020-02-29")+as.numeric(substr(variable, 4, 6))-5]

deaths[, variable:=NULL]

deaths$deaths <- as.numeric(deaths$deaths)
#deaths<-deaths[, list(deaths=sum(as.numeric(deaths),na.rm = T)), by=.(procode3,trust)]
sum(deaths$deaths)

# this is the number of all emergency admissions in 2018-2019 for each trust split by LA
dt1<-fread("Data/la_trust_lk.csv")

dt1<-dt1[is.na(areacode)==F]
length(unique(dt1$areacode))  #150
sum(deaths$deaths)
deaths<-merge(deaths, dt1, by="procode3", all.x = T, allow.cartesian = TRUE)
deaths[, fraction:=CountAdm/sum(CountAdm,na.rm = T),by=.(procode3, date)]
# 66 deaths not allocated to LA, trust is not in look up
sum(deaths[is.na(fraction)==T]$deaths)
deaths<-deaths[is.na(fraction)==F]

#we then assume that deaths from each Trust were distributed between LAs based on the historical share of admissions from that LA
deaths[, deaths:=deaths*fraction]

# 2019 LA boundary changes  
#deaths[areaname=="Bournemouth"|areaname=="Poole",`:=` (areacode="E10000009",areaname="Dorset CC")]
deaths[areaname=="Isles of Scilly",`:=` (areacode="E06000052",areaname="Cornwall")]
deaths[areacode=="E10000009",`:=` (areacode="E06000059")]
deaths[areacode=="E06000029",`:=` (areacode="E06000058")]

deaths <- deaths %>%
  group_by(areacode, date) %>%
  summarise(deaths=sum(deaths, na.rm=TRUE))

length(unique(deaths$areacode))  #147
sum(deaths$deaths)

colnames(deaths) <- c("code", "date", "deaths")

fulldata <- merge(fulldata, deaths, by=c("code", "date"), all.x=TRUE)

heatmap <- fulldata %>%
  group_by(code) %>%
  mutate(casesroll_avg=roll_mean(cases, 7, align="right", fill=0), deathsroll_avg=roll_mean(deaths, 7, align="right", fill=0)) %>%
  mutate(totalcases=max(cumul_cases), maxcaserate=max(casesroll_avg), maxcaseday=date[which(casesroll_avg==maxcaserate)][1],
         cumul_deaths=sum(deaths, na.rm=TRUE), totaldeaths=max(cumul_deaths, na.rm=TRUE), maxdeathrate=max(deathsroll_avg, na.rm=TRUE),
         maxdeathsday=date[which(deathsroll_avg==maxdeathrate)][1])

heatmap$maxcaseprop <- heatmap$casesroll_avg/heatmap$maxcaserate
heatmap$maxdeathprop <- heatmap$deathsroll_avg/heatmap$maxdeathrate

#Enter dates to plot from and to
plotfrom <- "2020-03-03"
plotto <- max(heatmap$date)

#Plot case trajectories
casetiles <- ggplot(heatmap, aes(x=date, y=fct_reorder(name, maxcaseday), fill=maxcaseprop))+
  geom_tile(colour="White", show.legend=FALSE)+
  theme_classic()+
  scale_fill_distiller(palette="Spectral")+
  #scale_fill_viridis_c()+
  scale_y_discrete(name="", expand=c(0,0))+
  scale_x_date(name="Date", limits=as.Date(c(plotfrom, plotto)), expand=c(0,0))+
  labs(title="Timelines for COVID-19 cases in English Local Authorities",
       subtitle=paste0("The heatmap represents the 7-day rolling average of the number of new confirmed cases, normalised to the maximum value within the Local Authority.\nLAs are ordered by the date at which they reached their peak number of new cases. Bars on the right represent the absolute number of cases in each LA.\nData updated to ", plotto, ". Data for most recent days is provisional and may be revised upwards as additional tests are processed."),
       caption="Data from Public Health England | Plot by @VictimOfMaths")+
  theme(axis.line.y=element_blank(), plot.subtitle=element_text(size=rel(0.78)), plot.title.position="plot",
        axis.text.y=element_text(colour="Black"), plot.title=element_text(size=rel(2.3)))

casebars <- ggplot(subset(heatmap, date==maxcaseday), aes(x=totalcases, y=fct_reorder(name, maxcaseday), fill=totalcases))+
  geom_col(show.legend=FALSE)+
  theme_classic()+
  scale_fill_distiller(palette="Spectral")+
  #scale_fill_viridis_c()+
  scale_x_continuous(name="Total confirmed cases", breaks=c(0,2000,4000,6000,8000,10000))+
  theme(axis.title.y=element_blank(), axis.line.y=element_blank(), axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), axis.text.x=element_text(colour="Black"))

tiff("Outputs/COVIDLACasesHeatmap.tiff", units="in", width=16, height=16, res=500)
plot_grid(casetiles, casebars, align="h", rel_widths=c(1,0.2))
dev.off()

png("Outputs/COVIDLACasesHeatmap.png", units="in", width=16, height=16, res=500)
plot_grid(casetiles, casebars, align="h", rel_widths=c(1,0.2))
dev.off()

#Plot death trajectories

deathtiles <- ggplot(heatmap, aes(x=date, y=fct_reorder(name, maxdeathsday), fill=maxdeathprop))+
  geom_tile(colour="White", show.legend=FALSE)+
  theme_classic()+
  scale_fill_distiller(palette="Spectral")+
  scale_y_discrete(name="")+
  scale_x_date(name="Date", limits=as.Date(c("2020-03-06", plotto)), expand=c(0,0))+
  labs(title="Timelines for COVID-19 deaths in hospital in English Local Authorities",
       subtitle=paste0("The heatmap represents the 7-day rolling average of the number of estimated deaths, normalised to the maximum value within the Local Authority.\nLAs are ordered by the date at which they reached their peak number of deaths. Bars on the right represent the absolute number of deaths estimated\nin each LA. Deaths are estimated as COVID-19 mortality data is only available from NHS England at hospital level. LA-level deaths are modelled using\n@Benj_Barr's approach, using the proportion of HES emergency admissions to each hospital in 2018-19 originating from each LA.\nData updated to ", plotto, ". Data for most recent days is provisional and may be revised upwards as additional tests are processed."),
       caption="Data from NHS England & Ben Barr | Plot by @VictimOfMaths")+
  theme(axis.line.y=element_blank(), plot.subtitle=element_text(size=rel(0.78)), plot.title.position="plot",
        axis.text=element_text(colour="Black"), plot.title=element_text(size=rel(2.3)))

deathbars <- ggplot(subset(heatmap, date==maxdeathsday), aes(x=totaldeaths, y=fct_reorder(name, maxdeathsday), fill=totaldeaths))+
  geom_col(show.legend=FALSE)+
  theme_classic()+
  scale_fill_distiller(palette="Spectral")+
  scale_x_continuous(name="Total confirmed deaths")+
  theme(axis.title.y=element_blank(), axis.line.y=element_blank(), axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), axis.text.x=element_text(colour="Black"))

tiff("Outputs/COVIDLADeathHeatmap.tiff", units="in", width=16, height=16, res=500)
plot_grid(deathtiles, deathbars, align="h", rel_widths=c(1,0.2))
dev.off()

png("Outputs/COVIDLADeathHeatmap.png", units="in", width=16, height=16, res=500)
plot_grid(deathtiles, deathbars, align="h", rel_widths=c(1,0.2))
dev.off()

##################################
#Absolute version of the heatmaps#
##################################

#Bring in population data 
temp <- tempfile()
source <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fpopulationandmigration%2fpopulationestimates%2fdatasets%2fpopulationestimatesforukenglandandwalesscotlandandnorthernireland%2fmid20182019laboundaries/ukmidyearestimates20182019ladcodes.xls"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
LApop <- read_excel(temp, sheet="MYE2-All", range="A5:D367", col_names=TRUE)
colnames(LApop) <- c("code", "name", "geography", "pop")

heatmap <- merge(heatmap, LApop[,c(1,4)], by="code")

heatmap$cumul_caserate <- heatmap$totalcases*100000/heatmap$pop
heatmap$cumul_deathrate <- heatmap$totaldeaths*100000/heatmap$pop
heatmap$avgcaserates <- heatmap$casesroll_avg*100000/heatmap$pop

#Plot absolute case trajectories
abscasetiles <- ggplot(heatmap, aes(x=date, y=fct_reorder(name, maxcaseday), fill=casesroll_avg))+
  geom_tile(colour="White", show.legend=FALSE)+
  theme_classic()+
  scale_fill_distiller(palette="Spectral")+
  scale_y_discrete(name="", expand=c(0,0))+
  scale_x_date(name="Date", limits=as.Date(c(plotfrom, plotto)), expand=c(0,0))+
  labs(title="Timelines for COVID-19 cases in English Local Authorities",
       subtitle=paste0("The heatmap represents the 7-day rolling average of the number of new confirmed cases within each Local Authority.\nLAs are ordered by the date at which they reached their peak number of cases. Bars on the right represent the cumulative number of cases per 100,000 population in each LA.\nData updated to ", plotto, ". Data for most recent days is provisional and may be revised upwards as additional tests are processed."),
       caption="Data from Public Health England | Plot by @VictimOfMaths")+
  theme(axis.line.y=element_blank(), plot.subtitle=element_text(size=rel(0.78)), plot.title.position="plot",
        axis.text=element_text(colour="Black"))

abscasebars <- ggplot(subset(heatmap, date==maxcaseday), aes(x=cumul_caserate, y=fct_reorder(name, maxcaseday), fill=cumul_caserate))+
  geom_col(show.legend=FALSE)+
  theme_classic()+
  scale_fill_distiller(palette="Spectral")+
  scale_x_continuous(name="Total confirmed cases\nper 100,000 population", breaks=c(0,500,1000,1500))+
  theme(axis.title.y=element_blank(), axis.line.y=element_blank(), axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), axis.text.x=element_text(colour="Black"))

tiff("Outputs/COVIDLACasesHeatmapAbs.tiff", units="in", width=16, height=16, res=500)
plot_grid(abscasetiles, abscasebars, align="h", rel_widths=c(1,0.2))
dev.off()

png("Outputs/COVIDLACasesHeatmapAbs.png", units="in", width=16, height=16, res=500)
plot_grid(abscasetiles, abscasebars, align="h", rel_widths=c(1,0.2))
dev.off()

#Plot absolute case rate trajectories
ratetiles <- ggplot(heatmap, aes(x=date, y=fct_reorder(name, maxcaseday), fill=avgcaserates))+
  geom_tile(colour="White", show.legend=FALSE)+
  theme_classic()+
  scale_fill_distiller(palette="Spectral")+
  scale_y_discrete(name="", expand=c(0,0))+
  scale_x_date(name="Date", limits=as.Date(c(plotfrom, plotto)), expand=c(0,0))+
  labs(title="Timelines for COVID-19 case rates in English Local Authorities",
       subtitle=paste0("The heatmap represents the 7-day rolling average of the number of new confirmed cases per 100,000 population within each Local Authority.\nLAs are ordered by the date at which they reached their peak number of cases. Bars on the right represent the total population of each LA.\nData updated to ", plotto, ". Data for most recent days is provisional and may be revised upwards as additional tests are processed."),
       caption="Data from Public Health England | Plot by @VictimOfMaths")+
  theme(axis.line.y=element_blank(), plot.subtitle=element_text(size=rel(1.2)), plot.title.position="plot",
        axis.text=element_text(colour="Black"), plot.title=element_text(size=rel(2.3)))

ratebars <- ggplot(subset(heatmap, date==maxcaseday), aes(x=pop, y=fct_reorder(name, maxcaseday), fill=pop))+
  geom_col(show.legend=FALSE)+
  theme_classic()+
  scale_fill_distiller(palette="Spectral")+
  scale_x_continuous(name="Population")+
  theme(axis.title.y=element_blank(), axis.line.y=element_blank(), axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), axis.text.x=element_text(colour="Black"))

tiff("Outputs/COVIDLARateHeatmap.tiff", units="in", width=16, height=16, res=500)
plot_grid(ratetiles, ratebars, align="h", rel_widths=c(1,0.2))
dev.off()

png("Outputs/COVIDLARateHeatmap.png", units="in", width=16, height=16, res=500)
plot_grid(ratetiles, ratebars, align="h", rel_widths=c(1,0.2))
dev.off()


#Plot absolute death trajectories
death <- ggplot(heatmap, aes(x=date, y=fct_reorder(name, maxdeathsday), fill=deathsroll_avg))+
  geom_tile(colour="White", show.legend=FALSE)+
  theme_classic()+
  scale_fill_distiller(palette="Spectral")+
  scale_y_discrete(name="", expand=c(0,0))+
  scale_x_date(name="Date", limits=as.Date(c("2020-03-06", plotto)), expand=c(0,0))+
  labs(title="Timelines for COVID-19 deaths in hospital in English Local Authorities",
       subtitle=paste0("The heatmap represents the 7-day rolling average of the number of daily confirmed deaths within each Local Authority.\nLAs are ordered by the date at which they reached their peak number of deaths Bars on the right represent the cumulative number of cases per 100,000 population in each LA.\nData updated to ", plotto, ". Data for most recent days is provisional and may be revised upwards as additional tests are processed."),
       caption="Data from Public Health England | Plot by @VictimOfMaths")+
  theme(axis.line.y=element_blank(), plot.subtitle=element_text(size=rel(0.78)), plot.title.position="plot",
        axis.text=element_text(colour="Black"), plot.title=element_text(size=rel(2.3)))

deathbars <- ggplot(subset(heatmap, date==maxdeathsday), aes(x=cumul_deathrate, y=fct_reorder(name, maxdeathsday), fill=cumul_deathrate))+
  geom_col(show.legend=FALSE)+
  theme_classic()+
  scale_fill_distiller(palette="Spectral")+
  scale_x_continuous(name="Total confirmed deaths\nper 100,000 population", breaks=c(0,50,100))+
  theme(axis.title.y=element_blank(), axis.line.y=element_blank(), axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), axis.text.x=element_text(colour="Black"))

tiff("Outputs/COVIDLADeathsHeatmapAbs.tiff", units="in", width=16, height=16, res=500)
plot_grid(death, deathbars, align="h", rel_widths=c(1,0.2))
dev.off()

png("Outputs/COVIDLADeathsHeatmapAbs.png", units="in", width=16, height=16, res=500)
plot_grid(death, deathbars, align="h", rel_widths=c(1,0.2))
dev.off()

#Plot death rate trajectories
deathrate <- ggplot(heatmap, aes(x=date, y=fct_reorder(name, maxdeathsday), fill=deathsroll_avg*100000/pop))+
  geom_tile(colour="White", show.legend=FALSE)+
  theme_classic()+
  scale_fill_distiller(palette="Spectral")+
  scale_y_discrete(name="", expand=c(0,0))+
  scale_x_date(name="Date", limits=as.Date(c("2020-03-06", plotto)), expand=c(0,0))+
  labs(title="Timelines for COVID-19 death rates in hospitals in English Local Authorities",
       subtitle=paste0("The heatmap represents the 7-day rolling average of the number of daily confirmed deaths per 100,000 within each Local Authority.\nLAs are ordered by the date at which they reached their peak number of deaths Bars on the right represent the cumulative number of cases per 100,000 population in each LA.\nData updated to ", plotto, ". Data for most recent days is provisional and may be revised upwards as additional tests are processed."),
       caption="Data from NHS England | Plot by @VictimOfMaths")+
  theme(axis.line.y=element_blank(), plot.subtitle=element_text(size=rel(0.78)), plot.title.position="plot",
        axis.text=element_text(colour="Black"), plot.title=element_text(size=rel(2.3)))

deathratebars <- ggplot(subset(heatmap, date==maxdeathsday), aes(x=pop, y=fct_reorder(name, maxdeathsday), fill=pop))+
  geom_col(show.legend=FALSE)+
  theme_classic()+
  scale_fill_distiller(palette="Spectral")+
  scale_x_continuous(name="Population")+
  theme(axis.title.y=element_blank(), axis.line.y=element_blank(), axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), axis.text.x=element_text(colour="Black"))

tiff("Outputs/COVIDLADeathsRateHeatmap.tiff", units="in", width=16, height=16, res=500)
plot_grid(deathrate, deathratebars, align="h", rel_widths=c(1,0.2))
dev.off()

png("Outputs/COVIDLADeathsRateHeatmap.png", units="in", width=16, height=16, res=500)
plot_grid(deathrate, deathratebars, align="h", rel_widths=c(1,0.2))
dev.off()

##########################
#Map of case trajectories#
##########################

#Download shapefile of LA boundaries
temp <- tempfile()
temp2 <- tempfile()
source <- "https://opendata.arcgis.com/datasets/6638c31a8e9842f98a037748f72258ed_0.zip?outSR=%7B%22latestWkid%22%3A27700%2C%22wkid%22%3A27700%7D"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)

#The actual shapefile has a different name each time you download it, so need to fish the name out of the unzipped file
name <- list.files(temp2, pattern=".shp")
shapefile <- st_read(file.path(temp2, name))

names(shapefile)[names(shapefile) == "ctyua17cd"] <- "code"

simplemap <- ms_simplify(shapefile, keep=0.2, keep_shapes = TRUE)

#Duplicate data to account for shapefile using pre-2019 codes
int1 <- filter(heatmap, name=="Bournemouth, Christchurch and Poole")
int1$code <- "E06000028"
int2 <- filter(heatmap, name=="Bournemouth, Christchurch and Poole")
int2$code <- "E06000029"
int3 <- filter(heatmap, name=="Bournemouth, Christchurch and Poole")
int3$code <- "E10000009"

temp <- rbind(heatmap, int1, int2, int3)

#Calculate change in cases in the past week
change <- temp %>%
  mutate(change=casesroll_avg-lag(casesroll_avg,7))

#Exclude most recent day as reporting is usually very incomplete
change <- subset(change, date==max-3)

map.change <- full_join(simplemap, change, by="code", all.y=TRUE)
map.change <- map.change %>% drop_na("maxcaseprop")

#Map of past week changes

changemap <- ggplot()+
  geom_sf(data=map.change, aes(geometry=geometry, fill=change), colour=NA)+
  geom_sf(data=subset(map.change, casesroll_avg==0), aes(geometry=geometry), fill="#41ab5d", colour=NA)+
  xlim(10000,655644)+
  ylim(5337,700000)+
  theme_classic()+
  scale_fill_paletteer_c("scico::roma", limit=c(-1,1)*max(abs(map.change$change)), 
                         name="Change in case numbers\nin the past week", breaks=c(-60,-30, 0,30,60),
                         labels=c("-60", "-30", "0", "+30", "+60"),direction=-1)+
  theme(axis.line=element_blank(), axis.ticks=element_blank(), axis.text=element_blank(),
        axis.title=element_blank(), plot.subtitle=element_markdown())+
  labs(title="Recent changes in COVID-19 case numbers across England",
       subtitle="<span style='color:Grey50;'>Has the 7-day rolling average of case numbers <span style='color:#854B01FF;'>risen<span style='color:Grey50;'> or <span style='color:#014380FF;'>fallen<span style='color:Grey50;'> in the past week?<br>Areas with 0 cases shown in <span style='color:#41ab5d;'>green",
       caption="Data from Public Health England | Plot by @VictimOfMaths")+
  geom_rect(aes(xmin=500000, xmax=560000, ymin=156000, ymax=200000), fill="transparent",
            colour="gray50")+
  geom_rect(aes(xmin=310000, xmax=405000, ymin=370000, ymax=430000), fill="transparent",
            colour="gray50")+
  geom_rect(aes(xmin=405000, xmax=490000, ymin=505000, ymax=580000), fill="transparent",
            colour="gray50")
#Add zoomed in areas
#London
London <- ggplot()+
  geom_sf(data=map.change, aes(geometry=geometry, fill=change), colour=NA, show.legend=FALSE)+
  geom_sf(data=subset(map.change, casesroll_avg==0), aes(geometry=geometry), fill="#41ab5d", colour=NA)+
  scale_x_continuous(limits=c(500000,560000), expand=c(0,0))+
  scale_y_continuous(limits=c(156000,200000), expand=c(0,0))+
  theme_classic()+
  scale_fill_paletteer_c("scico::roma", limit=c(-1,1)*max(abs(map.change$change)), direction=-1)+
  labs(title="Greater London")+
  theme(axis.line=element_blank(), axis.ticks=element_blank(), axis.text=element_blank(),
        axis.title=element_blank(), plot.title=element_text(size=rel(0.9)))

#North-West England
NWEng <-ggplot()+
  geom_sf(data=map.change, aes(geometry=geometry, fill=change), colour=NA, show.legend=FALSE)+
  geom_sf(data=subset(map.change, casesroll_avg==0), aes(geometry=geometry), fill="#41ab5d", colour=NA)+
  scale_x_continuous(limits=c(310000,405000), expand=c(0,0))+
  scale_y_continuous(limits=c(370000,430000), expand=c(0,0))+
  theme_classic()+
  scale_fill_paletteer_c("scico::roma", limit=c(-1,1)*max(abs(map.change$change)), direction=-1)+
  labs(title="The North West")+
  theme(axis.line=element_blank(), axis.ticks=element_blank(), axis.text=element_blank(),
        axis.title=element_blank(), plot.title=element_text(size=rel(0.9)))

#Tyne/Tees  
NEEng <- ggplot()+
  geom_sf(data=map.change, aes(geometry=geometry, fill=change), colour=NA, show.legend=FALSE)+
  geom_sf(data=subset(map.change, casesroll_avg==0), aes(geometry=geometry), fill="#41ab5d", colour=NA)+
  scale_x_continuous(limits=c(405000,490000), expand=c(0,0))+
  scale_y_continuous(limits=c(505000,580000), expand=c(0,0))+
  theme_classic()+
  scale_fill_paletteer_c("scico::roma", limit=c(-1,1)*max(abs(map.change$change)), direction=-1)+
  labs(title="The North East")+
  theme(axis.line=element_blank(), axis.ticks=element_blank(), axis.text=element_blank(),
        axis.title=element_blank(), plot.title=element_text(size=rel(0.9)))

tiff("Outputs/COVIDChangesmapEng.tiff", units="in", width=9, height=11, res=500)
ggdraw()+
  draw_plot(changemap)+
  draw_plot(London, 0.01,0.34,0.32,0.21)+
  draw_plot(NWEng, 0.01,0.57, 0.32, 0.24)+
  draw_plot(NEEng, 0.57, 0.62, 0.22, 0.22)
dev.off()

#For animation
map.data <- full_join(simplemap, temp, by="code", all.y=TRUE)

#remove areas with no HLE data (i.e. Scotland, Wales & NI)
map.data <- map.data %>% drop_na("maxcaseprop")

#Animation of case trajectories
CaseAnim <- ggplot(subset(map.data, date>as.Date("2020-02-25")), aes(geometry=geometry, fill=maxcaseprop))+
  geom_sf(colour=NA)+
  xlim(10000,655644)+
  ylim(5337,700000)+
  theme_classic()+
  scale_fill_distiller(palette="Spectral", name="Daily cases as a %\nof peak cases", breaks=c(0,0.25,0.5,0.75,1),
                       labels=c("0%", "25%", "50%", "75%", "100%"))+
  theme(axis.line=element_blank(), axis.ticks=element_blank(), axis.text=element_blank(),
        axis.title=element_blank(),  plot.title=element_text(face="bold"))+
  transition_time(date)+
  labs(title="Visualising the spread of the pandemic across England",
       subtitle="Rolling 7-day average number of new confirmed cases coloured relative to the\npeak in each Local Authority (i.e. dark red represents the peak of new cases).\nDate: {frame_time}",
       caption="Data from Public Health England | Visualisation by @VictimOfMaths")

animate(CaseAnim, duration=25, fps=10, width=2000, height=3000, res=300, renderer=gifski_renderer("Outputs/CaseAnim.gif"), end_pause=60)

#Animation of death trajectories
DeathAnim <- ggplot(subset(map.data, date>as.Date("2020-03-03")), aes(geometry=geometry, fill=maxdeathprop))+
  geom_sf(colour=NA)+
  xlim(10000,655644)+
  ylim(5337,700000)+
  theme_classic()+
  scale_fill_distiller(palette="Spectral", name="Daily deaths as a %\nof peak deaths", breaks=c(0,0.25,0.5,0.75,1),
                       labels=c("0%", "25%", "50%", "75%", "100%"))+
  theme(axis.line=element_blank(), axis.ticks=element_blank(), axis.text=element_blank(),
        axis.title=element_blank(),  plot.title=element_text(face="bold"))+
  transition_time(date)+
  labs(title="Visualising the spread of the pandemic across England",
       subtitle="Rolling 7-day average number of new confirmed deaths coloured relative to the\npeak in each Local Authority (i.e. dark red represents the peak in deaths).\nDate: {frame_time}",
       caption="Data from NHS England | Visualisation by @VictimOfMaths")

animate(DeathAnim, duration=18, fps=10, width=2000, height=3000, res=300, renderer=gifski_renderer("Outputs/DeathAnim.gif"), end_pause=60)

#Animation of absolute case numbers
CaseAnimAbs <- ggplot(subset(map.data, date>as.Date("2020-02-25")), aes(geometry=geometry, fill=casesroll_avg))+
  geom_sf(colour=NA)+
  xlim(10000,655644)+
  ylim(5337,700000)+
  theme_classic()+
  scale_fill_distiller(palette="Spectral", name="Daily confirmed cases", na.value="white")+
  theme(axis.line=element_blank(), axis.ticks=element_blank(), axis.text=element_blank(),
        axis.title=element_blank(),  plot.title=element_text(face="bold"))+
  transition_time(date)+
  labs(title="Visualising the spread of the pandemic across England",
       subtitle="Rolling 7-day average number of new confirmed cases.\nDate: {frame_time}",
       caption="Data from Public Health England | Visualisation by @VictimOfMaths")

animate(CaseAnimAbs, duration=25, fps=10, width=2000, height=3000, res=300, renderer=gifski_renderer("Outputs/CaseAnimAbs.gif"), end_pause=60)

#Animation of death rates
DeathRateAnim <- ggplot(subset(map.data, date>as.Date("2020-03-03")), aes(geometry=geometry, fill=deathsroll_avg*100000/pop))+
  geom_sf(colour=NA)+
  xlim(10000,655644)+
  ylim(5337,700000)+
  theme_classic()+
  scale_fill_distiller(palette="Spectral", name="Daily deaths\nper 100,000")+
  theme(axis.line=element_blank(), axis.ticks=element_blank(), axis.text=element_blank(),
        axis.title=element_blank(),  plot.title=element_text(face="bold"))+
  transition_time(date)+
  labs(title="Visualising the spread of the pandemic across England",
       subtitle="Rolling 7-day average number of new confirmed COVID-19 deaths in hospitals per 100,000\nDate: {frame_time}",
       caption="Data from NHS England | Visualisation by @VictimOfMaths")

animate(DeathRateAnim, duration=18, fps=10, width=2000, height=3000, res=300, renderer=gifski_renderer("Outputs/DeathRateAnim.gif"), end_pause=60)

#Quick analysis of potential COVID 'bumps'

temp1 <- subset(heatmap, name %in% c("Dorset", "Cornwall and Isles of Scilly", "Devon", "Bournemouth, Christchurch and Poole",
                                     "West Sussex", "East Sussex", "Brighton and Hove"))

tiff("Outputs/COVIDSouthCoast.tiff", units="in", width=8, height=4, res=500)
ggplot(subset(temp1, date>"2020-05-01"), aes(x=date, y=name, fill=avgcaserates))+
  geom_tile(colour="white")+
  scale_fill_distiller(palette="Spectral", name="New cases\nper 100,000")+
  scale_x_date(name="Date")+
  scale_y_discrete(name="")+
  theme_classic()+
  labs(title="No clear signs of a rise in cases after the sunny May weather",
       subtitle="7-day rolling average of new confirmed COVID-19 cases",
       caption="Data from PHE | Plot by @VictimOfMaths")
dev.off()

temp2 <- subset(heatmap, name %in% c("Islington", "Camden", "Hackney", "Southwark", "Tower Hamlets",
                                     "Lambeth", "Lewisham", "Haringey", "Westminster", "Kensington and Chelsea",
                                     "Hammersmith and Fulham", "Wandsworth", "Lewisham", "Newham"))

tiff("Outputs/COVIDLondon.tiff", units="in", width=8, height=5, res=500)
ggplot(subset(temp2, date>"2020-05-01"), aes(x=date, y=name, fill=avgcaserates))+
  geom_tile(colour="white")+
  scale_fill_distiller(palette="Spectral", name="New cases\nper 100,000")+
  scale_x_date(name="Date")+
  scale_y_discrete(name="")+
  theme_classic()+
  labs(title="No clear evidence of a rise in cases after the protests in Central London",
       subtitle="7-day rolling average of new confirmed COVID-19 cases",
       caption="Data from PHE | Plot by @VictimOfMaths")
dev.off()

temp3 <- subset(heatmap, name %in% c("Leicester", "Bedford", "Barnsley", "Rotherham",
                                     "Kirklees", "Bradford", "Rochdale", "Oldham",
                                     "Tameside", "Blackburn with Darwen"))

tiff("Outputs/COVIDPillarsHeatmap.tiff", units="in", width=10, height=5, res=500)
ggplot(subset(temp3, date>"2020-05-01"), aes(x=date, y=name, fill=avgcaserates))+
  geom_tile(colour="white")+
  scale_fill_distiller(palette="Spectral", name="New cases\nper 100,000")+
  scale_x_date(name="Date")+
  scale_y_discrete(name="")+
  theme_classic()+
  labs(title="Mixed Pillar 1 trajectories in areas with high combined Pillar 1 and 2 tests in week 25",
       subtitle="7-day rolling average of new confirmed COVID-19 cases",
       caption="Data from PHE | Plot by @VictimOfMaths")
dev.off()

#Graph of pillar 1 tests in any LA you like
LA <- "Bradford"
tiff(paste0("Outputs/COVIDNewCases", LA, ".tiff"), units="in", width=8, height=6, res=500)
ggplot()+
  geom_col(data=subset(heatmap, name==LA), aes(x=date, y=cases), fill="skyblue2")+
  geom_line(data=subset(heatmap, name==LA & date<max-1), aes(x=date, y=casesroll_avg), colour="red")+
  scale_x_date(name="Date")+
  scale_y_continuous("New COVID-19 cases")+
  theme_classic()+
  theme(plot.subtitle=element_markdown())+
  labs(title=paste0("Confirmed new COVID cases in ",LA),
       subtitle="Confirmed new COVID-19 cases identified through combined pillar 1 & 2 testing and the <span style='color:Red;'>7-day rolling average",
       caption="Data from PHE | Plot by @VictimOfMaths")
dev.off()

#New lockdown areas
LA <- c("Calderdale", "Blackburn with Darwen", "Leicester", "Bury", "Oldham", "Manchester",
        "Salford", "Rochdale", "Stockport", "Tameside", "Trafford", "Wigan", "Bolton",
        "Kirklees", "Lancashire")

tiff("Outputs/COVIDNewLockdown.tiff", units="in", width=10, height=5, res=500)
ggplot(subset(heatmap, name %in% LA), aes(x=date, y=name, fill=avgcaserates))+
  geom_tile(colour="white")+
  geom_segment(aes(x=as.Date("2020-06-29"), xend=as.Date("2020-06-29"), y=0, yend=16), colour="NavyBlue", linetype=2)+
  geom_segment(aes(x=as.Date("2020-07-31"), xend=as.Date("2020-07-31"), y=0, yend=16), colour="Red", linetype=2)+
  scale_fill_distiller(palette="Spectral", name="New cases\nper 100,000")+
  scale_x_date(name="Date")+
  scale_y_discrete(name="")+
  theme_classic()+
  labs(title="Trajectories of COVID cases in areas with second lockdown restrictions",
       subtitle="7-day rolling average of new confirmed COVID-19 cases per 100,000 inhabitants",
       caption="Data from PHE | Plot by @VictimOfMaths")
dev.off()
