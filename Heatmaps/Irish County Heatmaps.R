rm(list=ls())

library(tidyverse)
library(curl)
library(forcats)
library(readxl)
library(RcppRoll)
library(cowplot)
library(sf)
library(paletteer)

#Read in data
temp <- tempfile()
#source <- "https://opendata.arcgis.com/datasets/d9be85b30d7748b5b7c09450b8aede63_0.csv?geometry=%7B%22xmin%22%3A-23.251%2C%22ymin%22%3A51.133%2C%22xmax%22%3A6.632%2C%22ymax%22%3A55.71%2C%22type%22%3A%22extent%22%2C%22spatialReference%22%3A%7B%22wkid%22%3A4326%7D%7D"
source <- "https://opendata.arcgis.com/datasets/d9be85b30d7748b5b7c09450b8aede63_0.csv"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
data <- read_csv(temp)

#Strip out geographical information
data <- data[,c(5,3,4,11,12)]

colnames(data) <- c("TimeStamp", "county", "pop", "cumul_cases", "caseprop")

#Convert timestamp to date
data$date <- as.Date(substr(data$TimeStamp, 1, 10))

#Calculate daily cases
data <- data %>%
  arrange(county, date) %>%
  group_by(county) %>%
  mutate(cases=cumul_cases-lag(cumul_cases,1))

#Add in missing data for 30th June - assume no new cases
temp <- data.frame(date=as.Date("2020-06-30"),
                   county=unique(data$county),
                   cases=0)

data <- bind_rows(data, temp)

#For 3 counties (Leitrim, Limerick and Sligo) the case count goes *down* in early May. Ignore these for now
data$cases <- ifelse(is.na(data$cases), 0, data$cases)
data$cases <- ifelse(data$cases<0, 0, data$cases)
data$cumul_cases <- ifelse(is.na(data$cumul_cases), 0, data$cumul_cases)

heatmap <- data %>%
  group_by(county) %>%
  arrange(date) %>% 
  mutate(casesroll_avg=roll_mean(cases, 7, align="right", fill=0)) %>%
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
       subtitle=paste0("The heatmap represents the 7-day rolling average of the number of new confirmed cases, normalised to the maximum value within the county.\nCounties are ordered by the date at which they reached their peak number of new cases. Bars on the right represent the absolute number of cases in each county.\nData updated to ", plotto,". Data for most recent days is provisional and may be revised upwards as additional tests are processed."),
       caption="Data from data.gov.ie | Plot by @VictimOfMaths")+
  theme(axis.line.y=element_blank(), plot.subtitle=element_text(size=rel(0.78)), plot.title.position="plot",
        axis.text.y=element_text(colour="Black"))

casebars <- ggplot(subset(heatmap, date==maxcaseday), aes(x=totalcases, y=fct_reorder(county, maxcaseday), fill=totalcases))+
  geom_col(show.legend=FALSE)+
  theme_classic()+
  scale_fill_distiller(palette="Spectral")+
  scale_x_continuous(name="Total confirmed cases")+
  theme(axis.title.y=element_blank(), axis.line.y=element_blank(), axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), axis.text.x=element_text(colour="Black"))

tiff("Outputs/COVIDIrishLACasesHeatmap.tiff", units="in", width=11, height=6, res=500)
plot_grid(casetiles, casebars, align="h", rel_widths=c(1,0.2))
dev.off()

library(ggridges)

tiff("Outputs/COVIDIrishCountyCaseRidges.tiff", units="in", width=11, height=6, res=500)
ggplot(heatmap, aes(x=date, y=fct_reorder(county, totalcases), height=casesroll_avg, fill=casesroll_avg))+
  geom_density_ridges_gradient(stat="identity", rel_min_height=0.007)+
  theme_classic()+
  scale_fill_distiller(palette="Spectral", name="Cases per day\n7-day rolling avg.")+
  scale_x_date(name="Date", limits=as.Date(c(plotfrom, plotto)), expand=c(0,0))+
  scale_y_discrete(name="")+
  labs(title="Timelines of confirmed COVID-19 cases in Irish counties",
       caption="Data from data.gov.ie | Plot by @VictimOfMaths")
dev.off()

#Download shapefile
temp <- tempfile()
temp2 <- tempfile()
source <- "https://opendata.arcgis.com/datasets/0d5984f732c54246bd087768223c92eb_0.zip?outSR=%7B%22latestWkid%22%3A2157%2C%22wkid%22%3A2157%7D"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)

#The actual shapefile has a different name each time you download it, so need to fish the name out of the unzipped file
name <- list.files(temp2, pattern=".shp")
shapefile <- st_read(file.path(temp2, name))

#Tidy up country names
shapefile$county <- paste0(substr(shapefile$COUNTY,1,1), tolower(substr(shapefile$COUNTY,2,99)))

mapdata <- heatmap %>% 
  filter(date==as.Date(plotto)) %>% 
  full_join(shapefile, by="county")

#Bring in NI data
NIdata <- read.csv("COVID_LA_Plots/LACases.csv")[,c(2,3,4,5,16)] %>% 
  filter(country=="Northern Ireland" & name!="Northern Ireland") %>% 
  mutate(date=as.Date(date))

#Align end dates
plotto <- min(plotto, max(NIdata$date[!is.na(NIdata$caserate_avg)]))

NIdata <- subset(NIdata, date==as.Date(plotto))

#NI shapefile
temp <- tempfile()
temp2 <- tempfile()
source <- "https://opendata.arcgis.com/datasets/1d78d47c87df4212b79fe2323aae8e08_0.zip?outSR=%7B%22latestWkid%22%3A27700%2C%22wkid%22%3A27700%7D"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)

#The actual shapefile has a different name each time you download it, so need to fish the name out of the unzipped file
name <- list.files(temp2, pattern=".shp")
shapefile.NI <- st_read(file.path(temp2, name))

names(shapefile.NI)[names(shapefile.NI) == "lad19cd"] <- "code"

mapdata.NI <- full_join(shapefile.NI, NIdata, by="code")
mapdata.NI <- subset(mapdata.NI, !is.na(country))

#Transform to common projection (Irish Transverse Mercator)
mapdata.NI <- st_transform(mapdata.NI, 2157)

outline <- mapdata.NI %>% 
  summarise()

tiff("Outputs/COVIDIrelandRatesMap.tiff", units="in", width=8, height=8, res=500)
ggplot()+
  geom_sf(data=mapdata, aes(geometry=geometry, fill=casesroll_avg*100000/pop), colour=NA)+
  geom_sf(data=mapdata.NI, aes(geometry=geometry, fill=caserate_avg), colour=NA)+
  geom_sf(data=outline, aes(geometry=geometry), fill=NA, colour="White")+
  scale_fill_paletteer_c("viridis::inferno", name="Daily cases\nper 100,000")+
  theme_classic()+
  theme(axis.line=element_blank(), axis.ticks=element_blank(), axis.text=element_blank(),
        axis.title=element_blank(), plot.title=element_text(face="bold", size=rel(1.5)))+
  labs(title="COVID-19 cases across Ireland",
       subtitle=paste("Daily rates of confirmed new COVID-19 cases in the Republic of Ireland and Northern Ireland\nData from",plotto),
       caption="Data from data.gov.ie and DoHNI | Plot by @VictimOfMaths")
dev.off()
