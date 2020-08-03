rm(list=ls())

library(tidyverse)
library(curl)
library(paletteer)
library(ggtext)
library(RcppRoll)
library(readxl)
library(googledrive)
library(sf)

#Download latest testing data from 
# https://www.gov.uk/guidance/coronavirus-covid-19-information-for-the-public
temp <- tempfile()
source <- "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/906445/2020-08-03-COVID-19-UK-testing-time-series.csv"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
rawdata <- read.csv(temp)[,c(1,3,4,7,11,13)]
colnames(rawdata) <- c("Date", "Nation", "Pillar", "Tests", "Cases.old", "Cases.new")
rawdata$Date <- as.Date(rawdata$Date, format="%d/%m/%Y")

rawdata$Cases.new <- as.numeric(as.character(rawdata$Cases.new))
rawdata$Tests <- as.numeric(as.character(rawdata$Tests))

#Do some mangling of the data to address the change in methodology on 1st July
rawdata <- rawdata %>% 
  filter(!Pillar %in% c("Pillar 3", "Pillar 4") & Nation=="UK") %>% 
  mutate(Cases=coalesce(Cases.old, Cases.new), Cases=replace_na(Cases,0),
         Pillar=substr(Pillar,1,8), Tests=replace_na(Tests,0)) %>% 
  group_by(Pillar, Date) %>% 
  arrange(Date) %>% 
  summarise(Cases=sum(Cases), Tests=sum(Tests)) %>% 
  mutate(Cases_roll=roll_mean(Cases, 7, align="right", fill=0),
         Tests_roll=roll_mean(Tests, 7, align="right", fill=0),
         Posrate=Cases/Tests,
         Posrate_roll=roll_mean(Posrate, 7, align="right", fill=0))

tiff("Outputs/COVIDPillars.tiff", units="in", width=8, height=6, res=500)
ggplot(rawdata, aes(x=Date, y=Cases_roll, fill=Pillar))+
  geom_area(show.legend=FALSE)+
  scale_fill_paletteer_d("NineteenEightyR::malibu")+
  scale_y_continuous("New confirmed COVID-19 cases")+
  theme_classic()+
  theme(plot.subtitle=element_markdown())+
  labs(title="Positive tests from both Pillars 1 and 2 seem to have flatlined",
       subtitle="Rolling 7-day average of new COVID-19 cases in the UK identified through <span style='color:#FF4E86;'>Pillar 1</span> and <span style='color:#FF9E44;'>Pillar 2</span> testing<br>(Pillar 1 data includes Welsh data on both Pillars).",
       caption="Data from DHSC & PHE | Plot by @VictimOfMaths")
dev.off()

data_wide <- spread(rawdata[,-c(4:8)], Pillar, Cases)
data_wide$Cases=data_wide$`Pillar 1`+data_wide$`Pillar 2`
data_wide$Cases_roll <- roll_mean(data_wide$Cases, 7, align="right", fill=0)

#Bar chart version
tiff("Outputs/COVIDPillarsBar.tiff", units="in", width=8, height=6, res=500)
ggplot()+
  geom_col(data=rawdata, aes(x=Date, y=Cases, fill=Pillar), show.legend=FALSE)+
  geom_line(data=data_wide, aes(x=Date, y=Cases_roll), colour="navyblue")+
  scale_fill_paletteer_d("NineteenEightyR::malibu")+
  scale_y_continuous("New confirmed COVID-19 cases")+
  theme_classic()+
  theme(plot.subtitle=element_markdown())+
  labs(title="The decline in new COVID-19 cases has definitely stopped",
       subtitle="Rolling 7-day average of new COVID-19 cases in the UK identified through <span style='color:#FF4E86;'>Pillar 1</span> and <span style='color:#FF9E44;'>Pillar 2</span> testing<br>(Pillar 1 data includes Welsh data on both Pillars).",
       caption="Data from DHSC & PHE | Plot by @VictimOfMaths")+
  annotate("text", x=as.Date("2020-05-13"), y=4200, label="Rolling 7-day average of new cases",
           colour="navyblue", hjust=0)
dev.off()
  
#Plot of positivity rates
tiff("Outputs/COVIDPillarsPosRate.tiff", units="in", width=8, height=6, res=500)
ggplot(subset(rawdata, Date>as.Date("2020-04-06")), 
       aes(x=Date, y=Posrate_roll, colour=Pillar))+
  geom_line(show.legend=FALSE)+
  scale_y_continuous(name="Proportion of tests returning a positive result",
                     breaks=c(0,0.1,0.2,0.3), labels=c("0%", "10%", "20%", "30%"))+
  scale_colour_paletteer_d("NineteenEightyR::malibu")+
  theme_classic()+
  theme(plot.subtitle=element_markdown())+
  labs(title="The proportion of COVID-19 tests returning positive remains low",
       subtitle="Rolling 7-day average test positivity rate for <span style='color:#FF4E86;'>Pillar 1</span> and <span style='color:#FF9E44;'>Pillar 2</span> testing<br>(Pillar 1 data includes Welsh data on both Pillars).",
       caption="Data from DHSC & PHE | Plot by @VictimOfMaths")
dev.off()


#Download from Google Drive compiled by Daniel Howdon
#https://twitter.com/danielhowdon/status/1278062684622258177?s=20
#New version:https://twitter.com/danielhowdon/status/1278676343631347712?s=20
temp <- tempfile()
#drive_download(as_id("1JcyH0Z85Fi8LS_1F4l92-12biPQHY7biYO2EgwFK6CE"), path=temp, overwrite=TRUE)
drive_download(as_id("1uh3E396yMFCTr_d6P__gyi99EshQ1BxFpjqwljXxR_M"), path=temp, overwrite=TRUE)
temp <- paste0(temp, ".xlsx")
data <- read_excel(temp, sheet=, range="A1:R151")

#Calculate pillar 2 cases as a proportion of total
data$pillar2propwk26 <- data$`Week 26 pillar 2 cases`/data$`Week 26 implied cases`
data$pillar2propwk27 <- data$`Week 27 pillar 2 cases`/data$`Week 27 implied cases`

#set negative proportions to missing
data$pillar2propwk26 <- if_else(data$pillar2propwk26<0, -1, data$pillar2propwk26)
data$pillar2propwk26 <- na_if(data$pillar2propwk26, -1)
data$pillar2propwk27 <- if_else(data$pillar2propwk27<0, -1, data$pillar2propwk27)
data$pillar2propwk27 <- na_if(data$pillar2propwk27, -1)

#Download LA shapefile
#Download shapefile of LA boundaries
temp <- tempfile()
temp2 <- tempfile()
source <- "https://opendata.arcgis.com/datasets/6638c31a8e9842f98a037748f72258ed_0.zip?outSR=%7B%22latestWkid%22%3A27700%2C%22wkid%22%3A27700%7D"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)

#The actual shapefile has a different name each time you download it, so need to fish the name out of the unzipped file
name <- list.files(temp2, pattern=".shp")
shapefile <- st_read(file.path(temp2, name))

names(shapefile)[names(shapefile) == "ctyua17cd"] <- "UTLA code"

int1 <- filter(data, `UTLA name`=="Bournemouth, Christchurch and Poole")
int1$`UTLA code` <- "E06000028"
int2 <- filter(data, `UTLA name`=="Bournemouth, Christchurch and Poole")
int2$`UTLA code` <- "E06000029"
int3 <- filter(data, `UTLA name`=="Bournemouth, Christchurch and Poole")
int3$`UTLA code` <- "E10000009"

data <- rbind(data, int1, int2, int3)

map.data <- full_join(shapefile, data, by="UTLA code", all.y=TRUE)
map.data <- map.data %>% drop_na("population")

tiff("Outputs/COVIDPillars1vs2wk26.tiff", units="in", width=8, height=6, res=500)
ggplot()+
  geom_sf(data=map.data, aes(geometry=geometry, fill=pillar2propwk26), colour=NA)+
  xlim(10000,655644)+
  ylim(5337,700000)+
  scale_fill_paletteer_c("scico::roma", name="Proportion coming\nfrom Pillar 2",
                         breaks=c(0,0.25,0.5,0.75,1), 
                         labels=c("0%", "25%", "50%", "75%", "100%"), 
                         na.value="grey60")+
  theme_classic()+
  theme(axis.line=element_blank(), axis.ticks=element_blank(), axis.text=element_blank(),
        axis.title=element_blank(), plot.title.position="plot",
        plot.subtitle=element_markdown())+
  labs(title="New COVID cases are being identified through different routes across England",
       subtitle="Proportion of new confirmed cases estimated to have come from <span style='color:#1c3699;'>Pillar 2 </span>vs. <span style='color:#801f01;'>Pillar 1 </span>tests in week 26",
       caption="Data estimated fromn PHE figures by @danielhowdon | Plot by @VictimOfMaths")
dev.off()

tiff("Outputs/COVIDPillars1vs2wk27.tiff", units="in", width=8, height=6, res=500)
ggplot()+
  geom_sf(data=map.data, aes(geometry=geometry, fill=pillar2propwk27), colour=NA)+
  xlim(10000,655644)+
  ylim(5337,700000)+
  scale_fill_paletteer_c("scico::roma", name="Proportion coming\nfrom Pillar 2",
                         breaks=c(0,0.25,0.5,0.75,1), 
                         labels=c("0%", "25%", "50%", "75%", "100%"), 
                         na.value="grey60")+
  theme_classic()+
  theme(axis.line=element_blank(), axis.ticks=element_blank(), axis.text=element_blank(),
        axis.title=element_blank(), plot.title.position="plot",
        plot.subtitle=element_markdown())+
  labs(title="New COVID cases are being identified through different routes across England",
       subtitle="Proportion of new confirmed cases estimated to have come from <span style='color:#1c3699;'>Pillar 2 </span>vs. <span style='color:#801f01;'>Pillar 1 </span>tests in week27",
       caption="Data estimated fromn PHE figures by @danielhowdon | Plot by @VictimOfMaths")
dev.off()
