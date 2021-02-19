rm(list=ls())

library(tidyverse)
library(paletteer)
library(sf)
library(ragg)
library(ggtext)
library(scales)

#Start with LA level cases for the whole of the UK
cases <- tempfile()
url <- "https://api.coronavirus.data.gov.uk/v2/data?areaType=ltla&metric=newCasesBySpecimenDateRollingRate&format=csv"
cases <- curl_download(url=url, destfile=cases, quiet=FALSE, mode="wb")

casedata <- read.csv(cases) %>% 
  mutate(date=as.Date(date))

maxdate <- max(casedata$date)

casedata <- casedata %>% 
  #Take the most recent 2 weeks of data
  group_by(areaName) %>% 
  arrange(date) %>% 
  slice_tail(n=8) %>% 
  ungroup() %>% 
  spread(date, newCasesBySpecimenDateRollingRate) %>% 
  select(c(2,3,4,11))

colnames(casedata) <- c("Lacode", "areaName", "prev", "latest")

casedata <- casedata %>% 
  mutate(abschange=latest-prev, relchange=abschange/prev)

#Download Carl Baker's lovely map
ltla <- tempfile()
source <- ("https://github.com/houseofcommonslibrary/uk-hex-cartograms-noncontiguous/raw/main/geopackages/LocalAuthorities-lowertier.gpkg")
ltla <- curl_download(url=source, destfile=ltla, quiet=FALSE, mode="wb")

Background <- st_read(ltla, layer="6 Background")

ltlacases <- st_read(ltla, layer="4 LTLA-2019") %>% 
  left_join(casedata, by="Lacode")

Groups <- st_read(ltla, layer="2 Groups")

Group_labels <- st_read(ltla, layer="1 Group labels") %>% 
  mutate(just=if_else(LabelPosit=="Left", 0, 1))

plot1 <- ggplot()+
  geom_sf(data=Background, aes(geometry=geom))+
  geom_sf(data=ltlacases, aes(geometry=geom, fill=latest), colour="Black", size=0.1)+
  geom_sf(data=Groups, aes(geometry=geom), fill=NA, colour="Black")+
  geom_sf_text(data=Group_labels, aes(geometry=geom, label=Group.labe,
                                      hjust=just), size=rel(2.4), colour="Black")+
  scale_fill_paletteer_c("pals::ocean.haline", direction=-1,
                         name="Cases per\n100,000")+
  theme_void()+
  theme(plot.title=element_text(face="bold", size=rel(1.2)))+
  labs(title="COVID-19 cases are highest in the Midlands and urban areas in the North",
       subtitle=paste0("Rolling 7-day average number of cases in the past week at Lower Tier Local Authority level\nData up to ", maxdate),
       caption="Data from PHE, Cartogram from @carlbaker/House of Commons Library\nPlot by @VictimOfMaths")

agg_tiff("Outputs/COVIDCasesLTLACartogram.tiff", units="in", width=9, height=10, res=800)
plot1
dev.off()

agg_png("Outputs/COVIDCasesLTLACartogram.png", units="in", width=9, height=10, res=800)
plot1
dev.off()

plot2 <- ggplot()+
  geom_sf(data=Background, aes(geometry=geom))+
  geom_sf(data=ltlacases, aes(geometry=geom, fill=abschange), colour="Black", size=0.1)+
  geom_sf(data=Groups, aes(geometry=geom), fill=NA, colour="Black")+
  geom_sf_text(data=Group_labels, aes(geometry=geom, label=Group.labe,
                                      hjust=just), size=rel(2.4), colour="Black")+
  scale_fill_paletteer_c("scico::roma", limit=c(-1,1)*max(abs(casedata$abschange)), 
                         name="Change in cases\nper day per 100,000\nin the past week", direction=-1,
                         na.value="transparent")+
  theme_void()+
  theme(plot.title=element_markdown(face="bold", size=rel(1.5)))+
  labs(title="COVID-19 cases are falling *almost* everywhere",
       subtitle=paste0("Change in the past week in the rolling 7-day average number of cases at Lower Tier Local Authority level\nData up to ", maxdate),
       caption="Data from PHE, Cartogram from @carlbaker/House of Commons Library\nPlot by @VictimOfMaths")


agg_tiff("Outputs/COVIDCasesLTLAChangeCartogram.tiff", units="in", width=9, height=10, res=800)
plot2
dev.off()

agg_png("Outputs/COVIDCasesLTLAChangeCartogram.png", units="in", width=9, height=10, res=800)
plot2
dev.off()

#####################################
#MSOA level data
msoacases <- tempfile()
url <- "https://api.coronavirus.data.gov.uk/v2/data?areaType=msoa&metric=newCasesBySpecimenDateRollingRate&format=csv"
msoacases <- curl_download(url=url, destfile=msoacases, quiet=FALSE, mode="wb")

msoacasedata <- read.csv(msoacases) %>% 
  mutate(date=as.Date(date))

maxdate2 <- max(msoacasedata$date)

msoacasedata <- msoacasedata %>% 
  #Take the most recent 2 weeks of data
  group_by(areaName) %>% 
  arrange(date) %>% 
  slice_tail(n=2) %>% 
  ungroup() %>% 
  spread(date, newCasesBySpecimenDateRollingRate) %>% 
  select(c(8:11))

colnames(msoacasedata) <- c("msoa11cd", "areaName", "prev", "latest")

msoacasedata <- msoacasedata %>% 
  mutate(abschange=latest-prev, relchange=abschange/prev)

msoa <- tempfile()
source <- ("https://github.com/houseofcommonslibrary/uk-hex-cartograms-noncontiguous/raw/main/geopackages/MSOA.gpkg")
msoa <- curl_download(url=source, destfile=msoa, quiet=FALSE, mode="wb")

BackgroundMSOA <- st_read(msoa, layer="5 Background")

MSOA <- st_read(msoa, layer="4 MSOA hex") %>% 
  left_join(msoacasedata, by="msoa11cd")

GroupsMSOA <- st_read(msoa, layer="2 Groups")

Group_labelsMSOA <- st_read(msoa, layer="1 Group labels") %>% 
  mutate(just=if_else(LabelPosit=="Left", 0, 1))

LAsMSOA <- st_read(msoa, layer="3 Local authority outlines (2019)")

plot3 <- ggplot()+
  geom_sf(data=BackgroundMSOA, aes(geometry=geom))+
  geom_sf(data=MSOA, aes(geometry=geom, fill=latest), colour=NA)+
  geom_sf(data=LAsMSOA, aes(geometry=geom), fill=NA, colour="White", size=0.1)+
  geom_sf(data=GroupsMSOA, aes(geometry=geom), fill=NA, colour="Black")+
  geom_sf_text(data=Group_labelsMSOA, aes(geometry=geom, label=Group.labe,
                                      hjust=just), size=rel(2.4), colour="Black")+
  scale_fill_paletteer_c("pals::ocean.haline", direction=-1, 
                         name="Cases per\n100,000", limits=c(0,NA))+
  theme_void()+
  theme(plot.title=element_text(face="bold", size=rel(1.2)))+
  labs(title="COVID-19 cases are highest in the Midlands and urban areas in the North",
       subtitle=paste0("Rolling 7-day average number of cases in the past week at Middle Super Output Area level in England\nData up to ", 
                       maxdate, ". MSOAs with small populations and/or low case counts may be censored and appear in grey."),       
       caption="Data from PHE, Cartogram from @carlbaker/House of Commons Library\nPlot by @VictimOfMaths")

agg_tiff("Outputs/COVIDCasesMSOACartogram.tiff", units="in", width=10, height=8, res=800)
plot3
dev.off()

agg_png("Outputs/COVIDCasesMSOACartogram.png", units="in", width=10, height=8, res=800)
plot3
dev.off()

plot4 <- ggplot()+
  geom_sf(data=BackgroundMSOA, aes(geometry=geom))+
  geom_sf(data=MSOA, aes(geometry=geom, fill=abschange), colour=NA)+
  geom_sf(data=LAsMSOA, aes(geometry=geom), fill=NA, colour="White", size=0.1)+
  geom_sf(data=GroupsMSOA, aes(geometry=geom), fill=NA, colour="Black")+
  geom_sf_text(data=Group_labelsMSOA, aes(geometry=geom, label=Group.labe,
                                      hjust=just), size=rel(2.4), colour="Black")+
  scale_fill_paletteer_c("scico::roma", limit=c(-1,1)*max(abs(msoacasedata$abschange), na.rm=TRUE), 
                         name="Change in cases\nper day per 100,000\nin the past week", direction=-1,
                         na.value="transparent")+
  theme_void()+
  theme(plot.title=element_text(face="bold", size=rel(1.2)))+
  labs(title="COVID-19 cases are falling in almost all parts of England",
       subtitle=paste0("Change in the past week in the rolling 7-day average number of cases at Middle Super Output Area level in England\nData up to ", 
                       maxdate, ". MSOAs with small populations and/or low case counts may be censored and appear in grey."),       
       caption="Data from PHE, Cartogram from @carlbaker/House of Commons Library\nPlot by @VictimOfMaths")

agg_tiff("Outputs/COVIDCasesMSOAChangeCartogram.tiff", units="in", width=10, height=8, res=800)
plot4
dev.off()

agg_png("Outputs/COVIDCasesMSOAChangeCartogram.png", units="in", width=10, height=8, res=800)
plot4
dev.off()
