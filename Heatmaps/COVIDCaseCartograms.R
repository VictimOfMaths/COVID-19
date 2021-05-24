rm(list=ls())

library(tidyverse)
library(curl)
library(paletteer)
library(sf)
library(ragg)
library(ggtext)
library(scales)
library(extrafont)
library(ggrepel)
library(gganimate)

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
  select(c(1,2,10,11))

colnames(casedata) <- c("Lacode", "areaName", "prev", "latest")

casedata <- casedata %>% 
  mutate(abschange=latest-prev, relchange=abschange/prev)

#Download Carl Baker's lovely map
ltla <- tempfile()
source <- ("https://github.com/houseofcommonslibrary/uk-hex-cartograms-noncontiguous/raw/main/geopackages/LocalAuthorities-lowertier.gpkg")
ltla <- curl_download(url=source, destfile=ltla, quiet=FALSE, mode="wb")

Background <- st_read(ltla, layer="7 Background")

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
  theme(plot.title=element_text(face="bold", size=rel(1.2)),
        text=element_text(family="Roboto"))+
  labs(title="Current COVID-19 outbreaks are isolated to a small number of areas",
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
  theme(plot.title=element_markdown(face="bold", size=rel(1.5)),
        text=element_text(family="Roboto"))+
  labs(title="COVID-19 case rates are falling in some areas and rising in others",
       subtitle=paste0("Change in the past week in the rolling 7-day average number of cases at Lower Tier Local Authority level\nData up to ", maxdate),
       caption="Data from PHE, Cartogram from @carlbaker/House of Commons Library\nPlot by @VictimOfMaths")


agg_tiff("Outputs/COVIDCasesLTLAChangeCartogram.tiff", units="in", width=9, height=10, res=800)
plot2
dev.off()

agg_png("Outputs/COVIDCasesLTLAChangeCartogram.png", units="in", width=9, height=10, res=800)
plot2
dev.off()

#Scatter plot of case level vs change
#Bring in region
temp <- tempfile()
source <- "https://opendata.arcgis.com/datasets/0c3a9643cc7c4015bb80751aad1d2594_0.csv"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
LADtoRegion <- read.csv(temp)[,c(1,4)]
colnames(LADtoRegion) <- c("LTLA", "Region")

casedata <- casedata %>% 
  merge(LADtoRegion, all.x=TRUE, by.x="Lacode", by.y="LTLA") %>% 
  mutate(Region=case_when(
    Lacode %in% c("E07000244", "E07000245") ~ "East of England",
    Lacode %in% c("E06000058", "E06000059", "E07000246") ~ "South West",
    substr(Lacode, 1, 1) == "W" ~ "Wales",
    substr(Lacode, 1, 1) == "S" ~ "Scotland",
    substr(Lacode, 1, 1) == "N" ~ "Northern Ireland",
    TRUE ~ Region),
    Country=case_when(
      substr(Lacode, 1, 1) == "E" ~ "England",
      substr(Lacode, 1, 1) == "W" ~ "Wales",
      substr(Lacode, 1, 1) == "S" ~ "Scotland",
      substr(Lacode, 1, 1) == "N" ~ "Northern Ireland"))

#Bring in LA populations
temp <- tempfile()
source <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fpopulationandmigration%2fpopulationestimates%2fdatasets%2fpopulationestimatesforukenglandandwalesscotlandandnorthernireland%2fmid20182019laboundaries/ukmidyearestimates20182019ladcodes.xls"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
LApop <- read_excel(temp, sheet="MYE2-All", range="A5:D435", col_names=TRUE)
colnames(LApop) <- c("code", "name", "geography", "pop")

#Merge isles of Scilly in with Cornwall
LApop$code <- if_else(LApop$code=="E06000053", "E06000052", LApop$code)
LApop$name <- if_else(LApop$name=="Isles of Scilly", "Cornwall", LApop$name)

#Merge City of London & Hackney
LApop$code <- if_else(LApop$code=="E09000001", "E09000012", LApop$code)
LApop$name <- if_else(LApop$name=="City of London", "Hackney and City of London", LApop$name)
LApop$name <- if_else(LApop$name=="Hackney", "Hackney and City of London", LApop$name)

LApop <- LApop %>% 
  group_by(name, code) %>% 
  summarise(pop=sum(pop)) %>% 
  ungroup()

plotdata <- casedata %>% 
  merge(LApop, by.x="Lacode", by.y="code" , all.x=TRUE)

agg_tiff("Outputs/COVIDCasesLTLAChangeScatter.tiff", units="in", width=8, height=7, res=800)
ggplot(plotdata, aes(x=latest, y=abschange, fill=Country))+
  geom_point(aes(size=pop), shape=21, alpha=0.7)+
  geom_hline(yintercept=0)+
  geom_vline(xintercept=0)+
  geom_text_repel(aes(label=areaName), size=rel(2.3))+
  scale_x_continuous(name="New cases in the past week\n(rate per 100,000)", limits=c(0,NA))+
  scale_y_continuous(name="Change in case rate compared to the preceding week")+
  scale_fill_paletteer_d("fishualize::Scarus_quoyi", name="")+
  scale_size(guide=FALSE)+
  theme_classic()+
  theme(axis.line=element_blank(), text=element_text(family="Lato"),
        legend.position = "top", plot.title.position="plot",
        plot.title=element_text(face="bold", size=rel(1.6)))+
  labs(title="Recent COVID outbreaks are currently contained to a few areas",
       subtitle="COVID case rates and how these have changed in the past week in UK Local Authorities\nBubbles are sized by population",
       caption="Data from coronavirus.data.gov.uk and ONS\nPlot by @VictimOfMaths")
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
  select(c(7,8,23,24))

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
  geom_sf(data=MSOA %>% filter(RegionNation!="Wales"), 
          aes(geometry=geom, fill=latest), colour=NA)+
  geom_sf(data=LAsMSOA %>% filter(RegionNation!="Wales"), 
          aes(geometry=geom), fill=NA, colour="White", size=0.1)+
  geom_sf(data=GroupsMSOA %>% filter(RegionNation!="Wales"), 
          aes(geometry=geom), fill=NA, colour="Black")+
  geom_sf_text(data=Group_labelsMSOA %>% filter(RegionNation!="Wales"), 
               aes(geometry=geom, label=Group.labe,
                                      hjust=just), size=rel(2.4), colour="Black")+
  scale_fill_paletteer_c("pals::ocean.haline", direction=-1, 
                         name="Cases per\n100,000", limits=c(0,NA))+
  theme_void()+
  theme(plot.title=element_text(face="bold", size=rel(1.2)),
        text=element_text(family="Roboto"))+
  labs(title="COVID-19 cases are highest in Yorkshire and Manchester",
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
  geom_sf(data=MSOA %>% filter(RegionNation!="Wales"), 
          aes(geometry=geom, fill=abschange), colour=NA)+
  geom_sf(data=LAsMSOA %>% filter(RegionNation!="Wales"), 
          aes(geometry=geom), fill=NA, colour="White", size=0.1)+
  geom_sf(data=GroupsMSOA %>% filter(RegionNation!="Wales"), 
          aes(geometry=geom), fill=NA, colour="Black")+
  geom_sf_text(data=Group_labelsMSOA %>% filter(RegionNation!="Wales"), 
               aes(geometry=geom, label=Group.labe,
                                      hjust=just), size=rel(2.4), colour="Black")+
  scale_fill_paletteer_c("scico::roma", limit=c(-1,1)*max(abs(msoacasedata$abschange), na.rm=TRUE), 
                         name="Change in cases\nper day per 100,000\nin the past week", direction=-1,
                         na.value="transparent")+
  theme_void()+
  theme(plot.title=element_text(face="bold", size=rel(1.2)),
        text=element_text(family="Roboto"))+
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

#######################################################
#LTLA level animation
animdata <- read.csv(cases) %>% 
  mutate(date=as.Date(date)) %>% 
  rename(Lacode=areaCode)

ltlacaseanim <- st_read(ltla, layer="4 LTLA-2019") %>% 
  left_join(animdata, by="Lacode")

#extract latest date with full UK data
temp <- animdata %>%
  group_by(Lacode) %>%
  filter(!is.na(newCasesBySpecimenDateRollingRate)) %>% 
  mutate(min=min(date), max=max(date))

completefrom <- max(temp$min, na.rm=TRUE)
completeto <- min(temp$max, na.rm=TRUE)

CartogramanimUK <- ggplot()+
  geom_sf(data=Background, aes(geometry=geom))+
  geom_sf(data=subset(ltlacaseanim, date>=as.Date("2020-09-01") & date<=as.Date(completeto)), 
          aes(geometry=geom, fill=newCasesBySpecimenDateRollingRate), 
          colour="Black", size=0.1)+
  geom_sf(data=Groups, aes(geometry=geom), fill=NA, colour="Black")+
  geom_sf_text(data=Group_labels, aes(geometry=geom, label=Group.labe,
                                      hjust=just), size=rel(2.4), colour="Black")+
  scale_fill_distiller(palette="Spectral", name="Daily confirmed\ncases per\n100,000", 
                       na.value="white")+
  transition_time(date)+
  theme_void()+
  theme(plot.title=element_text(face="bold", size=rel(1.2)),
        text=element_text(family="Roboto"))+
  labs(title="Timeline of the UK's 'Second Wave'",
       subtitle="Rolling 7-day average number of case rates\nData up to  {frame_time}",
       caption="Data from PHE, Cartogram from @carlbaker/House of Commons Library\nPlot by @VictimOfMaths")

animate(CartogramanimUK, duration=18, fps=10, width=2000, height=3000, res=300, renderer=gifski_renderer("Outputs/CartogramanimUK.gif"), 
        device="ragg_png", end_pause=60)
