rm(list=ls())

library(tidyverse)
library(curl)
library(readxl)
library(paletteer)
library(gt)
library(sf)

#Read in vaccination data
#https://www.england.nhs.uk/statistics/statistical-work-areas/covid-19-vaccinations/
vax <- tempfile()
url <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/05/COVID-19-weekly-announced-vaccinations-13-May-2021.xlsx"
vax <- curl_download(url=url, destfile=vax, quiet=FALSE, mode="wb")

vaxdata <- read_excel(vax, sheet="MSOA", range="F16:Q6806", col_names=FALSE) %>% 
  rename(msoa11cd=`...1`, msoa11nm=`...2`, `<40`=`...3`,  `40-44`=`...4`, `45-49`=`...5`, 
         `50-54`=`...6`, `55-59`=`...7`, `60-64`=`...8`, `65-69`=`...9`, `70-74`=`...10`, 
         `75-79`=`...11`, `80+`=`...12`) %>% 
  gather(age, vaccinated, c(3:12))

#Download IMD data
temp <- tempfile()
source <- ("https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/833970/File_1_-_IMD2019_Index_of_Multiple_Deprivation.xlsx")
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

IMD <- read_excel(temp, sheet="IMD2019", range="A2:F32845", col_names=FALSE)[,c(1,2,5,6)]
colnames(IMD) <- c("LSOA11CD", "LSOA11NM", "IMDrank", "IMDdecile")

#Download LSOA to MSOA lookup
temp <- tempfile()
source <- ("https://opendata.arcgis.com/datasets/fe6c55f0924b4734adf1cf7104a0173e_0.csv")
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

lookup <- read.csv(temp) %>% 
  select(LSOA11CD, MSOA11CD, RGN11NM) %>% 
  unique()

#Merge into IMD data
IMD <- merge(IMD, lookup, by="LSOA11CD")

#Bring in population data for LSOAs
temp <- tempfile()
temp2 <- tempfile()
source <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fpopulationandmigration%2fpopulationestimates%2fdatasets%2flowersuperoutputareamidyearpopulationestimatesnationalstatistics%2fmid2019sape22dt13/sape22dt13mid2019lsoabroadagesestimatesunformatted.zip"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)

pop <- read_excel(file.path(temp2, "SAPE22DT13-mid-2019-lsoa-Broad_ages-estimates-unformatted.xlsx"),
                  sheet="Mid-2019 Persons", range="A6:G34758", col_names=FALSE)[,c(1,7)]
colnames(pop) <- c("LSOA11CD", "pop")

#Merge into IMD data
IMD <- merge(IMD, pop)

#Calculate IMD rank at MSOA level as weighted average of LSOA level ranks, weight by population
IMD_MSOA <- IMD %>% 
  group_by(MSOA11CD) %>% 
  summarise(IMDrank=weighted.mean(IMDrank, pop), pop=sum(pop)) %>% 
  ungroup() 

pop2 <- read_excel(vax, sheet="Population estimates (NIMS)", range="S16:AE6806", col_names=FALSE) %>% 
  select(-c(2,3)) %>% 
  rename(msoa11cd=`...1`) %>% 
  gather(age, pop, c(2:11)) %>% 
  mutate(age=case_when(
    age=="...4" ~ "<40", 
    age=="...5" ~ "40-44",
    age=="...6" ~ "45-49",
    age=="...7" ~ "50-54",
    age=="...8" ~ "55-59",
    age=="...9" ~ "60-64",
    age=="...10" ~ "65-69",
    age=="...11" ~ "70-74",
    age=="...12" ~ "75-79",
    TRUE ~ "80+")) %>% 
  group_by(msoa11cd, age) %>% 
  summarise(pop=sum(pop)) %>% 
  ungroup()

#COMBINE
vaxdata <- merge(vaxdata, pop2) %>% 
  merge(IMD_MSOA %>% select(-pop), by.x="msoa11cd", by.y="MSOA11CD") %>% 
  mutate(vaxprop=vaccinated/pop)

#Split into under and over 40
vaxdata2 <- vaxdata %>% 
  mutate(age2=if_else(age=="<40", "<40", "40+")) %>%
  group_by(msoa11cd, msoa11nm, IMDrank, age2) %>% 
  summarise(vaccinated=sum(vaccinated), pop=sum(pop)) %>% 
  ungroup() %>% 
  mutate(vaxprop=vaccinated/pop)

#Download Carl Baker's lovely cartogram
msoa <- tempfile()
source <- ("https://github.com/houseofcommonslibrary/uk-hex-cartograms-noncontiguous/raw/main/geopackages/MSOA.gpkg")
msoa <- curl_download(url=source, destfile=msoa, quiet=FALSE, mode="wb")

BackgroundMSOA <- st_read(msoa, layer="5 Background")

MSOA <- st_read(msoa, layer="4 MSOA hex") %>% 
  left_join(vaxdata2, by="msoa11cd") %>% 
  mutate(RegionNation=case_when(
    LA.label=="Hull" ~ "Yorkshire and The Humber", 
    TRUE ~ as.character(RegionNation)))

GroupsMSOA <- st_read(msoa, layer="2 Groups")

Group_labelsMSOA <- st_read(msoa, layer="1 Group labels") %>% 
  mutate(just=if_else(LabelPosit=="Left", 0, 1))

LAsMSOA <- st_read(msoa, layer="3 Local authority outlines (2019)")

LAlabels <- data.frame(x=c(34.3, 34, 39, 37, 34.2, 44.6, 44.7, 39.3, 42.7), 
                       y=c(33.5, 40.5, 40.6, 35.7, 36.2, 34.3, 36.8, 36.4, 40.5), 
                       label=c("Sheffield", "Bradford", "Leeds", "Barnsley", "Kirklees",
                               "NE Lincs", "Hull", "Wakefield", "York"))
Arealabels <- data.frame(x=c(41.5, 33.6, 40.3, 44.6), y=c(43, 40, 33.3, 38.3), 
                         label=c("North Yorks", "West Yorks", "South Yorks", "East Yorks\n& Humber"))

plot <- ggplot()+
  geom_sf(data=MSOA %>% filter(age2=="40+" & RegionNation=="Yorkshire and The Humber"), 
          aes(geometry=geom, fill=vaxprop), colour=NA)+
  geom_sf(data=LAsMSOA %>% filter(RegionNation=="Yorkshire and The Humber"), 
          aes(geometry=geom), fill=NA, colour="White", size=0.2)+
  geom_sf(data=GroupsMSOA %>% filter(RegionNation=="Yorkshire and The Humber"), 
          aes(geometry=geom), fill=NA, colour="Black")+
  scale_fill_paletteer_c("pals::ocean.haline", direction=-1, 
                         name="Proportion of adults aged 40+ who have received\nat least one vaccine dose", limits=c(0.4,1),
                         labels=label_percent(accuracy=1))+
  geom_text(data=LAlabels, aes(x=x, y=y, label=label))+
  geom_text(data=Arealabels, aes(x=x, y=y, label=label), fontface="bold")+
  theme_void()+
  coord_sf(clip="off")+
  theme(plot.title=element_text(face="bold", size=rel(2)),
        text=element_text(family="Roboto"), plot.caption.position="plot",
        legend.position="top")+
  guides(fill = guide_colorbar(title.position = 'top', title.hjust = .5,
                                barwidth = unit(20, 'lines'), barheight = unit(.5, 'lines')))+
  labs(title="Vaccination rates in Yorkshire\n ",
       caption="Data from NHS England\nMap by Colin Angus, University of Sheffield")+
  annotate("text", x=44, y=42.2, label="95% of people aged 40+\nin Easingwold & Stillington\nhave been vaccinated",
           family="Roboto", colour="Grey50")+
  annotate("text", x=36, y=41.6, label="43% of people aged 40+\nin Harehills South\nhave been vaccinated",
           family="Roboto", colour="Grey50")+
  annotate("text", x=44.6, y=39.5, label="Each hexagon represents an area\nof roughly 6,000 people",
           family="Roboto", colour="Grey50")+
  geom_curve(aes(x=42.38, y=42.55, xend=41, yend=41.5), curvature=0.2, colour="Grey50",
             arrow=arrow(length=unit(0.1, "cm"), type="closed"), lineend="round")+
  geom_curve(aes(x=37.3, y=41.5, xend=38.85, yend=39.45), curvature=-0.25, colour="Grey50",
             arrow=arrow(length=unit(0.1, "cm"), type="closed"), lineend="round")+
  geom_curve(aes(x=43.7, y=39.1, xend=43.18, yend=38.7), curvature=-0.25, colour="Grey50",
             arrow=arrow(length=unit(0.1, "cm"), type="closed"), lineend="round")
  
agg_png("Outputs/COVIDVaxMSOAYorkshire.png", units="in", width=10, height=8.5, res=800)
plot
dev.off()

#Replicate for the over 70s only
#Split into under and over 50
vaxdata3 <- vaxdata %>% 
  mutate(age2=if_else(age %in% c("<50", "50-54", "55-59", "60-64", "65-69"), "<70", "70+")) %>%
  group_by(msoa11cd, msoa11nm, IMDrank, age2) %>% 
  summarise(vaccinated=sum(vaccinated), pop=sum(pop)) %>% 
  ungroup() %>% 
  mutate(vaxprop=vaccinated/pop)

MSOA2 <- st_read(msoa, layer="4 MSOA hex") %>% 
  left_join(vaxdata3, by="msoa11cd") %>% 
  mutate(RegionNation=case_when(
    LA.label=="Hull" ~ "Yorkshire and The Humber", 
    TRUE ~ as.character(RegionNation)))

plot2 <- ggplot()+
  geom_sf(data=MSOA2 %>% filter(age2=="70+" & RegionNation=="Yorkshire and The Humber"), 
          aes(geometry=geom, fill=vaxprop), colour=NA)+
  geom_sf(data=LAsMSOA %>% filter(RegionNation=="Yorkshire and The Humber"), 
          aes(geometry=geom), fill=NA, colour="White", size=0.2)+
  geom_sf(data=GroupsMSOA %>% filter(RegionNation=="Yorkshire and The Humber"), 
          aes(geometry=geom), fill=NA, colour="Black")+
  scale_fill_paletteer_c("pals::ocean.haline", direction=-1, 
                         name="Proportion of adults aged 70+ who have received\nat least one vaccine dose", limits=c(0.4,1),
                         labels=label_percent(accuracy=1))+
  geom_text(data=LAlabels, aes(x=x, y=y, label=label))+
  geom_text(data=Arealabels, aes(x=x, y=y, label=label), fontface="bold")+
  theme_void()+
  coord_sf(clip="off")+
  theme(plot.title=element_text(face="bold", size=rel(2)),
        text=element_text(family="Roboto"), plot.caption.position="plot",
        legend.position="top")+
  guides(fill = guide_colorbar(title.position = 'top', title.hjust = .5,
                               barwidth = unit(20, 'lines'), barheight = unit(.5, 'lines')))+
  labs(title="Vaccination rates in Yorkshire\n ",
       caption="Data from NHS England and ONS, Cartogram from House of Commons Library\nPlot by @VictimOfMaths")+
  annotate("text", x=44, y=42.2, label="97% of people aged 70+\nin Newby & Scalby\nhave been vaccinated",
           family="Roboto", colour="Grey50")+
  annotate("text", x=36, y=41.6, label="72% of people aged 70+\nin Leeds Central\nhave been vaccinated",
           family="Roboto", colour="Grey50")+
  annotate("text", x=44.6, y=39.5, label="Each hexagon represents an area\nof roughly 6,000 people",
           family="Roboto", colour="Grey50")+
  geom_curve(aes(x=42.8, y=42.2, xend=41.7, yend=42.3), curvature=-0.15, colour="Grey50",
             arrow=arrow(length=unit(0.1, "cm"), type="closed"), lineend="round")+
  geom_curve(aes(x=37.3, y=41.5, xend=38.85, yend=39.45), curvature=-0.25, colour="Grey50",
             arrow=arrow(length=unit(0.1, "cm"), type="closed"), lineend="round")+
  geom_curve(aes(x=43.7, y=39.1, xend=43.18, yend=38.7), curvature=-0.25, colour="Grey50",
             arrow=arrow(length=unit(0.1, "cm"), type="closed"), lineend="round")

agg_tiff("Outputs/COVIDVaxMSOAYorkshirev2.tiff", units="in", width=10, height=8.5, res=800)
plot2
dev.off()

#Allocate to deciles
vaxdeciles <- MSOA2 %>% 
  filter(RegionNation=="Yorkshire and The Humber" & age2=="70+") %>% 
  mutate(decile=quantcut(-IMDrank, 10, labels=FALSE)) %>% 
  group_by(decile) %>% 
  mutate(decilemean=sum(vaccinated)/sum(pop)) %>% 
  ungroup()

agg_tiff("Outputs/COVIDVaxMSOAYorkshirev3.tiff", units="in", width=8, height=6, res=800)
ggplot(vaxdeciles, aes(x=vaxprop, y=as.factor(decile), fill=vaxprop))+
  geom_density_ridges_gradient(aes(fill=stat(x)), rel_min_height=0.01, show.legend=FALSE)+
  scale_y_discrete(name="Index of Multiple Deprivation", labels=c("1 - least deprived", "2", "3", "4", "5", "6", "7", 
                                                                  "8", "9", "10 - most deprived"))+ 
  scale_x_continuous(name="Proportion of population vaccinated",
                     labels=label_percent(accuracy=1))+
  scale_fill_paletteer_c("pals::ocean.haline", direction=-1)+
  theme_classic()+
  theme(plot.title=element_text(face="bold", size=rel(1.4)),
        strip.background=element_blank(),
        strip.text=element_text(face="bold", size=rel(1)),
        text=element_text(family="Roboto"), plot.title.position="plot")+
  labs(title="Vaccination rates in Yorkshire are lower in more deprived areas",
       subtitle="Distribution of vaccination rates for adults aged 70+ in neighbourhoods in Yorkshire",
       caption="Data from NHS England | Plot by @VictimOfMaths")
dev.off()

#Calculate uptake rates by age and region
reguptake <- st_read(msoa, layer="4 MSOA hex") %>% 
  left_join(vaxdata, by="msoa11cd") %>% 
  group_by(RegionNation, age) %>% 
  summarise(vaccinated=sum(vaccinated), pop=sum(pop)) %>% 
  ungroup() %>% 
  mutate(vaxprop=vaccinated/pop) %>% 
  filter(RegionNation!="Wales")

agg_tiff("Outputs/COVIDVaxUptakexAgexReg.tiff", units="in", width=8, height=6, res=800)
ggplot(reguptake)+ 
  geom_point(aes(x=vaxprop, y=age, colour=RegionNation), alpha=0.8)+
  scale_x_continuous(name="Proportion of the population vaccinated",
                     labels=label_percent(accuracy=1))+
  scale_y_discrete(name="Age group")+
  scale_colour_paletteer_d("LaCroixColoR::paired", name="")+
  theme_classic()+
  theme(plot.title=element_text(face="bold", size=rel(1.4)),
        text=element_text(family="Roboto"))+
  labs(title="Vaccine uptake is consistently lowest in London",
       subtitle="Proportion of adults who have received at least one dose of COVID vaccine",
       caption="Data from NHS England | Plot by @VictimOfMaths")
dev.off()

agg_tiff("Outputs/COVIDVaxUptakexAgexRegYorkshire.tiff", units="in", width=8, height=6, res=800)
ggplot(reguptake)+ 
  geom_point(aes(x=vaxprop, y=age), colour="Grey70", alpha=0.8)+  
  geom_point(data=reguptake %>% filter(RegionNation=="Yorkshire and The Humber"),
             aes(x=vaxprop, y=age), colour="#FF4E86", alpha=0.8)+
  scale_x_continuous(name="Proportion of the population vaccinated",
                     labels=label_percent(accuracy=1))+
  scale_y_discrete(name="Age group")+
  scale_colour_paletteer_d("LaCroixColoR::paired", name="")+
  theme_classic()+
  theme(plot.title=element_text(face="bold", size=rel(1.4)),
        text=element_text(family="Roboto"), plot.subtitle=element_markdown())+
  labs(title="Yorkshire has done a great job vaccinating the oldest age groups,\nbut there is work to do in the younger ones",
       subtitle="Proportion of adults who have received at least one dose of COVID vaccine<br>in <span style='color:#FF4E86;'>Yorkshire</span> compared to <span style='color:Grey50;'>other English regions",
       caption="Data from NHS England | Plot by @VictimOfMaths")
dev.off()

#Calculate age-standardised vaccination rate
asvax <- vaxdata %>% 
  select(-c(vaccinated, pop)) %>% 
  spread(age, vaxprop) %>% 
  mutate(asrate=(`<50`*45000+`50-54`*7000+`55-59`*6500+`60-64`*6000+`65-69`*5500+`70-74`*5000+
           `75-79`*4000+`80+`*5000)/84000)

MSOA3 <- st_read(msoa, layer="4 MSOA hex") %>% 
  left_join(asvax, by="msoa11cd") %>% 
  mutate(RegionNation=case_when(
    LA.label=="Hull" ~ "Yorkshire and The Humber", 
    TRUE ~ as.character(RegionNation)))

plot3 <- ggplot()+
  geom_sf(data=MSOA3 %>% filter(RegionNation=="Yorkshire and The Humber"), 
          aes(geometry=geom, fill=asrate), colour=NA)+
  geom_sf(data=LAsMSOA %>% filter(RegionNation=="Yorkshire and The Humber"), 
          aes(geometry=geom), fill=NA, colour="White", size=0.2)+
  geom_sf(data=GroupsMSOA %>% filter(RegionNation=="Yorkshire and The Humber"), 
          aes(geometry=geom), fill=NA, colour="Black")+
  scale_fill_paletteer_c("pals::ocean.haline", direction=-1, 
                         name="Age standardised rates of adults receiving\nat least one vaccine dose", limits=c(0,NA),
                         labels=label_percent(accuracy=1))+
  geom_text(data=LAlabels, aes(x=x, y=y, label=label))+
  geom_text(data=Arealabels, aes(x=x, y=y, label=label), fontface="bold")+
  theme_void()+
  coord_sf(clip="off")+
  theme(plot.title=element_text(face="bold", size=rel(2)),
        text=element_text(family="Roboto"), plot.caption.position="plot",
        legend.position="top")+
  guides(fill = guide_colorbar(title.position = 'top', title.hjust = .5,
                               barwidth = unit(20, 'lines'), barheight = unit(.5, 'lines')))+
  labs(title="Vaccination rates in Yorkshire\n ",
       caption="Data from NHS England and ONS, Cartogram from House of Commons Library\nPlot by @VictimOfMaths")+
  annotate("text", x=44.8, y=39.6, label="Vaccination rates are highest\nin Winterton & Winteringham\nand Newby & Scalby",
           family="Roboto", colour="Grey50")+
  annotate("text", x=36, y=41.6, label="Vaccination rates are lowest\nin Harehills South in the\ncentre of Leeds",
           family="Roboto", colour="Grey50")+
  annotate("text", x=43.2, y=33.6, label="Each hexagon represents an area\nof roughly 6,000 people",
           family="Roboto", colour="Grey50")+
  geom_curve(aes(x=44, y=40.2, xend=41.7, yend=42.3), curvature=0.25, colour="Grey50",
             arrow=arrow(length=unit(0.1, "cm"), type="closed"), lineend="round")+
  geom_curve(aes(x=43.6, y=39, xend=42.3, yend=36.1), curvature=0.25, colour="Grey50",
             arrow=arrow(length=unit(0.1, "cm"), type="closed"), lineend="round")+
  geom_curve(aes(x=37.1, y=41.3, xend=38.85, yend=39.45), curvature=-0.25, colour="Grey50",
             arrow=arrow(length=unit(0.1, "cm"), type="closed"), lineend="round")+
  geom_curve(aes(x=42.8, y=33.9, xend=43.45, yend=34.5), curvature=-0.25, colour="Grey50",
             arrow=arrow(length=unit(0.1, "cm"), type="closed"), lineend="round")

agg_tiff("Outputs/COVIDVaxMSOAYorkshirev3.tiff", units="in", width=10, height=8.5, res=800)
plot3
dev.off()

#Actual Sheffield Map
#Download shapefile of LA boundaries
temp <- tempfile()
temp2 <- tempfile()
source <- "https://opendata.arcgis.com/datasets/826dc85fb600440889480f4d9dbb1a24_0.zip?outSR=%7B%22latestWkid%22%3A27700%2C%22wkid%22%3A27700%7D"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)

#The actual shapefile has a different name each time you download it, so need to fish the name out of the unzipped file
name <- list.files(temp2, pattern=".shp")
shapefile <- st_read(file.path(temp2, name))


map <- full_join(shapefile, MSOA %>% select(Laname, msoa11cd, IMDrank, vaxprop, pop, age2) %>% as.data.frame(), 
                 by="msoa11cd", all.y=TRUE)

map %>% 
  filter(Laname=="Sheffield" & age2=="50+") %>% 
  ggplot()+
  geom_sf(aes(geometry=geometry, fill=vaxprop), colour=NA)+
  scale_fill_paletteer_c("pals::ocean.haline", direction=-1, 
                         name="Proportion of\npopulation\nvaccinated", limits=c(0,1),
                         labels=label_percent(accuracy=1))+
  theme_void()+
  theme(plot.title=element_text(face="bold", size=rel(1.4)), text=element_text(family="Roboto"))+
  labs(title="COVID-19 vaccination rates in Sheffield",
       subtitle="Proportion of adults over 50 who have received at least one vaccine dose",
       caption="Data from NHS England and ONS, Cartogram from @carlbaker/House of Commons Library\nPlot by @VictimOfMaths")

ggplot(MSOA %>% filter(age2=="50+" & Laname=="Sheffield"))+
  geom_point(aes(x=vaxprop, y=IMDrank))
