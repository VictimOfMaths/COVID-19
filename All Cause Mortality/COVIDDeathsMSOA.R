rm(list=ls())

library(tidyverse)
library(curl)
library(paletteer)
library(scales)
library(gtools)
library(ggtext)

#Download MSOA-level COVID deaths
temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/datasets/deathsduetocovid19bylocalareaanddeprivation/december2020/covidlocalareadeprivationupdate.xlsx"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

deaths <- read_excel(temp, sheet="Table 5", range="A13:AM7214") %>% 
  select(-c(4,16,28))

colnames(deaths) <- c("MSOA11CD", "msoanm", "Name", "All.Mar", "All.Apr", "All.May",
                      "All.Jun", "All.Jul", "All.Aug", "All.Sep", "All.Oct", "All.Nov",
                      "All.Dec", "All.Total", "COVID.Mar", "COVID.Apr", "COVID.May",
                      "COVID.Jun", "COVID.Jul", "COVID.Aug", "COVID.Sep", "COVID.Oct",
                      "COVID.Nov", "COVID.Dec", "COVID.Total", "Other.Mar", "Other.Apr",
                      "Other.May", "Other.Jun", "Other.Jul", "Other.Aug", "Other.Sep",
                      "Other.Oct", "Other.Nov", "Other.Dec", "Other.Total")

deaths <- pivot_longer(deaths, c(4:36), names_to=c("cause", "month"), names_sep="\\.", 
                       values_to=c("deaths"))

deaths.total <- deaths %>% 
  filter(month=="Total") %>% 
  spread(cause, deaths) %>% 
  mutate(covprop=COVID/All)

#Bring in deprivation estimates
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

#Bring in population data
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
  ungroup() %>% 
  #Then merge into COVID case data
  merge(deaths.total, by="MSOA11CD", all=TRUE) %>% 
  rename(msoa11cd=MSOA11CD)

#Download Carl Baker's lovely map
temp <- tempfile()
source <- ("https://github.com/houseofcommonslibrary/uk-hex-cartograms-noncontiguous/raw/main/geopackages/MSOA.gpkg")
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

Background <- st_read(temp, layer="5 Background")

MSOA <- st_read(temp, layer="4 MSOA hex") %>% 
  left_join(IMD_MSOA, by="msoa11cd")

Groups <- st_read(temp, layer="2 Groups")

Group_labels <- st_read(temp, layer="1 Group labels") %>% 
  mutate(just=if_else(LabelPosit=="Left", 0, 1))

LAs <- st_read(temp, layer="3 Local authority outlines (2019)")

tiff("Outputs/COVIDDeathsMSOA.tiff", units="in", width=10, height=8, res=800)
ggplot()+
  geom_sf(data=Background, aes(geometry=geom))+
  geom_sf(data=MSOA, aes(geometry=geom, fill=covprop), colour=NA)+
  geom_sf(data=LAs, aes(geometry=geom), fill=NA, colour="White")+
  geom_sf(data=Groups, aes(geometry=geom), fill=NA, colour="Black")+
  geom_sf_text(data=Group_labels, aes(geometry=geom, label=Group.labe,
                                      hjust=just), size=rel(2.4), colour="Black")+
  scale_fill_paletteer_c("pals::ocean.haline", direction=-1, 
                         name="Proportion of deaths\nfrom COVID-19", 
                         labels=label_percent(accuracy=1))+
  theme_void()+
  theme(plot.title=element_text(face="bold", size=rel(1.2)))+
  labs(title="COVID-19 is responsible for a greater proportion of deaths in urban areas",
       subtitle="Proportion of all deaths in Mar-Dec 2020 which are due to COVID in Middle Super Output Areas",
       caption="Data from ONS, Cartogram from @carlbaker/House of Commons Library\nPlot by @VictimOfMaths")
dev.off()

tiff("Outputs/COVIDDeathsMSOACumul.tiff", units="in", width=10, height=8, res=800)
ggplot()+
  geom_sf(data=Background, aes(geometry=geom))+
  geom_sf(data=MSOA, aes(geometry=geom, fill=COVID*100000/pop), colour=NA)+
  geom_sf(data=LAs, aes(geometry=geom), fill=NA, colour="White")+
  geom_sf(data=Groups, aes(geometry=geom), fill=NA, colour="Black")+
  geom_sf_text(data=Group_labels, aes(geometry=geom, label=Group.labe,
                                      hjust=just), size=rel(2.4), colour="Black")+
  scale_fill_paletteer_c("viridis::inferno", direction=-1,
                         name="Total COVID deaths\nper 100,000")+
  theme_void()+
  theme(plot.title=element_text(face="bold", size=rel(1.2)))+
  labs(title="COVID-19 death rates are highest in the North of England",
       subtitle="Total COVID-19 deaths between March and December 2020 in English Middle Super Output Areas",
       caption="Data from ONS, Cartogram from @carlbaker/House of Commons Library\nPlot by @VictimOfMaths")
dev.off()

#Bivariate cartogram
#Bivariate map
BVmapdata <- MSOA %>% 
  mutate(IMDtert=quantcut(-IMDrank, q=4, labels=FALSE),
         covtert=quantcut(covprop, q=4, labels=FALSE))

#Generate key
keydata <- data.frame(IMDtert=c(1,1,1,1,2,2,2,2,3,3,3,3,4,4,4,4), 
                      covtert=c(1,2,3,4,1,2,3,4,1,2,3,4,1,2,3,4),
                      RGB=c("#e8e8e8","#b9dddd","#89d3d3","#5ac8c8",
                            "#dabcd4","#acb2ca","#7ea8c1","#509eb7",
                            "#cc90c0","#9f86b7","#727dae","#4573a5",
                            "#be64ac","#925ba4","#67529c","#3b4994"))

#Bring colours into main data for plotting
BVmapdata <- left_join(BVmapdata, keydata, by=c("IMDtert", "covtert")) %>% 
  mutate(RGB=if_else(substr(cuacode,1,1)=="W", "White", as.character(RGB)))

#Bivariate map
BVmap <- BVmapdata %>% 
  ggplot()+
  geom_sf(data=Background, aes(geometry=geom))+
  geom_sf(data=BVmapdata, aes(geometry=geom, fill=RGB), colour=NA)+
  geom_sf(data=LAs, aes(geometry=geom), fill=NA, colour="White", size=0.5)+
  geom_sf(data=Groups, aes(geometry=geom), fill=NA, colour="Black")+
  geom_sf_text(data=Group_labels, aes(geometry=geom, label=Group.labe,
                                      hjust=just), size=rel(2.4), colour="Black")+
  scale_fill_identity()+
  theme_void()+
  theme(plot.title=element_text(face="bold", size=rel(1.6)))+
  labs(title="COVID-19 deaths are highest in deprived urban areas",
       subtitle="Proportion of total deaths in Mar-Dec 2020 attributable to COVID compared to Index of Multiple Deprivation ranks\nfor Middle Super Output Areas (MSOAs) in England",
       caption="Data from PHE, ONS & MHCLG | Cartogram by @carlbaker | Plot by @VictimOfMaths")

#Bivariate key
key <- ggplot(keydata)+
  geom_tile(aes(x=covtert, y=IMDtert, fill=RGB))+
  scale_fill_identity()+
  labs(x = expression("Greater % of deaths from COVID" %->%  ""),
       y = expression("Greater deprivation" %->%  "")) +
  theme_classic() +
  # make font small enough
  theme(
    axis.title = element_text(size = 9),axis.line=element_blank(), 
    axis.ticks=element_blank(), axis.text=element_blank())+
  # quadratic tiles
  coord_fixed()

#Final plot
tiff("Outputs/COVIDDeathPropMSOA.tiff", units="in", width=8, height=10, res=500)
ggdraw()+
  draw_plot(BVmap, 0,0,1,1)+
  draw_plot(key, 0.68,0.66,0.28,0.28) 
dev.off()

#Scatterplot
tiff("Outputs/COVIDDeathPropMSOAScatter.tiff", units="in", width=8, height=6, res=500)
ggplot(IMD_MSOA, aes(x=covprop, y=-IMDrank, colour=COVID))+
  geom_point()+
  geom_smooth(method="lm", colour="Darkred")+
  scale_x_continuous(name="Proportion of all deaths which were from COVID",
                     labels=label_percent(accuracy=1))+
  scale_y_continuous(name="Index of Multiple Deprivation",
                     breaks=c(-max(IMD_MSOA$IMDrank, na.rm=TRUE), 
                              -min(IMD_MSOA$IMDrank, na.rm=TRUE)),
                     labels=c("Least Deprived", "Most Deprived"))+
  scale_colour_paletteer_c("pals::ocean.haline", name="Total COVID deaths",
                           direction=-1)+
  theme_classic()+
  theme(plot.title=element_text(face="bold", size=rel(1.2)))+
  labs(title="On average more deprived areas have seen more COVID-19 deaths",
       subtitle="Proportion of all deaths in March-December 2020 which were due to COVID-19 by deprivation\nin English Middle Super Output Areas",
       caption="Data from PHE, ONS & MHCLG | Plot by @VictimOfMaths")
dev.off()

#########################################################################
#Look at wave 1 vs. wave 2 deaths
waves <- deaths %>% 
  filter(month!="Total") %>% 
  mutate(wave=if_else(month %in% c("Mar", "Apr", "May", "Jun", "Jul"), "Wave 1", "Wave 2")) %>% 
  group_by(MSOA11CD, cause, wave) %>% 
  summarise(deaths=sum(deaths)) %>% 
  spread(wave, deaths) %>% 
  ungroup() %>% 
  mutate(ratio=`Wave 1`/(`Wave 1`+ `Wave 2`)) %>% 
  rename(msoa11cd=MSOA11CD)

#Merge into map
temp <- tempfile()
source <- ("https://github.com/houseofcommonslibrary/uk-hex-cartograms-noncontiguous/raw/main/geopackages/MSOA.gpkg")
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

Background <- st_read(temp, layer="5 Background")

MSOA <- st_read(temp, layer="4 MSOA hex") %>% 
  left_join(waves, by="msoa11cd")

Groups <- st_read(temp, layer="2 Groups")

Group_labels <- st_read(temp, layer="1 Group labels") %>% 
  mutate(just=if_else(LabelPosit=="Left", 0, 1))

LAs <- st_read(temp, layer="3 Local authority outlines (2019)")

tiff("Outputs/COVIDDeathsMSOAWaves.tiff", units="in", width=10, height=8, res=800)
ggplot()+
  geom_sf(data=Background, aes(geometry=geom), fill="Grey60")+
  geom_sf(data=MSOA, aes(geometry=geom, fill=ratio), colour=NA)+
  geom_sf(data=LAs, aes(geometry=geom), fill=NA, colour="Grey40", size=0.1)+
  geom_sf(data=Groups, aes(geometry=geom), fill=NA, colour="Black")+
  geom_sf_text(data=Group_labels, aes(geometry=geom, label=Group.labe,
                                      hjust=just), size=rel(2.4), colour="Black")+
  scale_fill_paletteer_c("pals::ocean.balance",
                         name="", breaks=c(0, 0.5, 1),
                         labels=c("More deaths\nin Aug-Dec", "Equal deaths", "More deaths\nin Mar-Jul"))+
  theme_void()+
  theme(plot.title=element_text(face="bold", size=rel(1.2)),
        plot.subtitle=element_markdown())+
  labs(title="There seems to be little evidence for 'herd immunity' for mortality even at small geographies",
       subtitle="Proportion of total COVID-19 deaths which occurred in <span style='color:#982629;'>March-July</span> compared to <span style='color:#1d4e9f;'>August-December</span>.<br>Data at Middle Super Output Area level",
       caption="Data from ONS, Cartogram from @carlbaker/House of Commons Library\nPlot by @VictimOfMaths")

dev.off()

