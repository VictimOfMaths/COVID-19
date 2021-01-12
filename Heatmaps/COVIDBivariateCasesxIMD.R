rm(list=ls())

library(tidyverse)
library(curl)
library(sf)
library(paletteer)
library(ukcovid19)
library(readxl)
library(cowplot)
library(lubridate)

########
#London#
########

#Call MSOA level case data from coronavirus.data.gov.uk
temp <- tempfile()
source <- ("https://api.coronavirus.data.gov.uk/v2/data?areaType=msoa&metric=newCasesBySpecimenDateRollingRate&format=csv")
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

casedata <- read_csv(temp) 

#Download MSOA shapefile
temp <- tempfile()
temp2 <- tempfile()
source <- "https://opendata.arcgis.com/datasets/826dc85fb600440889480f4d9dbb1a24_0.zip?outSR=%7B%22latestWkid%22%3A27700%2C%22wkid%22%3A27700%7D"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)

#The actual shapefile has a different name each time you download it, so need to fish the name out of the unzipped file
name <- list.files(temp2, pattern=".shp")
shapefile <- st_read(file.path(temp2, name))

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
  summarise(IMDrank=weighted.mean(IMDrank, pop)) %>% 
  ungroup() %>% 
  #Then merge into COVID case data
  merge(casedata %>% filter(date==max(date)), by.x="MSOA11CD", by.y="areaCode", all=TRUE) %>% 
  rename(msoa11cd=MSOA11CD)

#Join with map
mapdata <- full_join(shapefile, IMD_MSOA)

#Map of Deprivation
mapdata %>% 
  filter(regionName=="London") %>% 
  ggplot()+
  geom_sf(aes(fill=max(IMDrank)-IMDrank, geometry=geometry), colour=NA)+
  scale_fill_paletteer_c("pals::ocean.matter", name="Deprivation\n(darker = more deprived)", 
                         limits=c(0,NA), direction=-1)+
  theme_void()

#Map of Deprivation
mapdata %>% 
  filter(regionName=="London") %>% 
  ggplot()+
  geom_sf(aes(fill=newCasesBySpecimenDateRollingRate, geometry=geometry), colour=NA)+
  scale_fill_paletteer_c("pals::ocean.deep", name="New cases\nper 100,000", 
                         limits=c(0,NA), direction=-1)+
  theme_void()

#Scatter plot
tiff("Outputs/COVIDLondonCasesScatter.tiff", units="in", width=8, height=6, res=300)
mapdata %>% 
  filter(regionName=="London") %>% 
  ggplot(aes(x=newCasesBySpecimenDateRollingRate, y=max(IMDrank)-IMDrank))+
  geom_point(colour="#c51b8a")+
  geom_smooth(method="lm", formula=y~x)+
  scale_x_continuous(name="New COVID-19 cases per 100,000 in the past week", limits=c(0,NA))+
  scale_y_continuous(name="Deprivation (higher = more deprived)")+
  theme_classic()+
  theme(plot.title=element_text(face="bold", size=rel(1.2)))+
  labs(title="There are more COVID-19 cases in deprived parts of London",
       subtitle="Rolling 7-day rate of new cases plotted against the Index of Multiple Deprivation for MSOAs in London",
       caption="Data from PHE, ONS & MHCLG | Plot by @VictimOfMaths")
dev.off()

#Bivariate map
BVmapdata <- mapdata %>% 
  filter(regionName=="London") %>% 
  mutate(IMDtert=quantcut(-IMDrank, q=4, labels=FALSE),
         casetert=quantcut(newCasesBySpecimenDateRollingRate, q=4, labels=FALSE))

#Generate key
keydata <- data.frame(IMDtert=c(1,1,1,1,2,2,2,2,3,3,3,3,4,4,4,4), 
                      casetert=c(1,2,3,4,1,2,3,4,1,2,3,4,1,2,3,4),
                      RGB=c("#e8e8e8","#b9dddd","#89d3d3","#5ac8c8",
                            "#dabcd4","#acb2ca","#7ea8c1","#509eb7",
                            "#cc90c0","#9f86b7","#727dae","#4573a5",
                            "#be64ac","#925ba4","#67529c","#3b4994"))

#Bring colours into main data for plotting
BVmapdata <- left_join(BVmapdata, keydata, by=c("IMDtert", "casetert"))

#Bivariate map
BVmap <- BVmapdata %>% 
  filter(regionName=="London") %>% 
  ggplot()+
  geom_sf(aes(fill=RGB, geometry=geometry), colour=NA)+
  scale_fill_identity()+
  theme_void()+
  theme(plot.title=element_text(face="bold", size=rel(2)))+
  labs(title="In London, more deprived areas have more COVID-19 cases",
       subtitle="Rolling 7-day average rates of new cases compared to Index of Multiple Deprivation ranks for Middle Super Output Areas (MSOAs)",
       cation="Data from PHE, ONS & MHCLG | Plot by @VictimOfMaths")+
  annotate("text", x=545000, y=199000, label="High deprivation,\nhigh cases", size=4)+
  annotate("text", x=515000, y=198000, label="High deprivation,\nlow cases", size=4)+
  annotate("text", x=561000, y=180000, label="Low deprivation,\nhigh cases", size=4)+
  annotate("text", x=507000, y=167000, label="Low deprivation,\nlow cases", size=4)+
  geom_curve(aes(x=541500, y=198800, xend=537000, yend=199000), curvature=-0.15,
            arrow=arrow(length=unit(0.1, "cm"), type="closed"))+
  geom_curve(aes(x=559000, y=181300, xend=556000, yend=185000), curvature=0.15,
             arrow=arrow(length=unit(0.1, "cm"), type="closed"))+  
  geom_curve(aes(x=518000, y=197000, xend=534000, yend=187600), curvature=-0.15,
             arrow=arrow(length=unit(0.1, "cm"), type="closed"))+
  geom_curve(aes(x=510000, y=166500, xend=516500, yend=169000), curvature=0.15,
             arrow=arrow(length=unit(0.1, "cm"), type="closed"))

#Bivariate key
key <- ggplot(keydata)+
  geom_tile(aes(x=casetert, y=IMDtert, fill=RGB))+
  scale_fill_identity()+
  labs(x = expression("More COVID-19 cases" %->%  ""),
       y = expression("Greater deprivation" %->%  "")) +
  theme_classic() +
  # make font small enough
  theme(
    axis.title = element_text(size = 9),axis.line=element_blank(), 
    axis.ticks=element_blank(), axis.text=element_blank())+
  # quadratic tiles
  coord_fixed()

#Final plot
tiff("Outputs/COVIDLondonCasesxIMD.tiff", units="in", width=12, height=8, res=300)
ggdraw()+
  draw_plot(BVmap, 0,0,1,1)+
  draw_plot(key, 0.68,0.05,0.3,0.3) 
dev.off()

#########
#Glasgow#
#########

#Call Intermediate Zone level case data from https://www.opendata.nhs.scot/dataset/covid-19-in-scotland
temp <- tempfile()
source <- ("https://www.opendata.nhs.scot/dataset/b318bddf-a4dc-4262-971f-0ba329e09b87/resource/8906de12-f413-4b3f-95a0-11ed15e61773/download/trend_iz_20210111.csv")
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

casedata.s <- read_csv(temp, col_types="cccccicicc") %>% 
  mutate(date=ymd(Date),
         caserate=Positive7Day*100000/Population) %>% 
  select(date, IntZone, IntZoneName, CA, CAName, caserate) %>% 
  #keep only the latest day's data
  filter(date==max(date)) %>% 
  mutate(region=if_else(CAName %in% c()))

#Bring in SIMD data (at datazone level)
temp <- tempfile()
source <- ("https://www.gov.scot/binaries/content/documents/govscot/publications/statistics/2020/01/scottish-index-of-multiple-deprivation-2020-ranks-and-domain-ranks/documents/scottish-index-of-multiple-deprivation-2020-ranks-and-domain-ranks/scottish-index-of-multiple-deprivation-2020-ranks-and-domain-ranks/govscot%3Adocument/SIMD%2B2020v2%2B-%2Branks.xlsx?forceDownload=true")
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

SIMD <- read_excel(temp, sheet=2, range="A1:F6977")

#Bring in DZ to IZ lookup
temp <- tempfile()
source <- ("http://statistics.gov.scot/downloads/file?id=2a2be2f0-bf5f-4e53-9726-7ef16fa893b7%2FDatazone2011lookup.csv")
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

DZIZlookup <- read.csv(temp) %>% 
  select(DZ2011_Code, IZ2011_Code) %>% 
  rename(Data_Zone=DZ2011_Code, IntZone=IZ2011_Code)

#Merge into SIMD data
SIMD_IZ <- SIMD %>% 
  merge(DZIZlookup) %>% 
  group_by(IntZone) %>% 
  summarise(SIMDrank=weighted.mean(SIMD2020v2_Rank, Total_population)) %>% 
  ungroup() %>% 
  #Merge into case data
  merge(casedata.s)

#Get IZ boundaries
temp <- tempfile()
temp2 <- tempfile()
source <- "http://sedsh127.sedsh.gov.uk/Atom_data/ScotGov/ZippedShapefiles/SG_IntermediateZoneBdry_2011.zip"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)

shapefile.s <- st_read(file.path(temp2, "SG_IntermediateZone_Bdry_2011.shp"))

mapdata.s <- full_join(shapefile.s, SIMD_IZ, by=c("InterZone"="IntZone")) 

