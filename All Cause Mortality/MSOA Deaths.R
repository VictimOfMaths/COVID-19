rm(list=ls())

library(tidyverse)
library(curl)
library(readxl)
library(paletteer)
library(hrbrthemes)
library(sf)

#Read in data
temp <- tempfile()
source <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fbirthsdeathsandmarriages%2fdeaths%2fdatasets%2fdeathsinvolvingcovid19bylocalareaanddeprivation%2f1march2020to30june2020/referencetablesworkbook2.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
data <- read_excel(temp, sheet="Table 5", range="A14:U7214", col_names=FALSE)[,c(1,2,3,9,15,21)]
colnames(data) <- c("code", "ONSName", "Name", "CV19Deaths", "OtherDeaths", "AllDeaths")

#Read in MSOA populations
temp <- tempfile()
temp2 <- tempfile()
source <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fpopulationandmigration%2fpopulationestimates%2fdatasets%2fmiddlesuperoutputareamidyearpopulationestimates%2fmid2018sape21dt3a/sape21dt3amid2018msoaon2019lasyoaestimatesformatted.zip"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)
MSOApop <- read_excel(file.path(temp2,"SAPE21DT3a-mid-2018-msoa-on-2019-LA-syoa-estimates-formatted.xlsx"), 
                      sheet="Mid-2018 Persons", range=c("A6:D7545"), col_names=FALSE)[,c(1,4)]
colnames(MSOApop) <- c("code", "pop")

#Merge into data and calculate rates
data <- merge(data, MSOApop)
data$CV19rate <- data$CV19Deaths*100000/data$pop

#Download shapefile of LA boundaries
temp <- tempfile()
temp2 <- tempfile()
source <- "https://opendata.arcgis.com/datasets/826dc85fb600440889480f4d9dbb1a24_0.zip?outSR=%7B%22latestWkid%22%3A27700%2C%22wkid%22%3A27700%7D"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)

#The actual shapefile has a different name each time you download it, so need to fish the name out of the unzipped file
name <- list.files(temp2, pattern=".shp")
shapefile <- st_read(file.path(temp2, name))

names(shapefile)[names(shapefile) == "msoa11cd"] <- "code"

map <- full_join(shapefile, data, by="code", all.y=TRUE)

png("Outputs/COVIDDeathsMapBW.png", units="in", width=20, height=23.3, res=500)
ggplot(map)+
  geom_sf(aes(geometry=geometry, fill=CV19rate), colour=NA, show.legend=FALSE)+
  scale_fill_paletteer_c("oompaBase::greyscale", name="")+
  theme_ipsum_rc()+
  theme(axis.line=element_blank(), axis.ticks=element_blank(), axis.text=element_text(colour="Black"),
        axis.title=element_blank(), plot.background=element_rect(fill="black"),
        panel.background=element_rect(fill="Black"), legend.background=element_rect(fill="Black"),
        text=element_text(colour="White", size=rel(5)), legend.text=element_text(colour="White", size=rel(3)),
        panel.grid.major=element_line(colour="transparent"))+
  guides(fill=guide_colourbar(ticks=FALSE))
dev.off()

#Then crop out the little bit of whitespace around the image, import into Aerialod and make it look pretty!

#Bring in Local Authority to allow LA-specific plots
#Read in MSOA to LA lookup
temp <- tempfile()
source <- "https://opendata.arcgis.com/datasets/fe6c55f0924b4734adf1cf7104a0173e_0.csv"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
MSOAtoLAD <- read.csv(temp)[,c(8,10,11,15,17)]
colnames(MSOAtoLAD) <- c("code", "LAcode", "LAname", "Region", "Country")

#Remove duplicate rows, as original data was LSOA-level
MSOAtoLAD <- MSOAtoLAD %>% 
  distinct(.keep_all = TRUE)

map <- full_join(map, MSOAtoLAD, by="code", all.y=TRUE)

#Set up outputs for any LA by name
LA <- c("Barking and Dagenham", "Barnet", "Bexley", "Brent", "Bromley", "Camden", "City of London",
        "Croydon", "Ealing", "Enfield", "Greenwich", "Hackney", "Hammersmith and Fulham",
        "Haringey", "Harrow", "Havering", "Hillingdon", "Hounslow", "Islington", "Kensington and Chelsea",
        "Kingston upon Thames", "Lambeth", "Lewisham", "Merton", "Newham", "Redbridge", 
        "Richmond upon Thames", "Southwark", "Sutton", "Tower Hamlets", "Waltham Forest",
        "Wandsworth", "Westminster")

#png(paste0("Outputs/COVIDDeathsMapBW", LA, ".png"), units="in", width=20, height=20, res=500)
png("Outputs/COVIDDeathsMapBWLondon.png", units="in", width=20, height=20, res=500)
ggplot(subset(map, LAname %in% LA))+
  geom_sf(aes(geometry=geometry, fill=CV19Deaths), colour=NA, show.legend=FALSE)+
  scale_fill_paletteer_c("oompaBase::greyscale", name="")+
  theme_ipsum_rc()+
  theme(axis.line=element_blank(), axis.ticks=element_blank(), axis.text=element_text(colour="Black"),
        axis.title=element_blank(), plot.background=element_rect(fill="black"),
        panel.background=element_rect(fill="Black"), legend.background=element_rect(fill="Black"),
        text=element_text(colour="White", size=rel(5)), legend.text=element_text(colour="White", size=rel(3)),
        panel.grid.major=element_line(colour="transparent"))+
  guides(fill=guide_colourbar(ticks=FALSE))
dev.off()

##################
#Scottish version#
##################
#Read in data
temp <- tempfile()
source <- "https://www.nrscotland.gov.uk/files//statistics/covid19/covid-deaths-extra-tables-week-28.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
data.S <- read_excel(temp, sheet="Table S8", range="A5:F1283", col_names=FALSE)
colnames(data.S) <- c("code", "IZname", "LAname", "CV19Deaths", "pop", "CV19rate")

#Read in shapefile
temp <- tempfile()
temp2 <- tempfile()
source <- "http://sedsh127.sedsh.gov.uk/Atom_data/ScotGov/ZippedShapefiles/SG_IntermediateZoneBdry_2011.zip"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)
shapefile.S <- st_read(file.path(temp2, "SG_IntermediateZone_Bdry_2011.shp"))

names(shapefile.S)[names(shapefile.S) == "InterZone"] <- "code"

map.S <- full_join(shapefile.S, data.S, by="code", all.y=TRUE)

png("Outputs/COVIDDeathsMapScotBW.png", units="in", width=20, height=25, res=500)
ggplot(map.S)+
  geom_sf(aes(geometry=geometry, fill=CV19rate), colour=NA, show.legend=FALSE)+
  scale_fill_paletteer_c("oompaBase::greyscale", name="")+
  theme_ipsum_rc()+
  theme(axis.line=element_blank(), axis.ticks=element_blank(), axis.text=element_text(colour="Black"),
        axis.title=element_blank(), plot.background=element_rect(fill="black"),
        panel.background=element_rect(fill="Black"), legend.background=element_rect(fill="Black"),
        text=element_text(colour="White", size=rel(5)), legend.text=element_text(colour="White", size=rel(3)),
        panel.grid.major=element_line(colour="transparent"))+
  guides(fill=guide_colourbar(ticks=FALSE))
dev.off()

#Merge the UK data
temp1 <- map[,c(2,8,9,12,13,18)]
temp2 <- map.S[,c(1,2,12,13,14,15)]
map.UK <- rbind(temp1, temp2)

png("Outputs/COVIDDeathsMapUKBW.png", units="in", width=20, height=25, res=500)
ggplot(map.UK)+
  geom_sf(aes(geometry=geometry, fill=CV19rate), colour=NA, show.legend=FALSE)+
  scale_fill_paletteer_c("oompaBase::greyscale", name="")+
  theme_ipsum_rc()+
  theme(axis.line=element_blank(), axis.ticks=element_blank(), axis.text=element_text(colour="Black"),
        axis.title=element_blank(), plot.background=element_rect(fill="black"),
        panel.background=element_rect(fill="Black"), legend.background=element_rect(fill="Black"),
        text=element_text(colour="White", size=rel(5)), legend.text=element_text(colour="White", size=rel(3)),
        panel.grid.major=element_line(colour="transparent"))+
  guides(fill=guide_colourbar(ticks=FALSE))
dev.off()
