rm(list=ls())

library(tidyverse)
library(curl)
library(readxl)
library(sf)
library(paletteer)
library(hrbrthemes)

#Read in data
temp <- tempfile()
source <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fbirthsdeathsandmarriages%2fdeaths%2fdatasets%2fdeathsinvolvingcovid19bylocalareaanddeprivation%2f1march2020to31may2020/referencetablesworkbook1.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
data <- read_excel(temp, sheet="Table 5", range="A14:R7214", col_names=FALSE)[,c(1,2,3,8,13,18)]
colnames(data) <- c("code", "ONSName", "Name", "CV19Deaths", "OtherDeaths", "AllDeaths")

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

#Then crop out the little bit of whitespace around the impact, import into Aerialod and make it look pretty!
