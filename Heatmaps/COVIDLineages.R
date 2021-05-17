rm(list=ls())

library(tidyverse)
library(curl)
library(paletteer)
library(extrafont)
library(geofacet)
library(scales)

url <- "https://covid-surveillance-data.cog.sanger.ac.uk/download/lineages_by_ltla_and_week.tsv"

temp <- tempfile()
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

rawdata <- read_tsv(temp)

#Read in LTLA to region lookup

temp <- tempfile()
source <- "https://opendata.arcgis.com/datasets/0c3a9643cc7c4015bb80751aad1d2594_0.csv"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
LADtoRegion <- read.csv(temp)[,c(1,4)]
colnames(LADtoRegion) <- c("LTLA", "Region")

data <- merge(rawdata, LADtoRegion,all.x=TRUE) %>% 
  mutate(Region=case_when(
    LTLA %in% c("E07000246", "E06000058", "E06000059") ~ "South West",
    LTLA %in% c("E07000245", "E07000244") ~ "East of England",
    TRUE ~ Region),
    WeekEndDate=as.Date(WeekEndDate),
    strain=case_when(
      Lineage=="B.1.617.2" ~ "'Indian' variant",
      Lineage=="B.1.1.7" ~ "'Kent' variant",
      TRUE ~ "Other variants")) %>% 
  group_by(WeekEndDate, strain, Region) %>% 
  summarise(Count=sum(Count)) %>% 
  ungroup() %>% 
  group_by(WeekEndDate, Region) %>% 
  mutate(Total=sum(Count)) %>% 
  ungroup() %>% 
  mutate(prop=Count/Total)

#Compare regions
mygrid <- data.frame(name=c("North East", "North West", "Yorkshire and The Humber",
                            "West Midlands", "East Midlands", "East of England",
                            "South West", "London", "South East"),
                     row=c(1,2,2,3,3,3,4,4,4), col=c(2,1,2,1,2,3,1,2,3),
                     code=c(1:9))

agg_tiff("Outputs/COVIDGenomesCountxReg.tiff", units="in", width=10, height=8, res=800)
ggplot(data, aes(x=WeekEndDate, y=Count, fill=strain))+
  geom_col(position="stack")+
  scale_x_date(name="")+
  scale_y_continuous(name="Genomes sequenced")+
  scale_fill_paletteer_d("rcartocolor::Vivid", name="Lineage")+
  facet_geo(~Region, grid=mygrid)+
  theme_classic()+
  theme(strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)),
        text=element_text(family="Lato"), plot.title=element_text(face="bold", size=rel(1.4)))+
  labs(title="In absolute terms, the number of cases of the 'Indian' variant is small",
       subtitle="Number of total COVID-19 genomes sequenced by the Wellcome Sanger Institute with identified as B.1.617.2 ('Indian'),\nB.1.1.7 ('Kent') or other lineage.",
       caption="Data from Wellcome Sanger Institute | Plot by @VictimOfMaths")
dev.off()

agg_tiff("Outputs/COVIDGenomesStackedxReg.tiff", units="in", width=10, height=8, res=800)
ggplot(data, aes(x=WeekEndDate, y=prop, fill=strain))+
  geom_col(position="stack")+
  scale_x_date(name="")+
  scale_y_continuous(name="Genomes sequenced", labels=label_percent(accuracy=1))+
  scale_fill_paletteer_d("rcartocolor::Vivid", name="Lineage")+
  facet_geo(~Region, grid=mygrid)+
  theme_classic()+
  theme(strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)),
        text=element_text(family="Lato"), plot.title=element_text(face="bold", size=rel(1.4)))+
  labs(title="The 'Indian' variant looks to be becoming dominant in many parts of England",
       subtitle="Proportion of total COVID-19 genomes sequenced by the Wellcome Sanger Institute with identified as B.1.617.2 ('Indian'),\nB.1.1.7 ('Kent') or other lineage.",
       caption="Data from Wellcome Sanger Institute | Plot by @VictimOfMaths")
dev.off()

