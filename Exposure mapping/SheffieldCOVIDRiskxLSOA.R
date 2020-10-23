rm(list=ls())

library(tidyverse)
library(curl)
library(readxl)
library(sf)
library(paletteer)

#Download population age/sex structures at LSOA level from ONS 2018 mid-year estimates
temp <- tempfile()
temp2 <- tempfile()
source <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fpopulationandmigration%2fpopulationestimates%2fdatasets%2flowersuperoutputareamidyearpopulationestimates%2fmid2018sape21dt1a/sape21dt1amid2018on2019lalsoasyoaestimatesformatted.zip"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)
data_m <- read_excel(file.path(temp2, "SAPE21DT1a-mid-2018-on-2019-LA-lsoa-syoa-estimates-formatted.xlsx"), 
                     sheet="Mid-2018 Males", range="A5:CQ35097", col_names=TRUE)
data_f <- read_excel(file.path(temp2, "SAPE21DT1a-mid-2018-on-2019-LA-lsoa-syoa-estimates-formatted.xlsx"), 
                     sheet="Mid-2018 Females", range="A5:CQ35097", col_names=TRUE)

#Merge sex-specific data
data_m$sex <- "Male"
data_f$sex <- "Female"
data <- rbind(data_m, data_f)%>% 
  filter(!is.na(LSOA)) %>% 
  gather(age, pop, c(6:ncol(.)-1)) %>% 
  select(c(1,5:7)) %>% 
  mutate(ageband=case_when(
    age<5 ~ "0-4", age<10 ~ "5-9", age<15 ~ "10-14", age<20 ~ "15-19", age<25 ~ "20-24",
    age<30 ~ "25-29", age<35 ~ "30-34", age<40 ~ "35-39", age<45 ~ "40-44",
    age<50 ~ "45-49", age<55 ~ "50-54", age<60 ~ "55-59", age<65 ~ "60-64",
    age<70 ~ "65-69", age<75 ~ "70-74", age<80 ~ "75-79", TRUE ~ "80+"
  ))

#Set IFRs based on O'Driscoll et al. synthesis of data across 45 countries
#https://www.medrxiv.org/content/10.1101/2020.08.24.20180851v1.full.pdf
#Date from Table S4
IFR <- data.frame(ageband=rep(c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39",
                            "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74",
                            "75-79", "80+"), times=2), 
                  sex=rep(c("Male", "Female"), each=17),
                  IFR=c(0.002, 0.000, 0.001, 0.002, 0.005, 0.012, 0.024, 0.040, 0.077, 0.118, 0.223, 
                        0.344, 0.473, 0.868, 1.445, 2.973, 8.619, 0.002, 0.001, 0.000, 0.001, 0.003, 
                        0.007, 0.010, 0.018, 0.028, 0.053, 0.084, 0.138, 0.246, 0.417, 0.707, 1.580,
                        5.928))

#Bring in IFRs and calculate expected mortality with 100% infection rates by LSOA
data <- merge(data, IFR, by=c("sex", "ageband"))

ex_data <- data %>% 
  mutate(ex_deaths=pop*IFR/100) %>% 
  group_by(`Area Codes`) %>% 
  summarise(ex_deaths=sum(ex_deaths), pop=sum(pop)) %>% 
  ungroup() %>% 
  mutate(ex_mortrate=ex_deaths*100000/pop)

#Bring in LA names to allow filtering
temp <- tempfile()
source <- "http://geoportal1-ons.opendata.arcgis.com/datasets/fe6c55f0924b4734adf1cf7104a0173e_0.csv"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
LSOAtoLAD <- read.csv(temp) %>% 
  select(LSOA11CD, LAD17CD, LAD17NM) %>% 
  unique()

ex_data <- merge(ex_data, LSOAtoLAD, by.x="Area Codes", by.y="LSOA11CD", all.x=TRUE) 

#Save Sheffield LSOAs
ex_data %>% 
  filter(LAD17NM=="Sheffield") %>% 
  select(`Area Codes`, ex_mortrate) %>% 
  write.csv("Data/SheffCOVIDAgeRiskByLSOA.csv")

#Download shapefile of LSOA boundaries
temp <- tempfile()
temp2 <- tempfile()
source <- "https://data.cambridgeshireinsight.org.uk/sites/default/files/Lower_Layer_Super_Output_Areas_December_2011_Generalised_Clipped__Boundaries_in_England_and_Wales.zip"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)

#The actual shapefile has a different name each time you download it, so need to fish the name out of the unzipped file
name <- list.files(temp2, pattern=".shp")
shapefile <- st_read(file.path(temp2, name))
names(shapefile)[names(shapefile) == "lsoa11cd"] <- "Area Codes"

map.data <- full_join(shapefile, ex_data, by="Area Codes")

png("Outputs/SheffieldCOVIDRiskLSOA.png", units="in", width=10, height=7, res=500)
map.data %>% 
  filter(LAD17NM=="Sheffield") %>% 
  ggplot()+
  geom_sf(aes(geometry=geometry, fill=ex_mortrate), colour=NA)+
  scale_fill_paletteer_c("pals::ocean.amp", name="Expected max. deaths\nper 100,000")+
  theme_classic()+
  theme(axis.line=element_blank(), axis.ticks=element_blank(), axis.text=element_blank(),
        axis.title=element_blank(),)+
  labs(title="Age-based COVID-19 risk",
       subtitle="Estimated mortality across Sheffield assuming 100% infection rates",
       caption="IFR data taken from O'Driscoll et al. | Population data from ONS 2018 mid-year estimates\nPlot by @VictimOfMaths")
dev.off()
