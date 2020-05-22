rm(list=ls())

library(tidyverse)
library(paletteer)
library(curl)
library(readxl)
library(sf)
library(gtools)
library(cowplot)

#Read in 2018 mid-year population estimates at LSOA level by sex and single year of age
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
data <- rbind(data_m, data_f)

#Collapse into age bands matching CFR data
data$`0-9` <- rowSums(data[,c(5:14)])
data$`10-19` <- rowSums(data[,c(15:24)])
data$`20-29` <- rowSums(data[,c(25:34)])
data$`30-39` <- rowSums(data[,c(35:44)])
data$`40-49` <- rowSums(data[,c(45:54)])
data$`50-59` <- rowSums(data[,c(55:64)])
data$`60-69` <- rowSums(data[,c(65:74)])
data$`70-79` <- rowSums(data[,c(75:84)])
data$`80+` <- rowSums(data[,c(85:95)])

data <- data[,c(1:3, 96:105)]

data_long <- gather(data, age, pop, c(5:13))

# Italian data updated 23rd April
# https://www.epicentro.iss.it/coronavirus/bollettino/Bollettino-sorveglianza-integrata-COVID-19_23-aprile-2020.pdf
# IFR from Imperial report https://www.imperial.ac.uk/media/imperial-college/medicine/sph/ide/gida-fellowships/Imperial-College-COVID19-NPI-modelling-16-03-2020.pdf

cfr <-  tibble::tribble(
  ~age, ~b, ~m, ~f, ~ifr,
  "0-9",      0.2,      0.1,      0.2,      0.002,
  "10-19",    0.000001, 0.000001, 0.000001, 0.006,
  "20-29",    0.1,      0.1,      0.000001, 0.03,
  "30-39",    0.4,      0.5,      0.3,      0.08,
  "40-49",    0.9,      1.6,      0.4,      0.15,
  "50-59",    2.6,      4.3,      1.1,      0.6,
  "60-69",   10.0,     12.6,      5.9,      2.2,
  "70-79",   24.9,     30.2,     17.2,      5.1,
  "80+",     30.8,     42.0,     22.0,      9.3
) 

#Calculate age-specific sex:population cfr ratios in Italian data
cfr$mtobratio <- cfr$m/cfr$b
cfr$ftobratio <- cfr$f/cfr$b

#Apply these to population estimates of ifr from Imperial figures
cfr$mifr <- cfr$ifr*cfr$mtobratio
cfr$fifr <- cfr$ifr*cfr$ftobratio

#Merge into population data
data_long <- merge(data_long,cfr, all.x=TRUE)

#Calculate expected deaths with 100% inflection by age group
data_long$ex_deaths <- case_when(
  data_long$sex=="Male" ~ data_long$pop*data_long$mifr/100,
  data_long$sex=="Female" ~ data_long$pop*data_long$fifr/100
)

#Summarise by LSOA
data_LSOA <- data_long %>% 
  group_by(`Area Codes`) %>% 
  summarise(name=unique(LSOA), pop=sum(pop), ex_deaths=sum(ex_deaths))

data_LSOA$mortrate <- data_LSOA$ex_deaths*100000/data_LSOA$pop

#Separate out LA-level data
data_LA <- subset(data_LSOA, is.na(name))

#Remove from LSOA-level data
data_LSOA <- subset(data_LSOA, !is.na(name))

#Bring in 2019 IMD data (England only)
temp <- tempfile()
source <- "https://opendatacommunities.org/downloads/cube-table?uri=http%3A%2F%2Fopendatacommunities.org%2Fdata%2Fsocietal-wellbeing%2Fimd2019%2Findices"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
IMD <- read.csv(temp)
IMD <- subset(IMD, (Measurement=="Decile " | Measurement=="Rank") & Indices.of.Deprivation=="e. Health Deprivation and Disability Domain")
IMD_wide <- spread(IMD, Measurement, Value)
data_LSOA <- merge(data_LSOA, IMD_wide[,c(1,5,6)], by.x="Area Codes", by.y="FeatureCode", all.x=TRUE )
colnames(data_LSOA) <- c("code", "name", "pop", "ex_deaths", "mortrate", "decile", "rank")

#Rank LSOAs within each decile
data_LSOA <- data_LSOA %>%
  group_by(decile) %>%
  mutate(decile_rank = order(order(mortrate, decreasing=FALSE)))

tiff("Outputs/COVIDMortDepGrid.tiff", units="in", width=15, height=5, res=300)
ggplot(subset(data_LSOA, !is.na(decile)), aes(y=as.factor(decile), x=decile_rank, fill=mortrate))+
  geom_tile()+
  theme_classic()+
  scale_fill_paletteer_c("viridis::magma", direction=-1,name="Potential deaths\nper 100,000")+
  scale_y_discrete(name="Health deprivation & disability", labels=c("1 - most deprived", "2", "3", "4", "5", "6", "7", 
                                               "8", "9", "10 - least deprived"))+
  scale_x_continuous(name="")+
  theme(axis.text.x=element_blank(), axis.line.x=element_blank(), axis.ticks.x=element_blank())+
  labs(title="Maximum potential exposure to COVID-19 mortality by health deprivation",
       subtitle="Calculated using LSOA-level population age/sex distribution and Case Fatality Rates from Imperial College\nadjusted using Italian sex-specific data, assuming 100% COVID-19 prevalence",
       caption="Population data from ONS, CFRs from Imperial College, sex-specific data from ISS\nPlot by @VictimOfMaths")
dev.off()

#calculate mean mortality rates by decile and overall (population weighted)
data_LSOA <- data_LSOA %>%
  group_by(decile) %>%
  mutate(decilemean = weighted.mean(mortrate, pop))

data_LSOA <- ungroup(data_LSOA)

data_LSOA <- data_LSOA %>%
  mutate(popmean = weighted.mean(mortrate, pop))

  tiff("Outputs/COVIDMortDepScatter.tiff", units="in", width=12, height=8, res=300)
  ggplot(subset(data_LSOA, !is.na(decile)), aes(x=mortrate, y=as.factor(decile), colour=mortrate))+
    geom_jitter(shape=21, alpha=0.6, show.legend=FALSE)+
    geom_segment(aes(x=popmean, xend=popmean, y=Inf, yend=-Inf), colour="Grey20")+
    geom_point(aes(x=decilemean, y=as.factor(decile)), colour="Grey20", fill="Cyan", shape=23, size=2)+
    scale_colour_paletteer_c("viridis::magma", direction=-1)+
    scale_x_continuous(name="Potential deaths per 100,000")+
    scale_y_discrete(name="Health deprivation & disability", labels=c("1 - most deprived", "2", "3", "4", "5", "6", "7", 
                                                                      "8", "9", "10 - least deprived"))+  
    theme_classic()+
    labs(title="Maximum potential exposure to COVID-19 mortality by health deprivation",
         subtitle="Calculated using LSOA-level population age/sex distribution and Case Fatality Rates from Imperial College\nadjusted using Italian sex-specific data, assuming 100% COVID-19 prevalence",
         caption="Population data from ONS, CFRs from Imperial College, sex-specific data from ISS\nPlot by @VictimOfMaths")+
  annotate("text", x=3200, y=8.51, label="Each circle = 1 LSOA", size=3)+
    annotate("text", x=1800, y=6.5, label="Population average", size=3)+
    annotate("text", x=800, y=3.5, label="Decile average", size=3)+
    geom_segment(aes(x=1170, y=6.5,  xend=1580, yend=6.5), colour="Grey20")+
    geom_segment(aes(x=800, y=3.55,  xend=1060, yend=3.91), colour="Grey20")
  dev.off()


#Download shapefile of LSOA boundaries
temp <- tempfile()
temp2 <- tempfile()
source <- "https://opendata.arcgis.com/datasets/e886f1cd40654e6b94d970ecf437b7b5_0.zip?outSR=%7B%22latestWkid%22%3A3857%2C%22wkid%22%3A102100%7D"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)

#The actual shapefile has a different name each time you download it, so need to fish the name out of the unzipped file
name <- list.files(temp2, pattern=".shp")
shapefile <- st_read(file.path(temp2, name))
names(shapefile)[names(shapefile) == "LSOA11CD"] <- "code"

#Convert to EPSG4326 projection to match lat/long and make siting annotations easier
shapefile <- st_transform(map.data, crs=4326)

map.data <- full_join(shapefile, data_LSOA, by="code")

#Map fo age-based risk
ggplot(subset(map.data, substr(name, 1,5)=="Sheff"), aes(fill=mortrate, geometry=geometry))+
  geom_sf(colour=NA)+
  theme_classic()+
  scale_fill_paletteer_c("pals::ocean.tempo", name="Potential deaths\nper 100,000")+
  theme(axis.line=element_blank(), axis.ticks=element_blank(), axis.text=element_blank(),
        axis.title=element_blank(),  plot.title=element_text(face="bold"))+
  labs(title="Maximum potential exposure to COVID-19 mortality by deprivation",
       subtitle="Calculated using LSOA-level population age/sex distribution and observed Case Fatality Rates from Italy, assuming 100% COVID-19 prevalence",
       caption="Population data from ONS, CFRs adapted from ISS & Imperial data\nPlot by @VictimOfMaths")

#Bring in Local Authorities (LADs)
temp <- tempfile()
source <- "https://opendata.arcgis.com/datasets/fe6c55f0924b4734adf1cf7104a0173e_0.csv"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
LSOAtoLAD <- read.csv(temp)[,c(4,10,11)]
colnames(LSOAtoLAD) <- c("code", "LAcode", "LAname")
  
map.data <- full_join(map.data, LSOAtoLAD, by="code")

#Bring in Regions
temp <- tempfile()
source <- "https://opendata.arcgis.com/datasets/0c3a9643cc7c4015bb80751aad1d2594_0.csv"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
LADtoRegion <- read.csv(temp)[,c(1,4)]
colnames(LADtoRegion) <- c("LAcode", "Region")

map.data <- full_join(map.data, LADtoRegion, by="LAcode")

#Remove Scottish Data Zones which have snuck into the data
map.data <- subset(map.data, substr(code, 1,1)=="E")

#Bivariate map
#tertile the IMD and mortrate variables
#generate tertiles
map.data$IMDtert <- quantcut(-map.data$rank, q=3, labels=FALSE)
map.data$morttert <- quantcut(map.data$mortrate, q=3, labels=FALSE)

#Generate key
keydata <- data.frame(IMDtert=c(1,1,1,2,2,2,3,3,3), morttert=c(1,2,3,1,2,3,1,2,3),
                      RGB=c("#e8e8e8","#ace4e4","#5ac8c8","#dfb0d6","#a5add3",
                               "#5698b9","#be64ac","#8c62aa","#3b4994"))

#Bring colours into main data for plotting
map.data <- left_join(map.data, keydata, by=c("IMDtert", "morttert"))

#strip out a few unnecessary columns for tidiness
map.data <- map.data[,-c(1,3:6)]

#Calculate mean mortrates and ranks by LA, region and nationally
map.data <- map.data %>%
  group_by(LAcode) %>%
  mutate(LAmeanrate = weighted.mean(mortrate, pop), LAmeanrank= weighted.mean(rank, pop)) %>%
  ungroup()

map.data <- map.data %>%
  group_by(Region) %>%
  mutate(regmeanrate = weighted.mean(mortrate, pop), regmeanrank= weighted.mean(rank, pop)) %>%
  ungroup()

map.data <- map.data %>%
  mutate(popmeanrate = weighted.mean(mortrate, pop), popmeanrank= weighted.mean(rank, pop)) 

#Save sf object
st_write(map.data, "Data/COVID19LSOA.shp")

##################################
#Only need to run code above once#
##################################

map.data <- st_read("Data/COVID19LSOA.shp")

#Generate key
keydata <- data.frame(IMDtert=c(1,1,1,2,2,2,3,3,3), morttert=c(1,2,3,1,2,3,1,2,3),
                      RGB=c("#e8e8e8","#ace4e4","#5ac8c8","#dfb0d6","#a5add3",
                            "#5698b9","#be64ac","#8c62aa","#3b4994"))

#Plot for Sheffield with annotations
plot <- ggplot(subset(map.data, LAname=="Sheffield"), aes(fill=RGB, geometry=geometry))+
  geom_sf(colour="White")+
  theme_classic()+
  scale_fill_identity()+
  theme(axis.line=element_blank(), axis.ticks=element_blank(), axis.text=element_blank(),
        axis.title=element_blank(),  plot.title=element_text(face="bold"))+
  labs(title="Mapping potential COVID-19 risk across Sheffield",
       subtitle="LSOA-level health deprivation and potential COVID-19 mortality risk based on age-sex structure of population",
       caption="Population data from ONS, CFRs adapted from ISS & Imperial data\nPlot by @VictimOfMaths")+
  annotate("text", x=-1.38, y=53.45, label="High deprivation,\nyoung population", size=3)+
  annotate("text", x=-1.34, y=53.38, label="High deprivation,\nold population", size=3)+
  annotate("text", x=-1.75, y=53.4, label="Low deprivation,\nold population", size=3)+
  geom_curve(aes(x=-1.38, y=53.44, xend=-1.4, yend=53.42), curvature=-0.15, 
             arrow=arrow(length=unit(0.1, "cm"), type="closed"))+
  geom_curve(aes(x=-1.345, y=53.37, xend=-1.36, yend=53.355), curvature=-0.15, 
             arrow=arrow(length=unit(0.1, "cm"), type="closed"))+
  geom_curve(aes(x=-1.725, y=53.4, xend=-1.62, yend=53.36), curvature=0.15, 
             arrow=arrow(length=unit(0.1, "cm"), type="closed"))

key <- ggplot(keydata)+
  geom_tile(aes(x=morttert, y=IMDtert, fill=RGB))+
  scale_fill_identity()+
  labs(x = expression("Greater age-based COVID-19 risk" %->%  ""),
       y = expression("Greter health deprivation" %->%  "")) +
  theme_classic() +
  # make font small enough
  theme(
    axis.title = element_text(size = 8),axis.line=element_blank(), 
    axis.ticks=element_blank(), axis.text=element_blank())+
  # quadratic tiles
  coord_fixed()

tiff("Outputs/COVIDBivariateSheff.tiff", units="in", width=12, height=8, res=300)
ggdraw()+
  draw_plot(plot, 0,0,1,1)+
  draw_plot(key, 0.03,0.03,0.3,0.3)
dev.off()

#Plot for all of greater London, with annotations
London <- ggplot(subset(map.data, Region=="London"), aes(fill=RGB, geometry=geometry))+
  geom_sf(color = NA)+
  theme_classic()+
  scale_fill_identity()+
  theme(axis.line=element_blank(), axis.ticks=element_blank(), axis.text=element_blank(),
        axis.title=element_blank(),  plot.title=element_text(face="bold", size=rel(3)),
        plot.subtitle=element_text(size=rel(2)), plot.caption=element_text(size=rel(2)))+
  labs(title="Mapping potential COVID-19 risk across London",
       subtitle="LSOA-level health deprivation and potential COVID-19 mortality risk based on age-sex structure of population",
       caption="Population data from ONS, CFRs adapted from ISS & Imperial data\nPlot by @VictimOfMaths")+
  annotate("text", x=0.29, y=51.48, label="High deprivation,\nyoung population", size=6)+
  annotate("text", x=0.24, y=51.4, label="High deprivation,\nold population", size=6)+
  annotate("text", x=-0.23, y=51.7, label="Low deprivation,\nold population", size=6)+
  geom_curve(aes(x=0.25, y=51.48, xend=0.2, yend=51.466), curvature=0.15,
             arrow=arrow(length=unit(0.1, "cm"), type="closed"))+
  geom_curve(aes(x=0.2, y=51.4, xend=0.1, yend=51.419), curvature=-0.15,
             arrow=arrow(length=unit(0.1, "cm"), type="closed"))+
  geom_curve(aes(x=-0.22, y=51.69, xend=-0.16, yend=51.67), curvature=0.15,
             arrow=arrow(length=unit(0.1, "cm"), type="closed"))

Lonkey <- ggplot(keydata)+
  geom_tile(aes(x=morttert, y=IMDtert, fill=RGB))+
  scale_fill_identity()+
  labs(x = expression("Greater age-based COVID-19 risk" %->%  ""),
       y = expression("Greter health deprivation" %->%  "")) +
  theme_classic() +
  # make font small enough
  theme(
    axis.title = element_text(size = rel(1.5)),axis.line=element_blank(), 
    axis.ticks=element_blank(), axis.text=element_blank())+
  # quadratic tiles
  coord_fixed()

tiff("Outputs/COVIDBivariateLondon.tiff", units="in", width=24, height=18, res=300)
ggdraw()+
  draw_plot(London, 0,0,1,1)+
  draw_plot(Lonkey, 0.03,0.03,0.23,0.23)
dev.off()

#Generic version - essentially a less user friendly version of https://t.co/zcHcYMFUjg?amp=1
#But with more detailed resolution on the shapefile (i.e. wigglier LSOA boundaries)

#Select LAs to map
userLA <- c("Salford", "Stockport", "Manchester", "Trafford", "Tameside", "Oldham", "Rochdale", "Wigan", "Bolton", "Bury")
#Select name for title
userLAname <- "Greater Manchester"
#Select whether to display LSOA boundaries or not (looks nicer without, but may be more useful with)
LSOABoundaries <- FALSE

userplot <- ggplot(subset(map.data, LAname %in% userLA), aes(fill=RGB, geometry=geometry))+
  geom_sf(colour=ifelse(LSOABoundaries==FALSE ,NA, "White"))+
  theme_classic()+
  scale_fill_identity()+
  theme(axis.line=element_blank(), axis.ticks=element_blank(), axis.text=element_blank(),
        axis.title=element_blank(),  plot.title=element_text(face="bold"))+
  labs(title=paste("Mapping potential COVID-19 risk across", userLAname),
       subtitle="LSOA-level health deprivation and potential COVID-19 mortality risk based on age-sex structure of population",
       caption="Population data from ONS, CFRs adapted from ISS & Imperial data\nPlot by @VictimOfMaths")

key <- ggplot(keydata)+
  geom_tile(aes(x=morttert, y=IMDtert, fill=RGB))+
  scale_fill_identity()+
  labs(x = expression("Greater age-based COVID-19 risk" %->%  ""),
       y = expression("Greater health deprivation" %->%  "")) +
  theme_classic() +
  # make font small enough
  theme(
    axis.title = element_text(size = 8),axis.line=element_blank(), 
    axis.ticks=element_blank(), axis.text=element_blank())+
  # quadratic tiles
  coord_fixed()

tiff(paste0("Outputs/COVIDBivariate",userLAname,".tiff"), units="in", width=12, height=8, res=300)
ggdraw()+
  draw_plot(userplot, 0,0,1,1)+
  #Comment out all but the desired legend position below
  #draw_plot(key, 0.03,0.03,0.3,0.3) #Bottom left
  draw_plot(key, 0.03,0.6,0.3,0.3) #Top left
  #draw_plot(key, 0.68,0.6,0.3,0.3) #Top right
  #draw_plot(key, 0.68,0.05,0.3,0.3) #Bottom right
dev.off()

#Add in stamen maps below the bivariate maps. Looks nice when it works, but sometimes it's tricky
#to align the geographies of the maps
library(ggmap)
library(osmdata)
library(ggExtra)

#get stamen map (can replace userLAname here with other names, e.g. 'Greater Manchester')
stamen.map <- get_stamenmap(getbb(userLAname), maptype="toner-lines", zoom=12)

userplot <- stamen.map %>%
  ggmap()+
  geom_sf(data=subset(map.data, LAname %in% userLA), aes(fill=RGB, geometry=geometry), 
          colour=ifelse(LSOABoundaries==FALSE ,NA, "White"), inherit.aes=FALSE, alpha=0.35)+
  theme_classic()+
  scale_fill_identity()+
  theme(axis.line=element_blank(), axis.ticks=element_blank(), axis.text=element_blank(),
        axis.title=element_blank(),  plot.title=element_text(face="bold"))+
  labs(title=paste("Mapping potential COVID-19 risk across", userLAname),
       subtitle="LSOA-level health deprivation and potential COVID-19 mortality risk based on age-sex structure of population",
       caption="Population data from ONS, CFRs adapted from ISS & Imperial data\nPlot by @VictimOfMaths")
key <- ggplot(keydata)+
  geom_tile(aes(x=morttert, y=IMDtert, fill=RGB))+
  scale_fill_identity()+
  labs(x = expression("Greater age-based COVID-19 risk" %->%  ""),
       y = expression("Greater health deprivation" %->%  "")) +
  theme_classic() +
  # make font small enough
  theme(
    axis.title = element_text(size = 8),axis.line=element_blank(), 
    axis.ticks=element_blank(), axis.text=element_blank())+
  # quadratic tiles
  coord_fixed()

tiff(paste0("Outputs/COVIDBivariate",userLAname,".tiff"), units="in", width=12, height=8, res=300)
ggdraw()+
  draw_plot(userplot, 0,0,1,1)+
  #Comment out all but the desired legend position below
  draw_plot(key, 0.03,0.03,0.3,0.3) #Bottom left
  #draw_plot(key, 0.03,0.6,0.3,0.3) #Top left
#draw_plot(key, 0.68,0.6,0.3,0.3) #Top right
#draw_plot(key, 0.68,0.05,0.3,0.3) #Bottom right
dev.off()
