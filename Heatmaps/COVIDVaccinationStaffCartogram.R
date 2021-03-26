rm(list=ls())

library(curl)
library(tidyverse)
library(readxl)
library(paletteer)
library(sf)
library(scales)
library(ragg)
library(patchwork)
library(extrafont)

#Download vaccination data for social care staff by UTLA
#https://www.england.nhs.uk/statistics/statistical-work-areas/covid-19-vaccinations/
vax <- tempfile()
url <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/03/COVID-19-weekly-announced-vaccinations-25-March-2021.xlsx"
vax <- curl_download(url=url, destfile=vax, quiet=FALSE, mode="wb")

carehome <- read_excel(vax, sheet="Older Adult Care Homes by UTLA", range="B17:L166", col_names=FALSE) %>% 
  rename(cua.name=`...1`, CHres=`...6`, CHstaff=`...11`) %>% 
  select(cua.name, CHres, CHstaff)
  
soccare <- read_excel(vax, sheet="Social Care Staff by UTLA", range="B17:N167", col_names=FALSE) %>% 
  rename(cua.name=`...1`, YAstaff=`...5`, Domstaff=`...9`, Othstaff=`...13`) %>% 
  select(cua.name, YAstaff, Domstaff, Othstaff) %>% 
  mutate(YAstaff=if_else(YAstaff=="	-", 0, as.numeric(YAstaff)),
         Othstaff=if_else(Othstaff=="	-", 0, as.numeric(Othstaff)))

data <- merge(carehome, soccare) %>% 
  mutate(cua.name=case_when(
    cua.name=="Bournemouth, Christchurch and Poole" ~ "Bournemouth, Christchurch & Poole",
    TRUE ~ cua.name))

#Download Carl Baker's lovely cartogram
utla <- tempfile()
source <- ("https://github.com/houseofcommonslibrary/uk-hex-cartograms-noncontiguous/raw/main/geopackages/LocalAuthorities-uppertier.gpkg")
utla <- curl_download(url=source, destfile=utla, quiet=FALSE, mode="wb")

BackgroundUTLA <- st_read(utla, layer="6 Background")

UTLA <- st_read(utla, layer="5 UTLA-2020") %>% 
  left_join(data, by="cua.name", all=TRUE) %>% 
  filter(!RegionNati %in% c("Wales", "Scotland", "Northern Ireland"))

GroupsUTLA <- st_read(utla, layer="2 Group")

Group_labelsUTLA <- st_read(utla, layer="1 Group labels") %>% 
  mutate(just=if_else(LabelPosit=="Left", 0, 1))

plot <- ggplot()+
  geom_sf(data=BackgroundUTLA %>% filter(id==45), aes(geometry=geom))+
  geom_sf(data=UTLA, 
          aes(geometry=geom, fill=CHres), colour=NA)+
  geom_sf(data=GroupsUTLA %>% filter(!RegionNati %in% c("Wales", "Scotland", "Northern Ireland")), 
          aes(geometry=geom), fill=NA, colour="Black")+
  geom_sf_text(data=Group_labelsUTLA %>% filter(!RegionNati %in% c("Wales", "Scotland", "Northern Ireland")), 
               aes(geometry=geom, label=Group.labe,
                   hjust=just), size=rel(2.4), colour="Black")+
  scale_fill_paletteer_c("pals::ocean.haline", direction=-1, 
                         name="Proportion of older adult care home residents\nwho have received at least one dose of COVID vaccine", 
                         limits=c(0.7,1),
                         labels=label_percent(accuracy=1))+
  theme_void()+
  theme(plot.title=element_text(face="bold", size=rel(2)),
        text=element_text(family="Roboto"), legend.position="top")+
  guides(fill = guide_colorbar(title.position = 'top', title.hjust = .5,
                               barwidth = unit(20, 'lines'), barheight = unit(.5, 'lines')))+
  labs(title="Vaccination rates in care home residents\n ",
       caption="Data from NHS England, Cartogram from @carlbaker/House of Commons Library\nPlot by @VictimOfMaths")

agg_tiff("Outputs/COVIDVaxCHResCartogram.tiff", units="in", width=9, height=10, res=500)
plot
dev.off()

plot2 <- ggplot()+
  geom_sf(data=BackgroundUTLA %>% filter(id==45), aes(geometry=geom))+
  geom_sf(data=UTLA, 
          aes(geometry=geom, fill=CHstaff), colour=NA)+
  geom_sf(data=GroupsUTLA %>% filter(!RegionNati %in% c("Wales", "Scotland", "Northern Ireland")), 
          aes(geometry=geom), fill=NA, colour="Black")+
  geom_sf_text(data=Group_labelsUTLA %>% filter(!RegionNati %in% c("Wales", "Scotland", "Northern Ireland")), 
               aes(geometry=geom, label=Group.labe,
                   hjust=just), size=rel(2.4), colour="Black")+
  scale_fill_paletteer_c("pals::ocean.haline", direction=-1, 
                         name="Proportion of older adult care home staff\nwho have received at least one dose of COVID vaccine", 
                         limits=c(0,1),
                         labels=label_percent(accuracy=1))+
  theme_void()+
  theme(plot.title=element_text(face="bold", size=rel(2)),
        text=element_text(family="Roboto"), legend.position="top")+
  guides(fill = guide_colorbar(title.position = 'top', title.hjust = .5,
                               barwidth = unit(20, 'lines'), barheight = unit(.5, 'lines')))+
  labs(title="Vaccination rates in care home staff\n ",
       caption="Data from NHS England, Cartogram from @carlbaker/House of Commons Library\nPlot by @VictimOfMaths")

agg_tiff("Outputs/COVIDVaxCHStaffCartogram.tiff", units="in", width=9, height=10, res=800)
plot2
dev.off()

plot3 <- ggplot()+
  geom_sf(data=BackgroundUTLA %>% filter(id==45), aes(geometry=geom))+
  geom_sf(data=UTLA, 
          aes(geometry=geom, fill=YAstaff), colour=NA)+
  geom_sf(data=GroupsUTLA %>% filter(!RegionNati %in% c("Wales", "Scotland", "Northern Ireland")), 
          aes(geometry=geom), fill=NA, colour="Black")+
  geom_sf_text(data=Group_labelsUTLA %>% filter(!RegionNati %in% c("Wales", "Scotland", "Northern Ireland")), 
               aes(geometry=geom, label=Group.labe,
                   hjust=just), size=rel(2.4), colour="Black")+
  scale_fill_paletteer_c("pals::ocean.haline", direction=-1, 
                         name="Proportion of younger adult care home staff\nwho have received at least one dose of COVID vaccine", 
                         limits=c(0,1),
                         labels=label_percent(accuracy=1))+
  theme_void()+
  theme(plot.title=element_text(face="bold", size=rel(2)),
        text=element_text(family="Roboto"), legend.position="top")+
  guides(fill = guide_colorbar(title.position = 'top', title.hjust = .5,
                               barwidth = unit(20, 'lines'), barheight = unit(.5, 'lines')))+
  labs(title="Vaccination rates in staff from younger adult care homes\n ",
       caption="Data from NHS England, Cartogram from @carlbaker/House of Commons Library\nPlot by @VictimOfMaths")

agg_tiff("Outputs/COVIDVaxYAStaffCartogram.tiff", units="in", width=9, height=10, res=800)
plot3
dev.off()

plot4 <- ggplot()+
  geom_sf(data=BackgroundUTLA %>% filter(id==45), aes(geometry=geom))+
  geom_sf(data=UTLA, 
          aes(geometry=geom, fill=Domstaff), colour=NA)+
  geom_sf(data=GroupsUTLA %>% filter(!RegionNati %in% c("Wales", "Scotland", "Northern Ireland")), 
          aes(geometry=geom), fill=NA, colour="Black")+
  geom_sf_text(data=Group_labelsUTLA %>% filter(!RegionNati %in% c("Wales", "Scotland", "Northern Ireland")), 
               aes(geometry=geom, label=Group.labe,
                   hjust=just), size=rel(2.4), colour="Black")+
  scale_fill_paletteer_c("pals::ocean.haline", direction=-1, 
                         name="Proportion of CQC-registered domicilliary care providers\nwho have received at least one dose of COVID vaccine", 
                         limits=c(0,1),
                         labels=label_percent(accuracy=1))+
  theme_void()+
  theme(plot.title=element_text(face="bold", size=rel(2)),
        text=element_text(family="Roboto"), legend.position="top")+
  guides(fill = guide_colorbar(title.position = 'top', title.hjust = .5,
                               barwidth = unit(20, 'lines'), barheight = unit(.5, 'lines')))+
  labs(title="Vaccination rates in domicilliary care providers\n ",
       caption="Data from NHS England, Cartogram from @carlbaker/House of Commons Library\nPlot by @VictimOfMaths")

agg_tiff("Outputs/COVIDVaxDomStaffCartogram.tiff", units="in", width=9, height=10, res=800)
plot4
dev.off()

plot5 <- ggplot()+
  geom_sf(data=BackgroundUTLA %>% filter(id==45), aes(geometry=geom))+
  geom_sf(data=UTLA, 
          aes(geometry=geom, fill=Othstaff), colour=NA)+
  geom_sf(data=GroupsUTLA %>% filter(!RegionNati %in% c("Wales", "Scotland", "Northern Ireland")), 
          aes(geometry=geom), fill=NA, colour="Black")+
  geom_sf_text(data=Group_labelsUTLA %>% filter(!RegionNati %in% c("Wales", "Scotland", "Northern Ireland")), 
               aes(geometry=geom, label=Group.labe,
                   hjust=just), size=rel(2.4), colour="Black")+
  scale_fill_paletteer_c("pals::ocean.haline", direction=-1, 
                         name="Proportion of other social care staff\nwho have received at least one dose of COVID vaccine", 
                         limits=c(0,1),
                         labels=label_percent(accuracy=1))+
  theme_void()+
  theme(plot.title=element_text(face="bold", size=rel(2)),
        text=element_text(family="Roboto"), legend.position="top")+
  guides(fill = guide_colorbar(title.position = 'top', title.hjust = .5,
                               barwidth = unit(20, 'lines'), barheight = unit(.5, 'lines')))+
  labs(title="Vaccination rates in other social care staff\n ",
       caption="Data from NHS England, Cartogram from @carlbaker/House of Commons Library\nPlot by @VictimOfMaths")

agg_tiff("Outputs/COVIDVaxOthStaffCartogram.tiff", units="in", width=9, height=10, res=500)
plot5
dev.off()

