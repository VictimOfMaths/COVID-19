rm(list=ls())

library(tidyverse)
library(curl)
library(RcppRoll)
library(lubridate)
library(sf)
library(extrafont)
library(ragg)
library(cowplot)
library(scales)

theme_custom <- function() {
  theme_classic() %+replace%
    theme(plot.title.position="plot", plot.caption.position="plot",
          strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)),
          plot.title=element_text(face="bold", size=rel(1.5), hjust=0,
                                  margin=margin(0,0,5.5,0)),
          text=element_text(family="Lato"))
}

#Download LA-level vaccine coverage from the dashboard
temp <- tempfile()
source <- "https://api.coronavirus.data.gov.uk/v2/data?areaType=ltla&metric=newPeopleVaccinatedFirstDoseByVaccinationDate&metric=newPeopleVaccinatedSecondDoseByVaccinationDate&metric=VaccineRegisterPopulationByVaccinationDate&format=csv"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

vaxdata <- read.csv(temp) %>% 
  mutate(date=as.Date(date),
         vaxprop1st=newPeopleVaccinatedFirstDoseByVaccinationDate/VaccineRegisterPopulationByVaccinationDate,
         vaxprop2nd=newPeopleVaccinatedSecondDoseByVaccinationDate/VaccineRegisterPopulationByVaccinationDate,
         totalvaxed=newPeopleVaccinatedFirstDoseByVaccinationDate+newPeopleVaccinatedSecondDoseByVaccinationDate,
         vaxpropall=totalvaxed/VaccineRegisterPopulationByVaccinationDate,
         totalvaxrate=totalvaxed/VaccineRegisterPopulationByVaccinationDate) %>% 
  group_by(areaName, areaCode) %>% 
  arrange(date) %>% 
  mutate(vax1_roll=roll_mean(vaxprop1st, 7, align="center", fill=NA),
         vax2_roll=roll_mean(vaxprop2nd, 7, align="center", fill=NA),
         vaxall_roll=roll_mean(vaxpropall, 7, align="center", fill=NA),
         totalvaxrate_roll=roll_mean(totalvaxrate, 7, align="center", fill=NA),
         maxvaxrate=max(vaxall_roll, na.rm=TRUE),
         maxvaxday=date[which(vaxall_roll==maxvaxrate)][1],
         maxvaxprop=vaxall_roll/maxvaxrate) %>% 
  ungroup()

plotfrom <- "2021-01-01"
plotto <- max(vaxdata$date)-days(4)

totals <- vaxdata %>% 
  group_by(areaName, areaCode, maxvaxday) %>% 
  summarise(vaxed1dose=sum(newPeopleVaccinatedFirstDoseByVaccinationDate, na.rm=TRUE)/
              unique(VaccineRegisterPopulationByVaccinationDate),
            vaxed2dose=sum(newPeopleVaccinatedSecondDoseByVaccinationDate, na.rm=TRUE)/
              unique(VaccineRegisterPopulationByVaccinationDate)) %>% 
  ungroup()

vaxtiles <- ggplot(vaxdata %>% filter(date>=plotfrom & date<=plotto), 
                   aes(x=date, y=fct_reorder(areaName, maxvaxday), fill=maxvaxprop))+
  geom_tile(colour="White", show.legend=FALSE)+
  theme_classic()+
  scale_fill_distiller(palette="Spectral")+
  scale_y_discrete(name="", expand=c(0,0))+
  scale_x_date(name="Date", expand=c(0,0), 
               breaks=pretty_breaks(n=interval(as.Date(plotfrom), plotto)%/% months(1)))+
  labs(title="Timelines for COVID-19 vaccinations in Local Authorities/Council Areas in England & Scotland",
       subtitle=paste0("The heatmap represents the 7-day rolling average of the number of new COVID vaccinations (1st or 2nd doses), normalised to the maximum value within the Local Authority.\nLAs are ordered by the date at which they reached their peak number of vaccinations. Bars on the right represent the cumulative proportion of the population in each LA that have now been vaccinated.\nData updated to ", plotto),
       caption="Data from coronavirus.data.gov.uk | Plot by @VictimOfMaths")+
  theme_custom()+
  theme(axis.line.y=element_blank(), plot.subtitle=element_text(size=rel(0.78)), 
        axis.text.y=element_text(colour="Black"), plot.title=element_text(size=rel(1.8)))

vaxbars <- ggplot(totals, 
aes(x=vaxed2dose, y=fct_reorder(areaName, maxvaxday), fill=vaxed2dose))+
  geom_col(show.legend=FALSE)+
  theme_classic()+
  scale_fill_distiller(palette="Spectral")+
  scale_x_continuous(name="Proportion of adults\nfully vaccinated",
                     labels=label_percent(accuracy=1))+
  theme_custom()+
  theme(axis.title.y=element_blank(), axis.line.y=element_blank(), axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), axis.text.x=element_text(colour="Black"))

agg_tiff("Outputs/COVIDVaxHeatmap.tiff", units="in", width=16, height=32, res=800)
plot_grid(vaxtiles, vaxbars, align="h", rel_widths=c(1,0.2))
dev.off()

agg_png("Outputs/COVIDVaxHeatmap.png", units="in", width=16, height=32, res=800)
plot_grid(vaxtiles, vaxbars, align="h", rel_widths=c(1,0.2))
dev.off()

#Alternative version ordered from North>South
#Get map
temp <- tempfile()
temp2 <- tempfile()
source <- "https://opendata.arcgis.com/datasets/1d78d47c87df4212b79fe2323aae8e08_0.zip?outSR=%7B%22latestWkid%22%3A27700%2C%22wkid%22%3A27700%7D"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)

#The actual shapefile has a different name each time you download it, 
#so need to fish the name out of the unzipped file
name <- list.files(temp2, pattern=".shp")

shapefile <- st_read(file.path(temp2, name)) %>% 
  rename(areaCode=lad19cd) 

latdata <- shapefile %>% 
  select(areaCode, lat) %>% 
  as.data.frame() %>% 
  select(areaCode, lat) %>% 
  distinct() %>% 
  filter(!is.na(areaCode) & !areaCode %in% c("E06000062", "E06000061")) %>% 
  #Fix Northamptonshire LA changes (use Northampton and Kettering as the latitudes for the new counties)
  bind_rows(shapefile %>% filter(areaCode %in% c("E07000154", "E07000153")) %>% 
              as.data.frame() %>% 
              select(areaCode, lat) %>% 
              mutate(areaCode=if_else(areaCode=="E07000154", "E06000062", "E06000061")))

vaxdata2 <- merge(vaxdata, latdata)

totals2 <- vaxdata2 %>% 
  group_by(areaName, areaCode, maxvaxday, lat) %>% 
  summarise(vaxed1dose=sum(newPeopleVaccinatedFirstDoseByVaccinationDate, na.rm=TRUE)/
              unique(VaccineRegisterPopulationByVaccinationDate),
            vaxed2dose=sum(newPeopleVaccinatedSecondDoseByVaccinationDate, na.rm=TRUE)/
              unique(VaccineRegisterPopulationByVaccinationDate)) %>% 
  ungroup()

vaxtiles2 <- ggplot(vaxdata2 %>% filter(date>=plotfrom & date<=plotto), 
                   aes(x=date, y=fct_reorder(areaName, lat), fill=maxvaxprop))+
  geom_tile(colour="White", show.legend=FALSE)+
  theme_classic()+
  scale_fill_distiller(palette="Spectral")+
  scale_y_discrete(name="", expand=c(0,0))+
  scale_x_date(name="Date", expand=c(0,0), 
               breaks=pretty_breaks(n=interval(as.Date(plotfrom), plotto)%/% months(1)))+
  labs(title="Timelines for COVID-19 vaccinations in Local Authorities/Council Areas in England & Scotland",
       subtitle=paste0("The heatmap represents the 7-day rolling average of the number of new COVID vaccinations (1st or 2nd doses), normalised to the maximum value within the Local Authority.\nLAs are ordered from North to South. Bars on the right represent the cumulative proportion of the population in each LA that have now been fully vaccinated.\nData updated to ", plotto),
       caption="Data from coronavirus.data.gov.uk | Plot by @VictimOfMaths")+
  theme_custom()+
  theme(axis.line.y=element_blank(), plot.subtitle=element_text(size=rel(0.78)), 
        axis.text.y=element_text(colour="Black"), plot.title=element_text(size=rel(1.8)))

vaxbars2 <- ggplot(totals2, 
                  aes(x=vaxed2dose, y=fct_reorder(areaName, lat), fill=vaxed2dose))+
  geom_col(show.legend=FALSE)+
  theme_classic()+
  scale_fill_distiller(palette="Spectral")+
  scale_x_continuous(name="Proportion of adults\nfully vaccinated",
                     labels=label_percent(accuracy=1))+
  theme_custom()+
  theme(axis.title.y=element_blank(), axis.line.y=element_blank(), axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), axis.text.x=element_text(colour="Black"))

agg_tiff("Outputs/COVIDVaxHeatmapOrdered.tiff", units="in", width=16, height=32, res=800)
plot_grid(vaxtiles2, vaxbars2, align="h", rel_widths=c(1,0.2))
dev.off()

agg_png("Outputs/COVIDVaxHeatmapOrdered.png", units="in", width=16, height=32, res=800)
plot_grid(vaxtiles2, vaxbars2, align="h", rel_widths=c(1,0.2))
dev.off()