rm(list=ls())

library(tidyverse)
library(curl)
library(ukcovid19)
library(paletteer)
library(ggtext)
library(RcppRoll)
library(ragg)
library(extrafont)
library(scales)

#Bring in daily data
temp <- tempfile()
source="https://api.coronavirus.data.gov.uk/v2/data?areaType=nation&metric=cumPeopleVaccinatedFirstDoseByPublishDate&metric=cumPeopleVaccinatedSecondDoseByPublishDate&metric=newPeopleVaccinatedFirstDoseByPublishDate&metric=newPeopleVaccinatedSecondDoseByPublishDate&format=csv"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
APIdata2 <- read.csv(temp)
colnames(APIdata2) <- c("code", "name", "type", "date", "cumdose1", "cumdose2", "dose1", "dose2")

#Generate totals
APIdata2 <- APIdata2 %>% 
  group_by(date) %>% 
  summarise(cumdose1=sum(cumdose1), cumdose2=sum(cumdose2),
            dose1=sum(dose1), dose2=sum(dose2)) %>% 
  mutate(name="UK") %>% 
  bind_rows(APIdata2) %>% 
  mutate(pop=case_when(name=="England" ~ 56286961,
                       name=="Wales" ~ 3152879,
                       name=="Scotland" ~ 5463300,
                       name=="Northern Ireland" ~ 1893667,
                       name=="UK" ~ 56286961+3152879+546330+1893667),
         dose1rate=dose1*1000000/pop,
         dose2rate=dose2*1000000/pop,
         cumpropdose1=cumdose1/pop,
         cumpropdose2=cumdose2/pop,
         date=as.Date(date)) %>% 
  ungroup() %>% 
  group_by(name) %>% 
  arrange(name, date) %>% 
  mutate(dose1rateroll=roll_mean(dose1rate, 7, align="center", fill=NA)) %>% 
  ungroup()

agg_tiff("Outputs/COVIDVaccinationRateDaily.tiff", units="in", width=9, height=6, res=500)
APIdata2 %>% 
  filter(name!="UK") %>% 
ggplot()+
  geom_line(aes(x=date, y=dose1rateroll, colour=name))+
  scale_x_date(name="Week ending")+
  scale_y_continuous(name="First doses delivered per 1m population", limits=c(0,NA))+
  scale_colour_paletteer_d("fishualize::Scarus_quoyi", name="")+
  theme_classic()+
  theme(plot.title=element_text(face="bold", size=rel(1.2)),
        text=element_text(family="Roboto"))+
  labs(title="Rates of delivery of 1st vaccine doses are currently much lower in England",
       subtitle="Rolling 7-day average rates of delivery of 1st COVID-19 vaccine doses by country by publication date",
       caption="Data from coronavirus.data.gov.uk | Plot by @VictimOfMaths")

dev.off()

agg_tiff("Outputs/COVIDVaccinationTotalsDaily.tiff", units="in", width=9, height=6, res=500)
APIdata2 %>% 
  filter(name!="UK") %>% 
  select(date, name, cumpropdose1, cumpropdose2) %>% 
  gather(dose, rate, c(3,4)) %>% 
  ggplot()+
  geom_line(aes(x=date, y=rate, colour=name, linetype=dose))+
  scale_x_date(name="")+
  scale_y_continuous(name="Proportion of the population vaccinated", 
                     labels=scales::label_percent(accuracy=1))+
  scale_colour_paletteer_d("fishualize::Scarus_quoyi", name="Nation")+
  scale_linetype_discrete(name="Dose", labels=c("1st", "2nd"))+
  theme_classic()+
  theme(plot.title=element_text(face="bold", size=rel(1.2)),
        text=element_text(family="Roboto"))+
  labs(title="The four UK nations are pursuing different vaccination strategies",
       subtitle="Rates of delivery of COVID-19 vaccines by country by publication date",
       caption="Data from coronavirus.data.gov.uk | Plot by @VictimOfMaths")

dev.off()

agg_tiff("Outputs/COVIDVaccinationTargetDaily.tiff", units="in", width=8, height=6, res=500)
APIdata2 %>% 
  filter(name=="UK") %>% 
  select(date, name, cumdose1, cumdose2) %>% 
  gather(dose, count, c(3,4)) %>% 
  mutate(date=as.Date(date)) %>% 
  ggplot()+
  geom_line(aes(x=date, y=count/1000000, linetype=dose), colour="Darkred")+
  geom_hline(yintercept=14.6, colour="SkyBlue")+
  scale_x_date(name="", limits=c(as.Date("2021-01-10"), as.Date("2021-02-14")))+
  scale_y_continuous(name="Total vaccine doses delivered (millions)")+
  scale_linetype_discrete(name="Dose", labels=c("1st", "2nd"))+
  theme_classic()+
  theme(plot.title=element_text(face="bold", size=rel(1.2)), plot.subtitle=element_markdown())+
  labs(title="Vaccine delivery in the UK has now surpassed the Mid-February target",
       subtitle="<span style='color:DarkRed;'>Total COVID-19 vaccinations delivered</span>, by dose, in the UK compared to <span style='color:SkyBlue2;'>Boris Johnson's mid-February target</span><br>Data by publication date",
       caption="Data from coronavirus.data.gov.uk | Plot by @VictimOfMaths")

dev.off()

#UK level map using dashboard data
source="https://api.coronavirus.data.gov.uk/v2/data?areaType=ltla&metric=cumVaccinationFirstDoseUptakeByVaccinationDatePercentage&metric=cumVaccinationSecondDoseUptakeByVaccinationDatePercentage&format=csv"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

vaxdata <- read.csv(temp) %>% 
  set_names(c("Lacode", "areaName", "areaType", "date", "First dose", "Second dose")) %>% 
  gather(dose, prop, c(5,6)) %>% 
  mutate(date=as.Date(date))

maxdate=max(vaxdata$date)

#Read in Carl Baker's lovely cartogram
ltla <- tempfile()
source <- ("https://github.com/houseofcommonslibrary/uk-hex-cartograms-noncontiguous/raw/main/geopackages/LocalAuthorities-lowertier.gpkg")
ltla <- curl_download(url=source, destfile=ltla, quiet=FALSE, mode="wb")

Background <- st_read(ltla, layer="7 Background")

ltlavax <- st_read(ltla, layer="4 LTLA-2019") %>% 
  left_join(vaxdata, by="Lacode")

Groups <- st_read(ltla, layer="2 Groups")

Group_labels <- st_read(ltla, layer="1 Group labels") %>% 
  mutate(just=if_else(LabelPosit=="Left", 0, 1))

plot1 <- ggplot()+
  geom_sf(data=Background %>% filter(Name!="Ireland"), aes(geometry=geom))+
  geom_sf(data=ltlavax %>% filter(date==maxdate), 
          aes(geometry=geom, fill=prop/100), colour="Black", size=0.1)+
  geom_sf(data=Groups %>% filter(!RegionNation %in% c("Wales", "Northern Ireland")), 
          aes(geometry=geom), fill=NA, colour="Black")+
  geom_sf_text(data=Group_labels %>% filter(RegionNation!="Wales"), 
               aes(geometry=geom, label=Group.labe,
                                      hjust=just), size=rel(2.4), colour="Black")+
  scale_fill_paletteer_c("pals::ocean.haline", direction=-1, limits=c(0,1),
                         name="Proportion of adults vaccinated", labels=label_percent(accuracy=1))+
  facet_wrap(~dose)+
  theme_void()+
  theme(plot.title=element_text(face="bold", size=rel(2.5)),
        text=element_text(family="Lato"), legend.position="top",
        strip.text=element_text(face="bold", size=rel(1), hjust=0))+
  guides(fill = guide_colorbar(title.position = 'top', title.hjust = .5,
                               barwidth = unit(20, 'lines'), barheight = unit(.5, 'lines')))+
  labs(title="London is lagging behind in the vaccine drive",
       subtitle="",
       caption=paste0("Data from coronavirus.data.gov.uk, Cartogram from @carlbaker/House of Commons Library\nData up to ",
       maxdate, "\nPlot by @VictimOfMaths"))

agg_tiff("Outputs/COVIDVaxLTLACartogramEngSco.tiff", units="in", width=9, height=10, res=800)
plot1
dev.off()

