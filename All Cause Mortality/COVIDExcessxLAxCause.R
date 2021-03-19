rm(list=ls())

library(tidyverse)
library(forcats)
library(paletteer)
library(extrafont)
library(ggtext)

#Read in data
load("COVID_LA_Plots/Alldata.RData")

#Filter out occurence data (as we don't have historic occurence data for English & Welsh LAs)
locdata <- data %>% 
  filter(measure=="Registrations" & week>9)

#Replace missing week 53 historic data with week 52 for England & Wales
temp <- locdata %>% 
  filter(week==52 & country!="Scotland") %>% 
  select(week, location, code, deaths.1519) %>% 
  mutate(week=53) %>% 
  rename(deaths.1519alt=deaths.1519)

#Calculate location-specific excess death rates for each LTLA
locdata <- locdata %>% 
  merge(temp, all.x=TRUE) %>% 
  mutate(deaths.1519=coalesce(deaths.1519, deaths.1519alt),
         allexcess=AllCause.20-deaths.1519) %>% 
  group_by(name, Region, country, code, location, pop) %>% 
  summarise(deaths.1519=sum(deaths.1519), allexcess=sum(allexcess),
            AllCause.20=sum(AllCause.20)) %>% 
  ungroup() %>% 
  group_by(name, Region, country, code, pop) %>% 
  mutate(alldeaths.1519=sum(deaths.1519), alldeaths.20=sum(AllCause.20)) %>% 
  ungroup() %>% 
  mutate(excessrate=allexcess*100000/pop, totexcessrate=(alldeaths.20-alldeaths.1519)*100000/pop,
         name=fct_reorder(name, totexcessrate), excessprop=allexcess/deaths.1519,
         totexcessprop=(alldeaths.20-alldeaths.1519)/alldeaths.1519)

agg_tiff("Outputs/COVIDExcessPropxLAxLoc.tiff", units="in", width=10, height=8, res=500)
ggplot(locdata %>% filter(!Region %in% c("Region", "Nation") & location=="Hospital" & totexcessprop>0.325),
       aes(x=totexcessprop, y=fct_reorder(name, totexcessprop), fill=totexcessprop))+
  geom_col(show.legend=FALSE)+
  geom_vline(xintercept=0)+
  scale_x_continuous(name="Proportional increase in deaths",
                     labels=scales::label_percent(accuracy=2))+
  scale_y_discrete(name="")+
  scale_fill_paletteer_c("pals::ocean.amp", limits=c(0,0.6))+
  theme_classic()+
  theme(text=element_text(family="Roboto"), plot.title=element_text(face="bold"))+
  labs(title="The Local Authorities with the biggest relative increases in deaths are mostly in London",
       subtitle="Local Authorities in England, Wales and Scotland with the largest proportional increase in mortality\nsince the start of the pandemic compared to average mortality in 2015-19",
       caption="Data from ONS | Plot by @VictimOfMaths")
dev.off()

agg_tiff("Outputs/COVIDExcessxLAxLoc.tiff", units="in", width=10, height=8, res=500)
ggplot(locdata %>% filter(!Region %in% c("Region", "Nation") & totexcessrate>250),
       aes(x=excessrate, y=name, fill=location))+
  geom_col(position="stack")+
  geom_vline(xintercept=0)+
  scale_x_continuous(name="Excess deaths per 100,000 population")+
  scale_y_discrete(name="")+
  scale_fill_paletteer_d("ggsci::planetexpress_futurama", name="Place of death")+
  theme_classic()+
  theme(text=element_text(family="Roboto"), plot.title=element_text(face="bold"))+
  labs(title="The Local Authorities with the highest excess mortality rates are (mostly) not in London",
       subtitle="Local Authorities in England, Wales and Scotland with excess mortality rates of more than 250 per 100,000\nsince the start of the pandemic compared to average mortality rates in 2015-19",
       caption="Data from ONS | Plot by @VictimOfMaths")
dev.off()

agg_tiff("Outputs/COVIDExcessBaselineRatexLA.tiff", units="in", width=10, height=8, res=500)
ggplot(locdata %>% filter(!Region %in% c("Region", "Nation") & location=="Hospital"),
       aes(x=alldeaths.1519*100000/pop, y=fct_reorder(name, alldeaths.1519*100000/pop),
       fill=Region))+
  geom_col(position="stack", show.legend=FALSE)+
  scale_x_continuous(name="Annual deaths per 100,000")+
  scale_y_discrete(name="")+
  scale_fill_manual(values=c("Grey60", "Grey60", "#FF007F", rep("Grey60", times=8)))+
  theme_classic()+
  theme(text=element_text(family="Roboto"), axis.text.y=element_blank(),
        axis.line.y=element_blank(), axis.ticks.y=element_blank(),
        plot.title=element_text(face="bold", size=rel(1.5)),
        plot.subtitle=element_markdown())+
  labs(title="Local Authorities in London have lower mortality rates",
       subtitle="Annual deaths per 100,000 in Local Authorities in <span style='color:#FF007F;'>London</span> compared to <span style='color:Grey60;'>the rest of Great Britain",
       caption="Data from ONS | Plot by @VictimOfMaths")+
  annotate("text", x=1100, y=30, 
           label="Local Authorities in London have younger populations,\non average, so see fewer deaths", 
           family="Roboto", fontface="bold", size=rel(3), colour="#FF007F")+
  geom_curve(aes(x=940, y=27, xend=600, yend=18), 
             colour="#FF007F", curvature=-0.15, arrow=arrow(length=unit(0.1, "cm"), type="closed"), 
             lineend="round")
dev.off()

#Map of the excess mortality rates
mapdata <- locdata %>% 
  filter(name=="Buckinghamshire") %>% 
  mutate(code="E07000004") %>% 
  bind_rows(locdata)

mapdata <- mapdata %>% 
  filter(code=="E07000004") %>% 
  mutate(code="E07000005") %>% 
  bind_rows(mapdata)

mapdata <- mapdata %>% 
  filter(code=="E07000004") %>% 
  mutate(code="E07000006") %>% 
  bind_rows(mapdata)

mapdata <- mapdata %>% 
  filter(code=="E07000004") %>% 
  mutate(code="E07000007") %>% 
  bind_rows(mapdata)

#Download Carl Baker's lovely map
ltla <- tempfile()
source <- ("https://github.com/houseofcommonslibrary/uk-hex-cartograms-noncontiguous/raw/main/geopackages/LocalAuthorities-lowertier.gpkg")
ltla <- curl_download(url=source, destfile=ltla, quiet=FALSE, mode="wb")

Background <- st_read(ltla, layer="6 Background")

ltlacases <- st_read(ltla, layer="4 LTLA-2019") %>% 
  left_join(mapdata, by=c("Lacode"="code"))

Groups <- st_read(ltla, layer="2 Groups")

Group_labels <- st_read(ltla, layer="1 Group labels") %>% 
  mutate(just=if_else(LabelPosit=="Left", 0, 1))

plot1 <- ggplot()+
  geom_sf(data=Background %>% filter(Name!="Ireland"), aes(geometry=geom))+
  geom_sf(data=ltlacases %>% filter(RegionNation!="Northern Ireland" & location=="Hospital"), 
          aes(geometry=geom, fill=totexcessrate), colour="Black", size=0.1)+
  geom_sf(data=Groups %>% filter(RegionNation!="Northern Ireland"), aes(geometry=geom), fill=NA, colour="Black")+
  geom_sf_text(data=Group_labels %>% filter(RegionNation!="Northern Ireland"), aes(geometry=geom, label=Group.labe,
                                      hjust=just), size=rel(2.4), colour="Black")+
  scale_fill_paletteer_c("pals::ocean.curl",
                         name="Excess deaths\nper 100,000", 
                         limit=c(-1,1)*max(abs((ltlacases %>% filter(location=="Hospital"))$totexcessrate)))+
  theme_void()+
  theme(plot.title=element_text(face="bold", size=rel(1.2)),
        text=element_text(family="Roboto"))+
  annotate("text", x=16, y=40, label="Excess deaths are highest\nin the North-West\nand West Midlands...",
           family="Roboto", fontface="bold", size=rel(2.5), colour="#6e0a1e")+
  annotate("text", x=50, y=1, label="...and isolated areas\nin the South East",
           family="Roboto", fontface="bold", size=rel(2.5), colour="#6e0a1e")+
  labs(title="Excess mortality rates for Local Authorities in Great Britain",
       subtitle="Additional deaths per 100,000 population from week 10 in 2020 to the latest week of available data,\ncompared to the average mortality rate in 2015-19.",
       caption="Data from ONS and NRS, Cartogram from @carlbaker/House of Commons Library\nPlot by @VictimOfMaths")

agg_tiff("Outputs/COVIDExcessLTLACartogram.tiff", units="in", width=9, height=10, res=800)
plot1
dev.off()

plot1b <- ggplot()+
  geom_sf(data=Background %>% filter(Name!="Ireland"), aes(geometry=geom))+
  geom_sf(data=ltlacases %>% filter(RegionNation!="Northern Ireland" & location=="Hospital"), 
          aes(geometry=geom, fill=totexcessprop), colour="Black", size=0.1)+
  geom_sf(data=Groups %>% filter(RegionNation!="Northern Ireland"), aes(geometry=geom), fill=NA, colour="Black")+
  geom_sf_text(data=Group_labels %>% filter(RegionNation!="Northern Ireland"), aes(geometry=geom, label=Group.labe,
                                                                                   hjust=just), size=rel(2.4), colour="Black")+
  scale_fill_paletteer_c("pals::ocean.curl",
                         name="Change in\nall-cause deaths", 
                         limit=c(-1,1)*max(abs((ltlacases %>% filter(location=="Hospital"))$totexcessprop)),
                         label=scales::label_percent(accuracy=2))+
  theme_void()+
  theme(plot.title=element_text(face="bold", size=rel(1.2)),
        text=element_text(family="Roboto"))+
  labs(title="Relative changes in all-cause deaths in Local Authorities in Great Britain",
       subtitle="Proportional changes in all-cause deaths from week 10 in 2020 to the latest week of available data,\ncompared to the average mortality rate in 2015-19.",
       caption="Data from ONS and NRS, Cartogram from @carlbaker/House of Commons Library\nPlot by @VictimOfMaths")

agg_tiff("Outputs/COVIDExcessPropLTLACartogram.tiff", units="in", width=9, height=10, res=800)
plot1b
dev.off()

plot2 <- ggplot()+
  geom_sf(data=Background %>% filter(Name!="Ireland"), aes(geometry=geom))+
  geom_sf(data=ltlacases %>% filter(RegionNation!="Northern Ireland" & location=="Hospital"), 
          aes(geometry=geom, fill=excessrate), colour="Black", size=0.1)+
  geom_sf(data=Groups %>% filter(RegionNation!="Northern Ireland"), aes(geometry=geom), fill=NA, colour="Black")+
  geom_sf_text(data=Group_labels %>% filter(RegionNation!="Northern Ireland"), aes(geometry=geom, label=Group.labe,
                                                                                   hjust=just), size=rel(2.4), colour="Black")+
  scale_fill_paletteer_c("pals::ocean.curl",
                         name="Excess deaths\nper 100,000", 
                         limit=c(-1,1)*max(abs((ltlacases %>% filter(location=="Hospital"))$excessrate)))+
  theme_void()+
  theme(plot.title=element_text(face="bold", size=rel(1.2)),
        text=element_text(family="Roboto"))+
  annotate("text", x=45, y=60, label="There have been fewer deaths\nthan usual in hospitals\nin many more remote areas...",
           family="Roboto", fontface="bold", size=rel(2.5), colour="#006D5B")+
  annotate("text", x=16, y=40, label="...but more in urban areas",
           family="Roboto", fontface="bold", size=rel(2.5), colour="#6e0a1e")+labs(title="Hospital excess mortality rates for Local Authorities in Great Britain",
       subtitle="Additional deaths per 100,000 population from week 10 in 2020 to the latest week of available data,\ncompared to the average mortality rate in 2015-19.",
       caption="Data from ONS and NRS, Cartogram from @carlbaker/House of Commons Library\nPlot by @VictimOfMaths")

agg_tiff("Outputs/COVIDExcessHospLTLACartogram.tiff", units="in", width=9, height=10, res=800)
plot2
dev.off()

plot2b <- ggplot()+
  geom_sf(data=Background %>% filter(Name!="Ireland"), aes(geometry=geom))+
  geom_sf(data=ltlacases %>% filter(RegionNation!="Northern Ireland" & location=="Hospital"), 
          aes(geometry=geom, fill=excessprop), colour="Black", size=0.1)+
  geom_sf(data=Groups %>% filter(RegionNation!="Northern Ireland"), aes(geometry=geom), fill=NA, colour="Black")+
  geom_sf_text(data=Group_labels %>% filter(RegionNation!="Northern Ireland"), aes(geometry=geom, label=Group.labe,
                                                                                   hjust=just), size=rel(2.4), colour="Black")+
  scale_fill_paletteer_c("pals::ocean.curl",
                         name="Change in\nall-cause deaths", 
                         limit=c(-1,1)*max(abs((ltlacases %>% filter(location=="Hospital"))$excessprop)),
                         label=scales::label_percent(accuracy=2))+
  theme_void()+
  theme(plot.title=element_text(face="bold", size=rel(1.2)),
        text=element_text(family="Roboto"))+
  labs(title="Relative changes in all-cause deaths in hospitals in Local Authorities in Great Britain",
       subtitle="Proportional changes in all-cause deaths from week 10 in 2020 to the latest week of available data,\ncompared to the average mortality rate in 2015-19.",
       caption="Data from ONS and NRS, Cartogram from @carlbaker/House of Commons Library\nPlot by @VictimOfMaths")

agg_tiff("Outputs/COVIDExcessHospPropLTLACartogram.tiff", units="in", width=9, height=10, res=800)
plot2b
dev.off()

plot3 <- ggplot()+
  geom_sf(data=Background %>% filter(Name!="Ireland"), aes(geometry=geom))+
  geom_sf(data=ltlacases %>% filter(RegionNation!="Northern Ireland" & location=="Care home"), 
          aes(geometry=geom, fill=excessrate), colour="Black", size=0.1)+
  geom_sf(data=Groups %>% filter(RegionNation!="Northern Ireland"), aes(geometry=geom), fill=NA, colour="Black")+
  geom_sf_text(data=Group_labels %>% filter(RegionNation!="Northern Ireland"), aes(geometry=geom, label=Group.labe,
                                                                                   hjust=just), size=rel(2.4), colour="Black")+
  scale_fill_paletteer_c("pals::ocean.curl",
                         name="Excess deaths\nper 100,000", 
                         limit=c(-1,1)*max(abs((ltlacases %>% filter(location=="Care home"))$excessrate)))+
  theme_void()+
  theme(plot.title=element_text(face="bold", size=rel(1.2)),
        text=element_text(family="Roboto"))+
  annotate("text", x=47, y=52, label="Excess care home deaths are\nhighest in the North East..",
           family="Roboto", fontface="bold", size=rel(2.5), colour="#6e0a1e")+
  annotate("text", x=48, y=1, label="...and isolated areas\nin the South East",
           family="Roboto", fontface="bold", size=rel(2.5), colour="#6e0a1e")+
  labs(title="Care home excess mortality rates for Local Authorities in Great Britain",
       subtitle="Additional deaths per 100,000 population from week 10 in 2020 to the latest week of available data,\ncompared to the average mortality rate in 2015-19.",
       caption="Data from ONS and NRS, Cartogram from @carlbaker/House of Commons Library\nPlot by @VictimOfMaths")

agg_tiff("Outputs/COVIDExcessCareHomeLTLACartogram.tiff", units="in", width=9, height=10, res=800)
plot3
dev.off()

plot3b <- ggplot()+
  geom_sf(data=Background %>% filter(Name!="Ireland"), aes(geometry=geom))+
  geom_sf(data=ltlacases %>% filter(RegionNation!="Northern Ireland" & location=="Care home"), 
          aes(geometry=geom, fill=excessprop), colour="Black", size=0.1)+
  geom_sf(data=Groups %>% filter(RegionNation!="Northern Ireland"), aes(geometry=geom), fill=NA, colour="Black")+
  geom_sf_text(data=Group_labels %>% filter(RegionNation!="Northern Ireland"), aes(geometry=geom, label=Group.labe,
                                                                                   hjust=just), size=rel(2.4), colour="Black")+
  scale_fill_paletteer_c("pals::ocean.curl",
                         name="Change in\nall-cause deaths", 
                         limit=c(-1,1)*max(abs((ltlacases %>% filter(location=="Care home"))$excessprop)),
                         label=scales::label_percent(accuracy=2))+
  theme_void()+
  theme(plot.title=element_text(face="bold", size=rel(1.2)),
        text=element_text(family="Roboto"))+
  labs(title="Relative changes in all-cause deaths in care homes in Local Authorities in Great Britain",
       subtitle="Proportional changes in all-cause deaths from week 10 in 2020 to the latest week of available data,\ncompared to the average mortality rate in 2015-19.",
       caption="Data from ONS and NRS, Cartogram from @carlbaker/House of Commons Library\nPlot by @VictimOfMaths")

agg_tiff("Outputs/COVIDExcessCareHomePropLTLACartogram.tiff", units="in", width=9, height=10, res=800)
plot3b
dev.off()

plot4 <- ggplot()+
  geom_sf(data=Background %>% filter(Name!="Ireland"), aes(geometry=geom))+
  geom_sf(data=ltlacases %>% filter(RegionNation!="Northern Ireland" & location=="Home/Other"), 
          aes(geometry=geom, fill=excessrate), colour="Black", size=0.1)+
  geom_sf(data=Groups %>% filter(RegionNation!="Northern Ireland"), aes(geometry=geom), fill=NA, colour="Black")+
  geom_sf_text(data=Group_labels %>% filter(RegionNation!="Northern Ireland"), aes(geometry=geom, label=Group.labe,
                                                                                   hjust=just), size=rel(2.4), colour="Black")+
  scale_fill_paletteer_c("pals::ocean.curl",
                         name="Excess deaths\nper 100,000", 
                         limit=c(-1,1)*max(abs((ltlacases %>% filter(location=="Home/Other"))$excessrate)))+
  theme_void()+
  theme(plot.title=element_text(face="bold", size=rel(1.2)),
        text=element_text(family="Roboto"))+
  annotate("text", x=45, y=60, label="Excess deaths at home are\nhighest in the West of Scotland",
           family="Roboto", fontface="bold", size=rel(2.5), colour="#6e0a1e")+
  labs(title="Home excess mortality rates for Local Authorities in Great Britain",
       subtitle="Additional deaths per 100,000 population from week 10 in 2020 to the latest week of available data,\ncompared to the average mortality rate in 2015-19.",
       caption="Data from ONS and NRS, Cartogram from @carlbaker/House of Commons Library\nPlot by @VictimOfMaths")

agg_tiff("Outputs/COVIDExcessHomeLTLACartogram.tiff", units="in", width=9, height=10, res=800)
plot4
dev.off()

plot4b <- ggplot()+
  geom_sf(data=Background %>% filter(Name!="Ireland"), aes(geometry=geom))+
  geom_sf(data=ltlacases %>% filter(RegionNation!="Northern Ireland" & location=="Home/Other"), 
          aes(geometry=geom, fill=excessprop), colour="Black", size=0.1)+
  geom_sf(data=Groups %>% filter(RegionNation!="Northern Ireland"), aes(geometry=geom), fill=NA, colour="Black")+
  geom_sf_text(data=Group_labels %>% filter(RegionNation!="Northern Ireland"), aes(geometry=geom, label=Group.labe,
                                                                                   hjust=just), size=rel(2.4), colour="Black")+
  scale_fill_paletteer_c("pals::ocean.curl",
                         name="Change in\nall-cause deaths", 
                         limit=c(-1,1)*max(abs((ltlacases %>% filter(location=="Home/Other"))$excessprop)),
                         label=scales::label_percent(accuracy=2))+
  theme_void()+
  theme(plot.title=element_text(face="bold", size=rel(1.2)),
        text=element_text(family="Roboto"))+
  labs(title="Relative changes in all-cause deaths at home in Local Authorities in Great Britain",
       subtitle="Proportional changes in all-cause deaths from week 10 in 2020 to the latest week of available data,\ncompared to the average mortality rate in 2015-19.",
       caption="Data from ONS and NRS, Cartogram from @carlbaker/House of Commons Library\nPlot by @VictimOfMaths")

agg_tiff("Outputs/COVIDExcessHomePropLTLACartogram.tiff", units="in", width=9, height=10, res=800)
plot4b
dev.off()