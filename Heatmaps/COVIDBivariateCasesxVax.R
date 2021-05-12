rm(list=ls())

library(tidyverse)
library(curl)
library(sf)
library(gtools)
library(extrafont)
library(cowplot)
library(ragg)
library(paletteer)
library(scales)

#Read in MSOA-level case data from the dashboard
caseurl <- "https://api.coronavirus.data.gov.uk/v2/data?areaType=msoa&metric=newCasesBySpecimenDateRollingSum&format=csv"

cases <- tempfile()
cases <- curl_download(url=caseurl, destfile=cases, quiet=FALSE, mode="wb")
casedata <- read.csv(cases) %>% 
  mutate(date=as.Date(date)) %>% 
  filter(date==max(date)) %>% 
  rename(cases=newCasesBySpecimenDateRollingSum, msoa11cd=areaCode) %>% 
  select(cases, msoa11cd)

#Read in vaccination data
#Download vaccination data by MSOA
#https://www.england.nhs.uk/statistics/statistical-work-areas/covid-19-vaccinations/
vax <- tempfile()
vaxurl <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/05/COVID-19-weekly-announced-vaccinations-06-May-2021.xlsx"
vax <- curl_download(url=vaxurl, destfile=vax, quiet=FALSE, mode="wb")

vaxdata <- read_excel(vax, sheet="MSOA", range="F16:P6806", col_names=FALSE) %>% 
  rename(msoa11cd=`...1`, msoa11nm=`...2`, `<45`=`...3`,  `45-49`=`...4`, `50-54`=`...5`, 
         `55-59`=`...6`, `60-64`=`...7`, 
         `65-69`=`...8`, `70-74`=`...9`, `75-79`=`...10`, `80+`=`...11`) %>% 
  gather(age, vaccinated, c(3:11))

pop2 <- read_excel(vax, sheet="Population estimates (NIMS)", range="R16:AC6806", col_names=FALSE) %>% 
  select(-c(2)) %>% 
  rename(msoa11cd=`...1`) %>% 
  gather(age, pop, c(2:11)) %>% 
  mutate(age=case_when(
    age %in% c("...3", "...4") ~ "<45",
    age=="...5" ~ "45-49",
    age=="...6" ~ "50-54",
    age=="...7" ~ "55-59",
    age=="...8" ~ "60-64",
    age=="...9" ~ "65-69",
    age=="...10" ~ "70-74",
    age=="...11" ~ "75-79",
    TRUE ~ "80+")) %>% 
  group_by(msoa11cd, age) %>% 
  summarise(pop=sum(pop)) %>% 
  ungroup()

#COMBINE and calcualte age-standardised vax rates
combineddata <- merge(vaxdata, pop2) %>% 
  mutate(vaxprop=vaccinated/pop) %>% 
  select(-c(vaccinated, pop)) %>% 
  spread(age, vaxprop) %>% 
  mutate(asrate=(`<45`*38000+`45-49`*7000+`50-54`*7000+`55-59`*6500+`60-64`*6000+`65-69`*5500+`70-74`*5000+
                   `75-79`*4000+`80+`*5000)/84000) %>% 
  merge(casedata, all=TRUE) %>% 
  merge(pop2 %>% group_by(msoa11cd) %>% summarise(pop=sum(pop)) %>%  ungroup()) %>% 
  mutate(caserate=cases*100000/pop)

ggplot(combineddata, aes(x=caserate, y=asrate))+
  geom_point()

#Download Carl Baker's lovely cartogram
msoa <- tempfile()
source <- ("https://github.com/houseofcommonslibrary/uk-hex-cartograms-noncontiguous/raw/main/geopackages/MSOA.gpkg")
msoa <- curl_download(url=source, destfile=msoa, quiet=FALSE, mode="wb")

BackgroundMSOA <- st_read(msoa, layer="5 Background")

MSOA <- st_read(msoa, layer="4 MSOA hex") %>% 
  left_join(combineddata, by="msoa11cd")

GroupsMSOA <- st_read(msoa, layer="2 Groups")

Group_labelsMSOA <- st_read(msoa, layer="1 Group labels") %>% 
  mutate(just=if_else(LabelPosit=="Left", 0, 1))

LAsMSOA <- st_read(msoa, layer="3 Local authority outlines (2019)")

#Option 1
#Remove missing MSOAs and calculate tertiles
MSOAv1 <- MSOA %>% 
  filter(!is.na(caserate)) %>% 
  mutate(casetert=quantcut(caserate, q=3, labels=FALSE),
         vaxtert=quantcut(asrate, q=3, labels=FALSE),
         key=case_when(
           casetert==1 & vaxtert==1 ~ 1,
           casetert==1 & vaxtert==2 ~ 2,
           casetert==1 & vaxtert==3 ~ 3,
           casetert==2 & vaxtert==1 ~ 4,
           casetert==2 & vaxtert==2 ~ 5,
           casetert==2 & vaxtert==3 ~ 6,
           casetert==3 & vaxtert==1 ~ 7,
           casetert==3 & vaxtert==2 ~ 8,
           TRUE ~ 9),
         fillcolour=case_when(
           key==1 ~ "#f0f0f0", key==2 ~ "#a0dcdd", key==3 ~ "#00cfc1",
           key==4 ~ "#ffa2aa", key==5 ~ "#afa7b7", key==6 ~ "#44b4cb",
           key==7 ~ "#ff3968", key==8 ~ "#c066b2", TRUE ~ "#6d87cc"))

#Option 2
#Have all missing MSOAs as a single category and calculate tertiles
temp <- MSOA %>% 
  filter(is.na(caserate) & RegionNation!="Wales") %>% 
  mutate(casetert=1)

MSOAv2 <- MSOA %>% 
  filter(!is.na(caserate)) %>% 
  mutate(casetert=quantcut(caserate, q=2, labels=FALSE)+1) %>% 
  bind_rows(temp) %>% 
  mutate(vaxtert=quantcut(asrate, q=3, labels=FALSE),
         key=case_when(
           casetert==1 & vaxtert==1 ~ 1,
           casetert==1 & vaxtert==2 ~ 2,
           casetert==1 & vaxtert==3 ~ 3,
           casetert==2 & vaxtert==1 ~ 4,
           casetert==2 & vaxtert==2 ~ 5,
           casetert==2 & vaxtert==3 ~ 6,
           casetert==3 & vaxtert==1 ~ 7,
           casetert==3 & vaxtert==2 ~ 8,
           TRUE ~ 9),
         fillcolour=case_when(
           key==1 ~ "#f0f0f0", key==2 ~ "#a0dcdd", key==3 ~ "#00cfc1",
           key==4 ~ "#ffa2aa", key==5 ~ "#afa7b7", key==6 ~ "#44b4cb",
           key==7 ~ "#ff3968", key==8 ~ "#c066b2", TRUE ~ "#6d87cc"))

#generate dataframe for key
keydata <- MSOAv2 %>%
  filter(!is.na(fillcolour)) %>%
  group_by(casetert, vaxtert) %>%
  summarise(RGB=unique(fillcolour))

key <- ggplot(keydata)+
  geom_tile(aes(x=casetert, y=vaxtert, fill=RGB))+
  scale_fill_identity()+
  labs(x = expression("More COVID cases" %->%  ""),
       y = expression("Higher vaccination rates" %->%  "")) +
  theme_classic() +
  # make font small enough
  theme(
    axis.title = element_text(size = 9),axis.line=element_blank(), 
    axis.ticks=element_blank(), axis.text=element_blank(),
    text=element_text(family="Lato"))+
  # quadratic tiles
  coord_fixed()

plot <- ggplot()+
  geom_sf(data=BackgroundMSOA, aes(geometry=geom))+
  geom_sf(data=MSOAv2, 
          aes(geometry=geom, fill=fillcolour), colour=NA)+
  geom_sf(data=LAsMSOA %>% filter(RegionNation!="Wales"), 
          aes(geometry=geom), fill=NA, colour="Black", size=0.1)+
  geom_sf(data=GroupsMSOA %>% filter(RegionNation!="Wales"), 
          aes(geometry=geom), fill=NA, colour="Black")+
  geom_sf_text(data=Group_labelsMSOA %>% filter(RegionNation!="Wales"), 
               aes(geometry=geom, label=Group.labe,
                   hjust=just), size=rel(2.4), colour="Black", family="Lato")+
  scale_fill_identity(na.value="Black")+
  theme_void()+
  theme(plot.title=element_text(face="bold", size=rel(1.6)),
        text=element_text(family="Lato"), plot.title.position = "panel")+
  annotate("text", x=55.5, y=14, label="Fewer cases,\nfewer vaccinations", size=3,
           fontface="bold", family="Lato")+
  geom_curve(aes(x=53, y=14, xend=47.8, yend=14.5), curvature=0.15)+
  annotate("text", x=15, y=10, label="Fewer cases,\nmore vaccinations", size=3,
           fontface="bold", family="Lato")+
  geom_curve(aes(x=16, y=9, xend=20, yend=5), curvature=0.2)+
  annotate("text", x=51, y=35, label="More cases,\nmore vaccinations", size=3,
           fontface="bold", family="Lato")+
  geom_curve(aes(x=47.5, y=34, xend=39.9, yend=34.5), curvature=-0.2)+
  annotate("text", x=24, y=54, label="More cases,\nfewer vaccinations", size=3,
           fontface="bold", family="Lato")+
  geom_curve(aes(x=26, y=52.8, xend=31.3, yend=50.9), curvature=0.1)+
  labs(title="Comparing COVID-19 case rates and vaccine coverage",
       subtitle="Rolling 7-day rate of new COVID cases and age-standardised rates of delivery of at least one vaccine dose.\nCase rates are censored for areas with fewer than 3 cases, which currently covers the majority of areas.\nAs a result there are considerably more areas in the lowest category of case rates.",       
       caption="Data from coronavirus.data.gov.uk and NHS England, cartogram from @carlbaker/House of Commons Library\nPlot by @VictimOfMaths")

agg_tiff("Outputs/COVIDBivariateCasesVax.tiff", units="in", width=8, height=10, res=800)
ggdraw()+
  draw_plot(plot, 0,0,1,1)+
  draw_plot(key, 0.66,0.66,0.28,0.28)
dev.off()

#Repeat for cumulative infection rates
casedata.full <- read.csv(cases) %>% 
  mutate(date=as.Date(date)) %>% 
  rename(cases=newCasesBySpecimenDateRollingSum, msoa11cd=areaCode) %>% 
  select(cases, date, msoa11cd) %>% 
  group_by(msoa11cd) %>% 
  summarise(cases=sum(cases)) %>% 
  ungroup() 

casedata.full <- merge(vaxdata, pop2) %>% 
  mutate(vaxprop=vaccinated/pop) %>% 
  select(-c(vaccinated, pop)) %>% 
  spread(age, vaxprop) %>% 
  mutate(asrate=(`<45`*38000+`45-49`*7000+`50-54`*7000+`55-59`*6500+`60-64`*6000+`65-69`*5500+`70-74`*5000+
                   `75-79`*4000+`80+`*5000)/84000) %>% 
  merge(casedata.full, all=TRUE) %>% 
  merge(pop2 %>% group_by(msoa11cd) %>% summarise(pop=sum(pop)) %>%  ungroup()) %>% 
  mutate(caserate=cases*100000/pop,
         caseprop=cases/pop,
         casetert=quantcut(caserate, q=3, labels=FALSE),
         vaxtert=quantcut(asrate, q=3, labels=FALSE),
         key=case_when(
           casetert==1 & vaxtert==1 ~ 1,
           casetert==1 & vaxtert==2 ~ 2,
           casetert==1 & vaxtert==3 ~ 3,
           casetert==2 & vaxtert==1 ~ 4,
           casetert==2 & vaxtert==2 ~ 5,
           casetert==2 & vaxtert==3 ~ 6,
           casetert==3 & vaxtert==1 ~ 7,
           casetert==3 & vaxtert==2 ~ 8,
           TRUE ~ 9),
         fillcolour=case_when(
           key==1 ~ "#f0f0f0", key==2 ~ "#a0dcdd", key==3 ~ "#00cfc1",
           key==4 ~ "#ffa2aa", key==5 ~ "#afa7b7", key==6 ~ "#44b4cb",
           key==7 ~ "#ff3968", key==8 ~ "#c066b2", TRUE ~ "#6d87cc"))

MSOA.full <- st_read(msoa, layer="4 MSOA hex") %>% 
  left_join(casedata.full, by="msoa11cd")

#Plot cumulative attack rates across the pandemic
agg_tiff("Outputs/COVIDCasesMSOACumul.tiff", units="in", width=8, height=10, res=800)
ggplot()+
  geom_sf(data=BackgroundMSOA, aes(geometry=geom))+
  geom_sf(data=MSOA.full%>% filter(RegionNation!="Wales"), 
          aes(geometry=geom, fill=caseprop), colour=NA)+
  geom_sf(data=LAsMSOA %>% filter(RegionNation!="Wales"), 
          aes(geometry=geom), fill=NA, colour="Black", size=0.1)+
  geom_sf(data=GroupsMSOA %>% filter(RegionNation!="Wales"), 
          aes(geometry=geom), fill=NA, colour="Black")+
  geom_sf_text(data=Group_labelsMSOA %>% filter(RegionNation!="Wales"), 
               aes(geometry=geom, label=Group.labe,
                   hjust=just), size=rel(2.4), colour="Black", family="Roboto")+
  scale_fill_paletteer_c("pals::ocean.haline", direction=-1, name="Cumulative\ninfection rate",
                         labels=label_percent(accuracy=1))+
  theme_void()+
  theme(plot.title=element_text(face="bold", size=rel(1.6)),
        text=element_text(family="Merriweather"), plot.margin=margin(0,10,0,10))+
  labs(title="Cumulative COVID infection rates across the pandemic",
       subtitle="Total COVID case rates since March 2020 in English Middle Super Output Areas.\nThe case rate figures are conservative as they exclude weeks with low numbers of cases. ",       
       caption="Data from coronavirus.data.gov.uk and NHS England, cartogram from @carlbaker/House of Commons Library\nPlot by @VictimOfMaths")
dev.off()

plot.full <- ggplot()+
  geom_sf(data=BackgroundMSOA, aes(geometry=geom))+
  geom_sf(data=MSOA.full, 
          aes(geometry=geom, fill=fillcolour), colour=NA)+
  geom_sf(data=LAsMSOA %>% filter(RegionNation!="Wales"), 
          aes(geometry=geom), fill=NA, colour="Black", size=0.1)+
  geom_sf(data=GroupsMSOA %>% filter(RegionNation!="Wales"), 
          aes(geometry=geom), fill=NA, colour="Black")+
  geom_sf_text(data=Group_labelsMSOA %>% filter(RegionNation!="Wales"), 
               aes(geometry=geom, label=Group.labe,
                   hjust=just), size=rel(2.4), colour="Black", family="Lato")+
  scale_fill_identity(na.value="Black")+
  theme_void()+
  theme(plot.title=element_text(face="bold", size=rel(1.6)),
        text=element_text(family="Lato"), plot.title.position = "panel")+
  annotate("text", x=55.5, y=14, label="Fewer cases,\nfewer vaccinations", size=3,
           fontface="bold", family="Lato")+
  geom_curve(aes(x=53, y=14, xend=43.1, yend=13.6), curvature=0.15)+
  annotate("text", x=15, y=10, label="Fewer cases,\nmore vaccinations", size=3,
           fontface="bold", family="Lato")+
  geom_curve(aes(x=16, y=9, xend=20, yend=5), curvature=0.2)+
  annotate("text", x=51, y=35, label="More cases,\nmore vaccinations", size=3,
           fontface="bold", family="Lato")+
  geom_curve(aes(x=47.5, y=34, xend=39.9, yend=34.5), curvature=-0.2)+
  annotate("text", x=19, y=43, label="More cases,\nfewer vaccinations", size=3,
           fontface="bold", family="Lato")+
  geom_curve(aes(x=19.5, y=41.5, xend=22.5, yend=35.4), curvature=0.1)+
  labs(title="Comparing total COVID-19 case rates across the pandemic \nwith current vaccine coverage",
       subtitle="Total COVID case rates since March 2020 and age-standardised rates of delivery of at least one vaccine dose.\nThe case rate figures are conservative as they exclude weeks with low numbers of cases and cases where\nindividuals did not get tested. This is particularly likely to underestimate case rates during the first wave,\nwhen testing was limited",       
       caption="Data from coronavirus.data.gov.uk and NHS England, cartogram from @carlbaker/House of Commons Library\nPlot by @VictimOfMaths")

key.full <- ggplot(keydata)+
  geom_tile(aes(x=casetert, y=vaxtert, fill=RGB))+
  scale_fill_identity()+
  scale_x_continuous(breaks=c(1,2,3), labels=c("<6%", "6%", ">6%"))+
  scale_y_continuous(breaks=c(1,2,3), labels=c("<55%", "55-57%", ">57%"))+
  labs(x = "Overall COVID incidence",
       y = "Adult vaccination rates") +
  theme_classic() +
  # make font small enough
  theme(
    axis.title = element_text(size = 9),axis.line=element_blank(), 
    axis.ticks=element_blank(), text=element_text(family="Lato"))+
  # quadratic tiles
  coord_fixed()

agg_tiff("Outputs/COVIDBivariateCasesVaxFull.tiff", units="in", width=8, height=10, res=800)
ggdraw()+
  draw_plot(plot.full, 0,0,1,1)+
  draw_plot(key.full, 0.66,0.64,0.28,0.28)
dev.off()

#Read in James Ward's estimated attack rate data
JWMSOAEst <- read.csv("Data/MSOAAttackRates.csv") %>% 
  rename(msoa11cd=MSOA.Code, attackrate=Estimated.Attack.Rate.to.8.3.21) %>% 
  mutate(attackrate=as.numeric(gsub("%", "", attackrate)))

casedata2 <- read.csv(cases) %>% 
  mutate(date=as.Date(date)) %>% 
  rename(cases=newCasesBySpecimenDateRollingSum, msoa11cd=areaCode) %>% 
  select(cases, date, msoa11cd) %>% 
  filter(date>as.Date("2021-03-10")) %>% 
  group_by(msoa11cd) %>% 
  summarise(cases=sum(cases)) %>% 
  ungroup() %>% 
  merge(JWMSOAEst, all=TRUE) %>% 
  mutate(cases=if_else(is.na(cases), 0, as.numeric(cases)))

casedata3 <- merge(vaxdata, pop2) %>% 
  mutate(vaxprop=vaccinated/pop) %>% 
  select(-c(vaccinated, pop)) %>% 
  spread(age, vaxprop) %>% 
  mutate(asrate=(`<45`*38000+`45-49`*7000+`50-54`*7000+`55-59`*6500+`60-64`*6000+`65-69`*5500+`70-74`*5000+
                   `75-79`*4000+`80+`*5000)/84000) %>% 
  merge(casedata2, all=TRUE) %>% 
  merge(pop2 %>% group_by(msoa11cd) %>% summarise(pop=sum(pop)) %>%  ungroup()) %>% 
  mutate(caserate=cases/pop,
         caseprop=caserate+attackrate,
         casetert=quantcut(caseprop, q=3, labels=FALSE),
         vaxtert=quantcut(asrate, q=3, labels=FALSE),
         key=case_when(
           casetert==1 & vaxtert==1 ~ 1,
           casetert==1 & vaxtert==2 ~ 2,
           casetert==1 & vaxtert==3 ~ 3,
           casetert==2 & vaxtert==1 ~ 4,
           casetert==2 & vaxtert==2 ~ 5,
           casetert==2 & vaxtert==3 ~ 6,
           casetert==3 & vaxtert==1 ~ 7,
           casetert==3 & vaxtert==2 ~ 8,
           TRUE ~ 9),
         fillcolour=case_when(
           key==1 ~ "#f0f0f0", key==2 ~ "#a0dcdd", key==3 ~ "#00cfc1",
           key==4 ~ "#ffa2aa", key==5 ~ "#afa7b7", key==6 ~ "#44b4cb",
           key==7 ~ "#ff3968", key==8 ~ "#c066b2", TRUE ~ "#6d87cc"))

MSOA2 <- st_read(msoa, layer="4 MSOA hex") %>% 
  left_join(casedata3, by="msoa11cd")

#Plot cumulative attack rates across the pandemic
agg_tiff("Outputs/COVIDCasesMSOACumulEst.tiff", units="in", width=8, height=10, res=800)
ggplot()+
  geom_sf(data=BackgroundMSOA, aes(geometry=geom))+
  geom_sf(data=MSOA2%>% filter(RegionNation!="Wales"), 
          aes(geometry=geom, fill=caseprop/100), colour=NA)+
  geom_sf(data=LAsMSOA %>% filter(RegionNation!="Wales"), 
          aes(geometry=geom), fill=NA, colour="Black", size=0.1)+
  geom_sf(data=GroupsMSOA %>% filter(RegionNation!="Wales"), 
          aes(geometry=geom), fill=NA, colour="Black")+
  geom_sf_text(data=Group_labelsMSOA %>% filter(RegionNation!="Wales"), 
               aes(geometry=geom, label=Group.labe,
                   hjust=just), size=rel(2.4), colour="Black", family="Roboto")+
  scale_fill_paletteer_c("pals::ocean.haline", direction=-1, name="Cumulative\ninfection rate",
                         labels=label_percent(accuracy=1))+
  theme_void()+
  theme(plot.title=element_text(face="bold", size=rel(1.6)),
        text=element_text(family="Lato"), plot.margin=margin(0,10,0,10))+
  labs(title="Cumulative COVID infection rates across the pandemic",
       subtitle="Total estimated COVID case rates since March 2020 in English Middle Super Output Areas.\nCase rate figures estimated by James Ward accounting for underreporting in official testing figures.",       
       caption="Data from James Ward (@JamesWard73), cartogram from @carlbaker/House of Commons Library\nPlot by @VictimOfMaths")
dev.off()

plot.full <- ggplot()+
  geom_sf(data=BackgroundMSOA, aes(geometry=geom))+
  geom_sf(data=MSOA2, 
          aes(geometry=geom, fill=fillcolour), colour=NA)+
  geom_sf(data=LAsMSOA %>% filter(RegionNation!="Wales"), 
          aes(geometry=geom), fill=NA, colour="Black", size=0.1)+
  geom_sf(data=GroupsMSOA %>% filter(RegionNation!="Wales"), 
          aes(geometry=geom), fill=NA, colour="Black")+
  geom_sf_text(data=Group_labelsMSOA %>% filter(RegionNation!="Wales"), 
               aes(geometry=geom, label=Group.labe,
                   hjust=just), size=rel(2.4), colour="Black", family="Lato")+
  scale_fill_identity(na.value="Black")+
  theme_void()+
  theme(plot.title=element_text(face="bold", size=rel(1.6)),
        text=element_text(family="Lato"), plot.title.position = "panel")+
  annotate("text", x=55.5, y=14, label="Fewer cases,\nfewer vaccinations", size=3,
           fontface="bold", family="Lato")+
  geom_curve(aes(x=53, y=14, xend=48.5, yend=14.2), curvature=0.15)+
  annotate("text", x=15, y=10, label="More cases,\nfewer vaccinations", size=3,
           fontface="bold", family="Lato")+
  geom_curve(aes(x=16, y=9, xend=19.9, yend=4.4), curvature=0.2)+
  annotate("text", x=52, y=33, label="Fewer cases,\nmore vaccinations", size=3,
           fontface="bold", family="Lato")+
  geom_curve(aes(x=49.5, y=33.3, xend=45.3, yend=31.8), curvature=0.2)+
  annotate("text", x=19, y=43, label="More cases,\nmore vaccinations", size=3,
           fontface="bold", family="Lato")+
  geom_curve(aes(x=19.5, y=41.5, xend=23, yend=36.6), curvature=0.1)+
  labs(title="Comparing total COVID-19 infection rates across the pandemic \nwith current vaccine coverage",
       subtitle="Estimated cumulative COVID infection rates, accounting for underrecording in official testing data\nand how this varied over time, compared against age-standardised rates of delivery of at least one vaccine dose",       
       caption="Data from James Ward (@JamesWard73), cartogram from @carlbaker/House of Commons Library\nPlot by @VictimOfMaths")

key.full <- ggplot(keydata)+
  geom_tile(aes(x=casetert, y=vaxtert, fill=RGB))+
  scale_fill_identity()+
  scale_x_continuous(breaks=c(1,2,3), labels=c("<19%", "19-26%", ">26%"))+
  scale_y_continuous(breaks=c(1,2,3), labels=c("<55%", "55-57%", ">57%"))+
  labs(x = "Overall COVID incidence",
       y = "Adult vaccination rates") +
  theme_classic() +
  # make font small enough
  theme(
    axis.title = element_text(size = 9),axis.line=element_blank(), 
    axis.ticks=element_blank(), text=element_text(family="Lato"))+
  # quadratic tiles
  coord_fixed()

agg_tiff("Outputs/COVIDBivariateCasesVaxFullEst.tiff", units="in", width=8, height=10, res=800)
ggdraw()+
  draw_plot(plot.full, 0,0,1,1)+
  draw_plot(key.full, 0.66,0.64,0.28,0.28)
dev.off()
