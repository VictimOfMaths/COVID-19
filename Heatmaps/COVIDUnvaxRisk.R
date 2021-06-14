rm(list=ls())

library(tidyverse)
library(curl)
library(readxl)
library(sf)
library(arrow)
library(extrafont)
library(paletteer)
library(gtools)
library(cowplot)

#Read in MSOA level vaccination rates
#https://www.england.nhs.uk/statistics/statistical-work-areas/covid-19-vaccinations/
maxdate <- "6th June"

vax <- tempfile()
url <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/06/COVID-19-weekly-announced-vaccinations-10-June-2021.xlsx"
vax <- curl_download(url=url, destfile=vax, quiet=FALSE, mode="wb")

vaxdata <- read_excel(vax, sheet="MSOA", range="F16:AF6806", col_names=FALSE) %>% 
  set_names("msoa11cd", "msoa11nm", "<30_1st", "30-34_1st", "35-39_1st", "40-44_1st", 
            "45-49_1st", "50-54_1st", "55-59_1st", "60-64_1st", "65-69_1st", "70-74_1st", 
            "75-79_1st", "80+_1st", "blank", "<30_2nd", "30-34_2nd", "35-39_2nd", "40-44_2nd", 
            "45-49_2nd", "50-54_2nd", "55-59_2nd", "60-64_2nd", "65-69_2nd", "70-74_2nd", 
            "75-79_2nd", "80+_2nd") %>% 
  select(-blank) %>% 
  pivot_longer(c(3:26), names_to=c("age", "dose"), names_sep="_", values_to="vaccinated")

pop2 <- read_excel(vax, sheet="Population estimates (NIMS)", range="U16:AI6806", col_names=FALSE) %>% 
  select(-c(2,3)) %>% 
  rename(msoa11cd=`...1`) %>% 
  gather(age, pop, c(2:13)) %>% 
  mutate(age=case_when(
    age=="...4" ~ "<30", 
    age=="...5" ~ "30-34",
    age=="...6" ~ "35-39",
    age=="...7" ~ "40-44",
    age=="...8" ~ "45-49",
    age=="...9" ~ "50-54",
    age=="...10" ~ "55-59",
    age=="...11" ~ "60-64",
    age=="...12" ~ "65-69",
    age=="...13" ~ "70-74",
    age=="...14" ~ "75-79",
    TRUE ~ "80+")) %>% 
  group_by(msoa11cd, age) %>% 
  summarise(pop=sum(pop)) %>% 
  ungroup()

#COMBINE
vaxdata <- merge(vaxdata, pop2) %>% 
  mutate(vaxprop=vaccinated/pop)

#Bring in CFRs estimated by Dan Howdon
CFRdata <- read_csv_arrow("Data/cfrs_2021_06_07.csv") %>% 
  mutate(age=case_when(
    agegroup==0 ~ "0-4", agegroup==5 ~ "5-9", agegroup==10 ~ "10-14", agegroup==15 ~ "15-19",
    agegroup==20 ~ "20-24", agegroup==25 ~ "25-29", agegroup==30 ~ "30-34", 
    agegroup==35 ~ "35-39", agegroup==40 ~ "40-44", agegroup==45 ~ "45-49", 
    agegroup==50 ~ "50-54", agegroup==55 ~ "55-59", agegroup==60 ~ "60-64", 
    agegroup==65 ~ "65-69", agegroup==70 ~ "70-74", agegroup==75 ~ "75-79",
    agegroup==80 ~ "80-84", agegroup==85 ~ "85-89", TRUE ~ "90+"),
    date=as.Date(date, format="%d/%m/%Y"),
    age = age %>% str_replace("_", "-") %>%
      factor(levels=c("0-4", "5-9", "10-14", "15-19",
                      "20-24", "25-29", "30-34", "35-39", 
                      "40-44", "45-49", "50-54", "55-59", 
                      "60-64", "65-69", "70-74", "75-79", 
                      "80-84", "85-89", "90+"))) 

maxCFRdate <- max(CFRdata$date[!is.na(CFRdata$cfr_month)])

#Just extract the most recent monthly CFRs
CFRs <- CFRdata %>% 
  filter(date==maxCFRdate) %>% 
  mutate(CFR=cfr_month*100) %>% 
  select(age, CFR)

#Here are the latest age-specific CFRs:
#CFRs <- tibble::tribble(
#  ~age, ~CFR,
#  "0-4",   0,
#  "5-9",   0,
#  "10-14", 0,
#  "15-19", 0.02164,
#  "20-24", 0,
#  "25-29", 0,
#  "30-34", 0,
#  "35-39", 0.0242269,
#  "40-44", 0.0289471,
#  "45-49", 0.2123436,
#  "50-54", 0.2208057,
#  "55-59", 0.5648288,
#  "60-64", 0.4692304,
#  "65-69", 3.5336073,
#  "70-74", 4.1047055,
#  "75-79", 6.2152125,
#  "80-84", 11.6088927,
#  "85-89", 16.7702883,
#  "90+",   22.6581156)

#Condense to age groups to match the vaxdata. Pop group weights from ONS mid-year estimates
CFRs <- CFRs %>% 
  mutate(weight=case_when(
    age=="0-4" ~ 3299637,
    age=="5-9"~ 3538206,
    age=="10-14" ~ 3354246,
    age=="15-19" ~ 3090232,
    age=="20-24" ~ 3487863,
    age=="25-29" ~ 3801409,
    age=="80-84" ~ 1439913,
    age=="85-89" ~ 879778,
    age=="90+" ~ 517273,
    TRUE ~ 1),
    wgt_cfr=weight*CFR,
    age=case_when(
      as.character(age) %in% c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29") ~ "<30",
      as.character(age) %in% c("80-84", "85-89", "90+") ~ "80+",
      TRUE ~ as.character(age))) %>% 
  group_by(age) %>% 
  summarise(CFR=sum(wgt_cfr)/sum(weight)) %>% 
  ungroup()

data <- merge(vaxdata, CFRs) %>% 
  mutate(ex_deaths=(pop-vaccinated)*CFR/100) %>% 
  group_by(msoa11nm, msoa11cd, dose) %>% 
  summarise(ex_deaths=sum(ex_deaths), unvax=sum(pop-vaccinated), pop=sum(pop)) %>% 
  ungroup() %>% 
  mutate(ex_deathrate=ex_deaths*100000/pop)

#Download Carl Baker's lovely cartogram
msoa <- tempfile()
source <- ("https://github.com/houseofcommonslibrary/uk-hex-cartograms-noncontiguous/raw/main/geopackages/MSOA.gpkg")
msoa <- curl_download(url=source, destfile=msoa, quiet=FALSE, mode="wb")

BackgroundMSOA <- st_read(msoa, layer="5 Background")

MSOA <- st_read(msoa, layer="4 MSOA hex") %>% 
  left_join(data, by="msoa11cd")

GroupsMSOA <- st_read(msoa, layer="2 Groups")

Group_labelsMSOA <- st_read(msoa, layer="1 Group labels") %>% 
  mutate(just=if_else(LabelPosit=="Left", 0, 1))

LAsMSOA <- st_read(msoa, layer="3 Local authority outlines (2019)")

Unvaxrisk <- ggplot()+
  geom_sf(data=BackgroundMSOA, aes(geometry=geom), fill="White")+
  geom_sf(data=MSOA %>% filter(dose=="1st"), 
          aes(geometry=geom, fill=ex_deathrate), colour=NA)+
  geom_sf(data=LAsMSOA %>% filter(RegionNation!="Wales"), 
          aes(geometry=geom), fill=NA, colour="White", size=0.1)+
  geom_sf(data=GroupsMSOA %>% filter(RegionNation!="Wales"), 
          aes(geometry=geom), fill=NA, colour="Black")+
  geom_sf_text(data=Group_labelsMSOA %>% filter(RegionNation!="Wales"), 
               aes(geometry=geom, label=Group.labe,
                   hjust=just), size=rel(2.4), colour="Black")+
  scale_fill_paletteer_c("pals::ocean.amp", 
                         name="Expected deaths per 100,000 adults",
                         limits=c(0,500))+
  theme_void()+
  theme(plot.title=element_text(face="bold", size=rel(1.6)),
        text=element_text(family="Lato"), legend.position="top",
        plot.title.position="plot", plot.caption.position="plot")+
  guides(fill = guide_colorbar(title.position = 'top', title.hjust = .5,
                               barwidth = unit(20, 'lines'), barheight = unit(.5, 'lines')))+
  labs(title="London has the highest risk through non-vaccination",
       subtitle=paste0("Expected maximum rate of COVID mortality if every unvaccinated person contracted COVID.\nUsing recent Case Fatality Rates and ignoring immunity acquired through prior infection,\nand assuming that a single dose of vaccine is 100% effective.\nData up to ", maxdate, "\n "),       
       caption="Data from NHS England, CFRs from Dan Howdon, Cartogram from @carlbaker/House of Commons Library\nPlot by @VictimOfMaths")

agg_tiff("Outputs/COVIDVaxMSOACartogramUnVaxRisk.tiff", units="in", width=8, height=9, res=800)
Unvaxrisk
dev.off()

#Incorporate estimates of vaccine effectiveness from James Ward (@JamesWard73)
#https://twitter.com/JamesWard73/status/1403496072606064642
VE1st=0.8
VE2nd=0.96

#Protection of 1st dose vs. death
data1 <-  merge(vaxdata, CFRs) %>% 
  pivot_wider(id_cols=c("age", "msoa11cd", "msoa11nm", "pop", "CFR"), 
              names_from=dose,
              values_from=vaccinated) %>% 
  mutate(ex_deaths=#unvaccinated risk
           (pop-`1st`)*CFR/100+
           #1st dose onlyrisk
           (`1st`-`2nd`)*(1-VE1st)*CFR/100+
           #2nd dose risk
           `2nd`*(1-VE2nd)*CFR/100) %>% 
  group_by(msoa11nm, msoa11cd) %>% 
  summarise(ex_deaths=sum(ex_deaths),pop=sum(pop)) %>% 
  ungroup() %>% 
  mutate(ex_deathrate=ex_deaths*100000/pop) %>% 
  #Remove MSOAs close to the Welsh border with questionable vaccination rates
  filter(!msoa11nm %in% c("Wigmore, Orleton & Brimfield",
                          "Clun & Bucknell",
                          "Gobowen, St Martin's & Weston Rhyn",
                          "Tidenham & Woolaston",
                          "Lydbrook, Newland & St Briavels",
                          "Penyard, Llangarron & Goodrich",
                          "Golden Valley",
                          "Ellesmere",
                          "Trefonen & Pant",
                          "Kington, Eardisley & Staunton",
                          "Bishop's Castle, Brockton & Chirbury"))

MSOA2 <- st_read(msoa, layer="4 MSOA hex") %>% 
  left_join(data1, by="msoa11cd")

Unvaxrisk2 <- ggplot()+
  geom_sf(data=BackgroundMSOA, aes(geometry=geom), fill="White")+
  geom_sf(data=MSOA2 %>% filter(RegionNation!="Wales"), 
          aes(geometry=geom, fill=ex_deathrate), colour=NA)+
  geom_sf(data=LAsMSOA %>% filter(RegionNation!="Wales"), 
          aes(geometry=geom), fill=NA, colour="White", size=0.1)+
  geom_sf(data=GroupsMSOA %>% filter(RegionNation!="Wales"), 
          aes(geometry=geom), fill=NA, colour="Black")+
  geom_sf_text(data=Group_labelsMSOA %>% filter(RegionNation!="Wales"), 
               aes(geometry=geom, label=Group.labe,
                   hjust=just), size=rel(2.4), colour="Black")+
  scale_fill_paletteer_c("pals::ocean.amp", 
                         name="Expected deaths per 100,000 adults")+
  theme_void()+
  theme(plot.title=element_text(face="bold", size=rel(1.6)),
        text=element_text(family="Lato"), legend.position="top",
        plot.title.position="plot", plot.caption.position="plot")+
  guides(fill = guide_colorbar(title.position = 'top', title.hjust = .5,
                               barwidth = unit(20, 'lines'), barheight = unit(.5, 'lines')))+
  labs(title="London has the highest risk through non-vaccination",
       subtitle=paste0("Expected maximum rate of COVID mortality if every unvaccinated person contracted COVID.\nUsing recent age-specific Case Fatality Rates estimated by Dan Howdon and vaccine\neffectiveness estimates calculated by James Ward. This map ignore immunity acquired\nthrough prior infection. A small number of areas on the Welsh border have been censored\ndue to questionable vaccination data\nData up to ", maxdate, "\n "),       
       caption="Data from NHS England, CFRs from Dan Howdon and vaccine effectiveness estimates from James Ward\nCartogram from @carlbaker/House of Commons Library\nPlot by @VictimOfMaths")

agg_tiff("Outputs/COVIDVaxMSOACartogramUnVaxRisk2.tiff", units="in", width=7.5, height=9, res=800)
Unvaxrisk2
dev.off()

#Bivariate map of risk vs. case rates
#Read in MSOA-level case data from the dashboard
caseurl <- "https://api.coronavirus.data.gov.uk/v2/data?areaType=msoa&metric=newCasesBySpecimenDateRollingRate&format=csv"

cases <- tempfile()
cases <- curl_download(url=caseurl, destfile=cases, quiet=FALSE, mode="wb")
casedata <- read.csv(cases) %>% 
  mutate(date=as.Date(date)) %>% 
  filter(date==max(date)) %>% 
  rename(caserate=newCasesBySpecimenDateRollingRate, msoa11cd=areaCode) %>% 
  select(caserate, msoa11cd) %>% 
  merge(data1, by="msoa11cd", all.y=TRUE) 

temp <- casedata %>% 
  filter(is.na(caserate)) %>% 
  mutate(casetert=1)

casedata2 <- casedata %>% 
  filter(!is.na(caserate)) %>% 
  mutate(casetert=quantcut(caserate, q=2, labels=FALSE)+1) %>% 
  bind_rows(temp) %>% 
  mutate(risktert=quantcut(ex_deathrate, q=3, labels=FALSE),
         key=case_when(
           casetert==1 & risktert==3 ~ 1,
           casetert==1 & risktert==2 ~ 2,
           casetert==1 & risktert==1 ~ 3,
           casetert==2 & risktert==3 ~ 4,
           casetert==2 & risktert==2 ~ 5,
           casetert==2 & risktert==1 ~ 6,
           casetert==3 & risktert==3 ~ 7,
           casetert==3 & risktert==2 ~ 8,
           TRUE ~ 9),
         fillcolour=case_when(
           key==1 ~ "#f0f0f0", key==2 ~ "#a0dcdd", key==3 ~ "#00cfc1",
           key==4 ~ "#ffa2aa", key==5 ~ "#afa7b7", key==6 ~ "#44b4cb", 
           key==7 ~ "#ff3968", key==8 ~ "#c066b2", TRUE ~ "#6d87cc"))

MSOA3 <- st_read(msoa, layer="4 MSOA hex") %>% 
  left_join(casedata2, by="msoa11cd")

#generate dataframe for key
keydata <- MSOA3 %>%
  filter(!is.na(fillcolour)) %>%
  group_by(casetert, risktert) %>%
  summarise(RGB=unique(fillcolour))

plot.full <- ggplot()+
  geom_sf(data=BackgroundMSOA, aes(geometry=geom))+
  geom_sf(data=MSOA3 %>% filter(RegionNation!="Wales"), 
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
  labs(title="Comparing COVID-19 infection rates with overall mortality risks",
       subtitle="Expected maximum rate of COVID mortality if every unvaccinated person contracted COVID,\ncompared with current case rates. Case rates are censored for areas with fewer than 3 cases,\nwhich currently covers the majority of areas. As a result there are considerably more areas in\nthe lowest category of case rates.",       
       caption="Data from NHS England & coronavirus.data.gov.uk,\nCFRs from Dan Howdon and vaccine effectiveness estimates from James Ward\nCartogram from @carlbaker/House of Commons Library\nPlot by @VictimOfMaths")

key <- ggplot(keydata)+
  geom_tile(aes(x=casetert, y=-risktert, fill=RGB))+
  scale_fill_identity()+
  labs(x = expression("More COVID cases" %->%  ""),
       y = expression("Lower mortality risk" %->%  "")) +
  theme_classic() +
  # make font small enough
  theme(
    axis.title = element_text(size = 9),axis.line=element_blank(), 
    axis.ticks=element_blank(), axis.text=element_blank(),
    text=element_text(family="Lato"))+
  # quadratic tiles
  coord_fixed()

agg_tiff("Outputs/COVIDBivariateCasesRisk.tiff", units="in", width=8, height=10, res=800)
ggdraw()+
  draw_plot(plot.full, 0,0,1,1)+
  draw_plot(key, 0.66,0.64,0.26,0.26)
dev.off()
