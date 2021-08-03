rm(list=ls())

library(tidyverse)
library(curl)
library(readxl)
library(lubridate)
library(extrafont)
library(paletteer)
library(RcppRoll)
library(sf)
library(snakecase)
library(ggrepel)
library(ragg)

theme_custom <- function() {
  theme_classic() %+replace%
    theme(plot.title.position="plot", plot.caption.position="plot",
          strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)),
          plot.title=element_text(face="bold", size=rel(1.5), hjust=0,
                                  margin=margin(0,0,5.5,0)),
          text=element_text(family="Lato"))
}

#Read in admissions data
#https://www.england.nhs.uk/statistics/statistical-work-areas/covid-19-hospital-activity/
admurl <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/07/Weekly-covid-admissions-and-beds-publication-210729.xlsx"

#Increment by 7 when each new report is published
admrange <- "DJ"
#Set latest date of admissions data
admdate <- as.Date("2021-07-25")

#Read in admissions
#First data up to 6th April
admurl.old <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/04/Weekly-covid-admissions-and-beds-publication-210429-up-to-210406.xlsx"

temp1 <- tempfile()
temp1 <- curl_download(url=admurl.old, destfile=temp1, quiet=FALSE, mode="wb")
raw.adm.old <- read_excel(temp1, sheet="Hosp ads & diag", range=paste0("B25:IS512"), 
                          col_names=FALSE)

#Read in more recent data
temp2 <- tempfile()
temp2 <- curl_download(url=admurl, destfile=temp2, quiet=FALSE, mode="wb")
raw.adm <- read_excel(temp2, sheet="Hosp ads & diag", range=paste0("B25:",admrange,"304"), 
                      col_names=FALSE)

#Tidy up data
admissions.old <- raw.adm.old %>% 
  gather(date, admissions, c(4:ncol(raw.adm.old))) %>% 
  mutate(date=as.Date("2020-08-01")+days(as.integer(substr(date, 4, 6))-4)) %>% 
  rename(Region=...1, code=...2, name=...3)

admissions <- raw.adm %>% 
  gather(date, admissions, c(4:ncol(raw.adm))) %>% 
  mutate(date=as.Date("2021-04-07")+days(as.integer(substr(date, 4, 6))-4)) %>% 
  rename(Region=...1, code=...2, name=...3) %>% 
  bind_rows(admissions.old)

#Bring in PHE data summarising admissions in HES to each trust by MSOA
MSOA.adm <- read.csv("COVID_LA_Plots/Trust to MSOA HES data.csv")

#1st lookup for admissions up to 4th October, when RD3 and RDZ merged to form R0D in the admissions (but not deaths) data
MSOA.adm1 <- MSOA.adm %>% 
  mutate(TrustCode=case_when(
    TrustCode %in% c("RE9", "RLN") ~ "R0B",
    TrustCode=="R1J" ~ "RTQ",
    TrustCode=="RQ6" ~ "REM",
    TrustCode=="RNL" ~ "RNN",
    TrustCode %in% c("RQ8", "RDD") ~ "RAJ",
    TrustCode=="RA3" ~ "RA7",
    TrustCode=="RC1" ~ "RC9",
    TrustCode=="RBA" ~ "RH5",
    TRUE ~ as.character(TrustCode))) %>% 
  group_by(CatchmentYear, msoa, TrustCode) %>% 
  summarise(msoa_total_catchment1=sum(msoa_total_catchment)) %>% 
  ungroup() 

#2nd lookup for after 4th October
MSOA.adm2 <- MSOA.adm %>% 
  mutate(TrustCode=case_when(
    TrustCode %in% c("RE9", "RLN") ~ "R0B",
    TrustCode=="R1J" ~ "RTQ",
    TrustCode=="RQ6" ~ "REM",
    TrustCode=="RNL" ~ "RNN",
    TrustCode %in% c("RQ8", "RDD") ~ "RAJ",
    TrustCode=="RA3" ~ "RA7",
    TrustCode=="RC1" ~ "RC9",
    TrustCode=="RBA" ~ "RH5",
    TrustCode %in% c("RDZ", "RD3") ~ "R0D",
    TRUE ~ as.character(TrustCode))) %>% 
  group_by(CatchmentYear, msoa, TrustCode) %>% 
  summarise(msoa_total_catchment2=sum(msoa_total_catchment)) %>% 
  ungroup()

temp1 <- data.frame(TrustCode=unique(MSOA.adm1$TrustCode))
temp2 <- data.frame(TrustCode=unique(MSOA.adm2$TrustCode))
temp <- bind_rows(temp1, temp2) %>%
  unique()

MSOA.adm <- merge(temp, MSOA.adm1, by="TrustCode", all=TRUE) %>% 
  merge(., MSOA.adm2, all=TRUE) 

#Bring in MSOA to LTLA lookup
temp <- tempfile()
source <- "http://geoportal1-ons.opendata.arcgis.com/datasets/0b3c76d1eb5e4ffd98a3679ab8dea605_0.csv"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
MSOA.lookup <- read.csv(temp) %>% 
  select(MSOA11CD, LAD19CD, LAD19NM) %>% 
  unique() %>% 
  rename(msoa=MSOA11CD)

#Merge into PHE lookup
MSOA.adm <- merge(MSOA.adm, MSOA.lookup, by="msoa", all.x=TRUE)

#Convert to the lookup we want (trust to LTLA), averaging across last 3 years in data (2016-18)
trust.lookup <- MSOA.adm %>% 
  filter(CatchmentYear>=2016) %>% 
  group_by(TrustCode, LAD19CD, LAD19NM) %>% 
  summarise(catchment1=sum(msoa_total_catchment1, na.rm=TRUE),
            catchment2=sum(msoa_total_catchment2, na.rm=TRUE)) %>% 
  ungroup() %>% 
  group_by(TrustCode) %>% 
  mutate(pop1=sum(catchment1, na.rm=TRUE), popprop1=catchment1/pop1,
         pop2=sum(catchment2, na.rm=TRUE), popprop2=catchment2/pop2) %>% 
  ungroup() %>% 
  rename(code=TrustCode)

#Bring lookup into admissions and deaths data
admissions <- merge(admissions, trust.lookup, by.x="code", by.y="code", all=TRUE)

#Allocate admissions and deaths to LTLA based on population proportions and 
#aggregate up to LTLA level
LAadmissions <- admissions %>% 
  mutate(LA.admissions=case_when(
           date<=as.Date("2020-10-04") ~ admissions*popprop1,
           TRUE ~ admissions*popprop2)) %>% 
  group_by(LAD19CD, date, LAD19NM) %>% 
  summarise(admissions=sum(LA.admissions, na.rm=TRUE)) %>% 
  ungroup() %>% 
  filter(!is.na(LAD19CD)) %>% 
  #merge City of London into Hackney and Isles of Scilly into Cornwall
  mutate(LAD19CD=case_when(LAD19CD=="E09000001" ~ "E09000012",
                           LAD19CD=="E06000053" ~ "E06000052",
                           TRUE ~ as.character(LAD19CD))) %>%
  group_by(LAD19CD, date, LAD19NM) %>% 
  summarise(admissions=sum(admissions)) %>% 
  ungroup()

#Bring in regions
temp <- tempfile()
source <- "https://opendata.arcgis.com/datasets/0c3a9643cc7c4015bb80751aad1d2594_0.csv"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
LADtoRegion <- read.csv(temp)[,c(1,4)]
colnames(LADtoRegion) <- c("LTLA", "Region")

#Bring in LA populations
temp <- tempfile()
source <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fpopulationandmigration%2fpopulationestimates%2fdatasets%2fpopulationestimatesforukenglandandwalesscotlandandnorthernireland%2fmid20182019laboundaries/ukmidyearestimates20182019ladcodes.xls"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
LApop <- read_excel(temp, sheet="MYE2-All", range="A5:D435", col_names=TRUE)
colnames(LApop) <- c("code", "name", "geography", "pop")

#Merge isles of Scilly in with Cornwall
LApop$code <- if_else(LApop$code=="E06000053", "E06000052", LApop$code)
LApop$name <- if_else(LApop$name=="Isles of Scilly", "Cornwall", LApop$name)

#Merge City of London & Hackney
LApop$code <- if_else(LApop$code=="E09000001", "E09000012", LApop$code)
LApop$name <- if_else(LApop$name=="City of London", "Hackney and City of London", LApop$name)
LApop$name <- if_else(LApop$name=="Hackney", "Hackney and City of London", LApop$name)

LApop <- LApop %>% 
  group_by(name, code) %>% 
  summarise(pop=sum(pop)) %>% 
  ungroup()

LAadmissions <- LAadmissions %>% 
  merge(LADtoRegion, all.x=TRUE, by.x="LAD19CD", by.y="LTLA") %>% 
  mutate(Region=case_when(
    LAD19CD %in% c("E07000244", "E07000245") ~ "East of England",
    LAD19CD %in% c("E06000058", "E06000059", "E07000246") ~ "South West",
    LAD19CD %in% c("E07000004", "E07000005", "E07000006", "E07000007") ~ "South East",
    TRUE ~ Region)) %>% 
  rename(Lacode=LAD19CD) %>% 
  merge(LApop, by.x="Lacode", by.y="code", all.x=TRUE) %>% 
  group_by(Lacode, LAD19NM) %>% 
  arrange(date) %>% 
  mutate(admrate=admissions*100000/pop,
         adm_roll=roll_mean(admrate, 7, align="center", fill=NA, na.rm=TRUE), 
         adm_change=adm_roll-lag(adm_roll, 7)) %>% 
  ungroup()

adm_max=max(LAadmissions$date[!is.na(LAadmissions$adm_roll)])

plotdata <-  LAadmissions %>% 
  filter(date==adm_max)

#Download Carl Baker's lovely map
ltla <- tempfile()
source <- ("https://github.com/houseofcommonslibrary/uk-hex-cartograms-noncontiguous/raw/main/geopackages/LocalAuthorities-lowertier.gpkg")
ltla <- curl_download(url=source, destfile=ltla, quiet=FALSE, mode="wb")

Background <- st_read(ltla, layer="7 Background")

ltlaadm <- st_read(ltla, layer="4 LTLA-2019") %>% 
  left_join(plotdata, by="Lacode")

Groups <- st_read(ltla, layer="2 Groups")

Group_labels <- st_read(ltla, layer="1 Group labels") %>% 
  mutate(just=if_else(LabelPosit=="Left", 0, 1))

plot1 <- ggplot()+
  geom_sf(data=Background %>% filter(Name=="England & Wales"), 
          aes(geometry=geom), fill="White")+
  geom_sf(data=ltlaadm %>% filter(!RegionNation %in% c("Scotland", "Wales", "Northern Ireland")), 
          aes(geometry=geom, fill=adm_roll), colour="Black", size=0.1)+
  geom_sf(data=Groups %>% filter(!RegionNation %in% c("Scotland", "Wales", "Northern Ireland")), 
          aes(geometry=geom), fill=NA, colour="Black")+
  geom_sf_text(data=Group_labels %>% filter(!RegionNation %in% c("Scotland", "Wales", 
                                                                 "Northern Ireland")), 
                                            aes(geometry=geom, label=Group.labe,
                                      hjust=just), size=rel(2.4), colour="Black")+
  scale_fill_paletteer_c("pals::ocean.haline", direction=-1, limits=c(0,NA),
                         name="Admissions per day\nper 100,000")+
  theme_void()+
  theme(plot.title=element_text(face="bold", size=rel(1.4)),
        text=element_text(family="Lato"), plot.title.position="plot",
        plot.caption.position="plot", legend.position="top")+
  guides(fill = guide_colorbar(title.position = 'top', title.hjust = .5,
                               barwidth = unit(20, 'lines'), barheight = unit(.5, 'lines')))+
  labs(title="COVID admission rates are still highest in the North East",
       subtitle=paste0("Rolling 7-day average number of daily new hospital admissions at Lower Tier Local Authority level\nData up to ", adm_max),
       caption="Data from NHS England & ONS, Cartogram from @carlbaker/House of Commons Library\nPlot by @VictimOfMaths")

agg_tiff("Outputs/COVIDAdmissionsLTLACartogram.tiff", units="in", width=8, height=9, res=800)
plot1
dev.off()

plotdata2 <-LAadmissions %>% 
  filter(date>=adm_max-days(7) & date<=adm_max)

plot2 <- ggplot()+
  geom_hline(yintercept=0)+
  geom_vline(xintercept=0)+
  geom_path(data=plotdata2,
            aes(x=adm_roll, y=adm_change, group=LAD19NM, alpha=7-as.integer(adm_max-date)),
            colour="Grey50", show.legend=FALSE)+
  geom_point(data=plotdata2 %>% filter(date==adm_max),
             aes(x=adm_roll, y=adm_change, fill=Region, size=pop), shape=21, alpha=0.7)+
  geom_text_repel(data=plotdata2 %>% filter(date==adm_max), 
                  aes(x=adm_roll, y=adm_change, label=LAD19NM), size=rel(2.3))+
  scale_x_continuous(name="Average new admissions per day in the past week\n(rate per 100,000)", limits=c(0,NA))+
  scale_y_continuous(name="Change in admission rate compared to the preceding week")+
  scale_fill_paletteer_d("LaCroixColoR::paired", name="")+
  scale_size(guide=FALSE)+
  theme_custom()+
  labs(title="Rises in COVID hospital admissions have slowed, but they are still rising in most of England",
       subtitle=paste0("Hospital admission rates and how these have changed in the past week in English Local Authorities.\nBubbles are sized by population. Trails represent each area's movement across the plot in the past week.\nData up to ",
                       adm_max),
       caption="Data from NHS England & ONS\nPlot by @VictimOfMaths")

agg_tiff("Outputs/COVIDAdmissionsLTLAChangeScatterPaths.tiff", units="in", width=9, height=7, res=800)
plot2
dev.off()

#Trust-level analysis
#Download PHE's trust  catchment data from and save as a csv.
#https://app.box.com/s/qh8gzpzeo1firv1ezfxx2e6c4tgtrudl
catchments <- read_csv("COVID_LA_Plots/2020 Trust Catchment Populations Worksheet.csv") %>% 
  filter(CatchmentYear==2018) %>% 
  group_by(TrustCode) %>% 
  summarise(catchpop=sum(Catchment)) %>% 
  ungroup()

#Sort out trust mergers/recodes
#1st lookup for admissions up to 4th October, when RD3 and RDZ merged to form R0D in the admissions (but not deaths) data
catchments1 <- catchments %>% 
  mutate(TrustCode=case_when(
    TrustCode %in% c("RE9", "RLN") ~ "R0B",
    TrustCode=="R1J" ~ "RTQ",
    TrustCode=="RQ6" ~ "REM",
    TrustCode=="RNL" ~ "RNN",
    TrustCode %in% c("RQ8", "RDD") ~ "RAJ",
    TrustCode=="RA3" ~ "RA7",
    TrustCode=="RC1" ~ "RC9",
    TrustCode=="RBA" ~ "RH5",
    TRUE ~ as.character(TrustCode))) %>% 
  group_by(TrustCode) %>% 
  summarise(catchpop1=sum(catchpop)) %>% 
  ungroup() 

#2nd lookup for after 4th October
catchments2 <- catchments %>% 
  mutate(TrustCode=case_when(
    TrustCode %in% c("RE9", "RLN") ~ "R0B",
    TrustCode=="R1J" ~ "RTQ",
    TrustCode=="RQ6" ~ "REM",
    TrustCode=="RNL" ~ "RNN",
    TrustCode %in% c("RQ8", "RDD") ~ "RAJ",
    TrustCode=="RA3" ~ "RA7",
    TrustCode=="RC1" ~ "RC9",
    TrustCode=="RBA" ~ "RH5",
    TrustCode %in% c("RDZ", "RD3") ~ "R0D",
    TRUE ~ as.character(TrustCode))) %>% 
  group_by(TrustCode) %>% 
  summarise(catchpop2=sum(catchpop)) %>% 
  ungroup()

#Merge into admissions data
trustadm <- admissions %>% 
  select(code, Region, name, admissions, date) %>%
  distinct() %>% 
  merge(catchments1, all.x=TRUE, by.x="code", by.y="TrustCode") %>% 
  merge(catchments2, all.x=TRUE, by.x="code", by.y="TrustCode") %>% 
  rename(trust=name) %>% 
  group_by(trust) %>% 
  arrange(date) %>% 
  mutate(catchpop=if_else(date<=as.Date("2020-10-04"), catchpop1, catchpop2),
         admrate=admissions*100000/catchpop,
         adm_roll=roll_mean(admrate, 7, align="center", fill=NA, na.rm=TRUE), 
         adm_change=adm_roll-lag(adm_roll, 7),
         trust=str_replace(trust, " NHS TRUST", ""),
         trust=str_replace(trust, "NHS FOUNDATION TRUST", ""),
         trust=to_any_case(trust, case="title"),
         trust=str_replace(trust, "King s", "King's"),
         trust=str_replace(trust, "Guy s", "Guy's"),
         trust=str_replace(trust, "George s", "George's"),
         trust=str_replace(trust, "Women s", "Women's"),
         trust=str_replace(trust, "Children s", "Children's"),
         trust=str_replace(trust, "Peter s", "Peter's")) %>% 
  ungroup()

plotdata3 <-trustadm %>% 
  filter(date>=adm_max-days(7) & date<=adm_max & code!= "REN")

plot3 <- ggplot()+
  geom_hline(yintercept=0)+
  geom_vline(xintercept=0)+
  geom_path(data=plotdata3,
            aes(x=adm_roll, y=adm_change, group=trust, alpha=7-as.integer(adm_max-date)),
            colour="Grey50", show.legend=FALSE)+
  geom_point(data=plotdata3 %>% filter(date==adm_max),
             aes(x=adm_roll, y=adm_change, size=catchpop, fill=Region), shape=21, alpha=0.7)+
  geom_text_repel(data=plotdata3 %>% filter(date==adm_max), 
                  aes(x=adm_roll, y=adm_change, label=trust), size=rel(2.3))+
  scale_x_continuous(name="Average new admissions per day in the past week\n(rate per 100,000)", limits=c(0,NA))+
  scale_y_continuous(name="Change in admission rate compared to the preceding week")+
  scale_fill_paletteer_d("colorblindr::OkabeIto", name="")+
  scale_size(guide=FALSE)+
  theme_custom()+
  theme(axis.line=element_blank())+
  labs(title="COVID admissions are still rising, albeit more slowly, across much of the country",
       subtitle=paste0("Hospital admission rates and how these have changed in the past week in English hospital trusts.\nBubbles are sized by population. Trails represent each trust's movement across the plot in the past week.\nData up to ",
                       adm_max),
       caption="Data from NHS England, PHE & ONS\nPlot by @VictimOfMaths")

agg_tiff("Outputs/COVIDAdmissionsTrustChangeScatterPaths.tiff", units="in", width=9, height=7, res=800)
plot3
dev.off()
