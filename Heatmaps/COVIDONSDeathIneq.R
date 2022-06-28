rm(list=ls())

library(tidyverse)
library(curl)
library(lubridate)
library(RcppRoll)
library(readxl)
library(paletteer)
library(extrafont)
library(ragg)
library(scales)

options(scipen=99999)

theme_custom <- function() {
  theme_classic() %+replace%
    theme(plot.title.position="plot", plot.caption.position="plot",
          strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)),
          plot.title=element_text(face="bold", size=rel(1.5), hjust=0,
                                  margin=margin(0,0,5.5,0)),
          text=element_text(family="Lato"),
          plot.subtitle=element_text(colour="Grey40", hjust=0, vjust=1),
          plot.caption=element_text(colour="Grey40", hjust=1, vjust=1, size=rel(0.8)),
          axis.text=element_text(colour="Grey40"),
          axis.title=element_text(colour="Grey20"),
          legend.text=element_text(colour="Grey40"),
          legend.title=element_text(colour="Grey20"))
}

#Read in 2022 deaths by LA from ONS
temp <- tempfile()
source22 <- ("https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/healthandsocialcare/causesofdeath/datasets/deathregistrationsandoccurrencesbylocalauthorityandhealthboard/2022/lahbtables2022week24.xlsx")
temp <- curl_download(url=source22, destfile=temp, quiet=FALSE, mode="wb")

deaths22 <- read_excel(temp, sheet="Registrations - All data", range="A4:G97348") %>% 
  set_names("LAD17CD", "Geography", "LAName", "Cause", "Week", "Location", "Deaths") %>% 
  mutate(Date=as.Date("2022-01-07")+weeks(Week-1))

#2021 deaths
source21 <- ("https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fhealthandsocialcare%2fcausesofdeath%2fdatasets%2fdeathregistrationsandoccurrencesbylocalauthorityandhealthboard%2f2021/lahbtables20211.xlsx")
temp <- curl_download(url=source21, destfile=temp, quiet=FALSE, mode="wb")

deaths21 <- read_excel(temp, sheet="Registrations - All data")[-c(1:4),] %>% 
  set_names("LAD17CD", "Geography", "LAName", "Cause", "Week", "Location", "Deaths") %>% 
  mutate(Week=as.numeric(Week), Deaths=as.numeric(Deaths),
         Date=as.Date("2021-01-08")+weeks(Week-1))

#2020 deaths
source20 <- ("https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fhealthandsocialcare%2fcausesofdeath%2fdatasets%2fdeathregistrationsandoccurrencesbylocalauthorityandhealthboard%2f2020/lahbtablesweek01to532020datawk232021.xlsx")
temp <- curl_download(url=source20, destfile=temp, quiet=FALSE, mode="wb")

deaths20 <- read_excel(temp, sheet="Registrations - All data")[-c(1:4),] %>% 
  set_names("LAD17CD", "Geography", "LAName", "Cause", "Week", "Location", "Deaths") %>% 
  mutate(Week=as.numeric(Week), Deaths=as.numeric(Deaths),
         Date=as.Date("2020-01-03")+weeks(Week-1))

#Stick together and tidy up
deaths <- bind_rows(deaths20, deaths21, deaths22) %>% 
  group_by(LAD17CD, LAName, Cause, Date) %>% 
  spread(Location, Deaths) %>% 
  mutate(CareHome=if_else(is.na(`Care home`),0,`Care home`)+if_else(is.na(Hospice), 0, Hospice),
         Home=if_else(is.na(Home), 0, Home)+if_else(is.na(Elsewhere), 0, Elsewhere)+
           if_else(is.na(`Other communal establishment`), 0, `Other communal establishment`),
         Hospital=if_else(is.na(Hospital), 0, Hospital),
         AllLocations=CareHome+Home+Hospital) %>% 
  select(LAD17CD, LAName, Cause, Date, CareHome, Home, Hospital, AllLocations) %>% 
  gather(Location, Deaths, c("CareHome", "Home", "Hospital", "AllLocations")) %>% 
  ungroup()

#Calculate mean LA-level deprivation score
#Start by calculating IMD at MSOA level
#Download IMD data
temp <- tempfile()
source <- ("https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/833970/File_1_-_IMD2019_Index_of_Multiple_Deprivation.xlsx")
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

IMD <- read_excel(temp, sheet="IMD2019", range="A2:F32845", col_names=FALSE) %>% 
  select(c(1,2,5,6)) %>% 
  set_names("LSOA11CD", "LSOA11NM", "IMDrank", "IMDdecile")

#Download LSOA to MSOA lookup
source <- ("https://opendata.arcgis.com/datasets/fe6c55f0924b4734adf1cf7104a0173e_0.csv")
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

lookup <- read.csv(temp) %>% 
  select(LSOA11CD, MSOA11CD, LAD17CD) %>% 
  unique()

#Merge into IMD data
IMD <- merge(IMD, lookup, by="LSOA11CD")

#Bring in population data for LSOAs
source <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fpopulationandmigration%2fpopulationestimates%2fdatasets%2flowersuperoutputareamidyearpopulationestimates%2fmid2020sape23dt2/sape23dt2mid2020lsoasyoaestimatesunformatted.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

pop <- read_excel(temp, sheet="Mid-2020 Persons", range="A6:G34758", col_names=FALSE) %>% 
  select(-c(2:6)) %>% 
  set_names("LSOA11CD", "pop")

pop_full <- read_excel(temp, sheet="Mid-2020 Persons", range="A6:CT34758", col_names=FALSE) %>% 
  select(-c(2:7)) %>% 
  set_names("LSOA11CD", c(0:90))

#Merge into IMD data and mess about with LA codes due to boundary changes
IMD <- merge(IMD, pop) %>% 
  mutate(LAD17CD=case_when(
    LAD17CD %in% c("E07000190", "E07000191") ~ "E07000246",
    LAD17CD %in% c("E07000206", "E07000205") ~ "E07000244",
    LAD17CD %in% c("E07000204", "E07000201") ~ "E07000245",
    LAD17CD %in% c("E06000028", "E06000029", "E07000048") ~ "E06000058",
    LAD17CD %in% c("E07000049", "E07000050", "E07000051", "E07000052", "E07000053") ~ "E06000059",
    LAD17CD %in% c("E07000004", "E07000005", "E07000006", "E07000007") ~ "E06000060",
    TRUE ~ LAD17CD))

#Calculate IMD rank at MSOA level as weighted average of LSOA level ranks, weight by population
#Need 2 versions as Northamptonshire split part way through the pandemic
IMD_LTLA2020 <- IMD %>% 
  group_by(LAD17CD)%>% 
  summarise(IMDrank=weighted.mean(IMDrank, pop), pop=sum(pop)) %>% 
  ungroup() %>% 
  arrange(IMDrank) %>% 
  mutate(cumpop=cumsum(pop),
         popprop=cumpop/max(cumpop),
         decile1=case_when(
           #popprop<=0.1 ~ 1,
           popprop<=0.2 ~ 2,
           #popprop<=0.3 ~ 3,
           popprop<=0.4 ~ 4,
           #popprop<=0.5 ~ 5,
           popprop<=0.6 ~ 6,
           #popprop<=0.7 ~ 7,
           popprop<=0.8 ~ 8,
           #popprop<=0.9 ~ 9,
           TRUE ~ 10)) 
  
IMD_LTLA2021 <- IMD %>% 
    #Sort out Northamptonshire
  mutate(LAD17CD=case_when(
    LAD17CD %in% c("E07000150", "E07000152", "E07000153", "E07000156") ~
        "E06000061",
    LAD17CD %in% c("E07000151", "E07000154", "E07000155") ~
        "E06000062",
    TRUE ~ LAD17CD)) %>% 
      group_by(LAD17CD)%>% 
        summarise(IMDrank=weighted.mean(IMDrank, pop), pop=sum(pop)) %>% 
        ungroup()%>% 
  arrange(IMDrank) %>% 
  mutate(cumpop=cumsum(pop),
         popprop=cumpop/max(cumpop),
         decile2=case_when(
           #popprop<=0.1 ~ 1,
           popprop<=0.2 ~ 2,
           #popprop<=0.3 ~ 3,
           popprop<=0.4 ~ 4,
           #popprop<=0.5 ~ 5,
           popprop<=0.6 ~ 6,
           #popprop<=0.7 ~ 7,
           popprop<=0.8 ~ 8,
           #popprop<=0.9 ~ 9,
           TRUE ~ 10)) 

#Join together, removing Welsh LAs as IMD is English only
fulldata <- merge(deaths %>% filter(substr(LAD17CD, 1, 1)=="E"), IMD_LTLA2020, all=TRUE, by="LAD17CD") %>% 
  merge(IMD_LTLA2021, all=TRUE, by="LAD17CD") %>% 
  mutate(decile=if_else(Date<as.Date("2021-01-02"), decile1, decile2),
         pop=if_else(Date<as.Date("2021-01-02"), pop.x, pop.y)) %>% 
  select(LAD17CD, LAName, Cause, Date, Location, Deaths, decile, pop)

deciledata <- fulldata %>% 
  group_by(decile, Date, Cause, Location) %>% 
  summarise(Deaths=sum(Deaths), pop=sum(pop)) %>% 
  ungroup()

deciledata %>% filter(Location=="AllLocations" & Cause=="COVID 19") %>% 
  ggplot(aes(x=Date, y=Deaths, colour=as.factor(decile)))+
  geom_line()+
  scale_x_date(name="")+
  scale_y_continuous(name="Weekly COVID deaths")+
  scale_colour_paletteer_d("dichromat::BrowntoBlue_10", name="",
                         labels=c("1 - Most deprived","2","3","4","5","6","7","8","9",
                                  "10 - least deprived"))+
  theme_custom()

agg_png("Outputs/COVIDDeathsONSIneq2122.png", units="in", width=8, height=6, res=800)
deciledata %>% filter(Location=="AllLocations" & Cause=="COVID 19" & 
                        Date>as.Date("2021-07-01")) %>% 
  ggplot(aes(x=Date, y=Deaths, colour=as.factor(decile)))+
  geom_line()+
  scale_x_date(name="", breaks=c(as.Date("2021-07-01"), as.Date("2021-09-01"), 
                                 as.Date("2021-11-01"), as.Date("2022-01-01"),
                                 as.Date("2022-03-01"), as.Date("2022-05-01")), 
               labels=c("Jul '21", "Sep '21","Nov '21",  "Jan '22", "Mar '22", "May '22"))+
  scale_y_continuous(name="Weekly COVID deaths", limits=c(0,NA))+
  #scale_colour_paletteer_d("dichromat::BrowntoBlue_10", name="Deprivation decile",
  #                         labels=c("1 - Most deprived","2","3","4","5","6","7","8","9",
  #                                  "10 - least deprived"))+
  scale_colour_manual(values=c("#663000", "#CC9B7A", "#F2DACE", "#66F0FF", "#00AACC"), name="Deprivation quintile",
                      labels=c("1 - Most deprived", "2", "3", "4", "5 - Least deprived"))+
  theme_custom()+
  labs(title="The BA.2 wave hit less deprived areas hardest",
       subtitle="Number of deaths registered with Covid on the death certificate by quintiles of the Index of Multiple Deprivation",
       caption="Data from the Office for National Statistics | Plot by @VictimOfMaths")
dev.off()

agg_png("Outputs/COVIDDeathsONSIneq2122Prop.png", units="in", width=8, height=6, res=800)
deciledata %>% filter(Location=="AllLocations" & Cause=="COVID 19" & 
                        Date>as.Date("2021-07-01")) %>% 
  ggplot(aes(x=Date, y=Deaths, fill=as.factor(decile)))+
  geom_area(position="fill")+
  scale_x_date(name="", breaks=c(as.Date("2021-07-01"), as.Date("2021-09-01"), 
                                 as.Date("2021-11-01"), as.Date("2022-01-01"),
                                 as.Date("2022-03-01"), as.Date("2022-05-01")), 
               labels=c("Jul '21", "Sep '21","Nov '21",  "Jan '22", "Mar '22", "May '22"))+
  scale_y_continuous(name="Proportion of COVID deaths", limits=c(0,NA), 
                     labels=label_percent(accuracy=1))+
  #scale_colour_paletteer_d("dichromat::BrowntoBlue_10", name="Deprivation decile",
  #                         labels=c("1 - Most deprived","2","3","4","5","6","7","8","9",
  #                                  "10 - least deprived"))+
  scale_fill_manual(values=c("#663000", "#CC9B7A", "#F2DACE", "#66F0FF", "#00AACC"), name="Deprivation quintile",
                      labels=c("1 - Most deprived", "2", "3", "4", "5 - Least deprived"))+
  theme_custom()+
  labs(title="The proportion of COVID deaths coming from less deprived areas is rising",
       subtitle="Number of deaths registered with Covid on the death certificate by quintiles of the Index of Multiple Deprivation",
       caption="Data from the Office for National Statistics | Plot by @VictimOfMaths")
dev.off()

agg_png("Outputs/COVIDDeathsONSIneq2022.png", units="in", width=8, height=6, res=800)
deciledata %>% filter(Location=="AllLocations" & Cause=="COVID 19") %>% 
  ggplot(aes(x=Date, y=Deaths, colour=as.factor(decile)))+
  geom_line()+
  scale_x_date(name="", )+
  scale_y_continuous(name="Weekly COVID deaths", limits=c(0,NA))+
  #scale_colour_paletteer_d("dichromat::BrowntoBlue_10", name="Deprivation decile",
  #                         labels=c("1 - Most deprived","2","3","4","5","6","7","8","9",
  #                                  "10 - least deprived"))+
  scale_colour_manual(values=c("#663000", "#CC9B7A", "#F2DACE", "#66F0FF", "#00AACC"), name="Deprivation quintile",
                      labels=c("1 - Most deprived", "2", "3", "4", "5 - Least deprived"))+
  theme_custom()+
  labs(title="COVID deaths have been consistently higher in more deprived areas",
       subtitle="Number of deaths registered with Covid on the death certificate by quintiles of the Index of Multiple Deprivation",
       caption="Data from the Office for National Statistics | Plot by @VictimOfMaths")
dev.off()

deciledata %>% filter(Location=="AllLocations" & Cause=="COVID 19") %>% 
  ggplot(aes(x=Date, y=Deaths, fill=as.factor(decile)))+
  geom_area(position="fill")+
  scale_x_date(name="")+
  scale_y_continuous(name="Proportion of COVID deaths", limits=c(0,NA), 
                     labels=label_percent(accuracy=1))+
  scale_fill_paletteer_d("dichromat::BrowntoBlue_10", name="",
                         labels=c("1 - Most deprived","2","3","4","5","6","7","8","9",
                                  "10 - least deprived"))+
  theme_custom()

agg_png("Outputs/COVIDDeathsONSIneqProp2122.png", units="in", width=10, height=6, res=800)
deciledata %>% filter(Location!="AllLocations" & Cause=="COVID 19" & 
                        Date>as.Date("2021-07-01")) %>% 
  mutate(Location=case_when(
    Location=="CareHome" ~ "Care home",
    Location=="Home" ~ "Home/other",
    Location=="Hospital" ~ "Hospital"),
    Location=factor(Location, levels=c("Hospital", "Care home", "Home/other"))) %>% 
  ggplot(aes(x=Date, y=Deaths, fill=as.factor(decile)))+
  geom_area(position="fill")+
  scale_x_date(name="")+
  scale_y_continuous(name="Proportion of COVID deaths", limits=c(0,NA), 
                     labels=label_percent(accuracy=1))+
  #scale_fill_paletteer_d("dichromat::BrowntoBlue_10", name="",
  #                       labels=c("1 - Most deprived","2","3","4","5","6","7","8","9",
  #                                "10 - least deprived"))+
  scale_fill_manual(values=c("#663000", "#CC9B7A", "#F2DACE", "#66F0FF", "#00AACC"), name="Deprivation quintile",
                      labels=c("1 - Most deprived", "2", "3", "4", "5 - Least deprived"))+
  facet_wrap(~Location)+
  theme_custom()+
  labs(title="The socioeconomic distribution of deaths has shifted in all locations",
       subtitle="Proportion of deaths registered weekly in England by place of death and quintile of the Index of Multiple Deprivation\n ",
       caption="Data from the Office for National Statistics | Plot by @VictimOfMaths")
dev.off()

agg_png("Outputs/COVIDDeathsONSIneqxLoc2122.png", units="in", width=10, height=6, res=800)
deciledata %>% filter(Location!="AllLocations" & Cause=="COVID 19" & 
                        Date>as.Date("2021-07-01")) %>% 
  mutate(Location=case_when(
    Location=="CareHome" ~ "Care home",
    Location=="Home" ~ "Home/other",
    Location=="Hospital" ~ "Hospital"),
    Location=factor(Location, levels=c("Hospital", "Care home", "Home/other"))) %>% 
  ggplot(aes(x=Date, y=Deaths, colour=as.factor(decile)))+
  geom_line()+
  scale_x_date(name="")+
  scale_y_continuous(name="COVID deaths")+
  #scale_fill_paletteer_d("dichromat::BrowntoBlue_10", name="",
  #                       labels=c("1 - Most deprived","2","3","4","5","6","7","8","9",
  #                                "10 - least deprived"))+
  scale_colour_manual(values=c("#663000", "#CC9B7A", "#F2DACE", "#66F0FF", "#00AACC"), name="Deprivation quintile",
                    labels=c("1 - Most deprived", "2", "3", "4", "5 - Least deprived"))+
  facet_wrap(~Location)+
  theme_custom()+
  labs(title="The biggest difference in COVID deaths between more and less deprived areas is in care homes",
       subtitle="Proportion of deaths registered weekly in England by place of death and quintile of the Index of Multiple Deprivation\n ",
       caption="Data from the Office for National Statistics | Plot by @VictimOfMaths")
dev.off()

summarydata <- deciledata %>% filter(Location=="AllLocations" & Cause=="COVID 19") %>% 
  select(decile, Deaths, Date) %>% 
  spread(Date, Deaths) %>% 
  mutate(decile=case_when(
    decile==1 ~ "1 - Most deprived",
    decile==10 ~ "10 - least deprived",
    TRUE ~ as.character(decile)
  ))

write.csv(summarydata, "Outputs/COVIDDeathsIneq.csv", row.names=FALSE)
  
