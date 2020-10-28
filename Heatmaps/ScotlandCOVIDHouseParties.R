rm(list=ls())

library(tidyverse)
library(googlesheets4)
library(lubridate)
library(curl)
library(readxl)
library(paletteer)
library(sf)

data <- read_sheet("https://docs.google.com/spreadsheets/d/1Cr6aBG656gUcpWDed7uyqsX1SMTLpiTjZgrfoZFl24o/edit#gid=0")

#Tidy up division data
data <- data %>% 
  mutate(division=case_when(
    `Division Letter`=="A" ~ "North East",
    `Division Letter`=="D" ~ "Tayside",
    `Division Letter`=="N" ~ "Highlands and Islands",
    `Division Letter`=="C" ~ "Forth Valley",
    `Division Letter`=="E" ~ "Edinburgh",
    `Division Letter`=="J" ~ "The Lothians & Scottish Borders",
    `Division Letter`=="P" ~ "Fife",
    `Division Letter`=="G" ~ "Glasgow",
    `Division Letter`=="U" ~ "Ayrshire",
    `Division Letter`=="Q" ~ "Lanarkshire",
    `Division Letter`=="L" ~ "Argyll and West Dunbartonshire",
    `Division Letter`=="K" ~ "Renfreshire and Inverclyde",
    `Division Letter`=="V" ~ "Dumfries & Galloway",
    TRUE ~ NA_character_))

#Faff about with dates (probably a much cleverer way to do this)
#For a reason I cannot fathom, dates in the first half of the month are parsed 
#As character strings that look like dates, while those in the second half are
#converted to the number of seconds since 01/01/1970, only with the days and weeks
#transposed. BECAUSE OF REASONS.
data <- data %>% 
  mutate(temp=as.numeric(Date),
         date=coalesce(as.Date(as.character(as.Date("1970-01-01")+seconds(as.numeric(Date))),
                               format="%Y-%d-%m"), 
                       as.Date(if_else(is.na(temp), as.character(Date), NA_character_), 
                               format="%d/%m/%Y")))

#Set missing dates to have 0 incidents
data <- data %>%
  complete(date, nesting(`SD Letter`, division, `Division Letter`, `Area Commands`)) %>% 
  mutate(incidents=if_else(is.na(`House Gatherings Attended`), 0, 
                           `House Gatherings Attended`),
         breaches=if_else(is.na(`House Gatherings in Breach of Restrictions`), 0,
                          `House Gatherings in Breach of Restrictions`),
         FPNS=if_else(is.na(FPNS), 0, FPNS),
         arrests=if_else(is.na(Arrests), 0, Arrests),
         students=if_else(is.na(`House Gatherings involving Students`), 0,
                          `House Gatherings involving Students`)) %>% 
  select(date, `SD Letter`, division, `Area Commands`, incidents, breaches, FPNS,
         arrests, students)

#Collapse to Division-level dataset
data.div <- data %>% 
  group_by(date, division) %>% 
  summarise(incidents=sum(incidents),
            breaches=sum(breaches),
            FPNS=sum(FPNS), arrests=sum(arrests),
            students=sum(students))

#Map to Council areas - download population data
temp <- tempfile()
source <- "https://www.nrscotland.gov.uk/files//statistics/population-estimates/mid-19/mid-year-pop-est-19-data.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
pop <- read_excel(temp, sheet="Table 4", range="A8:C39", col_names = FALSE)
colnames(pop) <- c("code", "council", "pop")

#Match areas
pop <- pop %>% 
  mutate(division=case_when(
    council %in% c("Argyll and Bute", "West Dunbartonshire") ~ "Argyll and West Dunbartonshire",
    council %in% c("East Ayrshire", "North Ayrshire", "South Ayrshire") ~ "Ayrshire",
    council=="Dumfries and Galloway" ~ "Dumfries & Galloway",
    council=="City of Edinburgh" ~ "Edinburgh",
    council=="Fife" ~ "Fife",
    council %in% c("Clackmannanshire", "Falkirk", "Stirling") ~ "Forth Valley",
    council %in% c("East Dunbartonshire", "East Renfrewshire", "Glasgow City") ~ "Glasgow",
    council %in% c("Highland", "Na h-Eileanan Siar", "Orkney Islands", "Shetland Islands") ~ "Highlands and Islands",
    council %in% c("North Lanarkshire", "South Lanarkshire") ~ "Lanarkshire",
    council %in% c("Aberdeen City", "Aberdeenshire", "Moray") ~ "North East",
    council %in% c("Inverclyde", "Renfrewshire") ~ "Renfreshire and Inverclyde",
    council %in% c("Dundee City", "Angus", "Perth and Kinross") ~ "Tayside",
    council %in% c("East Lothian", "Midlothian", "West Lothian", "Scottish Borders") ~ "The Lothians & Scottish Borders"
    ))

div.pop <- pop %>% 
  group_by(division) %>% 
  summarise(pop=sum(pop)) %>% 
  ungroup()

data.div <- merge(data.div, div.pop) %>% 
  mutate(non_students=incidents-students) %>% 
  gather(measure, incidents, c(3:7, 9)) %>% 
  mutate(incident_rate=incidents*100000/pop)

tiff("Outputs/HousePartyStudent.tiff", units="in", width=8, height=6, res=500)
data.div %>% 
  group_by(date, measure) %>% 
  summarise(incidents=sum(incidents)) %>% 
  filter(measure %in% c("students", "non_students")) %>% 
  ggplot()+
  geom_area(aes(x=date, y=incidents, fill=measure), show.legend=FALSE)+
  scale_x_date(name="")+
  scale_y_continuous(name="House gatherings attended")+
  scale_fill_paletteer_d("NineteenEightyR::electronic_night", direction=-1)+
  theme_classic()+
  theme(plot.title=element_text(face="bold"), plot.subtitle=element_markdown())+
  labs(title="Students haven't been driving police callouts to illegal parties in Scotland",
       subtitle="Police callouts to reported illegal house gatherings involving <span style='color:#362F78FF;'>students</span> and <span style='color:#57B4AEFF;'>non-students",
       caption="Data from BBC FoI request to Police Scotland | Plot by @VictimofMaths")
dev.off()

tiff("Outputs/HousePartyCouncil.tiff", units="in", width=10, height=7, res=500)
data.div %>% 
  filter(measure=="breaches") %>% 
  ggplot()+
  geom_line(aes(x=date, y=incident_rate), colour="tomato")+
  scale_x_date(name="")+
  scale_y_continuous(name="Breaches per 100,000 population")+
  geom_vline(xintercept=as.Date("2020-09-23"), linetype=2)+
  facet_wrap(~division)+
  theme_classic()+
  theme(strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(0.8)),
        plot.title=element_text(face="bold"))+
  labs(title="Illegal house parties have continued since the new restrictions",
       subtitle="Police recorded breaches of household gathering legislation in Scotland",
       caption="Data from BBC FoI request to Police Scotland | Plot by @VictimofMaths")
dev.off()

data.sum <- data.div %>% 
  group_by(measure, division) %>% 
  summarise(incident_rate=sum(incident_rate))

#Download shapefile
temp <- tempfile()
temp2 <- tempfile()
source <- "https://opendata.arcgis.com/datasets/910f48f3c4b3400aa9eb0af9f8989bbe_0.zip?outSR=%7B%22latestWkid%22%3A27700%2C%22wkid%22%3A27700%7D"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)

#The actual shapefile has a different name each time you download it, so need to fish the name out of the unzipped file
name <- list.files(temp2, pattern=".shp")
shapefile <- st_read(file.path(temp2, name))

names(shapefile)[names(shapefile) == "LAD20CD"] <- "code"

map.data <- pop %>% 
  merge(data.sum, by="division") 
  
map.data <- full_join(map.data, shapefile, by="code")

tiff("Outputs/HousePartyMap.tiff", units="in", width=7, height=7, res=500)
map.data %>% 
  filter(measure=="breaches") %>% 
ggplot()+
  geom_sf(aes(geometry=geometry, fill=incident_rate), colour=NA)+
  theme_classic()+
  scale_fill_paletteer_c("viridis::viridis", name="Breaches per 100,000")+
  theme(axis.line=element_blank(), axis.ticks=element_blank(), axis.text=element_blank(),
        axis.title=element_blank(), plot.title=element_text(face="bold"))+
  labs(title="Glasgow has been Scotland's illegal house party capital",
       subtitle="Rates of household gatherings found to breach restrictions",
       caption="Data from BBC FoI request to Police Scotland | Plot by @VictimofMaths")
dev.off()

