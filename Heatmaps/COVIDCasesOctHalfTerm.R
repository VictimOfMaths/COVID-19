rm(list=ls())

library(curl)
library(arrow)
library(tidyverse)
library(RcppRoll)
library(ggtext)
library(ragg)

options(scipen=10000)

theme_custom <- function() {
  theme_classic() %+replace%
    theme(plot.title.position="plot", plot.caption.position="plot",
          strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)),
          plot.title=element_text(face="bold", size=rel(1.5), hjust=0,
                                  margin=margin(0,0,5.5,0)),
          text=element_text(family="Lato"))
}

#Download case data by age and UTLA
temp <- tempfile()
source <- "https://api.coronavirus.data.gov.uk/v2/data?areaType=ltla&metric=newCasesBySpecimenDateAgeDemographics&format=csv"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

data <- read_csv_arrow(temp) %>% 
  select(c(1:6)) %>% 
  filter(age!="unassigned") %>% 
  mutate(age=case_when(
    age=="00_04" ~ "0_4",
    age=="05_09" ~ "5-9",
    TRUE ~ age),
    age = age %>% str_replace("_", "-") %>%
      factor(levels=c("0-4", "5-9", "10-14", "15-19",
                      "20-24", "25-29", "30-34", "35-39", 
                      "40-44", "45-49", "50-54", "55-59", 
                      "60-64", "65-69", "70-74", "75-79", 
                      "80-84", "85-89", "90+"))) %>% 
  #Remove two bonus age categories that we don't need (0-59 and 60+)
  filter(!is.na(age)) %>% 
  #Sort out Buckinghamsire (as 4 separate LTLAs in the data, but pop data only available for Bucks)
  mutate(Code=case_when(
    areaCode %in% c("E07000004", "E07000005", "E07000006", "E07000007") ~ "E06000060",
    TRUE ~ as.character(areaCode)),
  areaName=case_when(
    Code=="E06000060" ~ "Buckinghamshire",
    TRUE ~ as.character(areaName)),
  areaType=case_when(
    Code=="E06000060" ~ "ltla",
    TRUE ~ as.character(areaType)),
  date=as.Date(date)) %>% 
  group_by(Code, areaName, areaType, date, age) %>% 
  mutate(cases=sum(cases)) %>% 
  ungroup()

#Bring in populations
temp <- tempfile()
source <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fpopulationandmigration%2fpopulationestimates%2fdatasets%2fpopulationestimatesforukenglandandwalesscotlandandnorthernireland%2fmid2020/ukpopestimatesmid2020on2020geography.xls"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

pop <- read_excel(temp, sheet="MYE2 - Persons", range="A8:CQ434")

#Align age bands
pop <- pop %>% 
  gather(age.sgl, pop, c(5:95)) %>% 
  mutate(age.sgl=as.numeric(gsub("\\+", "", age.sgl)),
         age=case_when(
           age.sgl<5 ~ "0-4",
           age.sgl<10 ~ "5-9",
           age.sgl<15 ~ "10-14",
           age.sgl<20 ~ "15-19",
           age.sgl<25 ~ "20-24",
           age.sgl<30 ~ "25-29",
           age.sgl<35 ~ "30-34",
           age.sgl<40 ~ "35-39",
           age.sgl<45 ~ "40-44",
           age.sgl<50 ~ "45-49",
           age.sgl<55 ~ "50-54",
           age.sgl<60 ~ "55-59",
           age.sgl<65 ~ "60-64",
           age.sgl<70 ~ "65-69",
           age.sgl<75 ~ "70-74",
           age.sgl<80 ~ "75-79",
           age.sgl<85 ~ "80-84",
           age.sgl<90 ~ "85-89",
           TRUE ~ "90+") %>% 
           factor(levels = levels(data$age))) %>% 
  #And sort out Buckinghamshire codes
  mutate(Code=case_when(
    Code %in% c("E07000005", "E07000006", "E07000007", "E07000004") ~ "E06000060",
    TRUE ~ Code
  )) %>% 
  group_by(age, Code) %>%
  summarise(pop=sum(pop))

#Merge into case data
data <- data %>% 
  left_join(pop, by=c("Code", "age")) %>% 
  arrange(date) 

#Pick out Leicestershire vs rest of E Mids vs rest of England
plotdata <- data %>% 
  filter(date>as.Date("2021-10-01")) %>% 
  mutate(Region=case_when(
    areaName %in% c("Blaby", "Charnwood", "Harborough", "Hinckley and Bosworth", "Melton",
                    "North West Leicestershire", "Oadby and Wigston", "Leicester") ~ "Leicestershire",
    areaName %in% c("Ashfield", "Bassetlaw", "Broxtowe", "Gedling", "Mansfield", 
                    "Newark and Sherwood", "Rushcliffe", "Nottingham") ~ "Nottinghamshire",
    areaName %in% c("Kettering", "Corby", "East Northamptonshire", "Wellingborough", "Daventry",
                    "Northampton", "South Northamptonshire") ~ "Northamptonshire",
    areaName %in% c("Amber Valley", "Erewash", "Bolsover", "Chesterfield", "North East Derbyshire",
                    "High Peak", "Derbyshire Dales", "South Derbyshire", "Derby") ~ "Derbyshire",
    areaName %in% c("North Warwickshire", "Nuneaton and Bedworth", "Rugby", "Stratford", "Warwick") ~
      "Warwickshire",
    TRUE ~ "Rest of England")) %>% 
  group_by(date, age, Region) %>% 
  summarise(cases=sum(cases), pop=sum(pop)) %>% 
  ungroup() %>% 
  group_by(age, Region) %>% 
  arrange(date) %>% 
  mutate(cases_roll=roll_mean(cases, 7, align="center", fill=NA),
         caserate_roll=cases_roll*100000/pop,
         caserate=cases*100000/pop) %>% 
  ungroup()

agg_tiff("Outputs/COVIDCasesxHalfTerm1.tiff", units="in", width=10, height=6, res=500)
ggplot(plotdata %>% filter(age %in% c("5-9", "10-14", "15-19", "35-39", "40-44", "45-49") &
                             Region!="Rest of England"), 
       aes(x=date, y=caserate, colour=Region))+
  geom_rect(aes(xmin=as.Date("2021-10-25"), xmax=as.Date("2021-10-29"),
                ymin=0, ymax=550), fill="Grey90", colour=NA)+
  geom_rect(aes(xmin=as.Date("2021-10-18"), xmax=as.Date("2021-10-22"),
                ymin=0, ymax=550), fill="Deeppink", colour=NA, alpha=0.002)+
  geom_line(show.legend=FALSE)+
  scale_x_date(name="")+
  scale_y_continuous(name="New cases per 100,000 per day")+
  scale_colour_manual(values=c("Grey60", "Deeppink", "Grey60", "Grey60", "Grey60"))+
  facet_wrap(~age, scales="free_y")+
  theme_custom()+
  theme(plot.title=element_markdown())+
  labs(title="COVID cases in <span style='color:Deeppink;'>Leicestershire</span> compared to <span style='color:Grey60;'>neighbouring counties",
       subtitle="Daily rate of new COVID cases in selected age bands in Leicestershire, Derbyshire, Northamptonshire, Nottinghamshire and Warwickshire.\nColoured rectangles represent school half term dates.",
       caption="Data from coronavirus.data.gov.uk & ONS | Plot by @VictimOfMaths")
dev.off()

plotdata2 <- data %>% 
  filter(date>as.Date("2021-10-01")) %>% 
  mutate(Region=case_when(
    areaName %in% c("Blaby", "Charnwood", "Harborough", "Hinckley and Bosworth", "Melton",
                    "North West Leicestershire", "Oadby and Wigston", "Leicester") ~ "Leicestershire",
    TRUE ~ "Rest of England")) %>% 
  group_by(date, age, Region) %>% 
  summarise(cases=sum(cases), pop=sum(pop)) %>% 
  ungroup() %>% 
  group_by(age, Region) %>% 
  arrange(date) %>% 
  mutate(cases_roll=roll_mean(cases, 7, align="center", fill=NA),
         caserate_roll=cases_roll*100000/pop,
         caserate=cases*100000/pop) %>% 
  ungroup()

agg_tiff("Outputs/COVIDCasesxHalfTerm2.tiff", units="in", width=10, height=6, res=500)
ggplot(plotdata2 %>% filter(age %in% c("5-9", "10-14", "15-19", "35-39", "40-44", "45-49")), 
       aes(x=date, y=caserate, colour=Region))+
  geom_rect(aes(xmin=as.Date("2021-10-18"), xmax=as.Date("2021-10-22"),
                ymin=0, ymax=400), fill="Deeppink", colour=NA, alpha=0.005)+
  geom_rect(aes(xmin=as.Date("2021-10-25"), xmax=as.Date("2021-10-29"),
                ymin=0, ymax=400), fill="DodgerBlue", colour=NA, alpha=0.005)+
  geom_line(show.legend=FALSE)+
  scale_x_date(name="")+
  scale_y_continuous(name="New cases per 100,000 per day")+
  scale_colour_manual(values=c("Deeppink","DodgerBlue"))+
  facet_wrap(~age, scales="free_y")+
  theme_custom()+
  theme(plot.title=element_markdown())+
  labs(title="COVID cases in <span style='color:Deeppink;'>Leicestershire</span> started rising *slightly* before <span style='color:DodgerBlue;'>the rest of England</span>",
       subtitle="Daily rate of new COVID cases in selected age bands. Coloured rectangles represent school half term dates.",
       caption="Data from coronavirus.data.gov.uk & ONS | Plot by @VictimOfMaths")
dev.off()

