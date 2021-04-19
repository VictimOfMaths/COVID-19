rm(list=ls())

library(tidyverse)
library(readxl)
library(curl)
library(scales)
library(extrafont)
library(paletteer)
library(ragg)

#Read in Scottish and Welsh data, compiled from sources only available in pdf table (thanks folks)
#Scotland: https://beta.isdscotland.org/find-publications-and-data/population-health/covid-19/covid-19-statistical-report/
#Wales: http://www2.nphs.wales.nhs.uk:8080/CommunitySurveillanceDocs.nsf/3dc04669c9e1eaa880257062003b246b/e61c928e715ece3180258680003449c3/$FILE/Wales%20COVID-19%20vaccination%20enhanced%20surveillance%20-%20equality%20report.pdf
swdata <- read_excel("Data/COVIDUKVaxIneq.xlsx", range="A1:G131")

#Read in English data
#MSOA data 1st reported in 25th Feb file
temp <- tempfile()
url1 <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/03/COVID-19-weekly-announced-vaccinations-25-February-2021-revised.xlsx"
temp <- curl_download(url=url1, destfile=temp, quiet=FALSE, mode="wb")

edata1 <- read_excel(temp, sheet="Vaccinations by MSOA", range="F16:K6806", col_names=FALSE) %>% 
  rename(msoa11cd=`...1`, msoa11nm=`...2`, u70=`...3`, `70-74`=`...4`, `75-79`=`...5`, `80+`=...6) %>% 
  gather(Age, Vax, c(3:6)) %>% 
  mutate(Date=as.Date("2021-02-21"))

#4th March file
url2 <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/03/COVID-19-weekly-announced-vaccinations-4-March-2021-1.xlsx"
temp <- curl_download(url=url2, destfile=temp, quiet=FALSE, mode="wb")

edata2 <- read_excel(temp, sheet="MSOA", range="F16:L6806", col_names=FALSE) %>% 
  rename(msoa11cd=`...1`, msoa11nm=`...2`, u65=`...3`, `65-69`=`...4`, `70-74`=`...5`, 
         `75-79`=`...6`, `80+`=...7) %>% 
  gather(Age, Vax, c(3:7)) %>% 
  mutate(Date=as.Date("2021-02-28"))

#4th March file is the first time NIMS population denominators are available, 
#use these for the 25th Feb data
pop2 <- read_excel(temp, sheet="Population estimates (NIMS)", range="L16:S6806", col_names=FALSE) %>% 
  rename(msoa11cd=`...1`, msoa11nm=`...2`, u16=`...3`, `u65`=`...4`, `65-69`=`...5`, `70-74`=`...6`, 
         `75-79`=`...7`, `80+`=...8) %>% 
  gather(Age, Pop, c(3:8)) %>% 
  select(-"msoa11nm")

pop1 <- pop2 %>% 
  mutate(Age=case_when(
    Age %in% c("u65", "65-69") ~ "u70",
    TRUE ~ Age)) %>% 
  group_by(msoa11cd, Age) %>% 
  summarise(Pop=sum(Pop)) %>% 
  ungroup()

edata1 <- merge(edata1, pop1)
edata2 <- merge(edata2, pop2)

#11th March file
url3 <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/03/COVID-19-weekly-announced-vaccinations-11-March-2021.xlsx"
temp <- curl_download(url=url3, destfile=temp, quiet=FALSE, mode="wb")

edata3 <- read_excel(temp, sheet="MSOA", range="F16:M6806", col_names=FALSE) %>% 
  rename(msoa11cd=`...1`, msoa11nm=`...2`, u60=`...3`, `60-64`=`...4`, `65-69`=`...5`, 
         `70-74`=`...6`, `75-79`=`...7`, `80+`=`...8`) %>% 
  gather(Age, Vax, c(3:8)) %>% 
  mutate(Date=as.Date("2021-03-07"))

pop3 <- read_excel(temp, sheet="Population estimates (NIMS)", range="M16:U6806", col_names=FALSE) %>% 
  rename(msoa11cd=`...1`, msoa11nm=`...2`, u16=`...3`, `u60`=`...4`, `60-64`=`...5`, `65-69`=`...6`, 
         `70-74`=`...7`, `75-79`=`...8`, `80+`=`...9`) %>% 
  gather(Age, Pop, c(3:9)) %>% 
  select(-"msoa11nm")

edata3 <- merge(edata3, pop3)

#18th March file
url4 <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/03/COVID-19-weekly-announced-vaccinations-18-March-2021-revised.xlsx"
temp <- curl_download(url=url4, destfile=temp, quiet=FALSE, mode="wb")

edata4 <- read_excel(temp, sheet="MSOA", range="F16:N6806", col_names=FALSE) %>% 
  rename(msoa11cd=`...1`, msoa11nm=`...2`, u55=`...3`, `55-59`=`...4`, `60-64`=`...5`, 
         `65-69`=`...6`, `70-74`=`...7`, `75-79`=`...8`, `80+`=`...9`) %>% 
  gather(Age, Vax, c(3:9)) %>% 
  mutate(Date=as.Date("2021-03-14"))

pop4 <- read_excel(temp, sheet="Population estimates (NIMS)", range="N16:W6806", col_names=FALSE) %>% 
  rename(msoa11cd=`...1`, msoa11nm=`...2`, u16=`...3`, `u55`=`...4`, `55-59`=`...5`, `60-64`=`...6`, 
         `65-69`=`...7`, `70-74`=`...8`, `75-79`=`...9`, `80+`=`...10`) %>% 
  gather(Age, Pop, c(3:10)) %>% 
  select(-"msoa11nm")

edata4 <- merge(edata4, pop4)

#25th March file
url5 <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/03/COVID-19-weekly-announced-vaccinations-25-March-2021-revised.xlsx"
temp <- curl_download(url=url5, destfile=temp, quiet=FALSE, mode="wb")

edata5 <- read_excel(temp, sheet="MSOA", range="F16:O6806", col_names=FALSE) %>% 
  rename(msoa11cd=`...1`, msoa11nm=`...2`, u50=`...3`, `50-54`=`...4`, `55-59`=`...5`, 
         `60-64`=`...6`, `65-69`=`...7`, `70-74`=`...8`, `75-79`=`...9`, `80+`=`...10`) %>% 
  gather(Age, Vax, c(3:10)) %>% 
  mutate(Date=as.Date("2021-03-21"))

pop5 <- read_excel(temp, sheet="Population estimates (NIMS)", range="O16:Y6806", col_names=FALSE) %>% 
  rename(msoa11cd=`...1`, msoa11nm=`...2`, u16=`...3`, `u50`=`...4`, `50-54`=`...5`, `55-59`=`...6`, 
         `60-64`=`...7`, `65-69`=`...8`, `70-74`=`...9`, `75-79`=`...10`, `80+`=`...11`) %>% 
  gather(Age, Pop, c(3:11)) %>% 
  select(-"msoa11nm")

edata5 <- merge(edata5, pop5)

#1st April file
url6 <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/03/COVID-19-weekly-announced-vaccinations-25-March-2021-revised.xlsx"
temp <- curl_download(url=url6, destfile=temp, quiet=FALSE, mode="wb")

edata6 <- read_excel(temp, sheet="MSOA", range="F16:O6806", col_names=FALSE) %>% 
  rename(msoa11cd=`...1`, msoa11nm=`...2`, u50=`...3`, `50-54`=`...4`, `55-59`=`...5`, 
         `60-64`=`...6`, `65-69`=`...7`, `70-74`=`...8`, `75-79`=`...9`, `80+`=`...10`) %>% 
  gather(Age, Vax, c(3:10)) %>% 
  mutate(Date=as.Date("2021-03-28"))

pop6 <- read_excel(temp, sheet="Population estimates (NIMS)", range="O16:Y6806", col_names=FALSE) %>% 
  rename(msoa11cd=`...1`, msoa11nm=`...2`, u16=`...3`, `u50`=`...4`, `50-54`=`...5`, `55-59`=`...6`, 
         `60-64`=`...7`, `65-69`=`...8`, `70-74`=`...9`, `75-79`=`...10`, `80+`=`...11`) %>% 
  gather(Age, Pop, c(3:11)) %>% 
  select(-"msoa11nm")

edata6 <- merge(edata6, pop6)

#8th April file
url7 <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/03/COVID-19-weekly-announced-vaccinations-25-March-2021-revised.xlsx"
temp <- curl_download(url=url7, destfile=temp, quiet=FALSE, mode="wb")

edata7 <- read_excel(temp, sheet="MSOA", range="F16:O6806", col_names=FALSE) %>% 
  rename(msoa11cd=`...1`, msoa11nm=`...2`, u50=`...3`, `50-54`=`...4`, `55-59`=`...5`, 
         `60-64`=`...6`, `65-69`=`...7`, `70-74`=`...8`, `75-79`=`...9`, `80+`=`...10`) %>% 
  gather(Age, Vax, c(3:10)) %>% 
  mutate(Date=as.Date("2021-04-04"))

pop7 <- read_excel(temp, sheet="Population estimates (NIMS)", range="O16:Y6806", col_names=FALSE) %>% 
  rename(msoa11cd=`...1`, msoa11nm=`...2`, u16=`...3`, `u50`=`...4`, `50-54`=`...5`, `55-59`=`...6`, 
         `60-64`=`...7`, `65-69`=`...8`, `70-74`=`...9`, `75-79`=`...10`, `80+`=`...11`) %>% 
  gather(Age, Pop, c(3:11)) %>% 
  select(-"msoa11nm")

edata7 <- merge(edata7, pop7)

#15th April file
url8 <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/04/COVID-19-weekly-announced-vaccinations-15-April-2021.xlsx"
temp <- curl_download(url=url8, destfile=temp, quiet=FALSE, mode="wb")

edata8 <- read_excel(temp, sheet="MSOA", range="F16:O6806", col_names=FALSE) %>% 
  rename(msoa11cd=`...1`, msoa11nm=`...2`, u50=`...3`, `50-54`=`...4`, `55-59`=`...5`, 
         `60-64`=`...6`, `65-69`=`...7`, `70-74`=`...8`, `75-79`=`...9`, `80+`=`...10`) %>% 
  gather(Age, Vax, c(3:10)) %>% 
  mutate(Date=as.Date("2021-04-11"))

pop8 <- read_excel(temp, sheet="Population estimates (NIMS)", range="O16:Y6806", col_names=FALSE) %>% 
  rename(msoa11cd=`...1`, msoa11nm=`...2`, u16=`...3`, `u50`=`...4`, `50-54`=`...5`, `55-59`=`...6`, 
         `60-64`=`...7`, `65-69`=`...8`, `70-74`=`...9`, `75-79`=`...10`, `80+`=`...11`) %>% 
  gather(Age, Pop, c(3:11)) %>% 
  select(-"msoa11nm")

edata8 <- merge(edata8, pop8)

#Calculate MSOA-level IMD decile
#Download IMD data
temp <- tempfile()
source <- ("https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/833970/File_1_-_IMD2019_Index_of_Multiple_Deprivation.xlsx")
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

IMD <- read_excel(temp, sheet="IMD2019", range="A2:F32845", col_names=FALSE)[,c(1,2,5,6)]
colnames(IMD) <- c("LSOA11CD", "LSOA11NM", "IMDrank", "IMDdecile")

#Download LSOA to MSOA lookup
temp <- tempfile()
source <- ("https://opendata.arcgis.com/datasets/fe6c55f0924b4734adf1cf7104a0173e_0.csv")
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

lookup <- read.csv(temp) %>% 
  select(LSOA11CD, MSOA11CD, RGN11NM) %>% 
  unique()

#Merge into IMD data
IMD <- merge(IMD, lookup, by="LSOA11CD")

#Bring in population data for LSOAs
temp <- tempfile()
temp2 <- tempfile()
source <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fpopulationandmigration%2fpopulationestimates%2fdatasets%2flowersuperoutputareamidyearpopulationestimatesnationalstatistics%2fmid2019sape22dt13/sape22dt13mid2019lsoabroadagesestimatesunformatted.zip"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)

pop <- read_excel(file.path(temp2, "SAPE22DT13-mid-2019-lsoa-Broad_ages-estimates-unformatted.xlsx"),
                  sheet="Mid-2019 Persons", range="A6:G34758", col_names=FALSE)[,c(1,7)]
colnames(pop) <- c("LSOA11CD", "pop")

#Merge into IMD data
IMD <- merge(IMD, pop)

#Calculate IMD rank at MSOA level as weighted average of LSOA level ranks, weight by population
IMD_MSOA <- IMD %>% 
  group_by(MSOA11CD) %>% 
  summarise(IMDrank=weighted.mean(IMDrank, pop), pop=sum(pop)) %>% 
  ungroup()  %>% 
  arrange(-IMDrank) %>% 
  #Calculate weighted deciles (there must be a function to do this elegantly)
  mutate(rank=1:nrow(.), cumsum=cumsum(pop), prop=cumsum/max(cumsum),
         IMD=case_when(
           prop<0.1 ~ 1,
           prop<0.2 ~ 2,
           prop<0.3 ~ 3,
           prop<0.4 ~ 4,
           prop<0.5 ~ 5,
           prop<0.6 ~ 6,
           prop<0.7 ~ 7,
           prop<0.8 ~ 8,
           prop<0.9 ~ 9,
           TRUE ~ 10)) %>% 
  select(MSOA11CD, IMD)

#Bring together
edata <- bind_rows(edata1, edata2, edata3, edata4, edata5, edata6, edata7, edata8) %>% 
  merge(IMD_MSOA, by.x="msoa11cd", by.y="MSOA11CD") %>% 
  group_by(Age, Date, IMD) %>% 
  summarise(Vax=sum(Vax), Pop=sum(Pop)) %>% 
  ungroup() %>% 
  mutate(Vaxprop=Vax/Pop, Country="England")

data <- bind_rows(edata, swdata) %>% 
  mutate(IMD=case_when(
    Country=="Wales" ~ IMD*2-0.5,
    TRUE ~ IMD),
    Age=factor(Age, levels=c("u50", "50-54", "u55", "55-59", "u60", "60-64", "u65", "65-69", "u70",
                             "70-74", "75-79", "80+")))

#Full dataset
data %>% filter(!Age %in% c("u55", "u60", "u65", "u70")) %>% 
ggplot(aes(x=IMD, y=Vaxprop, group=Date, colour=Date))+
  geom_line()+
  scale_x_continuous(breaks=c(2,9), labels=c("Least\ndeprived", "Most\ndeprived"),
                     name="Deprivation")+
  scale_y_continuous(labels=label_percent(accuracy=1), name="Proportion of adults vaccinated",
                     limits=c(0,1))+
  #scale_colour_paletteer_c("pals::ocean.tempo")+
  facet_grid(Country~Age)+
  theme_classic()+
  theme(strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)),
        axis.text.x=element_text(size=rel(0.8)))

#Compare countries at ~mid-March
agg_tiff("Outputs/COVIDVaxIneq.tiff", units="in", width=8, height=7, res=800)
data %>% filter(!Age %in% c("u55", "u60", "u65", "u70")) %>% 
  filter(Country=="Wales" & Date==as.Date("2021-03-15") | Country=="Scotland" |
           Country=="England" & Date==as.Date("2021-03-14")) %>% 
  ggplot(aes(x=IMD, y=Vaxprop, group=Country, colour=Country))+
  geom_line()+
  scale_x_continuous(breaks=c(2,9), labels=c("Least\ndeprived", "Most\ndeprived"),
                     name="Deprivation")+
  scale_y_continuous(labels=label_percent(accuracy=1), name="Proportion of adults vaccinated",
                     limits=c(0,1))+
  scale_colour_paletteer_d("wesanderson::Darjeeling1")+
  facet_wrap(~Age)+
  theme_classic()+
  theme(strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)),
        axis.text.x=element_text(size=rel(1)), text=element_text(family="Roboto"),
        plot.title=element_text(face="bold", size=rel(1.4)))+
  labs(title="Scotland vaccinated more deprived groups first",
       subtitle="Vaccine delivery rates by age and deprivation. Data from mid-March.",
       caption="Data from NHS England, PHS & PHW | Plot by @VictimOfMaths")
dev.off()

#England only
agg_tiff("Outputs/COVIDVaxIneqEng.tiff", units="in", width=8, height=7, res=800)
data %>% filter(Country=="England" & !Age %in% c("u55", "u60", "u65", "u70")) %>% 
  ggplot(aes(x=IMD, y=Vaxprop, group=Date, colour=as.Date(Date)))+
  geom_line()+
  facet_wrap(~Age)+
  scale_x_continuous(breaks=c(2,9), labels=c("Least\ndeprived", "Most\ndeprived"),
                     name="Deprivation")+
  scale_y_continuous(labels=label_percent(accuracy=1), name="Proportion of adults vaccinated",
                     limits=c(0,1))+
  scale_colour_paletteer_c("viridis::magma", name="Date", direction=-1, trans="date")+
  facet_wrap(~Age)+
  theme_classic()+
  theme(strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)),
        axis.text.x=element_text(size=rel(1)), text=element_text(family="Roboto"),
        plot.title=element_text(face="bold", size=rel(1.4)),
        legend.text=element_text())+
  labs(title="Vaccination rates in more deprived areas remain lower",
       subtitle="Vaccine delivery rates in England by age and deprivation based on MSOA-level data",
       caption="Data from NHS England | Plot by @VictimOfMaths")
dev.off()