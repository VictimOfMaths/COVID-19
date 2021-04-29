rm(list=ls())

library(tidyverse)
library(curl)
library(readxl)
library(sf)
library(scales)
library(ragg)
library(extrafont)
library(ggrepel)
library(gganimate)

#Read in data for vax rates over 50, available since 25th March
#https://www.england.nhs.uk/statistics/statistical-work-areas/covid-19-vaccinations/

#25th March file
temp1 <- tempfile()
url1 <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/03/COVID-19-weekly-announced-vaccinations-25-March-2021-revised.xlsx"
temp1 <- curl_download(url=url1, destfile=temp1, quiet=FALSE, mode="wb")

data1 <- read_excel(temp1, sheet="MSOA", range="F16:O6806", col_names=FALSE) %>% 
  rename(msoa11cd=`...1`, msoa11nm=`...2`, `<50`=`...3`,  `50-54`=`...4`, `55-59`=`...5`, 
         `60-64`=`...6`, `65-69`=`...7`, 
         `70-74`=`...8`, `75-79`=`...9`, `80+`=`...10`) %>% 
  gather(age, vaccinated, c(3:10)) %>% 
  mutate(date=as.Date("2021-03-21"))

pop1 <- read_excel(temp1, sheet="Population estimates (NIMS)", range="O16:Y6806", col_names=FALSE) %>% 
  select(-c(2)) %>% 
  rename(msoa11cd=`...1`) %>% 
  gather(age, pop, c(2:10)) %>% 
  mutate(age=case_when(
    age=="...3" ~ "<16",
    age=="...4" ~ "<50",
    age=="...5" ~ "50-54",
    age=="...6" ~ "55-59",
    age=="...7" ~ "60-64",
    age=="...8" ~ "65-69",
    age=="...9" ~ "70-74",
    age=="...10" ~ "75-79",
    TRUE ~ "80+"))

data1 <- merge(data1, pop1)

#1st April file
temp2 <- tempfile()
url2 <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/04/COVID-19-weekly-announced-vaccinations-1-April-2021.xlsx"
temp2 <- curl_download(url=url2, destfile=temp2, quiet=FALSE, mode="wb")

data2 <- read_excel(temp2, sheet="MSOA", range="F16:O6806", col_names=FALSE) %>% 
  rename(msoa11cd=`...1`, msoa11nm=`...2`, `<50`=`...3`,  `50-54`=`...4`, `55-59`=`...5`, 
         `60-64`=`...6`, `65-69`=`...7`, 
         `70-74`=`...8`, `75-79`=`...9`, `80+`=`...10`) %>% 
  gather(age, vaccinated, c(3:10)) %>% 
  mutate(date=as.Date("2021-03-28"))

pop2 <- read_excel(temp2, sheet="Population estimates (NIMS)", range="O16:Y6806", col_names=FALSE) %>% 
  select(-c(2)) %>% 
  rename(msoa11cd=`...1`) %>% 
  gather(age, pop, c(2:10)) %>% 
  mutate(age=case_when(
    age=="...3" ~ "<16",
    age=="...4" ~ "<50",
    age=="...5" ~ "50-54",
    age=="...6" ~ "55-59",
    age=="...7" ~ "60-64",
    age=="...8" ~ "65-69",
    age=="...9" ~ "70-74",
    age=="...10" ~ "75-79",
    TRUE ~ "80+"))

data2 <- merge(data2, pop2)

#8th April file
temp3 <- tempfile()
url3 <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/04/COVID-19-weekly-announced-vaccinations-8-April-2021.xlsx"
temp3 <- curl_download(url=url3, destfile=temp3, quiet=FALSE, mode="wb")

data3 <- read_excel(temp3, sheet="MSOA", range="F16:O6806", col_names=FALSE) %>% 
  rename(msoa11cd=`...1`, msoa11nm=`...2`, `<50`=`...3`,  `50-54`=`...4`, `55-59`=`...5`, 
         `60-64`=`...6`, `65-69`=`...7`, 
         `70-74`=`...8`, `75-79`=`...9`, `80+`=`...10`) %>% 
  gather(age, vaccinated, c(3:10)) %>% 
  mutate(date=as.Date("2021-04-04"))

pop3 <- read_excel(temp3, sheet="Population estimates (NIMS)", range="O16:Y6806", col_names=FALSE) %>% 
  select(-c(2)) %>% 
  rename(msoa11cd=`...1`) %>% 
  gather(age, pop, c(2:10)) %>% 
  mutate(age=case_when(
    age=="...3" ~ "<16",
    age=="...4" ~ "<50",
    age=="...5" ~ "50-54",
    age=="...6" ~ "55-59",
    age=="...7" ~ "60-64",
    age=="...8" ~ "65-69",
    age=="...9" ~ "70-74",
    age=="...10" ~ "75-79",
    TRUE ~ "80+"))

data3 <- merge(data3, pop3)


#15th April file
temp4 <- tempfile()
url4 <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/04/COVID-19-weekly-announced-vaccinations-15-April-2021.xlsx"
temp4 <- curl_download(url=url4, destfile=temp4, quiet=FALSE, mode="wb")

data4 <- read_excel(temp4, sheet="MSOA", range="F16:O6806", col_names=FALSE) %>% 
  rename(msoa11cd=`...1`, msoa11nm=`...2`, `<50`=`...3`,  `50-54`=`...4`, `55-59`=`...5`, 
         `60-64`=`...6`, `65-69`=`...7`, 
         `70-74`=`...8`, `75-79`=`...9`, `80+`=`...10`) %>% 
  gather(age, vaccinated, c(3:10)) %>% 
  mutate(date=as.Date("2021-04-11"))

pop4 <- read_excel(temp4, sheet="Population estimates (NIMS)", range="O16:Y6806", col_names=FALSE) %>% 
  select(-c(2)) %>% 
  rename(msoa11cd=`...1`) %>% 
  gather(age, pop, c(2:10)) %>% 
  mutate(age=case_when(
    age=="...3" ~ "<16",
    age=="...4" ~ "<50",
    age=="...5" ~ "50-54",
    age=="...6" ~ "55-59",
    age=="...7" ~ "60-64",
    age=="...8" ~ "65-69",
    age=="...9" ~ "70-74",
    age=="...10" ~ "75-79",
    TRUE ~ "80+"))

data4 <- merge(data4, pop4)

#22nd April file
temp5 <- tempfile()
url5 <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/04/COVID-19-weekly-announced-vaccinations-22-April-2021.xlsx"
temp5 <- curl_download(url=url5, destfile=temp5, quiet=FALSE, mode="wb")

data5 <- read_excel(temp5, sheet="MSOA", range="F16:P6806", col_names=FALSE) %>% 
  rename(msoa11cd=`...1`, msoa11nm=`...2`, `<45`=`...3`,  `45-49`=`...4`, `50-54`=`...5`, 
         `55-59`=`...6`, `60-64`=`...7`, 
         `65-69`=`...8`, `70-74`=`...9`, `75-79`=`...10`, `80+`=`...11`) %>% 
  gather(age, vaccinated, c(3:11)) %>% 
  mutate(date=as.Date("2021-04-18"),
         age=if_else(age %in% c("<45", "45-49"), "<50", age)) %>% 
  group_by(age, msoa11cd, msoa11nm, date) %>% 
  summarise(vaccinated=sum(vaccinated)) %>% 
  ungroup()

pop5 <- read_excel(temp5, sheet="Population estimates (NIMS)", range="R16:AC6806", col_names=FALSE) %>% 
  select(-c(2)) %>% 
  rename(msoa11cd=`...1`) %>% 
  gather(age, pop, c(2:11)) %>% 
  mutate(age=case_when(
    age=="...3" ~ "<16",
    age %in% c("...4", "...5") ~ "<50",
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

data5 <- merge(data5, pop5)

#29th April file
temp6 <- tempfile()
url6 <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/04/COVID-19-weekly-announced-vaccinations-29-April-2021.xlsx"
temp6 <- curl_download(url=url6, destfile=temp6, quiet=FALSE, mode="wb")

data6 <- read_excel(temp6, sheet="MSOA", range="F16:P6806", col_names=FALSE) %>% 
  rename(msoa11cd=`...1`, msoa11nm=`...2`, `<45`=`...3`,  `45-49`=`...4`, `50-54`=`...5`, 
         `55-59`=`...6`, `60-64`=`...7`, 
         `65-69`=`...8`, `70-74`=`...9`, `75-79`=`...10`, `80+`=`...11`) %>% 
  gather(age, vaccinated, c(3:11)) %>% 
  mutate(date=as.Date("2021-04-25"),
         age=if_else(age %in% c("<45", "45-49"), "<50", age)) %>% 
  group_by(age, msoa11cd, msoa11nm, date) %>% 
  summarise(vaccinated=sum(vaccinated)) %>% 
  ungroup()

pop6 <- read_excel(temp6, sheet="Population estimates (NIMS)", range="R16:AC6806", col_names=FALSE) %>% 
  select(-c(2)) %>% 
  rename(msoa11cd=`...1`) %>% 
  gather(age, pop, c(2:11)) %>% 
  mutate(age=case_when(
    age=="...3" ~ "<16",
    age %in% c("...4", "...5") ~ "<50",
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

data6 <- merge(data6, pop6)

#Merge them all together
data <- bind_rows(data1, data2, data3, data4, data5, data6) %>% 
  mutate(vaxprop=vaccinated/pop)

#Calculate IMD at MSOA level
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
  ungroup() 

data_IMD <- data %>% 
  merge(IMD_MSOA %>% select(-pop), by.x="msoa11cd", by.y="MSOA11CD")

#Download Carl Baker's lovely cartogram
msoa <- tempfile()
source <- ("https://github.com/houseofcommonslibrary/uk-hex-cartograms-noncontiguous/raw/main/geopackages/MSOA.gpkg")
msoa <- curl_download(url=source, destfile=msoa, quiet=FALSE, mode="wb")

BackgroundMSOA <- st_read(msoa, layer="5 Background")

MSOA <- st_read(msoa, layer="4 MSOA hex") %>% 
  left_join(data_IMD, by="msoa11cd")

GroupsMSOA <- st_read(msoa, layer="2 Groups")

Group_labelsMSOA <- st_read(msoa, layer="1 Group labels") %>% 
  mutate(just=if_else(LabelPosit=="Left", 0, 1))

LAsMSOA <- st_read(msoa, layer="3 Local authority outlines (2019)")

#Local analysis
scatterdata <- MSOA %>% 
  filter(Laname=="Sheffield" & age %in% c("80+", "75-79", "70-74", "65-69", "60-64",
                                          "55-59", "50-54")) %>%
  group_by(msoa11cd, MSOA.name.HCL, IMDrank, date) %>% 
  summarise(vaccinated=sum(vaccinated), pop=sum(pop)) %>% 
  ungroup() %>% 
  mutate(vaxprop=vaccinated/pop) %>% 
  group_by(msoa11cd) %>% 
  mutate(labels=if_else(max(vaxprop)<0.8, unique(MSOA.name.HCL), "")) %>% 
  ungroup()

Sheffanim <- ggplot(scatterdata, aes(x=vaxprop, y=-IMDrank))+
  geom_point(aes(size=pop), shape=21, colour="DarkRed", fill="tomato", alpha=0.8)+
  geom_segment(aes(x=1, xend=1, y=-1000, yend=-32000), colour="Grey70")+
  geom_text_repel(aes(label=labels), family="Roboto", size=rel(3),
                  box.padding = 0.4, seed=19)+
  scale_x_continuous(name="Proportion of population aged 50+ vaccinated", 
                     labels=label_percent(accuracy=1), limits=c(NA, 1))+
  scale_y_continuous(name="Index of Multiple Deprivation rank", breaks=c(-1000, -32000),
                     labels=c("Most deprived", "Least deprived"))+
  scale_size_continuous(name="Over 50\npopulation")+
  theme_classic()+
  theme(axis.ticks.y=element_blank(), text=element_text(family="Roboto"),
        axis.text.y=element_text(size=rel(1.2), colour="Black"),
        plot.title.position="plot", plot.title=element_text(face="bold", size=rel(1.4)),
        plot.subtitle=element_text(colour="Grey50"), plot.caption.position ="plot",
        plot.caption=element_text(colour="Grey50"))+
  ggtitle("Vaccine delivery is lowest in a small number of deprived areas in Sheffield",
          subtitle="Proportion of adults aged 50+ who had received at least one dose of COVID-19 vaccine by {closest_state}")+
  labs(caption="Data from NHS England, populations from NIMS\nPlot by @VictimOfMaths")+
  transition_states(date, transition_length=2, state_length=1, wrap=FALSE)

animate(Sheffanim, units="in", width=8, height=8*4/5, res=500, 
        renderer=gifski_renderer("Outputs/COVIDVaxSheffScatterAnim.gif"), 
        device="ragg_png", end_pause=5, duration=10, fps=8)