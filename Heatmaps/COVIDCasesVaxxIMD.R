rm(list=ls())

library(tidyverse)
library(curl)
library(lubridate)
library(RcppRoll)
library(readxl)
library(gtools)
library(paletteer)
library(extrafont)
library(ragg)
library(scales)
library(forcats)

options(scipen=99999)

theme_custom <- function() {
  theme_classic() %+replace%
    theme(plot.title.position="plot", plot.caption.position="plot",
          strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)),
          plot.title=element_text(face="bold", size=rel(1.5), hjust=0,
                                  margin=margin(0,0,5.5,0)),
          text=element_text(family="Lato"))
}

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

#Merge into IMD data
IMD <- merge(IMD, pop)

#Calculate IMD rank at MSOA level as weighted average of LSOA level ranks, weight by population
IMD_MSOA <- IMD %>% 
  group_by(MSOA11CD, LAD17CD) %>% 
  summarise(IMDrank=weighted.mean(IMDrank, pop), pop=sum(pop)) %>% 
  ungroup() %>% 
  mutate(decile=quantcut(IMDrank, q=10, labels=FALSE))

IMD_LTLA <- IMD %>% 
  mutate(LAD17CD=case_when(
    LAD17CD %in% c("E07000049", "E07000050", "E07000051", "E07000052", "E07000053") ~ "E06000059",
    LAD17CD %in% c("E06000028", "E06000029", "E07000048") ~ "E06000058",
    LAD17CD %in% c("E07000201", "E07000204") ~ "E07000245",
    LAD17CD %in% c("E07000205", "E07000206") ~ "E07000244",
    LAD17CD %in% c("E07000190", "E07000191") ~ "E07000246",
    TRUE ~ LAD17CD)) %>% 
  group_by(LAD17CD)%>% 
  summarise(IMDrank=weighted.mean(IMDrank, pop), pop=sum(pop)) %>% 
  ungroup() %>% 
  mutate(decile=quantcut(IMDrank, q=10, labels=FALSE))

pop_LTLA <- pop_full %>% 
  gather(age, pop, c(2:ncol(.))) %>% 
  mutate(age=case_when(
    age<5 ~ "00_04", age<10 ~ "05_09", age<15 ~ "10_14", age<20 ~ "15_19", age<25 ~ "20_24",
    age<30 ~ "25_29", age<35 ~ "30_34", age<40 ~ "35_39", age<45 ~ "40_44", age<50 ~ "45_49",
    age<55 ~ "50_54", age<60 ~ "55_59", age<65 ~ "60_64", age<70 ~ "65_69", age<75 ~ "70_74",
    age<80 ~ "75_79", age<85 ~ "80_84", age<90 ~ "85_89", TRUE ~ "90+")) %>% 
  group_by(age, LSOA11CD) %>% 
  summarise(pop=sum(pop)) %>% 
  ungroup() %>% 
  merge(lookup) %>% 
  mutate(LAD17CD=case_when(
    LAD17CD %in% c("E07000049", "E07000050", "E07000051", "E07000052", "E07000053") ~ "E06000059",
    LAD17CD %in% c("E06000028", "E06000029", "E07000048") ~ "E06000058",
    LAD17CD %in% c("E07000201", "E07000204") ~ "E07000245",
    LAD17CD %in% c("E07000205", "E07000206") ~ "E07000244",
    LAD17CD %in% c("E07000190", "E07000191") ~ "E07000246",
    TRUE ~ LAD17CD)) %>% 
  group_by(age, LAD17CD) %>% 
  summarise(pop=sum(pop)) %>% 
  ungroup()

pop_LTLA_vax <- pop_full %>% 
  gather(age, pop, c(2:ncol(.))) %>% 
  mutate(age=case_when(
    age<18 ~ "Under18", age<25 ~ "18-24", age<30 ~ "25-29", age<35 ~ "30-34", age<40 ~ "35-39", 
    age<45 ~ "40-44", age<50 ~ "45-49", age<55 ~ "50-54", age<60 ~ "55-59", age<65 ~ "60-64", 
    age<70 ~ "65-69", age<75 ~ "70-74", age<80 ~ "75-79", TRUE ~ "80+")) %>% 
  group_by(age, LSOA11CD) %>% 
  summarise(pop=sum(pop)) %>% 
  ungroup() %>% 
  merge(lookup) %>% 
  mutate(LAD17CD=case_when(
    LAD17CD %in% c("E07000049", "E07000050", "E07000051", "E07000052", "E07000053") ~ "E06000059",
    LAD17CD %in% c("E06000028", "E06000029", "E07000048") ~ "E06000058",
    LAD17CD %in% c("E07000201", "E07000204") ~ "E07000245",
    LAD17CD %in% c("E07000205", "E07000206") ~ "E07000244",
    LAD17CD %in% c("E07000190", "E07000191") ~ "E07000246",
    TRUE ~ LAD17CD)) %>% 
  group_by(age, LAD17CD) %>% 
  summarise(pop=sum(pop)) %>% 
  ungroup()


#Bring in age-stratified cases by LTLA
source <- "https://api.coronavirus.data.gov.uk/v2/data?areaType=ltla&metric=newCasesBySpecimenDateAgeDemographics&format=csv"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

casedata <- read.csv(temp) %>% 
  select(areaCode, areaName, date, age, cases) %>% 
  filter(!age %in% c("00_59", "60+", "unassigned")) %>% 
  merge(pop_LTLA, by.x=c("age", "areaCode"), by.y=c("age", "LAD17CD")) %>% 
  merge(IMD_LTLA %>% select(-pop), by.x="areaCode", by.y="LAD17CD") %>% 
  mutate(date=as.Date(date), decile=as.factor(decile)) %>% 
  #collapse to IMD deciles
  group_by(date, age, decile) %>% 
  summarise(cases=sum(cases), pop=sum(pop)) %>% 
  ungroup() %>% 
  #age-standardise
  mutate(caserate=cases*100000/pop,
         stdpop=case_when(
           age %in% c("00_04", "70_74") ~ 5000, 
           age %in% c("05_09", "10_14", "15_19", "65_69") ~ 5500,
           age %in% c("20_24", "25_29", "60_64") ~ 6000,
           age %in% c("30_34", "55_59") ~ 6500,
           age %in% c("35_39", "40_44", "45_49", "50_54") ~ 7000,
           age=="75_79" ~ 4000,
           age=="80_84" ~ 2500,
           age=="85_89" ~ 1500,
           age=="90+" ~ 1000)) %>% 
  group_by(date, decile) %>% 
  arrange(date, decile) %>% 
  summarise(as_caserate=weighted.mean(caserate, stdpop)) %>% 
  ungroup() %>% 
  group_by(decile) %>% 
  mutate(as_caserate_roll=roll_mean(as_caserate, 7, align="center", fill=NA)) %>% 
  ungroup()
  
agg_tiff("Outputs/COVIDCasesxIMDAS.tiff", units="in", width=8, height=6, res=800)
ggplot(casedata, aes(x=as.Date(date), y=as_caserate_roll, colour=decile))+
  geom_line()+
  scale_x_date(name="")+
  scale_y_continuous(name="Cases per 100,000 (age-standardised)")+
  scale_colour_paletteer_d("dichromat::BrowntoBlue_10", name="",
                           labels=c("1 - Most deprived","2","3","4","5","6","7","8","9",
                                    "10 - least deprived"))+
  theme_custom()
dev.off()

agg_tiff("Outputs/COVIDCasesxIMDASRecent.tiff", units="in", width=8, height=6, res=800)
ggplot(casedata %>% filter(date>as.Date("2021-06-01")), 
       aes(x=as.Date(date), y=as_caserate_roll, colour=decile))+
  geom_line()+
  scale_x_date(name="")+
  scale_y_continuous(name="Cases per 100,000 (age-standardised)")+
  scale_colour_paletteer_d("dichromat::BrowntoBlue_10", name="",
                           labels=c("1 - Most deprived","2","3","4","5","6","7","8","9",
                                    "10 - least deprived"))+
  theme_custom()+
  labs(title="Omicron has hit more deprived areas harder",
       subtitle="Rolling 7-day average rate of age-standardised new COVID cases by decile of the Index of Multiple Deprivation",
       caption="Data from coronavirus.data.gov.uk and ONS | Plot by @VictimOfMaths")
dev.off()

#LA-level age-standardised cases
casedata_LA <- read.csv(temp) %>% 
  select(areaCode, areaName, date, age, cases) %>% 
  filter(!age %in% c("00_59", "60+", "unassigned")) %>% 
  merge(pop_LTLA, by.x=c("age", "areaCode"), by.y=c("age", "LAD17CD")) %>% 
  merge(IMD_LTLA %>% select(-pop), by.x="areaCode", by.y="LAD17CD") %>% 
  mutate(date=as.Date(date), decile=as.factor(decile)) %>% 
  #age-standardise
  mutate(caserate=cases*100000/pop,
         stdpop=case_when(
           age %in% c("00_04", "70_74") ~ 5000, 
           age %in% c("05_09", "10_14", "15_19", "65_69") ~ 5500,
           age %in% c("20_24", "25_29", "60_64") ~ 6000,
           age %in% c("30_34", "55_59") ~ 6500,
           age %in% c("35_39", "40_44", "45_49", "50_54") ~ 7000,
           age=="75_79" ~ 4000,
           age=="80_84" ~ 2500,
           age=="85_89" ~ 1500,
           age=="90+" ~ 1000)) %>% 
  group_by(date, areaCode, areaName) %>% 
  arrange(date) %>% 
  summarise(as_caserate=weighted.mean(caserate, stdpop)) %>% 
  ungroup() %>% 
  group_by(areaName, areaCode) %>% 
  mutate(as_caserate_roll=roll_mean(as_caserate, 7, align="center", fill=NA)) %>% 
  ungroup()


###############################
#Calculate age-standardised vaccination rates by IMD

#Download data from NHS England
source <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2022/01/COVID-19-weekly-announced-vaccinations-13-January-2022.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

vaxdata_LA <- read_excel(temp, sheet="LTLA", range="F15:AZ321", col_names=FALSE) %>% 
  select(c(1:17, 19:32, 34:47)) %>% 
  set_names("LAD17CD", "areaName", "12-15_1st", "16-17_1st", "18-24_1st", "25-29_1st", "30-34_1st", 
            "35-39_1st", "40-44_1st", "45-49_1st", "50-54_1st", "55-59_1st", "60-64_1st", 
            "65-69_1st", "70-74_1st", "75-79_1st", "80+_1st", "Under18_2nd", "18-24_2nd", 
            "25-29_2nd", "30-34_2nd", "35-39_2nd", "40-44_2nd", "45-49_2nd", "50-54_2nd", 
            "55-59_2nd", "60-64_2nd", "65-69_2nd", "70-74_2nd", "75-79_2nd", "80+_2nd", 
            "Under18_3rd", "18-24_3rd", "25-29_3rd", "30-34_3rd", "35-39_3rd",  "40-44_3rd", 
            "45-49_3rd", "50-54_3rd", "55-59_3rd", "60-64_3rd", "65-69_3rd", "70-74_3rd", 
            "75-79_3rd", "80+_3rd") %>% 
  mutate(`Under18_1st`=`12-15_1st`+`16-17_1st`) %>% 
  select(-c("12-15_1st", "16-17_1st")) %>% 
  pivot_longer(cols=c(3:44), names_to=c("age", "dose"), names_sep="_", values_to="vaxxed") %>% 
  merge(pop_LTLA_vax) %>% 
  mutate(vaxrate=vaxxed/pop,
         stdpop=case_when(
           age=="Under18" ~ 5000+5500+5500+5500*4/5,
           age=="18-24" ~ 5500*4/5+6000,
           age %in% c("25-29", "60-64") ~ 6000,
           age %in% c("30-34", "55-59") ~ 6500,
           age %in% c("35-39", "40-44", "45-49", "50-54") ~ 7000,
           age=="65-69" ~ 5500,
           age=="70-74" ~ 5000,
           age=="75-79" ~ 4000,
           age=="80+" ~ 2500+1500+1000)) %>% 
  group_by(LAD17CD, dose) %>% 
  summarise(as_vaxrate=weighted.mean(vaxrate, stdpop)) %>% 
  ungroup() 

LAcasesxvax <- casedata_LA %>% 
  filter(date==max(date[!is.na(as_caserate_roll)])) %>% 
  merge(vaxdata_LA, by.x="areaCode", by.y="LAD17CD") %>% 
  merge(IMD_LTLA, by.x="areaCode", by.y="LAD17CD")

agg_tiff("Outputs/COVIDCasesxVaxIMDAS.tiff", units="in", width=12, height=6, res=800)
ggplot(LAcasesxvax, aes(x=as_vaxrate, y=as_caserate_roll, fill=as.factor(decile)))+
  geom_point(shape=21)+
  scale_y_continuous(name="Cases per 100,000 (age-standardised)", limits=c(0,NA))+
  scale_x_continuous(name="Age-standardised vaccination rate (two doses)",
                     labels=label_percent(accuracy=1))+
  scale_fill_paletteer_d("dichromat::BrowntoBlue_10", name="",
                           labels=c("1 - Most deprived","2","3","4","5","6","7","8","9",
                                    "10 - least deprived"))+
  facet_wrap(~dose)+
  theme_custom()+
  labs(title="Less deprived Local Authorities have higher vaccination rates, but more cases",
       subtitle="Rolling 7-day average age-standardised rate of new COVID cases and age-standardised vaccination rates for\nEnglish Local Authorities",
       caption="Data from coronavirus.data.gov.uk, NHS England and ONS | Plot by @VictimOfMaths")

dev.off()

agg_tiff("Outputs/COVIDCasesxVaxIMDAS3rdDose.tiff", units="in", width=8, height=6, res=800)
ggplot(LAcasesxvax %>% filter(dose=="3rd"), 
       aes(x=as_vaxrate, y=as_caserate_roll, fill=as.factor(decile)))+
  geom_point(shape=21)+
  scale_y_continuous(name="Cases per 100,000 (age-standardised)", limits=c(0,NA))+
  scale_x_continuous(name="Age-standardised vaccination rate (two doses)",
                     labels=label_percent(accuracy=1))+
  scale_fill_paletteer_d("dichromat::BrowntoBlue_10", name="",
                         labels=c("1 - Most deprived","2","3","4","5","6","7","8","9",
                                  "10 - least deprived"))+
  theme_custom()+
  labs(title="More deprived areas have lower booster coverage and more cases",
       subtitle="Rolling 7-day average age-standardised rate of new COVID cases and age-standardised 3rd dose/booster vaccination\nrates for English Local Authorities",
       caption="Data from coronavirus.data.gov.uk, NHS England and ONS | Plot by @VictimOfMaths")

dev.off()
