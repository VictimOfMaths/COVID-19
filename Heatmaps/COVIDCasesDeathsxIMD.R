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
  group_by(LAD17CD)%>% 
  summarise(IMDrank=weighted.mean(IMDrank, pop), pop=sum(pop)) %>% 
  ungroup() %>% 
  mutate(decile=quantcut(IMDrank, q=10, labels=FALSE))

#####################
#Analysis of population age by IMD decile (at LSOA level)
u18xIMD <- pop_full %>% 
  gather(age, pop, c(2:ncol(.))) %>% 
  mutate(age=as.numeric(age)) %>% 
  filter(age<18) %>% 
  mutate(ageband=case_when(
    age<12 ~ "Under 12", 
    age<16 ~ "12-15",
    TRUE ~ "16-17")) %>% 
  merge(IMD %>% select(LSOA11CD, IMDdecile)) %>% 
  group_by(IMDdecile, ageband) %>% 
  summarise(pop=sum(pop)) %>% 
  ungroup() %>% 
  group_by(IMDdecile) %>% 
  mutate(total=sum(pop)) %>% 
  ungroup() %>% 
  mutate(prop=pop/total, ageband=factor(ageband, levels=c("Under 12", "12-15", "16-17")))

agg_tiff("Outputs/EngU18Pop.tiff", units="in", width=9, height=6, res=500)
ggplot(u18xIMD, aes(x=prop, y=as.factor(IMDdecile), fill=fct_rev(ageband)))+
  geom_col(position="stack")+
  scale_x_continuous(name="Proportion of under 18s", labels=label_percent(accuracy=1))+
  scale_y_discrete(name="IMD decile", labels=c("1 - Most deprived","2","3","4","5","6","7","8","9",
                            "10 - least deprived"))+
  scale_fill_paletteer_d("ggthemr::solarized", name="", guide=guide_legend(reverse=TRUE))+
  theme_custom()+
  theme(legend.position="top")+
  labs(title="You would expect under 18 vaccination rates in deprived areas to be *slightly* lower",
       subtitle="Age distribution of the under 18s in England by deprivation decile",
       caption="Population data from ONS 2020 estimates | Plot by @VictimOfMaths")
dev.off()

#Bring in MSOA level case data
temp.cases <- tempfile()
caseurl <- "https://api.coronavirus.data.gov.uk/v2/data?areaType=msoa&metric=newCasesBySpecimenDateRollingSum&format=csv"
temp.cases <- curl_download(url=caseurl, destfile=temp.cases, quiet=FALSE, mode="wb")

cases <- read.csv(temp.cases) %>% 
  select(areaCode, date, newCasesBySpecimenDateRollingSum) %>% 
  set_names("MSOA11CD", "date", "cases") %>% 
  mutate(date=as.Date(date)) %>% 
  merge(IMD_MSOA) %>% 
  group_by(decile, date) %>% 
  summarise(cases=sum(cases), pop=sum(pop)) %>% 
  ungroup() %>% 
  group_by(decile) %>% 
  mutate(caserate=cases*100000/pop) %>% 
  ungroup()

cases_MSOA <- read.csv(temp.cases) %>% 
  select(areaCode, date, newCasesBySpecimenDateRollingSum) %>% 
  set_names("MSOA11CD", "date", "cases") %>% 
  mutate(date=as.Date(date)) %>% 
  filter(date==max(date)) %>% 
  merge(IMD_MSOA) %>% 
  rename(allpop=pop)

agg_tiff("Outputs/COVIDCasesxIMD.tiff", units="in", width=8, height=6, res=800)
ggplot(cases, aes(x=date, y=caserate, colour=as.factor(decile)))+
  geom_line()+
  scale_x_date(name="")+
  scale_y_continuous(name="Weekly new cases per 100,000 people", limits=c(0,NA))+
  scale_colour_paletteer_d("dichromat::BrowntoBlue_10", name="",
                           labels=c("1 - Most deprived","2","3","4","5","6","7","8","9",
                                    "10 - least deprived"))+
  theme_custom()+
  labs(title="The socioeconomic profile of COVID cases has recently reversed",
       subtitle="Weekly rate of new COVID cases in England based on MSOA-level data.\nCase data is not available for neighbourhoods with fewer than 3 cases in any given week.",
       caption="Data from coronavirus.data.gov.uk | Plot by @VictimOfMaths")

dev.off()

agg_tiff("Outputs/COVIDCasesxIMDRecent.tiff", units="in", width=8, height=6, res=800)
ggplot(cases %>% filter(date>as.Date("2021-05-01")), 
       aes(x=date, y=caserate, colour=as.factor(decile)))+
  geom_line()+
  scale_x_date(name="")+
  scale_y_continuous(name="Weekly new cases per 100,000 people", limits=c(0,NA))+
  scale_colour_paletteer_d("dichromat::BrowntoBlue_10", name="",
                           labels=c("1 - Most deprived","2","3","4","5","6","7","8","9",
                                    "10 - least deprived"))+
  theme_custom()+
  labs(title="The socioeconomic profile of COVID cases has recently reversed",
       subtitle="Weekly rate of new COVID cases in England based on MSOA-level data.\nCase data is not available for neighbourhoods with fewer than 3 cases in any given week.",
       caption="Data from coronavirus.data.gov.uk | Plot by @VictimOfMaths")

dev.off()

agg_tiff("Outputs/COVIDCasesxIMDRecentArea.tiff", units="in", width=8, height=6, res=800)
ggplot(cases %>% filter(date>as.Date("2021-05-01")), 
       aes(x=date, y=cases, fill=as.factor(decile)))+
  geom_area(position="fill")+
  scale_x_date(name="")+
  scale_y_continuous(name="Proportion of new cases", limits=c(0,NA), 
                     labels=label_percent(accuracy=1))+
  scale_fill_paletteer_d("dichromat::BrowntoBlue_10", name="",
                         labels=c("1 - Most deprived","2","3","4","5","6","7","8","9",
                                  "10 - least deprived"))+
  theme_custom()+
  labs(title="The socioeconomic profile of COVID cases has recently reversed",
       subtitle="Proportion of new COVID cases by deprivation decile in England based on MSOA-level data.\nCase data is not available for neighbourhoods with fewer than 3 cases in any given week.",
       caption="Data from coronavirus.data.gov.uk | Plot by @VictimOfMaths")

dev.off()

agg_tiff("Outputs/COVIDCasesxIMDArea.tiff", units="in", width=8, height=6, res=800)
ggplot(cases %>% filter(date>as.Date("2020-04-01")), 
       aes(x=date, y=cases, fill=as.factor(decile)))+
  geom_area(position="fill")+
  scale_x_date(name="")+
  scale_y_continuous(name="Proportion of new cases", limits=c(0,NA), 
                     labels=label_percent(accuracy=1))+
  scale_fill_paletteer_d("dichromat::BrowntoBlue_10", name="",
                         labels=c("1 - Most deprived","2","3","4","5","6","7","8","9",
                                  "10 - least deprived"))+
  theme_custom()+
  labs(title="The socioeconomic profile of COVID cases has recently reversed",
       subtitle="Proportion of new COVID cases by deprivation decile in England based on MSOA-level data.\nCase data is not available for neighbourhoods with fewer than 3 cases in any given week.",
       caption="Data from coronavirus.data.gov.uk | Plot by @VictimOfMaths")

dev.off()

#Repeat using LA-level data
source <- ("https://api.coronavirus.data.gov.uk/v2/data?areaType=ltla&metric=newCasesBySpecimenDate&metric=newDeaths28DaysByDeathDate&format=csv")
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

LAcases <- read.csv(temp) %>% 
  rename(cases=newCasesBySpecimenDate, deaths=newDeaths28DaysByDeathDate) %>%  
  mutate(date=as.Date(date)) %>% 
  merge(IMD_LTLA, by.x="areaCode", by.y="LAD17CD") %>% 
  group_by(decile, date) %>% 
  summarise(cases=sum(cases), deaths=sum(deaths), pop=sum(pop)) %>% 
  ungroup() %>% 
  group_by(decile) %>% 
  mutate(caserate=cases*100000/pop,
         deathrate=deaths*100000/pop,
         caserate_roll=roll_mean(caserate, 7, align="center", fill=NA),
         deathrate_roll=roll_mean(deathrate, 7, align="center", fill=NA)) %>% 
  ungroup() 

agg_tiff("Outputs/COVIDCasesxIMDLTLA.tiff", units="in", width=8, height=6, res=800)
ggplot(LAcases, aes(x=date, y=caserate_roll, colour=as.factor(decile)))+
  geom_line()+
  scale_x_date(name="")+
  scale_y_continuous(name="Daily new cases per 100,000 people", limits=c(0,NA))+
  scale_colour_paletteer_d("dichromat::BrowntoBlue_10", name="",
                           labels=c("1 - Most deprived","2","3","4","5","6","7","8","9",
                                    "10 - least deprived"))+
  theme_custom()+
  labs(title="The socioeconomic profile of COVID cases has recently reversed",
       subtitle="Rolling 7-day average daily rate of new COVID cases in England based on Local Authority-level data.\nLocal Authorities are allocated to deprivation deciles based on their average IMD scores.",
       caption="Data from coronavirus.data.gov.uk | Plot by @VictimOfMaths")

dev.off()

agg_tiff("Outputs/COVIDCasesxIMDAreaLTLA.tiff", units="in", width=8, height=6, res=800)
ggplot(LAcases %>% filter(date>as.Date("2020-04-01")), 
       aes(x=date, y=caserate_roll, fill=as.factor(decile)))+
  geom_area(position="fill")+
  scale_x_date(name="")+
  scale_y_continuous(name="Proportion of new cases", limits=c(0,NA), 
                     labels=label_percent(accuracy=1))+
  scale_fill_paletteer_d("dichromat::BrowntoBlue_10", name="",
                         labels=c("1 - Most deprived","2","3","4","5","6","7","8","9",
                                  "10 - least deprived"))+
  theme_custom()+
  labs(title="The socioeconomic profile of COVID cases has recently reversed",
       subtitle="Proportion of new COVID cases in England based on Local Authority-level data.\nLocal Authorities are allocated to deprivation deciles based on their average IMD scores.",
       caption="Data from coronavirus.data.gov.uk | Plot by @VictimOfMaths")

dev.off()

agg_tiff("Outputs/COVIDDeathsxIMDLTLA.tiff", units="in", width=8, height=6, res=800)
ggplot(LAcases, aes(x=date, y=deathrate_roll, colour=as.factor(decile)))+
  geom_line()+
  scale_x_date(name="")+
  scale_y_continuous(name="Daily deaths per 100,000 people", limits=c(0,NA))+
  scale_colour_paletteer_d("dichromat::BrowntoBlue_10", name="",
                           labels=c("1 - Most deprived","2","3","4","5","6","7","8","9",
                                    "10 - least deprived"))+
  theme_custom()+
  labs(title="COVID deaths are still highest in more deprived groups",
       subtitle="Rolling 7-day average daily rate of deaths within 28 days of a positive COVID test in England\nbased on Local Authority-level data. Local Authorities are allocated to deprivation deciles based on\nthe average IMD scores of neighbourhoods in the Local Authority.",
       caption="Data from coronavirus.data.gov.uk | Plot by @VictimOfMaths")

dev.off()

agg_tiff("Outputs/COVIDDeathsxIMDAreaLTLA.tiff", units="in", width=8, height=6, res=800)
ggplot(LAcases %>% filter(date>as.Date("2020-04-01")), 
       aes(x=date, y=deathrate_roll, fill=as.factor(decile)))+
  geom_area(position="fill")+
  scale_x_date(name="")+
  scale_y_continuous(name="Proportion of deaths", limits=c(0,NA), 
                     labels=label_percent(accuracy=1))+
  scale_fill_paletteer_d("dichromat::BrowntoBlue_10", name="",
                         labels=c("1 - Most deprived","2","3","4","5","6","7","8","9",
                                  "10 - least deprived"))+
  theme_custom()+
  labs(title="COVID deaths are still highest in more deprived groups",
       subtitle="Proportion of deaths within 28 days of a positive COVID test in England based on Local Authority-level data.\nLocal Authorities are allocated to deprivation deciles based on the average IMD scores of neighbourhoods\nin the Local Authority.",
       caption="Data from coronavirus.data.gov.uk | Plot by @VictimOfMaths")

dev.off()

agg_tiff("Outputs/COVIDDeathsxIMDLTLARecent.tiff", units="in", width=8, height=6, res=800)
ggplot(LAcases %>% filter(date>as.Date("2021-05-01")), 
                          aes(x=date, y=deathrate_roll, colour=as.factor(decile)))+
  geom_line()+
  scale_x_date(name="")+
  scale_y_continuous(name="Daily deaths per 100,000 people", limits=c(0,NA))+
  scale_colour_paletteer_d("dichromat::BrowntoBlue_10", name="",
                           labels=c("1 - Most deprived","2","3","4","5","6","7","8","9",
                                    "10 - least deprived"))+
  theme_custom()+
  labs(title="COVID deaths are still highest in more deprived groups",
       subtitle="Rolling 7-day average daily rate of deaths within 28 days of a positive COVID test in England\nbased on Local Authority-level data. Local Authorities are allocated to deprivation deciles based on\nthe average IMD scores of neighbourhoods in the Local Authority.",
       caption="Data from coronavirus.data.gov.uk | Plot by @VictimOfMaths")

dev.off()

agg_tiff("Outputs/COVIDDeathsxIMDAreaLTLARecent.tiff", units="in", width=8, height=6, res=800)
ggplot(LAcases %>% filter(date>as.Date("2021-05-01")), 
       aes(x=date, y=deathrate_roll, fill=as.factor(decile)))+
  geom_area(position="fill")+
  scale_x_date(name="")+
  scale_y_continuous(name="Proportion of deaths", limits=c(0,NA), 
                     labels=label_percent(accuracy=1))+
  scale_fill_paletteer_d("dichromat::BrowntoBlue_10", name="",
                         labels=c("1 - Most deprived","2","3","4","5","6","7","8","9",
                                  "10 - least deprived"))+
  theme_custom()+
  labs(title="COVID deaths are still highest in more deprived groups",
       subtitle="Proportion of deaths within 28 days of a positive COVID test in England based on Local Authority-level data.\nLocal Authorities are allocated to deprivation deciles based on the average IMD scores of neighbourhoods\nin the Local Authority.",
       caption="Data from coronavirus.data.gov.uk | Plot by @VictimOfMaths")

dev.off()

#Bring in u18 vaccine uptake
url <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/11/COVID-19-weekly-announced-vaccinations-04-November-2021.xlsx"

temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

vaxdata <- read_excel(temp, sheet="MSOA", range="F15:H6803", col_names=FALSE) %>% 
  set_names("MSOA11CD", "areaName", "vaccinated") %>% 
  merge(pop_full %>% select(c(1:19)) %>% 
          gather(age, pop, c(2:19)) %>% 
          merge(lookup) %>% 
          group_by(MSOA11CD) %>% 
          summarise(pop=sum(pop)) %>% 
          ungroup()) %>% 
  merge(IMD_MSOA %>% select(MSOA11CD, decile)) %>% 
  group_by(decile) %>% 
  summarise(vaccinated=sum(vaccinated), vaxpop=sum(pop)) %>% 
  ungroup() %>% 
  mutate(vaxrate=vaccinated/vaxpop)

vaxdata_MSOA <- read_excel(temp, sheet="MSOA", range="F15:H6803", col_names=FALSE) %>% 
  set_names("MSOA11CD", "areaName", "vaccinated") %>% 
  merge(pop_full %>% select(c(1:19)) %>% 
          gather(age, pop, c(2:19)) %>% 
          merge(lookup) %>% 
          group_by(MSOA11CD) %>% 
          summarise(pop=sum(pop)) %>% 
          ungroup()) %>% 
  merge(IMD_MSOA %>% select(MSOA11CD, decile)) %>% 
  mutate(vaxprop=vaccinated/pop)

agg_tiff("Outputs/COVIDVaxU18xIMDONS.tiff", units="in", width=8, height=6, res=500)
ggplot(vaxdata, aes(x=vaxrate, y=as.factor(decile), fill=as.factor(decile)))+
  geom_col(show.legend=FALSE)+
  scale_x_continuous(name="Proportion of under 18s vaccinated", labels=label_percent(accuracy=1))+
  scale_y_discrete(name="IMD decile", labels=c("1 - Most deprived","2","3","4","5","6","7","8","9",
                                               "10 - least deprived"))+
  scale_fill_paletteer_d("dichromat::BrowntoBlue_10", name="",
                         labels=c("1 - Most deprived","2","3","4","5","6","7","8","9",
                                  "10 - least deprived"))+
  theme_custom()+
  labs(title="Child vaccination rates are lowest in deprived areas",
       subtitle="Proportion of under 18s who have received at least one COVID-19 vaccine dose",
       caption="Vaccination data from NHS England, population data from ONS\nPlot by @VictimOfMaths")

dev.off()

agg_tiff("Outputs/COVIDVaxU18AdjxIMDONS.tiff", units="in", width=8, height=6, res=500)
vaxdata %>% merge(u18xIMD %>% filter(ageband=="Under 12") %>% 
                    select(IMDdecile, prop), by.x="decile", by.y="IMDdecile") %>% 
  mutate(vaxprop.adj=vaccinated/(vaxpop*(1-prop))) %>% 
  ggplot(aes(x=vaxprop.adj, y=as.factor(decile), fill=as.factor(decile)))+
  geom_col(show.legend=FALSE)+
  scale_x_continuous(name="Proportion of 12-17 year olds vaccinated", labels=label_percent(accuracy=1))+
  scale_y_discrete(name="IMD decile", labels=c("1 - Most deprived","2","3","4","5","6","7","8","9",
                                               "10 - least deprived"))+
  scale_fill_paletteer_d("dichromat::BrowntoBlue_10", name="",
                         labels=c("1 - Most deprived","2","3","4","5","6","7","8","9",
                                  "10 - least deprived"))+
  theme_custom()+
  labs(title="Child vaccination rates are lowest in deprived areas",
       subtitle="Estimated proportion of 12-17 year olds who have received at least one COVID-19 vaccine dose",
       caption="Vaccination data from NHS England, population data from ONS\nPlot by @VictimOfMaths")
dev.off()

########
#Combine analysis of u18 vax uptake with cases and IMD deciles

combined <- vaxdata_MSOA %>% 
  merge(cases_MSOA) %>% 
  mutate(caserate=cases*100000/allpop)

agg_tiff("Outputs/COVIDVaxU18xCasesxIMDBW.tiff", units="in", width=9, height=6, res=500)
ggplot(combined, aes(x=caserate, y=vaxprop))+
  geom_point(alpha=0.6)+
  geom_smooth(method="lm")+
  scale_x_continuous(name="Cases per 100,000 in the past week")+
  scale_y_continuous(name="Proportion of under 18s vaccinated", labels=label_percent(accuracy=1))+
  theme_custom()+
  labs(title="Areas with higher cases have higher child vaccination rates",
       subtitle="Association between the latest weekly COVID case rates and the proportion of the under 18 population who have received\nat least one vaccine dose",
       caption="Data from NHS England, ONS and coronavirus.data.gov.uk | Plot by @VictimOfMaths")
dev.off()

agg_tiff("Outputs/COVIDVaxU18xCasesxIMD.tiff", units="in", width=9, height=6, res=500)
ggplot(combined, aes(x=caserate, y=vaxprop, colour=as.factor(decile)))+
  geom_point(alpha=0.6)+
  scale_x_continuous(name="Cases per 100,000 in the past week")+
  scale_y_continuous(name="Proportion of under 18s vaccinated", labels=label_percent(accuracy=1))+
  scale_colour_paletteer_d("dichromat::BrowntoBlue_10", name="",
                         labels=c("1 - Most deprived","2","3","4","5","6","7","8","9",
                                  "10 - least deprived"))+
  theme_custom()+
  labs(title="Areas with higher cases are more affluent and have higher child vaccination rates",
       subtitle="Association between the latest weekly COVID case rates and the proportion of the under 18 population\nwho have received at least one vaccine dose, coloured by deprivation decile",
       caption="Data from NHS England, ONS and coronavirus.data.gov.uk | Plot by @VictimOfMaths")
dev.off()

model <- lm(caserate ~ vaxprop + decile, combined)

af <- anova(model)
afss <- af$"Sum Sq"
print(cbind(af,PctExp=afss/sum(afss)*100))

#COVID case rates *by age* and deprivation over time using ltla data
url <- "https://api.coronavirus.data.gov.uk/v2/data?areaType=ltla&metric=newCasesBySpecimenDateAgeDemographics&format=csv"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

cases_age <- read.csv(temp) %>% 
  filter(!age %in% c("00_59", "60+", "unassigned")) %>% 
  mutate(date=as.Date(date)) %>% 
  merge(IMD_LTLA, by.x="areaCode", by.y="LAD17CD") %>% 
  group_by(age, decile, date) %>% 
  summarise(cases=sum(cases), pop=sum(pop)) %>% 
  ungroup() %>% 
  mutate(caserate=cases*100000/pop, 
         caserate_roll=roll_mean(caserate, 7, align="center", fill=NA),
         age=gsub("_", "-", age))

agg_tiff("Outputs/COVIDCasesxAgexIMD.tiff", units="in", width=11, height=8, res=500)
ggplot(cases_age %>% filter(date>=as.Date("2021-05-01")), 
       aes(x=date, y=caserate_roll, colour=as.factor(decile)))+
  geom_line()+
  scale_x_date(name="")+
  scale_y_continuous(name="Daily cases per 100,000")+
  scale_colour_paletteer_d("dichromat::BrowntoBlue_10", name="",
                           labels=c("1 - Most deprived","2","3","4","5","6","7","8","9",
                                    "10 - least deprived"))+
  facet_wrap(~age)+
  theme_custom()+
  labs(title="Cases have risen in less deprived areas in schoolkids and their parents age group",
       subtitle="7-day rolling average rate of new COVID cases by age and IMD decile, based on average deprivation across each Local Authority",
       caption="Data from coronavirus.data.gov.uk & ONS | Plot by @VictimOfMaths")
dev.off()

agg_tiff("Outputs/COVIDCasesxAgexIMDU20.tiff", units="in", width=11, height=6, res=500)
ggplot(cases_age %>% filter(date>=as.Date("2021-05-01") & age %in% c("05-09", "10-14", "15-19")), 
       aes(x=date, y=caserate_roll, colour=as.factor(decile)))+
  geom_line()+
  scale_x_date(name="")+
  scale_y_continuous(name="Daily cases per 100,000")+
  scale_colour_paletteer_d("dichromat::BrowntoBlue_10", name="",
                           labels=c("1 - Most deprived","2","3","4","5","6","7","8","9",
                                    "10 - least deprived"))+
  facet_wrap(~age)+
  theme_custom()+
  labs(title="Recent peaks in COVID cases in children have been in less deprived areas",
       subtitle="7-day rolling average rate of new COVID cases by age and IMD decile, based on average deprivation across each Local Authority",
       caption="Data from coronavirus.data.gov.uk & ONS | Plot by @VictimOfMaths")
dev.off()

agg_tiff("Outputs/COVIDCasesxAgexIMD2039.tiff", units="in", width=11, height=6, res=500)
ggplot(cases_age %>% filter(date>=as.Date("2021-05-01") & 
                              age %in% c("20-24", "25-29", "30-34", "35-39")), 
       aes(x=date, y=caserate_roll, colour=as.factor(decile)))+
  geom_line()+
  scale_x_date(name="")+
  scale_y_continuous(name="Daily cases per 100,000")+
  scale_colour_paletteer_d("dichromat::BrowntoBlue_10", name="",
                           labels=c("1 - Most deprived","2","3","4","5","6","7","8","9",
                                    "10 - least deprived"))+
  facet_wrap(~age)+
  theme_custom()+
  labs(title="The socioeconomic gradient in 20-39 year olds has been more stable",
       subtitle="7-day rolling average rate of new COVID cases by age and IMD decile, based on average deprivation across each Local Authority",
       caption="Data from coronavirus.data.gov.uk & ONS | Plot by @VictimOfMaths")
dev.off()

agg_tiff("Outputs/COVIDCasesxAgexIMD4059.tiff", units="in", width=11, height=6, res=500)
ggplot(cases_age %>% filter(date>=as.Date("2021-05-01") & 
                              age %in% c("40-44", "45-49", "50-54", "55-59")), 
       aes(x=date, y=caserate_roll, colour=as.factor(decile)))+
  geom_line()+
  scale_x_date(name="")+
  scale_y_continuous(name="Daily cases per 100,000")+
  scale_colour_paletteer_d("dichromat::BrowntoBlue_10", name="",
                           labels=c("1 - Most deprived","2","3","4","5","6","7","8","9",
                                    "10 - least deprived"))+
  facet_wrap(~age)+
  theme_custom()+
  labs(title="The socioeconomic gradient in 40-59 year olds has reversed",
       subtitle="7-day rolling average rate of new COVID cases by age and IMD decile, based on average deprivation across each Local Authority",
       caption="Data from coronavirus.data.gov.uk & ONS | Plot by @VictimOfMaths")
dev.off()

