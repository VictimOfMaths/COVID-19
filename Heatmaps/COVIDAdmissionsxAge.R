rm(list=ls())

library(tidyverse)
library(curl)
library(readxl)
library(lubridate)
library(ggstream)
library(extrafont)
library(paletteer)
library(RcppRoll)
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

#Read in latest monthly age-stratified admissions data from NHS England website
url <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/08/Covid-Publication-12-08-2021-Supplementary-Data.xlsx"
temp <- tempfile()
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

rawdata <- read_excel(temp, range="D16:KR25", col_names=FALSE) %>% 
  mutate(age=c("0-5", "6-17", "18-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75-84", "85+")) %>% 
  gather(date, admissions, c(1:(ncol(.)-1))) %>% 
  mutate(date=as.Date("2020-10-12")+days(as.integer(substr(date, 4, 6))-1),
         age=factor(age, levels=c("0-5", "6-17", "18-24", "25-34", "35-44", "45-54", "55-64", "65-74", 
                                  "75-84", "85+")))

agg_tiff("Outputs/COVIDAdmissionsHeatmap.tiff", units="in", width=9, height=6, res=800)
ggplot(rawdata, aes(x=date, y=age, fill=admissions))+
  geom_tile()+
  scale_x_date(name="")+
  scale_y_discrete(name="Age group")+
  scale_fill_paletteer_c("viridis::inferno", name="Daily admissions")+
  theme_custom()+
  theme(legend.position="top")+
  guides(fill = guide_colorbar(title.position = 'top', title.hjust = .5,
                               barwidth = unit(20, 'lines'), barheight = unit(.5, 'lines')))+
  labs(title="COVID admission rates have been much lower and younger in the latest wave",
       subtitle="Number of new daily admissions to hospital of patients with a positive COVID test, or new COVID diagnoses in hospital in England",
       caption="Data from NHS England | Plot by @VictimOfMaths")
dev.off()

#Bring in populations to calculate rates
popurl <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fpopulationandmigration%2fpopulationestimates%2fdatasets%2fpopulationestimatesforukenglandandwalesscotlandandnorthernireland%2fmid2020/ukpopestimatesmid2020on2021geography.xls"
temp1 <- tempfile()
temp1 <- curl_download(url=popurl, destfile=temp1, quiet=FALSE, mode="wb")

pop <- as.data.frame(t(read_excel(temp1, sheet="MYE2 - Persons", range="E12:CQ12", 
                                  col_names=FALSE))) %>% 
  mutate(age=c(0:90),
         age=case_when(
           age<6 ~ "0-5",
           age<18 ~ "6-17",
           age<25 ~ "18-24",
           age<35 ~ "25-34",
           age<45 ~ "35-44",
           age<55 ~ "45-54",
           age<65 ~ "55-64",
           age<75 ~ "65-74",
           age<85 ~ "75-84",
           TRUE ~ "85+")) %>% 
  group_by(age) %>% 
  summarise(pop=sum(V1)) %>% 
  ungroup()

data <- rawdata %>% 
  merge(pop) %>% 
  mutate(admrate=admissions*100000/pop)

agg_tiff("Outputs/COVIDAdmissionsHeatmapRate.tiff", units="in", width=9, height=6, res=800)
ggplot(data, aes(x=date, y=age, fill=admrate))+
  geom_tile()+
  scale_x_date(name="")+
  scale_y_discrete(name="Age group")+
  scale_fill_paletteer_c("viridis::inferno", name="Daily admissions per 100,000")+
  theme_custom()+
  theme(legend.position="top")+
  guides(fill = guide_colorbar(title.position = 'top', title.hjust = .5,
                               barwidth = unit(20, 'lines'), barheight = unit(.5, 'lines')))+
  labs(title="COVID admission rates have been much lower and younger in the latest wave",
       subtitle="Rate of new daily admissions to hospital of patients with a positive COVID test, or new COVID diagnoses in hospital in England",
       caption="Admissions data from NHS England | Population date from ONS | Plot by @VictimOfMaths")
dev.off()

#Bring in case data
caseurl <- "https://api.coronavirus.data.gov.uk/v2/data?areaType=nation&areaCode=E92000001&metric=newCasesBySpecimenDateAgeDemographics&format=csv"
temp2 <- tempfile()
temp2 <- curl_download(url=caseurl, destfile=temp2, quiet=FALSE, mode="wb")

casedata <- read.csv(temp2) %>% 
  filter(!age %in% c("00_59", "60+", "unassigned")) %>% 
  select(date, age, cases) %>% 
  #Do some simplistic apportioning of cases to align with admissions age bands assuming equal
  #distributions of cases within each band
  spread(age, cases) %>% 
  rowwise() %>% 
  mutate(`0-5`=`00_04`+0.2*`05_09`,
         `6-17`=0.8*`05_09`+`10_14`+0.6*`15_19`,
         `18-24`=0.4*`15_19`+`20_24`,
         `25-34`=`25_29`+`30_34`,
         `35-44`=`35_39`+`40_44`,
         `45-54`=`45_49`+`50_54`,
         `55-64`=`55_59`+`60_64`,
         `65-74`=`65_69`+`70_74`,
         `75-84`=`75_79`+`80_84`,
         `85+`=`85_89`+`90+`) %>% 
  ungroup() %>% 
  select(date, `0-5`, `6-17`, `18-24`, `25-34`, `35-44`, `45-54`, `55-64`, `65-74`, `75-84`, `85+`) %>% 
  gather(age, cases, c(2:11))

#Bring it all together
alldata <- data %>% 
  merge(casedata, all=TRUE) %>%
  #Add in total rows
  bind_rows(data %>% merge(casedata, all=TRUE) %>% 
              group_by(date) %>% 
              summarise(admissions=sum(admissions), pop=sum(pop), cases=sum(cases)) %>%
              mutate(admrate=admissions*100000/pop, age="Total")) %>% 
  #Take rolling 7-day averages
  group_by(age) %>% 
  arrange(date) %>% 
  mutate(casesroll=roll_mean(cases, 7, align="center", fill=NA),
         caserate=cases*100000/pop,
         caserateroll=roll_mean(caserate, 7, align="center", fill=NA),
         admroll=roll_mean(admissions, 7, align="center", fill=NA),
         admrateroll=roll_mean(admrate, 7, align="center", fill=NA)) %>% 
  #Lag cases by 8 days from hospitalisations. Rationale for 8 days lag is here:
  #https://twitter.com/nicfreeman1209/status/1404915641728081921
  mutate(lagcasesroll=lag(casesroll, 8),
         lagcaserateroll=lag(caserateroll, 8)) %>% 
  ungroup() %>% 
  mutate(CHR=admrateroll/lagcaserateroll,
         age=factor(age, levels=c("0-5", "6-17", "18-24", "25-34", "35-44", "45-54", "55-64", "65-74", 
                                  "75-84", "85+", "Total")))

agg_tiff("Outputs/COVIDCHROverall.tiff", units="in", width=9, height=6, res=800)
ggplot(alldata %>% filter(!is.na(CHR) & age=="Total"), aes(x=date, y=CHR))+
  geom_line(colour="dodgerblue")+
  scale_x_date(name="")+
  scale_y_continuous(name="Proportion of cases admitted to hospital",
                     labels=label_percent(accuracy=1), limits=c(0,NA))+
  theme_custom()+
  labs(title="The proportion of COVID cases being admitted to hospital has risen in recent days",
       subtitle="Rolling 7-day average COVID admission rate in English hospitals compared to the rolling 7-day average case rate,\nassuming an 8-day lag between testing positive and hospital admission",
       caption="Data from NHS England and coronavirues.data.gov.uk | Plot and analysis by Colin Angus")
dev.off()

agg_tiff("Outputs/COVIDCHRxAge.tiff", units="in", width=9, height=6, res=800)
ggplot(alldata %>% filter(!is.na(CHR) & age!="Total"), aes(x=date, y=CHR, colour=age))+
  geom_line()+
  scale_x_date(name="")+
  scale_y_continuous(name="Proportion of cases admitted to hospital",
                     labels=label_percent(accuracy=1), limits=c(0,NA))+
  scale_colour_paletteer_d("ggsci::default_gsea", name="Age")+
  theme_custom()+
  labs(title="The proportion of COVID cases being admitted to hospital varies a lot with age",
       subtitle="Rolling 7-day average COVID admission rate in English hospitals compared to the rolling 7-day average case rate,\nassuming an 8-day lag between testing positive and hospital admission",
       caption="Data from NHS England and coronavirues.data.gov.uk | Plot by @VictimOfMaths")
dev.off()

agg_tiff("Outputs/COVIDCHRxAgeFacets.tiff", units="in", width=9, height=6, res=800)
ggplot(alldata %>% filter(!is.na(CHR) & age!="Total"), aes(x=date, y=CHR, colour=age))+
  geom_line(show.legend=FALSE)+
  scale_x_date(name="")+
  scale_y_continuous(name="Proportion of cases admitted to hospital",
                     labels=label_percent(accuracy=1), limits=c(0,NA))+
  scale_colour_paletteer_d("ggsci::default_gsea", name="Age")+
  facet_wrap(~age, scales="free_y")+
  theme_custom()+
  labs(title="The proportion of COVID cases being admitted to hospital has risen in all age groups",
       subtitle="Rolling 7-day average COVID admission rate in English hospitals compared to the rolling 7-day average case rate,\nassuming an 8-day lag between testing positive and hospital admission",
       caption="Data from NHS England and coronavirues.data.gov.uk | Plot by @VictimOfMaths")
dev.off()

agg_tiff("Outputs/COVIDAdmissionsStreamgraph.tiff", units="in", width=9, height=6, res=800)
ggplot(alldata %>% filter(!is.na(admroll) & age!="Total"),
       aes(x=date, y=admroll, fill=age))+
  geom_stream()+
  scale_x_date(name="")+
  scale_y_continuous(name="Daily new hospital admissions", labels=abs)+
  scale_fill_paletteer_d("ggsci::default_gsea", name="Age")+
  theme_custom()+
  labs(title="COVID patients being admitted to hospital are younger than in previous waves",
       subtitle="Rolling 7-day average of daily new hospital admissions with a positive COVID test, or patients in hospital testing positive,\nby age group in England",
       caption="Data from NHS England | Plot by @VictimOfMaths")
dev.off()
