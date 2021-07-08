rm(list=ls())

library(tidyverse)
library(curl)
library(readxl)
library(lubridate)
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
url <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/07/Covid-Publication-08-07-2021-Supplementary-Data.xlsx"
temp <- tempfile()
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

rawdata <- read_excel(temp, range="D16:JI22", col_names=FALSE) %>% 
  mutate(age=c("0-5", "6-17", "18-54", "55-64", "65-74", "75-84", "85+")) %>% 
  gather(date, admissions, c(1:(ncol(.)-1))) %>% 
  mutate(date=as.Date("2020-10-30")+days(as.integer(substr(date, 4, 6))-1),
         age=factor(age, levels=c("0-5", "6-17", "18-54", "55-64", "65-74", "75-84", "85+")))

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
  labs(title="COVID hospital admissions are largely confined to 18-54 year olds",
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
           age<55 ~ "18-54",
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

agg_tiff("Outputs/COVIDAdmissionsHeatmaRatep.tiff", units="in", width=9, height=6, res=800)
ggplot(data, aes(x=date, y=age, fill=admrate))+
  geom_tile()+
  scale_x_date(name="")+
  scale_y_discrete(name="Age group")+
  scale_fill_paletteer_c("viridis::inferno", name="Daily admissions per 100,000")+
  theme_custom()+
  theme(legend.position="top")+
  guides(fill = guide_colorbar(title.position = 'top', title.hjust = .5,
                               barwidth = unit(20, 'lines'), barheight = unit(.5, 'lines')))+
  labs(title="COVID hospital admissions are nothing like previous waves (yet)",
       subtitle="Rate of new daily admissions to hospital of patients with a positive COVID test, or new COVID diagnoses in hospital in England",
       caption="Admissions data from NHS England | Population date from ONS | Plot by @VictimOfMaths")
dev.off()
