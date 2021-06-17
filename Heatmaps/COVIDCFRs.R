rm(list=ls())

library(tidyverse)
library(curl)
library(lubridate)
library(scales)
library(extrafont)
library(paletteer)
library(ragg)

theme_custom <- function() {
  theme_classic() %+replace%
    theme(plot.title.position="plot", plot.caption.position="plot",
          strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)),
          plot.title=element_text(face="bold", size=rel(1.5), hjust=0,
                                  margin=margin(0,0,5.5,0)),
          text=element_text(family="Lato"))
}

#Read in CFR data provided by Dan Howdon
#These estimates are not mine to share, sorry, contact Dan for more info.
#https://medicinehealth.leeds.ac.uk/medicine/staff/447/dr-dan-howdon
CFRdata <- read.csv("Data/cfrs_2021_06_16.csv") %>% 
  mutate(age=case_when(
    agegroup==0 ~ "0-4", agegroup==5 ~ "5-9", agegroup==10 ~ "10-14", agegroup==15 ~ "15-19",
    agegroup==20 ~ "20-24", agegroup==25 ~ "25-29", agegroup==30 ~ "30-34", 
    agegroup==35 ~ "35-39", agegroup==40 ~ "40-44", agegroup==45 ~ "45-49", 
    agegroup==50 ~ "50-54", agegroup==55 ~ "55-59", agegroup==60 ~ "60-64", 
    agegroup==65 ~ "65-69", agegroup==70 ~ "70-74", agegroup==75 ~ "75-79",
    agegroup==80 ~ "80-84", agegroup==85 ~ "85-89", TRUE ~ "90+"),
    date=as.Date(date, format="%d/%m/%Y"),
    age = age %>% str_replace("_", "-") %>%
      factor(levels=c("0-4", "5-9", "10-14", "15-19",
                      "20-24", "25-29", "30-34", "35-39", 
                      "40-44", "45-49", "50-54", "55-59", 
                      "60-64", "65-69", "70-74", "75-79", 
                      "80-84", "85-89", "90+"))) 

#Bring in dashboard vaccination data by age
temp=tempfile()
url <- "https://api.coronavirus.data.gov.uk/v2/data?areaType=nation&areaCode=E92000001&metric=vaccinationsAgeDemographics&format=csv"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

vaxdata <- read.csv(temp) %>% 
  select(date, age, cumVaccinationFirstDoseUptakeByVaccinationDatePercentage,
         cumVaccinationSecondDoseUptakeByVaccinationDatePercentage) %>% 
  set_names(c("date", "age", "1st Dose", "2nd Dose")) %>% 
  mutate(age=str_replace(age, "_", "-"),
         age=if_else(age=="18-24", "20-24", age)) %>% 
  group_by(age) %>% 
  #Pick out the data when vaccination coverage first passed 25% of the population
  summarise(vaxstart1=min(date[`1st Dose`>=20]),
            vaxstart2=min(date[`2nd Dose`>=20]))

#Merge together
data <- merge(CFRdata , CFRdata %>% 
  merge(vaxdata, by="age") %>% 
    filter(date==vaxstart1) %>% 
    select(age, cfr_month, vaxstart1, vaxstart2) %>% 
    rename(indexcfr=cfr_month)) %>% 
  select(age, date, cfr_month, indexcfr, vaxstart1, vaxstart2) %>% 
  mutate(indexed=cfr_month/indexcfr,
         dayssince=as.integer(date-as.Date(vaxstart1)),
         daysto2nd=as.integer(as.Date(vaxstart2)-as.Date(vaxstart1)))
  
agg_tiff("Outputs/COVIDCFRChangesLagged.tiff", units="in", width=10, height=7, res=800)
ggplot(data %>% filter(!age %in% c("20-24", "25-29", "30-34") & date>vaxstart1), 
       aes(x=dayssince, y=indexed, colour=age))+
  geom_line(show.legend=FALSE)+
  geom_hline(yintercept=1, colour="Grey70", linetype=2)+
  #geom_segment(aes(y=0.12, yend=1.2, x=daysto2nd, xend=daysto2nd), colour="Grey50")+
  scale_x_continuous(name="Days since at least 20% of population received 1st dose")+
  scale_y_continuous(name="Change in Case Fatality Rate \n(log scale)", trans="log",
                     breaks=c(0.25,0.5,1),
                     labels=c("-75%", "-50%", "0%"))+
  facet_wrap(~age)+
  scale_colour_paletteer_d("pals::tol")+
  theme_custom()+
  labs(title="Across all age groups, Case Fatality Rates have fallen as vaccine coverage increases",
       subtitle="Changes in age-specific Case Fatality Rates (the % of people with a positive test who die within 28 days) after the date when at least\n1 in 5 of the age group had received at least one vaccine dose",
       caption="CFRs estimated by Daniel Howdon\nVaccination data from coronavirus.data.gov.uk\nPlot by @VictimOfMaths")

dev.off()

agg_tiff("Outputs/COVIDCFRChangesLagged2.tiff", units="in", width=10, height=7, res=800)
ggplot(data %>% filter(!age %in% c("20-24", "25-29", "30-34") & date>vaxstart1), aes(x=date, y=indexed, colour=age))+
  geom_line(show.legend=FALSE)+
  geom_hline(yintercept=1, colour="Grey70", linetype=2)+
  #geom_vline(xintercept=as.Date("2021-05-15"))+
  scale_x_date(name="")+
  scale_y_continuous(name="Change in Case Fatality Rate \n(log scale)", trans="log",
                     breaks=c(0.25,0.5,1),
                     labels=c("-75%", "-50%", "0%"))+
  facet_wrap(~age)+
  scale_colour_paletteer_d("pals::tol")+
  theme_custom()+
  labs(title="Across all age groups, Case Fatality Rates have fallen as vaccine coverage increases",
       subtitle="Changes in age-specific Case Fatality Rates (the % of people with a positive test who die within 28 days) after the date when at least\n1 in 5 of the age group had received at least one vaccine dose",
       caption="CFRs estimated by Daniel Howdon\nVaccination data from coronavirus.data.gov.uk\nPlot by @VictimOfMaths")

dev.off()
  
agg_tiff("Outputs/COVIDCFRChangesLagged3.tiff", units="in", width=10, height=7, res=800)
ggplot(data %>% filter(!age %in% c("20-24", "25-29", "30-34") & date>as.Date(vaxstart1)-days(50)), 
       aes(x=dayssince, y=indexed, colour=age))+
  geom_line(show.legend=FALSE)+
  geom_hline(yintercept=1, colour="Grey70", linetype=2)+
  geom_vline(xintercept=0)+
  #geom_segment(aes(y=0.12, yend=1.2, x=daysto2nd, xend=daysto2nd), colour="Grey50")+
  scale_x_continuous(name="Days since at least 20% of population received 1st dose")+
  scale_y_continuous(name="Change in Case Fatality Rate \n(log scale)", trans="log",
                     breaks=c(0.25,0.5,1),
                     labels=c("-75%", "-50%", "0%"))+
  facet_wrap(~age)+
  scale_colour_paletteer_d("pals::tol")+
  theme_custom()+
  labs(title="Across all age groups, Case Fatality Rates have fallen as vaccine coverage increases",
       subtitle="Changes in age-specific Case Fatality Rates (the % of people with a positive test who die within 28 days) after the date when at least\n1 in 5 of the age group had received at least one vaccine dose",
       caption="CFRs estimated by Daniel Howdon\nVaccination data from coronavirus.data.gov.uk\nPlot by @VictimOfMaths")

dev.off()
