rm(list=ls())

library(tidyverse)
library(curl)
library(readxl)
library(lubridate)
library(paletteer)
library(scales)
library(extrafont)
library(ggrepel)

#LTLA analysis
temp <- tempfile()
dashurl <- "https://api.coronavirus.data.gov.uk/v2/data?areaType=ltla&metric=VaccineRegisterPopulationByVaccinationDate&metric=cumPeopleVaccinatedFirstDoseByVaccinationDate&metric=cumPeopleVaccinatedSecondDoseByVaccinationDate&metric=newCasesBySpecimenDateRollingRate&format=csv"
temp <- curl_download(url=dashurl, destfile=temp, quiet=FALSE, mode="wb")

dashdata <- read.csv(temp) %>% 
  set_names(c("Lacode", "name", "areaType", "date", "NIMSpop", "Vax1st", "Vax2nd", "caserate")) %>% 
  mutate(date=as.Date(date), Vax1st=Vax1st/NIMSpop, Vax2nd=Vax2nd/NIMSpop)

maxdate=max(dashdata$date[!is.na(dashdata$caserate)])

#Bring in region
temp <- tempfile()
source <- "https://opendata.arcgis.com/datasets/0c3a9643cc7c4015bb80751aad1d2594_0.csv"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
LADtoRegion <- read.csv(temp)[,c(1,4)]
colnames(LADtoRegion) <- c("LTLA", "Region")

dashdata <- dashdata %>% 
  merge(LADtoRegion, all.x=TRUE, by.x="Lacode", by.y="LTLA") %>% 
  mutate(Region=case_when(
    Lacode %in% c("E07000244", "E07000245") ~ "East of England",
    Lacode %in% c("E06000058", "E06000059", "E07000246") ~ "South West",
    substr(Lacode, 1, 1) == "W" ~ "Wales",
    substr(Lacode, 1, 1) == "S" ~ "Scotland",
    substr(Lacode, 1, 1) == "N" ~ "Northern Ireland",
    TRUE ~ Region),
    Country=case_when(
      substr(Lacode, 1, 1) == "E" ~ "England",
      substr(Lacode, 1, 1) == "W" ~ "Wales",
      substr(Lacode, 1, 1) == "S" ~ "Scotland",
      substr(Lacode, 1, 1) == "N" ~ "Northern Ireland")) %>% 
  arrange(date)

agg_tiff("Outputs/COVIDCasesvsVaxLTLA.tiff", units="in", width=9, height=7, res=800)
ggplot()+
  geom_path(data=dashdata %>% filter(date>maxdate-days(7)), 
            aes(x=caserate, y=Vax1st, group=name, alpha=7-as.integer(maxdate-date)),
            show.legend=FALSE)+
  geom_point(data=dashdata %>% filter(date==maxdate & !is.na(Vax1st)), 
             aes(x=caserate, y=Vax1st, fill=Region, size=NIMSpop), shape=21, alpha=0.7)+
  geom_text_repel(data=dashdata %>% filter(date==maxdate), 
                  aes(x=caserate, y=Vax1st, label=name), size=rel(2.3),
                  label.padding=1)+
  scale_fill_manual(name="", values=c("#C70E7B", "#FC6882", "#007BC3", "#54BCD1", "#EF7C12",
                                      "Black", "#F4B95A", "#009F3F", "#8FDA04", "#AF6125"))+
  scale_size(guide=FALSE)+
  scale_y_continuous(name="Proportion of adults who have received at least one dose",
                     labels=label_percent(accuracy=1))+
  scale_x_continuous(name="Cases per 100,000 population in the past week")+
  theme_classic()+
  theme(text=element_text(family="Lato"), plot.title.position="plot", plot.caption.position = "plot",
        plot.title=element_text(face="bold", size=rel(1.5)))+
  labs(title="Areas with high COVID case rates have fairly average vaccine coverage",
       subtitle="Rate of new COVID-19 cases in the past week compared to 1st dose vaccine coverage in English/Scottish Local Authorites.\nBubble size corresponds to area population. Paths represent changes in the past 7 days.",
       caption="Data from coronavirus.data.gov.uk\nInspired by Russ Garrett (@russss)\nPlot by @VictimOfMaths")
dev.off()

#MSOA-level version (England only)
temp <- tempfile()
vaxurl <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/05/COVID-19-weekly-announced-vaccinations-27-May-2021.xlsx"
temp <- curl_download(url=vaxurl, destfile=temp, quiet=FALSE, mode="wb")

msoadata <- read_excel(temp, sheet="MSOA", range="B16:Q6806", col_names=FALSE) %>% 
  mutate(Vax1st=`...7`+`...8`+`...9`+`...10`+`...11`+`...12`+`...13`+`...14`+`...15`+`...16`) %>% 
  select(c(2,5,6,17)) %>% 
  set_names(c("Region", "code", "name", "Vax1st"))
  
#NIMS populations
pop <- read_excel(temp, sheet="Population estimates (NIMS)", range="S16:AF6806", col_names=FALSE) %>% 
  select(c(1, 14)) %>% 
  set_names(c("code", "pop"))

#MSOA-level cases
temp <- tempfile()
MSOArateurl <- "https://api.coronavirus.data.gov.uk/v2/data?areaType=msoa&metric=newCasesBySpecimenDateRollingRate&format=csv"
temp <- curl_download(url=MSOArateurl, destfile=temp, quiet=FALSE, mode="wb")

MSOAcases <- read.csv(temp) %>% 
  filter(date=max(date))

MSOAdata <- msoadata %>% 
  merge(pop, by="code") %>% 
  merge(MSOAcases, by.x="code", by.y="areaCode", all.x=TRUE) %>% 
  mutate(cases=if_else(is.na(newCasesBySpecimenDateRollingRate), 0, 
                       newCasesBySpecimenDateRollingRate),
         vaxprop=Vax1st/pop,
         date=as.Date(date))

agg_tiff("Outputs/COVIDCasesvsVaxMSOA.tiff", units="in", width=9, height=7, res=800)
ggplot()+
  geom_point(data=MSOAdata %>% filter(date==maxdate & !is.na(Vax1st)), 
             aes(x=cases, y=vaxprop, fill=Region, size=pop), shape=21, alpha=0.7)+
  #geom_text_repel(data=MSOAdata %>% filter(date==maxdate), 
  #                aes(x=cases, y=vaxprop, label=name), size=rel(2.3),
  #                label.padding=1)+
  scale_fill_paletteer_d("LaCroixColoR::paired", name="")+
  scale_size(guide=FALSE)+
  scale_y_continuous(name="Proportion of adults who have received at least one dose",
                     labels=label_percent(accuracy=1))+
  scale_x_continuous(name="Cases per 100,000 population in the past week")+
  theme_classic()+
  theme(text=element_text(family="Lato"), plot.title.position="plot", plot.caption.position = "plot",
        plot.title=element_text(face="bold", size=rel(1.5)))+
  labs(title="Areas with high COVID case rates have fairly average vaccine coverage",
       subtitle="Rate of new COVID-19 cases in the past week compared to 1st dose vaccine coverage in English Middle Super Output Areas.\nBubble size corresponds to area population.",
       caption="Data from coronavirus.data.gov.uk\nInspired by Russ Garrett (@russss)\nPlot by @VictimOfMaths")

dev.off()