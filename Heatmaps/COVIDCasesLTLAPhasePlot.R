rm(list=ls())

library(tidyverse)
library(curl)
library(paletteer)
library(sf)
library(ragg)
library(ggtext)
library(scales)
library(extrafont)
library(ggrepel)
library(gganimate)
library(readxl)

#Start with LA level cases for the whole of the UK
cases <- tempfile()
url <- "https://api.coronavirus.data.gov.uk/v2/data?areaType=ltla&metric=newCasesBySpecimenDateRollingRate&format=csv"
cases <- curl_download(url=url, destfile=cases, quiet=FALSE, mode="wb")

casedata <- read.csv(cases) %>% 
  mutate(date=as.Date(date))

maxdate <- max(casedata$date)

casedata <- casedata %>% 
  #Take the most recent 5 weeks of data
  group_by(areaName) %>% 
  arrange(date) %>% 
  slice_tail(n=36) %>% 
  ungroup() %>% 
  spread(date, newCasesBySpecimenDateRollingRate) %>% 
  select(c(1,2,4,11,18,25,32,39)) %>% 
  set_names("Lacode", "areaName", "week_1", "week_2", "week_3", "week_4", "week_5", "week_6")

casedata <- casedata %>% 
  mutate(change_2=week_2-week_1, change_3=week_3-week_2, change_4=week_4-week_3,
         change_5=week_5-week_4, change_6=week_6-week_5) %>% 
  select(-week_1) %>% 
  pivot_longer(c(3:12), names_to=c("measure", "time"), names_sep="_", values_to="cases") %>% 
  spread(measure, cases)

#Bring in region
temp <- tempfile()
source <- "https://opendata.arcgis.com/datasets/0c3a9643cc7c4015bb80751aad1d2594_0.csv"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
LADtoRegion <- read.csv(temp)[,c(1,4)]
colnames(LADtoRegion) <- c("LTLA", "Region")

casedata <- casedata %>% 
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
      substr(Lacode, 1, 1) == "N" ~ "Northern Ireland"))

#Bring in LA populations
temp <- tempfile()
source <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fpopulationandmigration%2fpopulationestimates%2fdatasets%2fpopulationestimatesforukenglandandwalesscotlandandnorthernireland%2fmid20182019laboundaries/ukmidyearestimates20182019ladcodes.xls"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
LApop <- read_excel(temp, sheet="MYE2-All", range="A5:D435", col_names=TRUE)
colnames(LApop) <- c("code", "name", "geography", "pop")

#Merge isles of Scilly in with Cornwall
LApop$code <- if_else(LApop$code=="E06000053", "E06000052", LApop$code)
LApop$name <- if_else(LApop$name=="Isles of Scilly", "Cornwall", LApop$name)

#Merge City of London & Hackney
LApop$code <- if_else(LApop$code=="E09000001", "E09000012", LApop$code)
LApop$name <- if_else(LApop$name=="City of London", "Hackney and City of London", LApop$name)
LApop$name <- if_else(LApop$name=="Hackney", "Hackney and City of London", LApop$name)

LApop <- LApop %>% 
  group_by(name, code) %>% 
  summarise(pop=sum(pop)) %>% 
  ungroup()

plotdata <- casedata %>% 
  merge(LApop, by.x="Lacode", by.y="code" , all.x=TRUE) %>% 
  mutate(time=as.numeric(time)) %>% 
  arrange(areaName, time)

agg_tiff("Outputs/COVIDCasesLTLAChangeScatter.tiff", units="in", width=8, height=7, res=800)
ggplot(plotdata %>% filter(time==6), aes(x=week, y=change, fill=Country))+
  geom_point(aes(size=pop), shape=21, alpha=0.7)+
  geom_hline(yintercept=0)+
  geom_vline(xintercept=0)+
  geom_text_repel(aes(label=areaName), size=rel(2.3))+
  scale_x_continuous(name="New cases in the past week\n(rate per 100,000)", limits=c(0,NA))+
  scale_y_continuous(name="Change in case rate compared to the preceding week")+
  scale_fill_paletteer_d("fishualize::Scarus_quoyi", name="")+
  scale_size(guide=FALSE)+
  theme_classic()+
  theme(axis.line=element_blank(), text=element_text(family="Lato"),
        legend.position = "top", plot.title.position="plot",
        plot.title=element_text(face="bold", size=rel(1.6)))+
  labs(title="Recent COVID outbreaks are currently contained to a few areas",
       subtitle="COVID case rates and how these have changed in the past week in UK Local Authorities\nBubbles are sized by population",
       caption="Data from coronavirus.data.gov.uk and ONS\nPlot by @VictimOfMaths")
dev.off()

agg_tiff("Outputs/COVIDCasesLTLAChangeScatterPaths.tiff", units="in", width=8, height=7, res=800)
ggplot()+
  geom_hline(yintercept=0)+
  geom_vline(xintercept=0)+
  geom_path(data=plotdata %>% filter(time>4), aes(x=week, y=change, group=areaName),
            colour="Grey80", show.legend=FALSE)+
  geom_point(data=plotdata %>% filter(time==6),
            aes(x=week, y=change, fill=Country, size=pop), shape=21, alpha=0.7)+
  geom_text_repel(data=plotdata %>% filter(time==6), 
                  aes(x=week, y=change, label=areaName), size=rel(2.3))+
  scale_x_continuous(name="New cases in the past week\n(rate per 100,000)", limits=c(0,NA))+
  scale_y_continuous(name="Change in case rate compared to the preceding week")+
  scale_fill_paletteer_d("fishualize::Scarus_quoyi", name="")+
  scale_size(guide=FALSE)+
  theme_classic()+
  theme(axis.line=element_blank(), text=element_text(family="Lato"),
        legend.position = "top", plot.title.position="plot",
        plot.title=element_text(face="bold", size=rel(1.6)))+
  labs(title="Recent COVID outbreaks are currently contained to a few areas",
       subtitle="COVID case rates and how these have changed in the past week in UK Local Authorities\nBubbles are sized by population",
       caption="Data from coronavirus.data.gov.uk and ONS\nPlot by @VictimOfMaths")
dev.off()
