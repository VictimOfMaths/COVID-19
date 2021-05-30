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
library(lubridate)
library(gganimate)

#Start with LA level cases for the whole of the UK
cases <- tempfile()
url <- "https://api.coronavirus.data.gov.uk/v2/data?areaType=ltla&metric=newCasesBySpecimenDateRollingRate&format=csv"
cases <- curl_download(url=url, destfile=cases, quiet=FALSE, mode="wb")

casedata <- read.csv(cases) %>% 
  mutate(date=as.Date(date)) %>% 
  group_by(areaName) %>% 
  arrange(date) %>% 
  rename(caserate=newCasesBySpecimenDateRollingRate) %>% 
  mutate(change=caserate-lag(caserate,7)) %>% 
  ungroup() %>% 
  rename("Lacode"="areaCode")
  
maxdate <- max(casedata$date)

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
  mutate(dayssince=as.integer(maxdate-date)) %>% 
  arrange(date)

agg_tiff("Outputs/COVIDCasesLTLAChangeScatter.tiff", units="in", width=8, height=7, res=800)
ggplot(plotdata %>% filter(date==maxdate), aes(x=caserate, y=change, fill=Country))+
  geom_hline(yintercept=0)+
  geom_vline(xintercept=0)+
  geom_point(aes(size=pop), shape=21, alpha=0.7)+
  geom_text_repel(aes(label=areaName), size=rel(2.3), box.padding=0.3)+
  scale_x_continuous(name="New cases in the past week\n(rate per 100,000)", limits=c(0,NA))+
  scale_y_continuous(name="Change in case rate compared to the preceding week")+
  scale_fill_paletteer_d("fishualize::Scarus_quoyi", name="")+
  scale_size(guide=FALSE)+
  theme_classic()+
  theme(axis.line=element_blank(), text=element_text(family="Lato"),
        legend.position = "top", plot.title.position="plot",
        plot.title=element_text(face="bold", size=rel(1.6)))+
  labs(title="Recent COVID outbreaks are currently contained to a few areas",
       subtitle=paste0("COVID case rates and how these have changed in the past week in UK Local Authorities.\nBubbles are sized by population. Data up to ",
       maxdate),
       caption="Data from coronavirus.data.gov.uk and ONS\nPlot by @VictimOfMaths")
dev.off()

plot <- ggplot()+
  geom_hline(yintercept=0)+
  geom_vline(xintercept=0)+
  geom_path(data=plotdata %>% filter(date>=maxdate-days(7)), 
            aes(x=caserate, y=change, group=areaName, alpha=7-dayssince),
            colour="Grey50", show.legend=FALSE)+
  geom_point(data=plotdata %>% filter(date==maxdate),
            aes(x=caserate, y=change, fill=Country, size=pop), shape=21, alpha=0.7)+
  geom_text_repel(data=plotdata %>% filter(date==maxdate), 
                  aes(x=caserate, y=change, label=areaName), size=rel(2.3))+
  scale_x_continuous(name="New cases in the past week\n(rate per 100,000)", limits=c(0,NA))+
  scale_y_continuous(name="Change in case rate compared to the preceding week")+
  scale_fill_paletteer_d("fishualize::Scarus_quoyi", name="")+
  scale_size(guide=FALSE)+
  theme_classic()+
  theme(axis.line=element_blank(), text=element_text(family="Lato"),
        legend.position = "top", plot.title.position="plot",
        plot.title=element_text(face="bold", size=rel(1.6)))+
  labs(title="Rossendale: 😟, Blackburn & Glasgow: 🤷,\nBolton, Bedford & Clackmannanshire: 👍",
       subtitle=paste0("COVID case rates and how these have changed in the past week in UK Local Authorities.\nBubbles are sized by population. Trails represent each area's movement across the plot in the past week.\nData up to ",
                       maxdate),
       caption="Data from coronavirus.data.gov.uk and ONS\nPlot by @VictimOfMaths")

agg_tiff("Outputs/COVIDCasesLTLAChangeScatterPaths.tiff", units="in", width=8, height=7, res=800)
plot
dev.off()

#Animated version
anim <- ggplot(plotdata %>% filter(date>=as.Date("2021-04-19")), 
               aes(x=caserate, y=change, fill=Country))+
  geom_hline(yintercept=0)+
  geom_vline(xintercept=0)+
  geom_point(aes(size=pop), shape=21, alpha=0.7)+
  geom_text_repel(aes(label=areaName), size=rel(2.3), box.padding=0.3)+
  scale_x_continuous(name="New cases in the past week\n(rate per 100,000)", limits=c(0,NA))+
  scale_y_continuous(name="Change in case rate compared to the preceding week")+
  scale_fill_paletteer_d("fishualize::Scarus_quoyi", name="")+
  scale_size(guide=FALSE)+
  theme_classic()+
  theme(axis.line=element_blank(), text=element_text(family="Lato"),
        legend.position = "top", plot.title.position="plot",
        plot.title=element_text(face="bold", size=rel(1.6)))+
  labs(title="Recent COVID outbreaks are currently contained to a few areas",
       subtitle="COVID case rates and how these have changed in the past week in UK Local Authorities.\nBubbles are sized by population. Data up to {closest_state}",
       caption="Data from coronavirus.data.gov.uk and ONS\nPlot by @VictimOfMaths")+
  transition_states(date, transition_length=1, state_length=1)

animate(anim, units="in", width=8, height=8*4/5, res=500, 
        renderer=gifski_renderer("Outputs/COVIDCasesPhasePlotAnim.gif"), 
        device="ragg_png", end_pause=5, duration=10, fps=8)
