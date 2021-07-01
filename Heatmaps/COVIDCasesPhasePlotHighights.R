#You need to make sure all these packages are installed on your machine

library(tidyverse)
library(curl)
library(ragg)
library(ggtext)
library(readxl)
library(ggrepel)
library(lubridate)

#Define Local Authority/Authorities to highlight
highlightareas <- c("Barnsley", "Bradford", "Calderdale", "Doncaster", "Kirklees", "Leeds", "Craven",
                    "Rotherham", "Sheffield", "Wakefield", "Richmondshire", "Hambleton", 
                    "York", "Selby", "Ryedale", "Scarborough")

#Pick title for areas for plot
areaname <- "Yorkshire"

#Define colour to pick out areas of interest
highlightcolour <- "#FF4E86"

#Start with LA level cases for the whole of the UK from the dashboard
cases <- tempfile()
url <- "https://api.coronavirus.data.gov.uk/v2/data?areaType=ltla&metric=newCasesBySpecimenDateRollingRate&format=csv"
cases <- curl_download(url=url, destfile=cases, quiet=FALSE, mode="wb")

#Tidy up the data
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

#Bring in LA populations (this is only 2019 mid-year estimates, but we're only using them for a 
#general sense of LA sizes, so the precise numbers aren't totally critical)
#If anyone wants to rewrite this for to 2020 populations then be my guest!
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

#Merge populations into case data
plotdata <- casedata %>% 
  merge(LApop, by.x="Lacode", by.y="code" , all.x=TRUE) %>% 
  mutate(dayssince=as.integer(maxdate-date)) %>% 
  arrange(date) %>% 
  #Add flag to pick out Local Authorities of interest
  mutate(highlight=if_else(areaName %in% highlightareas, 1, 0))

#Create plot
plot <- ggplot()+
  #Draw axes
  geom_hline(yintercept=0)+
  geom_vline(xintercept=0)+
  #Draw all LA paths
  geom_path(data=plotdata %>% filter(date>=maxdate-days(7)), 
            aes(x=caserate, y=change, group=areaName, alpha=7-dayssince),
            colour="Grey70", show.legend=FALSE)+
  #Draw all LA points
  geom_point(data=plotdata %>% filter(date==maxdate),
             aes(x=caserate, y=change, size=pop), shape=21, alpha=0.5, colour="Grey50", fill="Grey70")+
  #Draw highlighted area paths
  geom_path(data=plotdata %>% filter(date>=maxdate-days(7) & highlight==1), 
            aes(x=caserate, y=change, group=areaName, alpha=7-dayssince),
            colour="Black", show.legend=FALSE)+
  #Draw highlighted area points
  geom_point(data=plotdata %>% filter(date==maxdate & highlight==1),
             aes(x=caserate, y=change, size=pop), shape=21, alpha=0.9, colour="Black", fill=highlightcolour)+
  #Label highlighted areas
  geom_text_repel(data=plotdata %>% filter(date==maxdate & highlight==1), 
                  aes(x=caserate, y=change, label=areaName), size=rel(2.3))+
  scale_x_continuous(name="New cases in the past week\n(rate per 100,000)", limits=c(0,NA))+
  scale_y_continuous(name="Change in case rate compared to the preceding week")+
  scale_size(guide=FALSE)+
  theme_classic()+
  theme(axis.line=element_blank(), plot.title.position="plot",
        plot.title=element_text(face="bold", size=rel(1.6)),
        plot.subtitle=element_markdown())+
  labs(title="Trajectories of new COVID cases",
       subtitle=paste0("COVID case rates and how these have changed in the past week in <span style='color:", highlightcolour, 
                       ";'>", areaname, "</span> compared to<br>other UK Local Authorities. Bubbles are sized by population. Trails represent each area's movement in the past week.<br>Data up to ",
                       maxdate),
       caption="Data from coronavirus.data.gov.uk and ONS\nPlot by @VictimOfMaths")

#Write the plot out to the working directory
#You can choose either a .tiff or a .png by commenting out the one you don't want in these two lines:
agg_tiff("COVIDCasesPhasePlotHighlights.tiff", units="in", width=9, height=7, res=800)
#agg_png("COVIDCasesPhasePlotHighlights.png", units="in", width=9, height=7, res=800)
plot
dev.off()
