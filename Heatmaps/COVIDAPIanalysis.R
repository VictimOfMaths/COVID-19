rm(list=ls())

library(tidyverse)
library(ukcovid19) #remotes::install_github("publichealthengland/coronavirus-dashboard-api-R-sdk")
library(curl)
library(readxl)
library(RcppRoll)
library(cowplot)

casedata <- get_data(filters="areaType=ltla", structure=list(date="date",
                                                             name="areaName",
                                                             code="areaCode",
                                                             cases="newCasesBySpecimenDate"))

#Bring in LA populations
temp <- tempfile()
source <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fpopulationandmigration%2fpopulationestimates%2fdatasets%2fpopulationestimatesforukenglandandwalesscotlandandnorthernireland%2fmid20182019laboundaries/ukmidyearestimates20182019ladcodes.xls"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
LApop <- read_excel(temp, sheet="MYE2-All", range="A5:D435", col_names=TRUE)
colnames(LApop) <- c("code", "name", "geography", "pop")

LApop <- LApop %>% 
  group_by(name, code) %>% 
  summarise(pop=sum(pop)) %>% 
  ungroup()

casedata <- merge(casedata, LApop, all.x=TRUE)

casedata <- casedata %>% 
  group_by(name) %>% 
  mutate(date=as.Date(date)) %>% 
  arrange(date) %>% 
  mutate(caserate=cases*100000/pop, casesroll_avg=roll_mean(cases, 7, align="right", fill=0),
         caserate_avg=casesroll_avg*100000/pop, maxcaserate=max(caserate_avg, na.rm=FALSE),
         maxcaseday=date[which(caserate_avg==maxcaserate)][1],
         maxcaseprop=caserate_avg/maxcaserate,
         totalcases=sum(cases)) %>% 
  ungroup()

plotfrom <- "2020-03-01"
plotto <- max(casedata$date)

#Plot case trajectories
casetiles.all <- ggplot(casedata, aes(x=date, y=fct_reorder(name, maxcaseday), fill=maxcaseprop))+
  geom_tile(colour="White", show.legend=FALSE)+
  theme_classic()+
  scale_fill_distiller(palette="Spectral")+
  scale_y_discrete(name="", expand=c(0,0))+
  scale_x_date(name="Date", limits=as.Date(c(plotfrom, plotto)), expand=c(0,0))+
  labs(title="Timelines for COVID-19 cases in Local Authorities/Council Areas across the UK",
       subtitle=paste0("The heatmap represents the 7-day rolling average of the number of new confirmed cases, normalised to the maximum value within the Local Authority.\nLAs are ordered by the date at which they reached their peak number of new cases. Bars on the right represent the absolute number of cases in each LA.\nData updated to ", plotto, ". Data for most recent days is provisional and may be revised upwards as additional tests are processed."),
       caption="Data from PHE, PHW, PHS & DoHNI | Plot by @VictimOfMaths")+
  theme(axis.line.y=element_blank(), plot.subtitle=element_text(size=rel(0.78)), plot.title.position="plot",
        axis.text.y=element_text(colour="Black"))

casebars.all <- ggplot(subset(casedata, date==maxcaseday), aes(x=totalcases, y=fct_reorder(name, maxcaseday), fill=totalcases))+
  geom_col(show.legend=FALSE)+
  theme_classic()+
  scale_fill_distiller(palette="Spectral")+
  scale_x_continuous(name="Total confirmed cases")+
  theme(axis.title.y=element_blank(), axis.line.y=element_blank(), axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), axis.text.x=element_text(colour="Black"))

tiff("Outputs/test.tiff", units="in", width=16, height=30, res=500)
plot_grid(casetiles.all, casebars.all, align="h", rel_widths=c(1,0.2))
dev.off()

#testdata <- get_data(filters="areaType=nation", structure=list(date="date",
#                                                             region="areaName",
#                                                             code="areaCode",
#                                                             cases="newCasesByPublishDate",
#                                                             p1tests="newPillarOneTestsByPublishDate",
#                                                             p2tests="newPillarTwoTestsByPublishDate",
#                                                             admissions="newAdmissions",
#                                                             hospitalisations="hospitalCases",
#                                                             MVbeds="covidOccupiedMVBeds"))