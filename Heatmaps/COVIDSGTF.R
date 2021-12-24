rm(list=ls())

library(tidyverse)
library(curl)
library(extrafont)
library(paletteer)
library(ragg)
library(RcppRoll)
library(ggtext)

theme_custom <- function() {
  theme_classic() %+replace%
    theme(plot.title.position="plot", plot.caption.position="plot",
          strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)),
          plot.title=element_text(face="bold", size=rel(1.5), hjust=0,
                                  margin=margin(0,0,5.5,0)),
          text=element_text(family="Lato"))
}

#Bring in SGTF data from UKHSA
#https://www.gov.uk/government/publications/covid-19-omicron-daily-overview
source <- "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/1043759/sgtf_regionepicurve_2021-12-22.csv"
temp <- tempfile()
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

SGTFdata <- read.csv(temp) %>% 
  mutate(specimen_date=as.Date(specimen_date, format="%d/%m/%Y"),
  #mutate(specimen_date=as.Date(specimen_date),
         areaName=if_else(UKHSA_region=="Yorkshire and Humber", 
                          "Yorkshire and The Humber", UKHSA_region))

#Add in case data from dashboard
source <- "https://api.coronavirus.data.gov.uk/v2/data?areaType=region&metric=newCasesBySpecimenDate&format=csv"
temp <- tempfile()
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

casedata <- read.csv(temp) %>% 
  mutate(specimen_date=as.Date(date))

data <- merge(SGTFdata, casedata, all.x=T) %>% 
  select(specimen_date, areaName, percent, newCasesBySpecimenDate, sgtf) %>% 
  mutate(cases=newCasesBySpecimenDate*percent/100) %>% 
  group_by(areaName, sgtf) %>% 
  mutate(cases_roll=roll_mean(cases, 7, align="center", fill=NA)) %>% 
  ungroup()

maxdate=max(data$specimen_date)

agg_tiff("Outputs/COVIDSGTFxRegion.tiff", units="in", width=9, height=7, res=500)
ggplot(data)+
  geom_line(aes(x=specimen_date, y=cases_roll, colour=sgtf), show.legend=FALSE)+
  geom_point(aes(x=specimen_date, y=cases, colour=sgtf), shape=21, show.legend=FALSE)+
  scale_x_date(name="")+
  scale_y_continuous(name="Daily cases")+
  scale_colour_manual(values=c("#3D98D3", "#FD0409"))+
  facet_wrap(~areaName)+
  theme_custom()+
  theme(plot.subtitle=element_markdown(),
        strip.text=element_blank())+
  labs(title="Omicron cases *may* be falling in London, but they are rising elsewhere",
       subtitle=paste0("Estimated total number of <span style='color:#FD0409;'>Omicron</span> and <span style='color:#3D98D3;'>Delta</span> cases based on SGTF data and total positve tests.<br> Dots represent daily figures, lines the 7-day centered rolling average. Data up to ", maxdate),
       caption="Data from UKHSA & coronavirus.data.gov.uk| Plot by @VictimOfMaths")+
  geom_text(aes(x=as.Date("2021-11-20"), y=13000, label=areaName), family="Lato", fontface="bold",
            size=rel(4))

dev.off()

