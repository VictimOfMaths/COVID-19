rm(list=ls())

library(tidyverse)
library(curl)
library(lubridate)
library(paletteer)
library(ggrepel)
library(extrafont)
library(ragg)

theme_custom <- function() {
  theme_classic() %+replace%
    theme(plot.title.position="plot", plot.caption.position="plot",
          strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)),
          plot.title=element_text(face="bold", size=rel(1.5), hjust=0,
                                  margin=margin(0,0,5.5,0)),
          text=element_text(family="Lato"))
}

source <- "https://api.coronavirus.data.gov.uk/v2/data?areaType=ltla&metric=newCasesBySpecimenDateRollingRate&metric=newCasesBySpecimenDateRollingSum&format=csv"

temp <- tempfile()
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

rawdata <- read.csv(temp) %>% 
  rename(casesum=newCasesBySpecimenDateRollingSum, caserate=newCasesBySpecimenDateRollingRate) %>% 
  mutate(date=as.Date(date), country=case_when(
    substr(areaCode, 1, 1)=="E" ~ "England",
    substr(areaCode, 1, 1)=="S" ~ "Scotland",
    substr(areaCode, 1, 1)=="W" ~ "Wales",
    substr(areaCode, 1, 1)=="N" ~ "Northern Ireland"),
    pop=casesum*100000/caserate)

#Pick out date cases first exceeded 100/100,000 in 7-day period since 1st May
data <- rawdata %>%
  merge(rawdata %>% 
          group_by(areaName, areaCode) %>% 
          filter(date>as.Date("2021-05-01") & caserate>100) %>% 
          summarise(startdate=min(date)) %>% 
          ungroup() %>% 
          select(areaCode, startdate), by="areaCode", all.x=TRUE) %>% 
  mutate(dayssince=as.integer(if_else(date-startdate>=0, date-startdate, NA_real_)))

agg_tiff("Outputs/COVIDCasesSince100k.tiff", units="in", width=9, height=7, res=800)
ggplot()+
  geom_line(data=data %>% filter(dayssince>=0), aes(x=dayssince, y=caserate, group=areaName), colour="Grey80")+
  geom_line(data=data %>% filter(dayssince>=0 & areaName %in% c("Bolton", "Blackburn with Darwen")), 
            aes(x=dayssince, y=caserate, group=areaName), colour="Black")+
  geom_point(data=data %>% filter(date==max(date)), 
             aes(x=dayssince, y=caserate, fill=country, size=pop),
             shape=21, alpha=0.7)+
  geom_text_repel(data=data %>% filter(date==max(date)),
                  aes(x=dayssince, y=caserate, label=areaName), size=rel(2.3),
                  box.padding=0.5)+
  scale_x_continuous(name="Days since cases first passed 100/100,000")+
  scale_y_continuous(name="Weekly cases per 100,000 people")+
  scale_fill_paletteer_d("fishualize::Scarus_quoyi", name="")+
  scale_size(guide=FALSE)+
  theme_custom()+
  theme(legend.position="top")+
  labs(title="Local Delta variant outbreaks seem to follow different trajectories",
       subtitle=paste0("Rolling 7-day average of new COVID case rates in UK Local Authorities since the date that cases within that area\nfirst exceeded 100/100,000 in the current wave. Date up to ",
                       max(data$date), ". Bubbles are sized by area population."), 
       caption="Data from coronavirus.data.gov.uk | Plot by @VictimOfMaths")
dev.off()

