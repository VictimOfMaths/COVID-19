rm(list=ls())

library(tidyverse)
library(curl)
library(paletteer)
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

temp <- tempfile()
url <- "https://api.coronavirus.data.gov.uk/v2/data?areaType=region&metric=vaccinationsAgeDemographics&format=csv"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

data <- read.csv(temp) %>% 
  select(areaName, date, age, cumVaccinationFirstDoseUptakeByVaccinationDatePercentage) %>% 
  rename("uptake"="cumVaccinationFirstDoseUptakeByVaccinationDatePercentage") %>% 
  filter(age=="12_15" & date>as.Date("2021-08-01")) %>% 
  mutate(date=as.Date(date))

agg_tiff("Outputs/COVIDVaxUptake1215xReg.tiff", units="in", width=8, height=6, res=500)
ggplot(data, aes(x=date, y=uptake/100, colour=areaName))+
  geom_line()+
  scale_x_date(name="")+
  scale_y_continuous(name="First dose coverage", labels=label_percent(accuracy=1))+
  scale_colour_paletteer_d("LaCroixColoR::paired", name="")+
  theme_custom()+
  labs(title="There is big regional variation in 12-15 COVID vaccination rates",
       subtitle="Vaccine uptake among 12-15 year-olds in England by region",
       caption="Data from coronavirus.data.gov.uk | Plot by @VictimOfMaths")
dev.off()
  