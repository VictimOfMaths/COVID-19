rm(list=ls())

library(tidyverse)
library(curl)
library(paletteer)
library(scales)
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

#Download vaccination data from the dashboard
url <- "https://api.coronavirus.data.gov.uk/v2/data?areaType=nation&areaCode=E92000001&metric=vaccinationsAgeDemographics&format=csv"
temp <- tempfile()
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

data <- read.csv(temp) %>% 
  select(date, age, cumVaccinationFirstDoseUptakeByVaccinationDatePercentage,
         cumVaccinationSecondDoseUptakeByVaccinationDatePercentage) %>% 
  set_names(c("date", "age", "First dose", "Second dose")) %>% 
  gather(dose, prop, c("First dose", "Second dose")) %>% 
  mutate(prop=prop/100, date=as.Date(date),
         age=gsub("_", "-", age))

agg_tiff("Outputs/COVIDVaxUptakexAge.tiff", units="in", width=9, height=6, res=800)
ggplot(data, aes(x=date, y=prop, colour=age, group=age))+
  geom_line()+
  geom_hline(yintercept=1, colour="Grey70", linetype=2)+
  scale_x_date(name="")+
  scale_y_continuous(name="Proportion of age group vaccinated", 
                     labels=label_percent(accuracy=1))+
  scale_colour_paletteer_d("pals::stepped", guide=guide_legend(reverse=TRUE),
                           name="Age")+
  facet_wrap(~dose)+
  theme_custom()+
  labs(title="Vaccine uptake is lower in younger age groups",
       subtitle="Cumulative proportion of English population vaccinated against COVID with one or two doses by age group",
       caption="Date from coronavirus.data.gov.uk | Plot by @VictimOfMaths")
dev.off()

  
  
  
  