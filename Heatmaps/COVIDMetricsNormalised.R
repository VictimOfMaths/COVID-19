#Code to replicate this nice graph from Paul Mainwood
#https://twitter.com/PaulMainwood/status/1449467211442098181

library(tidyverse)
library(curl)
library(lubridate)
library(RcppRoll)
library(scales)

#Download data from dashboard
url <- "https://api.coronavirus.data.gov.uk/v2/data?areaType=overview&metric=newAdmissions&metric=newCasesBySpecimenDate&metric=newDeaths28DaysByDeathDate&format=csv"
temp <- tempfile()
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

#Read the data in and tidy it up
data <- read.csv(temp) %>% 
  select(-c(1:3)) %>% 
  set_names(c("date", "Admissions", "Cases", "Deaths")) %>% 
  gather(metric, count, c(2:4)) %>% 
  mutate(date=as.Date(date),
         metric=factor(metric, levels=c("Cases", "Admissions", "Deaths"))) %>% 
  group_by(metric) %>% 
  #Calculate rolling means
  mutate(count_roll=roll_mean(count, 7, align="center", fill=NA)) %>% 
  #Normalise against the peak.
  #Step 1 - grab the peak value for each metric
  #Option 1, just using the max value across the entire time series
  #mutate(max=max(count, na.rm=TRUE)) %>% 
  #Option 2, normalising against the max value within a chosen date range
  mutate(max=max(count_roll[date>as.Date("2020-08-01") & date<as.Date("2021-05-01")], 
                 na.rm=TRUE)) %>% 
  ungroup() %>% 
  #Step 2 - calculate each date's value as a proportion of the peak
  mutate(prop=count_roll/max)

#Simple replication
ggplot(data, aes(x=date, y=prop, colour=metric))+
  geom_line()

#Prettier version (because why not)
library(extrafont)
library(paletteer)
library(ggtext)
library(ragg)

agg_tiff("Outputs/COVIDMetricsNormalised.tiff", units="in", width=9, height=6, res=500)
ggplot(data %>% filter(date>as.Date("2020-04-01")), 
       aes(x=date, y=prop, colour=metric))+
  geom_line(show.legend=FALSE)+
  scale_x_date(name="", date_labels="%B %y")+
  scale_y_continuous(labels=label_percent(accuracy=1), 
                     name="Level as a proportion of January 2021 peak")+
  scale_colour_paletteer_d("wesanderson::Darjeeling1")+
  theme_classic()+
  theme(text=element_text(family="Lato"), plot.title.position="plot",
        plot.title=element_text(face="bold", size=rel(1.5)),
        plot.subtitle=element_markdown())+
  labs(title="Vaccination is keeping admissions and deaths lower than previous waves",
       subtitle="Rolling 7-day average of new COVID <span style='color:#FF0000;'>cases</span>, <span style='color:#00A08A;'>admissions</span> and <span style='color:#F2AD00;'>deaths</span> as a proportion of their peak value in January 2021",
       caption="Plot inspired by @PaulMainwood | Data from coronavirus.data.gov.uk | Plot by @VictimOfMaths")
dev.off()

