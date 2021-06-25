rm(list=ls())

library(tidyverse)
library(curl)
library(RcppRoll)
library(lubridate)
library(ggtext)
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

#Download data (have to do this separately for male and females because the dashboard is weird)
source.f <- "https://api.coronavirus.data.gov.uk/v2/data?areaType=nation&areaCode=E92000001&metric=femaleCases&format=csv"

temp <- tempfile()
temp <- curl_download(url=source.f, destfile=temp, quiet=FALSE, mode="wb")

data.f <- read.csv(temp) %>% 
  mutate(sex="Female")

source.m <- "https://api.coronavirus.data.gov.uk/v2/data?areaType=nation&areaCode=E92000001&metric=maleCases&format=csv"

temp <- tempfile()
temp <- curl_download(url=source.m, destfile=temp, quiet=FALSE, mode="wb")

data.m <- read.csv(temp) %>% 
  mutate(sex="Male")

#Combine and extract daily figures from cumulative ones
data <- bind_rows(data.f, data.m) %>% 
  mutate(date=as.Date(date)) %>% 
  group_by(age, sex) %>% 
  arrange(date) %>% 
  mutate(newcases=value-lag(value, 1),
         ratechange=rate-lag(rate, 1),
         cases_roll=roll_mean(newcases, 7, align="center", fill=NA),
         rates_roll=roll_mean(ratechange, 7, align="center", fill=NA)) %>% 
  ungroup() %>% 
  mutate(age=gsub("_", " ", age),
         age=factor(age, levels=c("0 to 4", "5 to 9", "10 to 14", "15 to 19", "20 to 24",
                                  "25 to 29", "30 to 34", "35 to 39", "40 to 44", 
                                  "45 to 49", "50 to 54", "55 to 59", "60 to 64",
                                  "65 to 69", "70 to 74", "75 to 79", "80 to 84",
                                  "85 to 89", "90+")))

agg_tiff("Outputs/COVIDCasesxSex.tiff", units="in", width=10, height=7, res=800)
ggplot(data %>% filter(date>as.Date("2021-05-10") & date<max(date)-days(3)), 
       aes(x=date, y=rates_roll, colour=sex))+
  geom_line(show.legend=FALSE)+
  scale_x_date(name="")+
  scale_y_continuous(name="Daily new cases per 100,000")+
  scale_colour_manual(values=c("#00cc99", "#6600cc"))+
  facet_wrap(~age)+
  theme_custom()+
  theme(plot.subtitle=element_markdown())+
  labs(title="There is an emerging gender gap in COVID cases in younger age groups",
       subtitle="Rolling 7-day average of new COVID case rates in <span style='color:#6600cc;'>men</span> and <span style='color:#00cc99;'>women</span> in England, by age.",
       caption="Data from coronavirus.data.gov.uk | Plot by @VictimOfMaths")
dev.off()

data %>% select(date, age, rates_roll, sex) %>% 
  spread(sex, rates_roll) %>% 
  mutate(ratio=Male/(Male+Female), test=Female-Male) %>% 
  filter(date>as.Date("2021-05-01")) %>% 
  ggplot(aes(x=date, y=ratio, colour=age, group=age))+
  geom_line()+
  theme_custom()

