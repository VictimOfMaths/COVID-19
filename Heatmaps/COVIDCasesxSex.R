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
  labs(title="The gap in COVID case rates between young men and women in England is growing",
       subtitle="Rolling 7-day average of new COVID case rates in <span style='color:#6600cc;'>men</span> and <span style='color:#00cc99;'>women</span> in England, by age.",
       caption="Data from coronavirus.data.gov.uk | Plot by @VictimOfMaths")+
  #Quarter final
  geom_vline(xintercept=as.Date("2021-07-03")+days(6), colour="Grey75", linetype=2)
  #geom_vline(xintercept=as.Date("2021-07-07")+days(6), colour="Grey75", linetype=2)+
  #geom_vline(xintercept=as.Date("2021-07-11")+days(6), colour="Grey75", linetype=2)
dev.off()

agg_tiff("Outputs/COVIDCasesxSexFull.tiff", units="in", width=10, height=7, res=800)
ggplot(data %>% filter(date<max(date)-days(3)), 
       aes(x=date, y=rates_roll, colour=sex))+
  geom_line(show.legend=FALSE)+
  scale_x_date(name="")+
  scale_y_continuous(name="Daily new cases per 100,000")+
  scale_colour_manual(values=c("#00cc99", "#6600cc"))+
  facet_wrap(~age)+
  theme_custom()+
  theme(plot.subtitle=element_markdown())+
  labs(title="The gender gap in COVID cases in younger adults in England is growing",
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

#Scotland
temp <- tempfile()
source <- "https://www.opendata.nhs.scot/dataset/b318bddf-a4dc-4262-971f-0ba329e09b87/resource/9393bd66-5012-4f01-9bc5-e7a10accacf4/download/trend_agesex_20210713.csv"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

scotdata <- read.csv(temp) %>% 
  filter(!AgeGroup %in% c("Total", "0 to 59", "60+")) %>% 
  mutate(date=as.Date(as.character(Date), format="%Y%m%d")) %>% 
  select(date, Sex, AgeGroup, DailyPositive) %>% 
  group_by(Sex, AgeGroup) %>% 
  mutate(cases_roll=roll_mean(DailyPositive, 7, align="center", fill=NA),
         #Populations hard coded because the data is unhelpfully structured and
         #I am a lazy, terrible, person. Source:
         #https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/population/population-estimates/mid-year-population-estimates/mid-2020
         pop=case_when(
           AgeGroup=="0 to 14" & Sex=="Male" ~ 135959+152847+151875,
           AgeGroup=="15 to 19" & Sex=="Male" ~ 144207,
           AgeGroup=="20 to 24" & Sex=="Male" ~ 173302,
           AgeGroup=="25 to 44" & Sex=="Male" ~ 189139+185637+174079+159586,
           AgeGroup=="45 to 64" & Sex=="Male" ~ 169376+189355+193348+170701,
           AgeGroup=="65 to 74" & Sex=="Male" ~ 144529+135910,
           AgeGroup=="75 to 84" & Sex=="Male" ~ 89206+60270,
           AgeGroup=="85plus" & Sex=="Male" ~ 32227+13659,
           AgeGroup=="0 to 14" & Sex=="Female" ~ 127847+145056+146206,
           AgeGroup=="15 to 19" & Sex=="Female" ~ 137913,
           AgeGroup=="20 to 24" & Sex=="Female" ~ 168453,
           AgeGroup=="25 to 44" & Sex=="Female" ~ 188065+188432+181587+164780,
           AgeGroup=="45 to 64" & Sex=="Female" ~ 180548+203758+205996+181868,
           AgeGroup=="65 to 74" & Sex=="Female" ~ 155904+149920,
           AgeGroup=="75 to 84" & Sex=="Female" ~ 109004+83026,
           AgeGroup=="85plus" & Sex=="Female" ~ 52335+30090),
         caserate_roll=cases_roll*100000/pop)

agg_tiff("Outputs/COVIDCasesxSexScot.tiff", units="in", width=10, height=7, res=800)
ggplot(scotdata %>% filter(date>as.Date("2021-05-10") & date<max(date)-days(3) &
                             Sex!="Total"), 
       aes(x=date, y=caserate_roll, colour=Sex))+
  geom_line(show.legend=FALSE)+
  scale_x_date(name="")+
  scale_y_continuous(name="Daily new cases per 100,000")+
  scale_colour_manual(values=c("#00cc99", "#6600cc"))+
  facet_wrap(~AgeGroup)+
  theme_custom()+
  theme(plot.subtitle=element_markdown())+
  labs(title="The gender gap in Scottish COVID cases has all but disappeared",
       subtitle="Rolling 7-day average of new COVID case rates in <span style='color:#6600cc;'>men</span> and <span style='color:#00cc99;'>women</span> in Scotland, by age.",
       caption="Data from Public Health Scotland | Plot by @VictimOfMaths")
dev.off()
