rm(list=ls())

library(tidyverse)
library(paletteer)
library(ukcovid19)
library(RcppRoll)
library(scales)

#Call data from PHE API
APIdata <- get_data(filters="areaType=nation", 
                    structure=list(date="date",
                                   name="areaName",
                                   cases="newCasesByPublishDate",
                                   tests="newTestsByPublishDate"))

data <- APIdata %>% 
  group_by(name) %>% 
  mutate(date=as.Date(date),
         #Calculate rolling averages
         tests_avg=roll_mean(tests, 7, align="center", fill=NA),
         cases_avg=roll_mean(cases, 7, align="center", fill=NA),
         #Calculate positivity rate
         posrate_avg=cases_avg/tests_avg,
         #Bring in population
         pop=case_when(
           name=="England" ~ 56286961,
           name=="Scotland" ~ 5463300,
           name=="Wales" ~ 3152879,
           name=="Northern Ireland" ~ 1893667
         ),
         testrate_avg=tests_avg*100000/pop,
         ) %>% 
  ungroup()

plotto=data %>% 
  filter(!is.na(tests_avg)) %>% 
  mutate(temp=max(date, na.rm=TRUE)) %>% 
  filter(date==temp & name=="England") %>% 
  select(date)

#Plot test rates by country
tiff("Outputs/COVIDTestRatesUK.tiff", units="in", width=8, height=6, res=500)
ggplot(data)+
  geom_line(aes(x=date, y=testrate_avg, colour=name))+
  scale_x_date(name="", limits=c(as.Date("2020-08-01"), NA_Date_))+
  scale_y_continuous(name="Daily tests published per 100,000 population")+
  scale_colour_paletteer_d("fishualize::Scarus_quoyi", name="")+
  theme_classic()+
  theme(plot.title=element_text(face="bold", size=rel(1.2)))+
  labs(title="Testing rates have rising in Wales in recent weeks",
       subtitle="Rolling 7-day average of new COVID-19 tests by publication date",
       caption="Date from coronavirus.gov.uk | Plot by @VictimOfMaths")
dev.off()

#Plot positivity rates by country
tiff("Outputs/COVIDPosRatesUK.tiff", units="in", width=8, height=6, res=500)
ggplot(data)+
  geom_line(aes(x=date, y=posrate_avg, colour=name))+
  scale_x_date(name="", limits=c(as.Date("2020-08-01"), NA_Date_))+
  scale_y_continuous(name="Proportion of tests returned as positive", labels=percent_format(accuracy = 1))+
  scale_colour_paletteer_d("fishualize::Scarus_quoyi", name="")+
  theme_classic()+
  theme(plot.title=element_text(face="bold", size=rel(1.2)))+
  labs(title="Test positivity rates in Wales have stayed fairly steady as test numbers have risen",
       subtitle="Rolling 7-day average proportion of tests which are returned positive",
       caption="Date from coronavirus.gov.uk | Plot by @VictimOfMaths")
dev.off()