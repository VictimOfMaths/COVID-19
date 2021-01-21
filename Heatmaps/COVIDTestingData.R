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
                                   tests="newTestsByPublishDate",
                                   positivity="uniqueCasePositivityBySpecimenDateRollingSum"))

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

plotto <- data %>% 
  filter(!is.na(tests_avg)) %>% 
  mutate(temp=max(date, na.rm=TRUE)) %>% 
  filter(date==temp & name=="England") %>% 
  select(date)

#Plot case rates by country by specimen date
ggplot(data)+
  geom_line(aes(x=date, y=testrate_avg, colour=name))+
  scale_x_date(name="", limits=c(as.Date("2020-08-01"), NA))+
  scale_y_continuous(name="Daily tests published per 100,000 population")+
  scale_colour_paletteer_d("fishualize::Scarus_quoyi", name="")+
  theme_classic()+
  theme(plot.title=element_text(face="bold", size=rel(1.2)))+
  labs(title="Testing rates have risen in England since Christmas",
       subtitle="Rolling 7-day average of new COVID-19 tests by publication date",
       caption="Date from coronavirus.gov.uk | Plot by @VictimOfMaths")

#Plot test rates by country
tiff("Outputs/COVIDTestRatesUK.tiff", units="in", width=8, height=6, res=500)
ggplot(data)+
  geom_line(aes(x=date, y=testrate_avg, colour=name))+
  scale_x_date(name="", limits=c(as.Date("2020-11-01"), NA))+
  scale_y_continuous(name="Daily tests published per 100,000 population")+
  scale_colour_paletteer_d("fishualize::Scarus_quoyi", name="")+
  theme_classic()+
  theme(plot.title=element_text(face="bold", size=rel(1.2)))+
  labs(title="Testing rates have risen in England since Christmas",
       subtitle="Rolling 7-day average of new COVID-19 tests by publication date",
       caption="Date from coronavirus.gov.uk | Plot by @VictimOfMaths")
dev.off()

#Plot positivity rates by country
tiff("Outputs/COVIDPosRatesUK.tiff", units="in", width=8, height=6, res=500)
ggplot(data)+
  geom_line(aes(x=date, y=posrate_avg, colour=name))+
  scale_x_date(name="", limits=c(as.Date("2020-11-01"), NA))+
  scale_y_continuous(name="Proportion of tests returned as positive", labels=percent_format(accuracy = 1))+
  scale_colour_paletteer_d("fishualize::Scarus_quoyi", name="")+
  theme_classic()+
  theme(plot.title=element_text(face="bold", size=rel(1.2)))+
  labs(title="Test positivity rates are falling in all four UK nations",
       subtitle="Rolling 7-day average proportion of tests which are returned positive",
       caption="Date from coronavirus.gov.uk | Plot by @VictimOfMaths")
dev.off()

#Regional data
APIdata2 <- get_data(filters="areaType=region", 
                    structure=list(date="date",
                                   name="areaName",
                                   cases="newCasesBySpecimenDate",
                                   positivity="uniqueCasePositivityBySpecimenDateRollingSum",
                                   people="uniquePeopleTestedBySpecimenDateRollingSum")) %>% 
            group_by(name) %>% 
              mutate(date=as.Date(date),
                     cases_avg=roll_mean(cases, 7, align="center", fill=NA),
                     pop=case_when(
                       name=="East of England" ~ 6236072,
                       name=="London" ~ 8961989,
                       name=="West Midlands" ~ 5934037,
                       name=="East Midlands" ~ 4835928,
                       name=="North East" ~ 2669941,
                       name=="Yorkshire and The Humber" ~ 5502967,
                       name=="North West" ~ 7341196,
                       name=="South East" ~ 9180135,
                       name=="South West" ~ 5624696),
                     peoplerate=people*100000/pop,
                     caserate=cases_avg*100000/pop)

tiff("Outputs/COVIDRegTestCounts.tiff", units="in", width=8, height=6, res=500)
ggplot(subset(APIdata2, date>as.Date("2020-11-01")))+
  geom_line(aes(x=date, y=peoplerate, colour=name))+
  scale_x_date(name="")+
  scale_y_continuous(name="Number of unique people tested per 100,000")+
  theme_classic()+
  theme(plot.title=element_text(face="bold", size=rel(1.2)))+
  scale_colour_paletteer_d("LaCroixColoR::paired", name="")+
  labs(title="The number of people being tested for COVID-19 fell dramatically over the holidays",
       subtitle="Rolling 7-day rate of the number of individuals being tested per 100,000 population",
       caption="Data from coronavirus.gov.uk | Plot by @VictimOfMaths")
dev.off()

tiff("Outputs/COVIDRegPosRates.tiff", units="in", width=8, height=6, res=500)
ggplot(subset(APIdata2, date>as.Date("2020-11-01")))+
  geom_line(aes(x=date, y=positivity/100, colour=name))+
  scale_x_date(name="")+
  scale_y_continuous(name="Positivity rate", labels=scales::label_percent(accuracy=1))+
  theme_classic()+
  theme(plot.title=element_text(face="bold", size=rel(1.2)))+
  scale_colour_paletteer_d("LaCroixColoR::paired", name="")+
  labs(title="The fall in test positivity has stalled",
       subtitle="Rolling 7-day average proportion of COVID-19 tests returned as positive",
       caption="Data from coronavirus.gov.uk | Plot by @VictimOfMaths")
dev.off()

tiff("Outputs/COVIDRegTestsxCases.tiff", units="in", width=8, height=6, res=500)
ggplot(subset(APIdata2, date>as.Date("2020-11-01")))+
  geom_path(aes(x=peoplerate, y=caserate, colour=name, alpha=date))+
  theme_classic()+
  theme(plot.title=element_text(face="bold", size=rel(1.2)))+
  scale_colour_paletteer_d("LaCroixColoR::paired", name="")+
  scale_alpha_date(guide="none")+
  scale_x_continuous(name="Number of unique people tested per 100,000")+
  scale_y_continuous(name="Number of positive test results per 100,000")+
  labs(title="COVID-19 testing seems to have changed dramatically during the holidays",
       subtitle="Rolling 7-day average rates of individuals being tested and positive results being returned",
       caption="Data from coronavirus.gov.uk | Plot by @VictimOfMaths")
dev.off()

