rm(list=ls())

library(tidyverse)
library(curl)
library(readxl)
library(ragg)
library(lubridate)
library(extrafont)
library(ggtext)
library(ggstream)
library(paletteer)

theme_custom <- function() {
  theme_classic() %+replace%
    theme(plot.title.position="plot", plot.caption.position="plot",
          strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)),
          plot.title=element_text(face="bold", size=rel(1.5), hjust=0,
                                  margin=margin(0,0,5.5,0)),
          text=element_text(family="Lato"))
}

#Get 2021 COVID deaths data
url21 <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fbirthsdeathsandmarriages%2fdeaths%2fdatasets%2fweeklyprovisionalfiguresondeathsregisteredinenglandandwales%2f2021/publishedweek3820211.xlsx"

temp <- tempfile()
temp <- curl_download(url=url21, destfile=temp, quiet=FALSE, mode="wb")

COVIDdeaths21 <- read_excel(temp, sheet=7, range="B12:AN31", col_names=FALSE) %>% 
  gather(week, COVID, c(2:ncol(.))) %>% 
  rename(age=`...1`) %>% 
  mutate(week=as.numeric(substr(week, 4, 5))-1,
         date=as.Date("2021-01-02")+weeks(week-1))

#Get 2020 COVID deaths data
url20 <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fbirthsdeathsandmarriages%2fdeaths%2fdatasets%2fweeklyprovisionalfiguresondeathsregisteredinenglandandwales%2f2020/publishedweek532020.xlsx"

temp <- tempfile()
temp <- curl_download(url=url20, destfile=temp, quiet=FALSE, mode="wb")

COVIDdeaths20 <- read_excel(temp, sheet=6, range="B12:BC31", col_names=FALSE) %>% 
  gather(week, COVID, c(2:ncol(.))) %>% 
  rename(age=`...1`) %>% 
  mutate(week=as.numeric(substr(week, 4, 5))-1,
         date=as.Date("2019-12-28")+weeks(week-1))

data <- bind_rows(COVIDdeaths20, COVIDdeaths21) %>% 
  mutate(age=factor(age, levels=c("<1", "1-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34",
                                  "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69",
                                  "70-74", "75-79", "80-84", "85-89", "90+")))

meanage <- data %>% 
  group_by(date) %>% 
  mutate(agemid=case_when(
    age=="<1" ~ 0.5, age=="1-4" ~ 2.5,  age=="5-9" ~ 7.5, age=="10-14" ~ 12.5,
    age=="15-19" ~ 17.5, age=="20-24" ~ 22.5, age=="25-29" ~ 27.5, age=="30-34" ~ 32.5,
    age=="35-39" ~ 37.5, age=="40-44" ~ 42.5, age=="45-49" ~ 47.5, age=="50-54" ~ 52.5,
    age=="55-59" ~ 57.5, age=="60-64" ~ 62.5, age=="65-69" ~ 67.5,  age=="70-74" ~ 72.5,
    age=="75-79" ~ 77.5, age=="80-84" ~ 82.5, age=="85-89" ~ 87.5, age=="90+" ~ 92.5)) %>% 
  summarise(meanage=weighted.mean(agemid, COVID)) %>% 
  ungroup()

agg_tiff("Outputs/COVIDeathsMeanAge.tiff", units="in", width=9, height=6, res=500)
ggplot(meanage, aes(x=date, y=meanage))+
  geom_line(colour="Red")+
  scale_x_date(name="")+
  scale_y_continuous(name="Average age of COVID death")+
  theme_custom()+
  labs(title="COVID vaccines have substantially reduced the average age of COVID deaths",
       subtitle="Mean age of deaths involving COVID by week of registration in England & Wales",
       caption="Data from ONS | Plot by @VictimOfMaths")
dev.off()

agg_tiff("Outputs/COVIDeathsEWxAgeProp.tiff", units="in", width=10, height=7, res=500)
ggplot(data %>% filter(date>=as.Date("2020-03-07")), aes(x=date, y=COVID, fill=age))+
  geom_col(position="fill")+
  scale_x_date(name="")+
  scale_y_continuous(name="Proportion of COVID deaths", labels=label_percent(accuracy=1))+
  scale_fill_paletteer_d("pals::stepped", name="Age")+
  theme_custom()+
  labs(title="The proportion of COVID deaths among older age groups fell as vaccines were rolled out",
       subtitle="Deaths involving COVID by age and week of registration in England & Wales",
       caption="Data from ONS | Plot by @VictimOfMaths")
dev.off()

agg_tiff("Outputs/COVIDeathsEWxAge.tiff", units="in", width=9, height=6, res=500)
ggplot(data %>% filter(date>=as.Date("2020-03-07")), aes(x=date, y=COVID, fill=age))+
  geom_col()+
  scale_x_date(name="")+
  scale_y_continuous(name="Deaths involving COVID")+
  scale_fill_paletteer_d("pals::stepped", name="Age")+
  theme_custom()+
  labs(title="COVID deaths in the delta wave are much lower than previous waves for all ages",
       subtitle="Deaths involving COVID by age and week of registration in England & Wales",
       caption="Data from ONS | Plot by @VictimOfMaths")
dev.off()

agg_tiff("Outputs/COVIDeathsEWxAgeStream.tiff", units="in", width=9, height=6, res=500)
ggplot(data %>% filter(date>=as.Date("2020-03-07")), aes(x=date, y=COVID, fill=age))+
  geom_stream()+
  scale_x_date(name="")+
  scale_y_continuous(name="Deaths involving COVID")+
  scale_fill_paletteer_d("pals::stepped", name="Age")+
  theme_custom()+
  labs(title="COVID deaths in the delta wave are much lower than previous waves for all ages",
       subtitle="Mean age of deaths involving COVID by week of registration in England & Wales",
       caption="Data from ONS | Plot by @VictimOfMaths")
dev.off()
