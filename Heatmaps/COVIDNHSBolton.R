rm(list=ls())

library(tidyverse)
library(curl)
library(readxl)
library(paletteer)
library(ggtext)
library(extrafont)

newfile <- tempfile()
url <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/05/Weekly-covid-admissions-and-beds-publication-210527.xlsx"
maxday="BA"

newfile <- curl_download(url=url, destfile=newfile, quiet=FALSE, mode="wb")

newdata1 <- read_excel(newfile, sheet="All beds COVID", range=paste0("C18:", maxday,"304"), 
                       col_names=FALSE) %>% 
  gather(date, allbeds, c(3:ncol(.))) %>% 
  mutate(date=as.Date("2021-04-07")+days(as.numeric(substr(date, 4,7))-2)) %>% 
  rename(code=`...1`, trust=`...2`)

newdata2 <- read_excel(newfile, sheet="MV beds COVID", range=paste0("C18:", maxday,"304"), 
                       col_names=FALSE) %>% 
  gather(date, MVbeds, c(3:ncol(.))) %>% 
  mutate(date=as.Date("2021-04-07")+days(as.numeric(substr(date, 4,7))-2)) %>% 
  rename(code=`...1`, trust=`...2`)

oldfile <- tempfile()
oldurl <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/04/Weekly-covid-admissions-and-beds-publication-210429-up-to-210406.xlsx"

oldfile <- curl_download(url=oldurl, destfile=oldfile, quiet=FALSE, mode="wb")

olddata1 <- read_excel(oldfile, sheet="All beds COVID", range="C18:IS512", col_names=FALSE) %>% 
  gather(date, allbeds, c(3:ncol(.))) %>% 
  mutate(date=as.Date("2020-08-01")+days(as.numeric(substr(date, 4,7))-2)) %>% 
  rename(code=`...1`, trust=`...2`)

olddata2 <- read_excel(oldfile, sheet="MV beds COVID", range="C18:IS512", col_names=FALSE) %>% 
  gather(date, MVbeds, c(3:ncol(.))) %>% 
  mutate(date=as.Date("2020-08-01")+days(as.numeric(substr(date, 4,7))-2)) %>% 
  rename(code=`...1`, trust=`...2`) 

data <- bind_rows(newdata1, olddata1) %>% 
  merge(bind_rows(newdata2, olddata2), by=c("code", "date")) %>% 
  mutate(Otherbeds=allbeds-MVbeds) %>% 
  gather(bedtype, count, c("MVbeds", "Otherbeds")) %>% 
  mutate(count=if_else(is.na(count), 0, count),
         date=as.Date(date))

#Interpolate missing data from 29th November
plotdata <- data %>% 
  filter(code=="RMC") %>% 
  group_by(bedtype) %>% 
  mutate(count=if_else(date==as.Date("2020-11-29"), (lag(count, 1)+lead(count,1))/2,count))

ggplot(plotdata, aes(x=date, y=count, fill=fct_rev(bedtype)))+
  geom_area(position="stack", show.legend=FALSE)+
  scale_x_date(name="")+
  scale_y_continuous(name="Beds occupied")+
  scale_fill_paletteer_d("rcartocolor::Vivid", direction=-1)+
  theme_classic()+
  theme(plot.title.position="plot", plot.title=element_text(face="bold", size=rel(1.6)),
        text=element_text(family="Lato"), 
        plot.subtitle=element_markdown())+
  labs(title="Both hospital occupancy and ventilator use has risen in Bolton",
       subtitle="Daily number of patients in <span style='color:#CC3A8E;'>mechanical ventilator</span> and <span style='color:#A5AA99;'>other</span> beds with a positive COVID diagnosis",
       caption="Data from NHS England | Plot by @VictimOfMaths")
