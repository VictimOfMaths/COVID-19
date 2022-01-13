rm(list=ls())

library(tidyverse)
library(curl)
library(readxl)
library(paletteer)
library(extrafont)
library(ragg)
library(lubridate)
library(scales)
library(ggtext)
library(geofacet)

theme_custom <- function() {
  theme_classic() %+replace%
    theme(plot.title.position="plot", plot.caption.position="plot",
          strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)),
          plot.title=element_text(face="bold", size=rel(1.5), hjust=0,
                                  margin=margin(0,0,5.5,0)),
          text=element_text(family="Lato"))
}

#Download latest primary diagnosis data
source <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2022/01/Primary-Diagnosis-Supplement-20220113.xlsx"
temp <- tempfile()
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

newdata_with <- read_excel(temp, sheet=1, range="C14:DB22", col_names=FALSE) %>% 
  drop_na() %>% 
  gather(date, with, c(2:ncol(.))) %>% 
  rename("Region"=`...1`) %>% 
  mutate(date=as.Date("2021-10-01")+days(as.numeric(substr(date, 4, 6))-2))

newdata_from <- read_excel(temp, sheet=2, range="C14:DB22", col_names=FALSE) %>% 
  drop_na() %>% 
  gather(date, from, c(2:ncol(.))) %>% 
  rename("Region"=`...1`) %>% 
  mutate(date=as.Date("2021-10-01")+days(as.numeric(substr(date, 4, 6))-2))

#download older data
source <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/12/Primary-Diagnosis-Supplement-20211209-20210618-20210930.xlsx"
temp <- tempfile()
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

olddata_with <- read_excel(temp, sheet=1, range="C14:DD22", col_names=FALSE) %>% 
  drop_na() %>% 
  gather(date, with, c(2:ncol(.))) %>% 
  rename("Region"=`...1`) %>% 
  mutate(date=as.Date("2021-06-18")+days(as.numeric(substr(date, 4, 6))-2))

olddata_from <- read_excel(temp, sheet=2, range="C14:DD22", col_names=FALSE) %>% 
  drop_na() %>% 
  gather(date, from, c(2:ncol(.))) %>% 
  rename("Region"=`...1`) %>% 
  mutate(date=as.Date("2021-06-18")+days(as.numeric(substr(date, 4, 6))-2))

data <- bind_rows(olddata_with, newdata_with) %>% 
  merge(bind_rows(olddata_from, newdata_from)) %>% 
  mutate(incidental=with-from) %>% 
  gather(type, count, c("with", "from", "incidental"))

data <- data %>% 
  group_by(date, type) %>% 
  summarise(count=sum(count)) %>% 
  mutate(Region="England") %>% 
  ungroup() %>% 
  bind_rows(data)

agg_tiff("Outputs/COVIDAdmissionsCause.tiff", units="in", width=9, height=7, res=500)
ggplot(data %>% filter(Region=="England" & type!="with"), 
       aes(x=date, y=count, colour=type))+
  #geom_area(position="stack")+
  geom_line(show.legend=FALSE)+
  scale_x_date(name="")+
  scale_y_continuous(name="Number of patients", limits=c(0,NA))+
  scale_colour_paletteer_d("palettetown::porygon")+
  theme_custom()+
  theme(plot.subtitle=element_markdown())+
  labs(title="The number of patients being treated 'for' COVID appears to have levelled off",
       subtitle="Patients in English acute hospitals <span style='color:#40A0D8;'>'for' COVID</span> and <span style='color:#F89088;'>where COVID is not the primary cause of hospitalisation</span>",
       caption="Data from NHS England | Plot by @VictimOfMaths")+
  annotate("text", x=as.Date("2021-10-30"), y=11500, label="Patients being treated for COVID",
           colour="#40A0D8", family="Lato")+
  annotate("text", x=as.Date("2021-09-30"), y=4500, label="Patients being treated for other causes, but 'with' COVID",
           colour="#F89088", family="Lato")
dev.off()

agg_tiff("Outputs/COVIDAdmissionsCauseLondon.tiff", units="in", width=9, height=7, res=500)
ggplot(data %>% filter(Region=="London" & type!="with"), 
       aes(x=date, y=count, colour=type))+
  #geom_area(position="stack")+
  geom_line(show.legend=FALSE)+
  scale_x_date(name="")+
  scale_y_continuous(name="Number of patients", limits=c(0,NA))+
  scale_colour_paletteer_d("palettetown::porygon")+
  theme_custom()+
  theme(plot.subtitle=element_markdown())+
  labs(title="Half of London's COVID patients are being treated 'with' COVID",
       subtitle="Patients in London's acute hospitals <span style='color:#40A0D8;'>'for' COVID</span> and <span style='color:#F89088;'>where COVID is not the primary cause of hospitalisation</span>",
       caption="Data from NHS England | Plot by @VictimOfMaths")+
  annotate("text", x=as.Date("2021-10-30"), y=920, label="Patients being treated for COVID",
           colour="#40A0D8", family="Lato")+
  annotate("text", x=as.Date("2021-09-30"), y=280, label="Patients being treated for other causes, but 'with' COVID",
           colour="#F89088", family="Lato")
dev.off()

agg_tiff("Outputs/COVIDAdmissionsCauseLondonStacked.tiff", units="in", width=9, height=7, res=500)
ggplot(data %>% filter(Region=="London" & type!="with"), 
       aes(x=date, y=count, fill=type))+
  geom_area(position="stack", show.legend=FALSE)+
  scale_x_date(name="")+
  scale_y_continuous(name="Number of patients", limits=c(0,NA))+
  scale_fill_paletteer_d("palettetown::porygon")+
  theme_custom()+
  theme(plot.subtitle=element_markdown())+
  labs(title="London's rising number of COVID patients are both 'with' and 'for' COVID",
       subtitle="Patients in London hospitals <span style='color:#40A0D8;'>'for' COVID</span> and <span style='color:#F89088;'>where COVID is not the primary cause of hospitalisation</span>",
       caption="Data from NHS England | Plot by @VictimOfMaths")
dev.off()

agg_tiff("Outputs/COVIDAdmissionsCauseLondonProp.tiff", units="in", width=9, height=7, res=500)
data %>% filter(type!="incidental" & Region=="London") %>% 
  spread(type, count) %>% 
  mutate(prop=from/with) %>% 
  ggplot(aes(x=date, y=prop))+
  geom_line(colour="RoyalBlue")+
  scale_x_date(name="")+
  scale_y_continuous(name="Proportion of COVID patients for whom\nCOVID is the main cause of hospitalisation",
                     limits=c(0,NA), labels=label_percent(accuracy=1))+
  theme_custom()+
  labs(title="COVID remains the main reason people with COVID end up in hospital in London",
       subtitle="Proportion of total COVID-positive patients assessed to be in hospital 'for' rather than 'with' COVID in London NHS trusts",
       caption="Data from NHS England | Plot by @VictimOfMaths")
dev.off()

agg_tiff("Outputs/COVIDAdmissionsCauseNW.tiff", units="in", width=9, height=7, res=500)
ggplot(data %>% filter(Region=="North West" & type!="with"), 
       aes(x=date, y=count, colour=type))+
  #geom_area(position="stack")+
  geom_line(show.legend=FALSE)+
  scale_x_date(name="")+
  scale_y_continuous(name="Number of patients", limits=c(0,NA))+
  scale_colour_paletteer_d("palettetown::porygon")+
  theme_custom()+
  theme(plot.subtitle=element_markdown())+
  labs(title="London's rising number of COVID patients are both 'with' and 'for' COVID",
       subtitle="Patients in London hospitals <span style='color:#40A0D8;'>'for' COVID</span> and <span style='color:#F89088;'>where COVID is not the primary cause of hospitalisation</span>",
       caption="Data from NHS England | Plot by @VictimOfMaths")
dev.off()

#Set up geofacet grid of NHS regions
mygrid <- data.frame(name=c("North West", "North East and Yorkshire", 
                            "Midlands","East of England",
                            "South West", "London", "South East"),
                     row=c(1,1,2,2,3,3,3), col=c(2,3,2,3,1,2,3),
                     code=c(1:7))


agg_tiff("Outputs/COVIDAdmissionsCausePropxReg.tiff", units="in", width=9.5, height=7, res=500)
data %>% filter(type!="incidental" & Region!="ENGLAND"& Region !="England") %>% 
  spread(type, count) %>% 
  mutate(prop=from/with) %>% 
  ggplot(aes(x=date, y=prop, colour=Region))+
  geom_line()+
  scale_x_date(name="")+
  scale_y_continuous(name="Proportion of COVID patients for whom\nCOVID is the main cause of hospitalisation",
                     limits=c(0,NA), labels=label_percent(accuracy=1))+
  scale_colour_paletteer_d("colorblindr::OkabeIto")+
  theme_custom()+
  labs(title="The proportion of COVID patients hospitalised because of COVID has fallen everywhere",
       subtitle="Proportion of total COVID-positive patients assessed to be in hospital 'for' rather than 'with' COVID by NHS region",
       caption="Data from NHS England | Plot by @VictimOfMaths")
dev.off()

agg_tiff("Outputs/COVIDAdmissionsCausexReg.tiff", units="in", width=9, height=7, res=500)
ggplot(data %>% filter(Region!="ENGLAND" & Region !="England" & type!="with"), 
       aes(x=date, y=count, colour=type))+
  #geom_area(position="stack")+
  geom_line(show.legend=FALSE)+
  scale_x_date(name="")+
  scale_y_continuous(name="Number of patients", limits=c(0,NA))+
  scale_colour_paletteer_d("palettetown::porygon")+
  facet_geo(~Region, scales="free_y", grid=mygrid)+
  theme_custom()+
  theme(plot.subtitle=element_markdown())+
  labs(title="Admissions 'with' have caught up with admissions 'for' COVID in several regions",
       subtitle="Patients in London hospitals <span style='color:#40A0D8;'>'for' COVID</span> and <span style='color:#F89088;'>where COVID is not the primary cause of hospitalisation</span>",
       caption="Data from NHS England | Plot by @VictimOfMaths")
dev.off()

#Bring in unseparated data to get figures from non-acute trusts
source2 <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2022/01/COVID-19-daily-admissions-and-beds-20220112.xlsx"
temp2 <- tempfile()
temp2 <- curl_download(url=source2, destfile=temp2, quiet=FALSE, mode="wb")

combdata <- read_excel(temp2, range="B90:DB97", col_names=FALSE) %>% 
  gather(date, alltrusts, c(2:ncol(.))) %>% 
  rename("Region"=`...1`) %>% 
  mutate(date=as.Date("2021-10-01")+days(as.numeric(substr(date, 4, 6))-2))

alldata <- data %>% spread(type, count) %>% 
  merge(combdata, all.x=TRUE) %>% 
  mutate("Non-acute"=alltrusts-with) %>% 
  gather(type, count, c(3:7))

agg_tiff("Outputs/COVIDAdmissionsCauseLondonv2.tiff", units="in", width=9, height=7, res=500)
ggplot(alldata %>% filter(Region=="London" & type %in% c("from", "incidental", "Non-acute") & 
                            date>as.Date("2021-10-01")), 
       aes(x=date, y=count, colour=type))+
  geom_line(show.legend=FALSE)+
  scale_x_date(name="")+
  scale_y_continuous(name="Number of patients", limits=c(0,NA))+
  scale_colour_manual(values=c("#40A0D8", "#F89088", "DarkRed"))+
  theme_custom()+
  theme(plot.subtitle=element_markdown())+
  labs(title="The number of COVID patients in non-acute hospitals in London is also rising",
       subtitle="Patients in London in acute hospitals <span style='color:#40A0D8;'>due to COVID</span> and <span style='color:#F89088;'>with COVID</span> and <span style='color:DarkRed;'>COVID-positive patients in non-acute hospitals</span>",
       caption="Data from NHS England | Plot by @VictimOfMaths")
dev.off()

agg_tiff("Outputs/COVIDAdmissionsCauseLondonv3.tiff", units="in", width=9, height=7, res=500)
alldata %>% filter(Region=="London" & type %in% c("from", "incidental", "Non-acute") & 
                     date>as.Date("2021-10-01")) %>% 
  spread(type, count) %>% 
  mutate(with=incidental+`Non-acute`) %>% 
  gather(type, count, c(3:6)) %>% 
  filter(type %in% c("from", "with")) %>% 
  ggplot(aes(x=date, y=count, colour=type))+
  geom_line(show.legend=FALSE)+
  scale_x_date(name="")+
  scale_y_continuous(name="Number of patients", limits=c(0,NA))+
  scale_colour_manual(values=c("#40A0D8", "DarkRed"))+
  theme_custom()+
  theme(plot.subtitle=element_markdown())+
  labs(title="In total the rise in patients 'with' COVID *may* be greater than patients 'for' COVID",
       subtitle="Patients in London in acute hospitals <span style='color:#40A0D8;'>due to COVID</span> and <span style='color:DarkRed;'>with COVID and COVID-positive patients in non-acute hospitals</span>",
       caption="Data from NHS England | Plot by @VictimOfMaths")
dev.off()
