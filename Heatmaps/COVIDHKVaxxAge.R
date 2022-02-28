rm(list=ls())

library(tidyverse)
library(curl)
library(RcppRoll)
library(lubridate)
library(ggtext)
library(extrafont)
library(ragg)
library(paletteer)
library(readxl)
library(scales)
library(stringr)

theme_custom <- function() {
  theme_classic() %+replace%
    theme(plot.title.position="plot", plot.caption.position="plot",
          strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)),
          plot.title=element_text(face="bold", size=rel(1.5), hjust=0,
                                  margin=margin(0,0,5.5,0)),
          text=element_text(family="Lato"),
          plot.subtitle=element_text(colour="Grey40", hjust=0, vjust=1),
          plot.caption=element_text(colour="Grey40", hjust=1, vjust=1, size=rel(0.8)),
          axis.text=element_text(colour="Grey40"),
          axis.title=element_text(colour="Grey20"),
          legend.text=element_text(colour="Grey40"),
          legend.title=element_text(colour="Grey20"))
}


#Download HK vaccination data by age and dose from HK gov dashboard
source <- "https://static.data.gov.hk/covid-vaccine/bar_age.csv"
temp <- tempfile()
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

data <- read.csv(temp) %>% 
  select(age_group, population, firstDose.total, secondDose.total, thirdDose.total) %>% 
  set_names("age", "pop", "Dose1", "Dose2", "Dose3") 

areadata <- data %>% 
  mutate(unvax=pop-Dose1,
         Dose1only=Dose1-Dose2,
         Dose2only=Dose2-Dose3,
         agemin=case_when(
           age=="Aged 3-11" ~ 3,
           TRUE ~ as.numeric(substr(age, 6,7))),
         agemax=case_when(
           age=="Aged 3-11" ~ 12,
           age=="Aged 80 and above" ~ 100,
           TRUE ~ as.numeric(substr(age, 9,10))+1),
         agewidth=agemax-agemin,
         dose3plot=pop/agewidth,
         dose2plot=(Dose2only+Dose1only+unvax)/agewidth,
         dose1plot=(Dose1only+unvax)/agewidth,
         unvaxplot=unvax/agewidth)
  
tiff("Outputs/COVIDVaxxAgeHKArea.tiff", units="in", width=9, height=6.6, res=500)
ggplot(areadata)+
  geom_rect(aes(xmin=0, xmax=unvaxplot, ymin=agemin, ymax=agemax), fill="Grey70")+
  geom_rect(aes(xmin=unvaxplot, xmax=dose1plot, ymin=agemin, ymax=agemax), fill="#D32934")+
  geom_rect(aes(xmin=dose1plot, xmax=dose2plot, ymin=agemin, ymax=agemax), fill="#2F191B")+
  geom_rect(aes(xmin=dose2plot, xmax=dose3plot, ymin=agemin, ymax=agemax), fill="#2BAA92")+
  scale_x_continuous(name="Number of people at each single year of age", labels=abs)+
  scale_y_continuous(name="Age", limits=c(0,100))+
  theme_custom()+
  theme(plot.subtitle=element_markdown(colour="Black"))+
  labs(title="Hong Kong's vaccination programme has been poorly targeted",
       subtitle="The number of people who are <span style='color:Grey70;'>unvaccinated</span>, or have received <span style='color:#D32934;'>one </span>,<span style='color:#2F191B;'>two </span>or <span style='color:#2BAA92;'>three </span>doses",
       caption="Data from www.covidvaccine.gov.hk | Plot by @VictimOfMaths")
dev.off()
  
bardata <- data %>% 
  mutate(unvax=pop-Dose1) %>% 
  gather(Doses, Number, c(3:6)) %>% 
  mutate(age=factor(age, levels=c("Aged 3-11", "Aged 12-19", "Aged 20-29", "Aged 30-39",
                                  "Aged 40-49", "Aged 50-59", "Aged 60-69", "Aged 70-79",
                                  "Aged 80 and above")),
         Doses=factor(Doses, levels=c("Dose3", "Dose2", "Dose1", "unvax")))

tiff("Outputs/COVIDVaxxAgeHK.tiff", units="in", width=7, height=6, res=500)
ggplot(bardata, aes(x=Number, y=age, fill=Doses))+
  geom_col(position="fill")+
  scale_x_continuous(name="Proportion of population", labels=label_percent(accuracy=1))+
  scale_y_discrete(name="")+
  scale_fill_manual(values=c("#2BAA92", "#2F191B", "#D32934", "Grey70"), 
                    labels=c("Booster", "2 doses", "1 dose", "Unvaccinated"),
                    guide = guide_legend(reverse = TRUE), name="")+
  theme_custom()+
  theme(legend.position="top")+
  labs(title="Hong Kong has many unvaccinated older people",
       caption="Data from www.covidvaccine.gov.hk | Plot by @VictimOfMaths")
dev.off()
