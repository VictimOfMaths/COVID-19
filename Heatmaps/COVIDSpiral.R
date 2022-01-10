rm(list=ls())

library(tidyverse)
library(curl)
library(ggtext)
library(extrafont)
library(RcppRoll)
library(ragg)
library(paletteer)
library(lubridate)

theme_custom <- function() {
  theme_classic() %+replace%
    theme(plot.title.position="plot", plot.caption.position="plot",
          strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)),
          plot.title=element_text(face="bold", size=rel(1.5), hjust=0,
                                  margin=margin(0,0,5.5,0)),
          text=element_text(family="Lato"))
}

#Read in case data from dashboard
source <- "https://api.coronavirus.data.gov.uk/v2/data?areaType=nation&metric=newCasesBySpecimenDate&format=csv"

temp <- tempfile()
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

data <- read.csv(temp) %>% 
  mutate(date=as.Date(date)) %>% 
  rename("cases"="newCasesBySpecimenDate")

data <- data %>% 
  group_by(date) %>% 
  summarise(cases=sum(cases)) %>% 
  mutate(areaName="UK") %>% 
  ungroup() %>% 
  bind_rows(data) %>% 
  group_by(areaName) %>% 
  mutate(cases_roll=roll_mean(cases, 7, align="center", fill=NA),
         #Create variable to represent the base of each bar - change the number to tighten/relax the spiral
         increment=220*c(1:n()),
         #Add cases to the base to get the top of each bar
         incrementcases=increment+cases_roll,
         year=year(date)) %>% 
  ungroup() %>% 
  #Calculate the number of days since the start of the year
  group_by(year) %>% 
  mutate(yeardays=as.numeric(difftime(date ,as.Date(paste0(year, "-01-01")) , units = c("days")))) %>% 
  ungroup() %>% 
  #2020 being a leap year messes things up, so remove 31st December 2020 (arbitrarily) 
  #to make all the years the same length
  filter(!yeardays==365)

agg_tiff("Outputs/COVIDCasesSpiral.tiff", units="in", width=8, height=8, res=800)
ggplot()+
  #Need to plot each year separately, the +500 is a bit arbitrary to try and reduce the overlap
  #at the bottom of each bar (i.e. with the black baseline)
  #Use geom_boxplot as we want the base of each bar not to be at y=0, but on the spiral
  geom_boxplot(data=data %>% filter(areaName=="UK" & year=="2020" & ! is.na(cases_roll)), 
               aes(x=yeardays, ymin=increment+500, ymax=incrementcases, colour=cases_roll, 
                   fill=cases_roll, lower=increment+500, upper=incrementcases, middle=increment+500, 
                   group=date), stat = 'identity', show.legend=FALSE)+
  geom_boxplot(data=data %>% filter(areaName=="UK" & year=="2021"), 
               aes(x=yeardays, ymin=increment+500, ymax=incrementcases, colour=cases_roll, 
                   fill=cases_roll, lower=increment+500, upper=incrementcases, middle=increment+500, 
                   group=date), stat = 'identity', show.legend=FALSE)+
  geom_boxplot(data=data %>% filter(areaName=="UK" & year=="2022" & ! is.na(cases_roll)), 
               aes(x=yeardays, ymin=increment+500, ymax=incrementcases, colour=cases_roll, 
                   fill=cases_roll, lower=increment+500, upper=incrementcases, middle=increment+500, 
                   group=date), stat = 'identity', show.legend=FALSE)+
  geom_line(data=data %>% filter(areaName=="UK" & year=="2020" & ! is.na(cases_roll)),
            aes(x=yeardays, y=increment), colour="black")+
  geom_line(data=data %>% filter(areaName=="UK" & year=="2021"),
            aes(x=yeardays, y=increment), colour="black")+
  geom_line(data=data %>% filter(areaName=="UK" & year=="2022" & ! is.na(cases_roll)),
            aes(x=yeardays, y=increment), colour="black")+
  scale_x_continuous(breaks=c(0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334),
                     labels=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep",
                              "Oct", "Nov", "Dec"))+
  scale_colour_paletteer_c("viridis::rocket", direction=-1)+
  scale_fill_paletteer_c("viridis::rocket", direction=-1)+
  coord_polar()+
  theme_void()+
  theme(panel.grid.major.x=element_line(colour="Grey90"),
        axis.text.x=element_text(colour="Grey60"),
        text=element_text(family="Lato"), plot.title=element_text(face="bold", size=rel(1.8)),
        plot.title.position = "plot", plot.caption.position = "plot")+
  #Add low key legend for a bit of context
  geom_segment(aes(y=156200, yend=356200, x=8, xend=8), colour="Grey30",
               arrow = arrow(length=unit(0.20,"cm"), ends="both", type = "closed"))+
  annotate("text", x=10, y=250000, label="200,000\ncases\nper\nday", hjust=0, colour="Grey30",
           size=rel(2.5), family="Lato")+
  labs(title="The eternal spiral of COVID",
       subtitle="COVID case numbers in the UK since the start of the pandemic",
       caption="Data from coronavirus.data.gov.uk | Inspiration from the NYT | Plot by @VictimOfMaths")
    
dev.off()

#Repeat with hospital admissions. Because why not

#Read in data from dashboard
source2 <- "https://api.coronavirus.data.gov.uk/v2/data?areaType=overview&metric=newAdmissions&format=csv"

temp2 <- tempfile()
temp2 <- curl_download(url=source2, destfile=temp2, quiet=FALSE, mode="wb")

data2 <- read.csv(temp2) %>% 
  mutate(date=as.Date(date)) %>% 
  arrange(date) %>% 
  mutate(adm_roll=roll_mean(newAdmissions, 5, align="center", fill=NA),
         #Create variable to represent the base of each bar - change the number to tighten/relax the spiral
         increment=20*c(1:n()),
         #Add cases to the base to get the top of each bar
         incrementadm=increment+adm_roll,
         year=year(date)) %>% 
  #Calculate the number of days since the start of the year
  group_by(year) %>% 
  mutate(yeardays=as.numeric(difftime(date ,as.Date(paste0(year, "-01-01")) , units = c("days")))) %>% 
  ungroup() %>% 
  #2020 being a leap year messes things up, so remove 31st December 2020 (arbitrarily) 
  #to make all the years the same length
  filter(!yeardays==365)

agg_tiff("Outputs/COVIDAdmissionsSpiral.tiff", units="in", width=8, height=8, res=800)
ggplot()+
  #Need to plot each year separately
  geom_boxplot(data=data2 %>% filter(year=="2020" & ! is.na(adm_roll)), 
               aes(x=yeardays, ymin=increment, ymax=incrementadm, colour=adm_roll, 
                   fill=adm_roll, lower=increment, upper=incrementadm, middle=increment, 
                   group=date), stat = 'identity', show.legend=FALSE)+
  geom_boxplot(data=data2 %>% filter(year=="2021"), 
               aes(x=yeardays, ymin=increment, ymax=incrementadm, colour=adm_roll, 
                   fill=adm_roll, lower=increment, upper=incrementadm, middle=increment, 
                   group=date), stat = 'identity', show.legend=FALSE)+
  geom_boxplot(data=data2 %>% filter(year=="2022" & ! is.na(adm_roll)), 
               aes(x=yeardays, ymin=increment, ymax=incrementadm, colour=adm_roll, 
                   fill=adm_roll, lower=increment, upper=incrementadm, middle=increment, 
                   group=date), stat = 'identity', show.legend=FALSE)+
  #negative offset is to cover up the base of the bars
  geom_line(data=data2 %>% filter(year=="2020" & ! is.na(adm_roll)),
            aes(x=yeardays, y=increment-50), colour="black")+
  geom_line(data=data2 %>% filter(year=="2021"),
            aes(x=yeardays, y=increment-50), colour="black")+
  geom_line(data=data2 %>% filter(year=="2022"),
            aes(x=yeardays, y=increment-50), colour="black")+
  scale_x_continuous(breaks=c(0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334),
                     labels=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep",
                              "Oct", "Nov", "Dec"))+
  scale_colour_paletteer_c("viridis::mako", direction=-1)+
  scale_fill_paletteer_c("viridis::mako", direction=-1)+
  coord_polar()+
  theme_void()+
  theme(panel.grid.major.x=element_line(colour="Grey90"),
        axis.text.x=element_text(colour="Grey60"),
        text=element_text(family="Lato"), plot.title=element_text(face="bold", size=rel(1.8)),
        plot.title.position = "plot", plot.caption.position = "plot")+
  #Add low key legend for a bit of context
  geom_segment(aes(y=13050, yend=13050+2500, x=5, xend=5), colour="Grey30",
               arrow = arrow(length=unit(0.10,"cm"), ends="both", type = "closed"))+
  annotate("text", x=7, y=13100+1250, label="2,500\nadmissions\nper day", hjust=0, colour="Grey30",
           size=rel(2.5), family="Lato")+
  labs(title="The eternal spiral of COVID",
       subtitle="Daily new COVID admissions in the UK since the start of the pandemic",
       caption="Data from coronavirus.data.gov.uk | Inspiration from the NYT | Plot by @VictimOfMaths")

dev.off()

#And a sensible version
ggplot(data2, aes(x=yeardays, y=adm_roll, colour=as.factor(year)))+
  geom_line()+
  scale_x_continuous(breaks=c(0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334),
                     labels=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep",
                              "Oct", "Nov", "Dec"))+
  coord_polar()+
  theme_void()+
  theme(panel.grid.major.x=element_line(colour="Grey90"),
        axis.text.x=element_text(colour="Grey60"),
        text=element_text(family="Lato"), plot.title=element_text(face="bold", size=rel(1.8)),
        plot.title.position = "plot", plot.caption.position = "plot")

