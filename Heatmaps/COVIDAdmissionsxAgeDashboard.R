rm(list=ls())

library(tidyverse)
library(curl)
library(readxl)
library(lubridate)
library(ggstream)
library(extrafont)
library(paletteer)
library(RcppRoll)
library(ggrepel)
library(ragg)

theme_custom <- function() {
  theme_classic() %+replace%
    theme(plot.title.position="plot", plot.caption.position="plot",
          strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)),
          plot.title=element_text(face="bold", size=rel(1.5), hjust=0,
                                  margin=margin(0,0,5.5,0)),
          text=element_text(family="Lato"))
}

#Download admissions by age
#Nationally
source1 <- "https://api.coronavirus.data.gov.uk/v2/data?areaType=nation&areaCode=E92000001&metric=cumAdmissionsByAge&format=csv"
#Regionally
source2 <- "https://api.coronavirus.data.gov.uk/v2/data?areaType=nhsRegion&metric=cumAdmissionsByAge&format=csv"

temp <- tempfile()

temp <- curl_download(url=source1, destfile=temp, quiet=FALSE, mode="wb")
natdata <- read.csv(temp)
temp <- curl_download(url=source2, destfile=temp, quiet=FALSE, mode="wb")
regdata <- read.csv(temp)

data <- bind_rows(natdata, regdata) %>% 
  mutate(date=as.Date(date)) %>% 
  group_by(areaName, age) %>% 
  arrange(date) %>% 
  mutate(admrate=rate-lag(rate, 1),
         admissions=value-lag(value, 1),
         age=gsub("_to_", "-", age),
         age=factor(age, levels=c("0-5", "6-17", "18-64", "65-84", "85+")),
         admrate_roll=roll_mean(admrate, 7, align="center", fill=NA),
         peakrate=max(admrate_roll[date>as.Date("2020-10-01") & date<as.Date("2021-02-01")], na.rm=TRUE)) %>% 
  ungroup() %>% 
  mutate(peakprop=admrate_roll/peakrate)

agg_tiff("Outputs/COVIDAdmissionsxAge.tiff", units="in", width=9, height=6, res=500)
ggplot(data %>% filter(areaName=="England"), aes(x=date, y=peakprop, colour=age))+
  geom_hline(yintercept=1, linetype=2, colour="Grey80")+
  geom_line(show.legend=FALSE)+
  geom_text_repel(data=data %>% filter(date==max(date[!is.na(peakprop)]) & areaName=="England"),
                  aes(x=max(date[!is.na(peakprop)]), y=peakprop, label = age, 
                      colour=age),
                  family = "Calibri", direction = "y", xlim = c(as.Date("2022-01-01"), NA),
                  hjust = 0, segment.color = NA, box.padding = .3, show.legend = FALSE)+
  scale_x_date(name="", limits=c(NA_Date_, as.Date("2022-02-01")))+
  scale_y_continuous(name="Proportion of January 2021 peak",
                     labels=label_percent(accuracy=1))+
  scale_colour_paletteer_d("awtools::a_palette")+
  theme_custom()+
  labs(title="COVID vaccinations are doing a great job of keeping older age groups out of hospital",
       subtitle="Rolling 7-day average new COVID admissions (including those testing positive in hospital) as a proportion of the peak in January 2021",
       caption="Data from coronavirus.data.gov.uk | Plot by @VictimOfMaths")

dev.off()

agg_tiff("Outputs/COVIDAdmissionsxAgeAbs.tiff", units="in", width=9, height=6, res=500)
ggplot(data %>% filter(areaName=="England"), aes(x=date, y=admrate_roll, colour=age))+
  geom_line()+
  scale_x_date(name="")+
  scale_y_continuous(name="Daily new admissions per 100,000")+
  scale_colour_paletteer_d("awtools::a_palette", name="Age")+
  theme_custom()+
  labs(title="COVID admission rates are still highest in older age groups",
       subtitle="Rolling 7-day average new COVID admissions (including those testing positive in hospital)",
       caption="Data from coronavirus.data.gov.uk | Plot by @VictimOfMaths")

dev.off()

agg_tiff("Outputs/COVIDAdmissionsxAgexReg.tiff", units="in", width=9, height=6, res=500)
ggplot(data %>% filter(areaName!="England"), aes(x=date, y=peakprop, colour=age))+
  geom_hline(yintercept=1, linetype=2, colour="Grey80")+
  geom_line()+
  scale_x_date(name="", limits=c(NA_Date_, as.Date("2022-02-01")))+
  scale_y_continuous(name="Proportion of January 2021 peak",
                     labels=label_percent(accuracy=1))+
  scale_colour_paletteer_d("awtools::a_palette")+
  facet_wrap(~areaName)+
  theme_custom()+
  labs(title="The rise in COVID admissions in children isn't uniform across the country",
       subtitle="Rolling 7-day average new COVID admissions (including those testing positive in hospital) as a proportion of the peak in January 2021",
       caption="Data from coronavirus.data.gov.uk | Plot by @VictimOfMaths")

dev.off()

agg_tiff("Outputs/COVIDAdmissionsxAgexRegAbs.tiff", units="in", width=9, height=6, res=500)
ggplot(data %>% filter(areaName!="England"), aes(x=date, y=admrate_roll, colour=age))+
  geom_line()+
  scale_x_date(name="", limits=c(NA_Date_, as.Date("2022-02-01")))+
  scale_y_continuous(name="Proportion of January 2021 peak")+
  scale_colour_paletteer_d("awtools::a_palette")+
  facet_wrap(~areaName)+
  theme_custom()+
  labs(title="The rise in COVID admissions in children isn't uniform across the country",
       subtitle="Rolling 7-day average new COVID admissions (including those testing positive in hospital) as a proportion of the peak in January 2021",
       caption="Data from coronavirus.data.gov.uk | Plot by @VictimOfMaths")

dev.off()

agg_tiff("Outputs/COVIDAdmissionsxRegxAgeAbs.tiff", units="in", width=13, height=8, res=500)
ggplot(data %>% filter(areaName!="England"), aes(x=date, y=admrate_roll, colour=areaName))+
  geom_line()+
  scale_x_date(name="", limits=c(NA_Date_, as.Date("2022-02-01")))+
  scale_y_continuous(name="Proportion of January 2021 peak")+
  scale_colour_paletteer_d("colorblindr::OkabeIto", name="")+
  facet_wrap(~age, scales="free_y")+
  theme_custom()+
  theme(legend.position="top")+
  labs(title="The rise in COVID admissions in children isn't uniform across the country",
       subtitle="Rolling 7-day average new COVID admissions (including those testing positive in hospital) as a proportion of the peak in January 2021",
       caption="Data from coronavirus.data.gov.uk | Plot by @VictimOfMaths")

dev.off()

natdata %>% mutate(date=as.Date(date),
                   age=gsub("_to_", "-", age),
                   age=factor(age, levels=c("0-5", "6-17", "18-64", "65-84", "85+"))) %>% 
  group_by(age) %>% 
  arrange(date) %>% 
  mutate(newadm=value-lag(value, 1),
         newadm_roll=roll_mean(newadm, 7, align="center", fill=NA)) %>% 
  ungroup() %>% 
ggplot(aes(x=date, y=newadm_roll, fill=age))+
  geom_col(position="fill")+
  scale_x_date(name="")+
  scale_y_continuous(name="Proportion of admissions", labels=label_percent(accuracy=1))+
  scale_fill_paletteer_d("awtools::a_palette", name="Age")+
  
  theme_custom()

#Repeat for Omicron-specific analysis
data2 <- bind_rows(natdata, regdata) %>% 
  mutate(date=as.Date(date)) %>% 
  group_by(areaName, age) %>% 
  arrange(date) %>% 
  mutate(admrate=rate-lag(rate, 1),
         admissions=value-lag(value, 1),
         age=gsub("_to_", "-", age),
         age=factor(age, levels=c("0-5", "6-17", "18-64", "65-84", "85+")),
         admrate_roll=roll_mean(admrate, 7, align="center", fill=NA),
         peakrate=max(admrate_roll[date>as.Date("2021-10-01") & date<as.Date("2022-02-01")], na.rm=TRUE)) %>% 
  ungroup() %>% 
  mutate(peakprop=admrate_roll/peakrate)

agg_tiff("Outputs/COVIDAdmissionsxAgeOmi.tiff", units="in", width=9, height=6, res=500)
ggplot(data2 %>% filter(areaName=="England" & date>as.Date("2021-12-01")), 
       aes(x=date, y=peakprop, colour=age))+
  geom_hline(yintercept=1, linetype=2, colour="Grey80")+
  geom_line(show.legend=FALSE)+
  geom_text_repel(data=data %>% filter(date==max(date[!is.na(peakprop)]) & areaName=="England"),
                  aes(x=max(date[!is.na(peakprop)]), y=peakprop, label = age, 
                      colour=age),
                  family = "Calibri", direction = "y", xlim = c(as.Date("2022-03-04"), NA),
                  hjust = 0, segment.color = NA, box.padding = .3, show.legend = FALSE)+
  scale_x_date(name="", limits=c(NA_Date_, as.Date("2022-03-13")))+
  scale_y_continuous(name="Proportion of Omicron peak",
                     labels=label_percent(accuracy=1))+
  scale_colour_paletteer_d("awtools::a_palette")+
  theme_custom()+
  labs(title="COVID hospital admissions are rising in all age groups",
       subtitle="Rolling 7-day average new COVID admissions (including those testing positive in hospital) as a proportion of the peak in January 2022",
       caption="Data from coronavirus.data.gov.uk | Plot by @VictimOfMaths")

dev.off()

agg_tiff("Outputs/COVIDAdmissionsxAgeAbsOmi.tiff", units="in", width=9, height=6, res=500)
ggplot(data2 %>% filter(areaName=="England" & date>as.Date("2021-12-01")), 
       aes(x=date, y=admrate_roll, colour=age))+
  geom_line()+
  scale_x_date(name="")+
  scale_y_continuous(name="Daily new admissions per 100,000")+
  scale_colour_paletteer_d("awtools::a_palette", name="Age")+
  theme_custom()+
  labs(title="COVID admission rates are still highest in older age groups",
       subtitle="Rolling 7-day average new COVID admissions (including those testing positive in hospital)",
       caption="Data from coronavirus.data.gov.uk | Plot by @VictimOfMaths")

dev.off()

agg_tiff("Outputs/COVIDAdmissionsxAgexRegOmi.tiff", units="in", width=9, height=6, res=500)
ggplot(data2 %>% filter(areaName!="England"  & date>as.Date("2021-12-01")), 
       aes(x=date, y=peakprop, colour=age))+
  geom_hline(yintercept=1, linetype=2, colour="Grey80")+
  geom_line()+
  scale_x_date(name="", limits=c(NA_Date_, as.Date("2022-03-15")))+
  scale_y_continuous(name="Proportion of Omicron peak",
                     labels=label_percent(accuracy=1))+
  scale_colour_paletteer_d("awtools::a_palette", name="Age")+
  facet_wrap(~areaName)+
  theme_custom()+
  labs(title="COVID admissions are rising across all regions and age groups",
       subtitle="Rolling 7-day average new COVID admissions (including those testing positive in hospital) as a proportion of the peak in January 2021",
       caption="Data from coronavirus.data.gov.uk | Plot by @VictimOfMaths")

dev.off()

agg_tiff("Outputs/COVIDAdmissionsxAgexRegAbsOmi.tiff", units="in", width=9, height=6, res=500)
ggplot(data2 %>% filter(areaName!="England" & date>as.Date("2021-12-01")), 
       aes(x=date, y=admrate_roll, colour=age))+
  geom_line()+
  scale_x_date(name="", limits=c(NA_Date_, as.Date("2022-03-10")))+
  scale_y_continuous(name="Proportion of January 2022 peak")+
  scale_colour_paletteer_d("awtools::a_palette")+
  facet_wrap(~areaName)+
  theme_custom()+
  labs(title="The rise in COVID admissions in children isn't uniform across the country",
       subtitle="Rolling 7-day average new COVID admissions (including those testing positive in hospital) as a proportion of the peak in January 2021",
       caption="Data from coronavirus.data.gov.uk | Plot by @VictimOfMaths")

dev.off()

agg_tiff("Outputs/COVIDAdmissionsxRegxAgeAbsOmi.tiff", units="in", width=13, height=8, res=500)
ggplot(data2 %>% filter(areaName!="England"  & date>as.Date("2021-12-01")), 
       aes(x=date, y=admrate_roll, colour=areaName))+
  geom_line()+
  scale_x_date(name="", limits=c(NA_Date_, as.Date("2022-03-10")))+
  scale_y_continuous(name="Daily new admissions")+
  scale_colour_paletteer_d("colorblindr::OkabeIto", name="")+
  facet_wrap(~age, scales="free_y")+
  theme_custom()+
  theme(legend.position="top")+
  labs(title="The rise in COVID admissions in children isn't uniform across the country",
       subtitle="Rolling 7-day average new COVID admissions (including those testing positive in hospital) as a proportion of the peak in January 2021",
       caption="Data from coronavirus.data.gov.uk | Plot by @VictimOfMaths")

dev.off()
