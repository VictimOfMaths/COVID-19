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
library(snakecase)
library(ggrepel)
library(RcppRoll)

theme_custom <- function() {
  theme_classic() %+replace%
    theme(plot.title.position="plot", plot.caption.position="plot",
          strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)),
          plot.title=element_text(face="bold", size=rel(1.5), hjust=0,
                                  margin=margin(0,0,5.5,0)),
          text=element_text(family="Lato"))
}

#Download latest absence data
source <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2022/01/Staff-Absences-Web-File-Timeseries-2.xlsx"
temp <- tempfile()
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

#Trust:Region lookup
lookup <- read_excel(temp, sheet="Total Absences", range="B26:C163", col_names=FALSE) %>% 
  set_names("Region", "TrustCode")

totalraw <- read_excel(temp, sheet="Total Absences", range="C16:BA163", col_names=FALSE) 
  
COVIDraw <- read_excel(temp, sheet="COVID Absences", range="C16:BA163", col_names=FALSE) 

#Pull out national figures
nattotals <- totalraw %>% 
  slice(1) %>% 
  gather(Date, Total, c(3:ncol(.))) %>% 
  select(-c(1,2)) %>% 
  mutate(Date=as.Date("2021-11-29")+days(as.numeric(substr(Date, 4, 6))-3))

natCOVID <- COVIDraw %>% 
  slice(1) %>% 
  gather(Date, COVID, c(3:ncol(.))) %>% 
  select(-c(1,2)) %>% 
  mutate(Date=as.Date("2021-11-29")+days(as.numeric(substr(Date, 4, 6))-3))

natdata <- merge(nattotals, natCOVID) %>% 
  mutate(Other=Total-COVID) %>% 
  gather(Cause, Count, c(2:4)) %>% 
  group_by(Cause) %>% 
  mutate(Count_roll=roll_mean(Count, 7, align="center", fill=NA)) %>% 
  ungroup()

agg_tiff("Outputs/COVIDNHSAbsences.tiff", units="in", width=8, height=6, res=500)
ggplot(natdata %>% filter(Cause!="Total"), aes(x=Date, y=Count_roll, fill=Cause))+
  geom_area(show.legend=FALSE)+
  scale_x_date(name="")+
  scale_y_continuous(name="Total staff absent")+
  scale_fill_paletteer_d("lisa::Jean_MichelBasquiat_1")+
  theme_custom()+
  theme(plot.subtitle=element_markdown())+
  labs(title="NHS staff absences are falling, but still high",
       subtitle="Rolling 7-day average number of staff ill or isolating <span style='color:#C11432FF;'>due to COVID</span> or <span style='color:#009ADAFF ;'>absent for other reasons</span><br>in English acute NHS trusts",
       caption="Data from NHS England | Plot by @VictimOfMaths")

dev.off()

#Regional version
regtotals <- totalraw[c(3:9),] %>% 
  gather(Date, Total, c(3:ncol(.))) %>% 
  select(-c(1)) %>% 
  rename(Region=`...2`) %>% 
  mutate(Date=as.Date("2021-11-29")+days(as.numeric(substr(Date, 4, 6))-3))

regCOVID <- COVIDraw[c(3:9),] %>% 
  gather(Date, COVID, c(3:ncol(.))) %>% 
  select(-c(1)) %>% 
  rename(Region=`...2`) %>%   
  mutate(Date=as.Date("2021-11-29")+days(as.numeric(substr(Date, 4, 6))-3))

regdata <- merge(regtotals, regCOVID) %>% 
  mutate(Other=Total-COVID) %>% 
  gather(Cause, Count, c(3:5)) %>% 
  group_by(Cause, Region) %>% 
  mutate(Count_roll=roll_mean(Count, 7, align="center", fill=NA)) %>% 
  ungroup()

#Set up geofacet grid of NHS regions
mygrid <- data.frame(name=c("North West", "North East and Yorkshire", 
                            "Midlands","East of England",
                            "South West", "London", "South East"),
                     row=c(1,1,2,2,3,3,3), col=c(2,3,2,3,1,2,3),
                     code=c(1:7))

agg_tiff("Outputs/COVIDNHSAbsencesxReg.tiff", units="in", width=8, height=6, res=500)
ggplot(regdata %>% filter(Cause!="Total"), aes(x=Date, y=Count_roll, fill=Cause))+
  geom_area(show.legend=FALSE)+
  scale_x_date(name="")+
  scale_y_continuous(name="Total staff absent")+
  scale_fill_paletteer_d("lisa::Jean_MichelBasquiat_1")+
  facet_geo(~Region, grid=mygrid)+
  theme_custom()+
  theme(plot.subtitle=element_markdown())+
  labs(title="NHS staff absences are most acute in the Midlands and North of England",
       subtitle="Rolling 7-day average number of staff ill or isolating <span style='color:#C11432FF;'>due to COVID</span> or <span style='color:#009ADAFF ;'>absent for other reasons</span><br>in English acute NHS trusts",
       caption="Data from NHS England | Plot by @VictimOfMaths")

dev.off()

#Tidy up trust-level data
trusttotals <- totalraw[c(11:148),] %>%
  gather(Date, Total, c(3:ncol(.))) %>% 
  rename(Trust=`...2`, TrustCode=`...1`) %>% 
  mutate(Date=as.Date("2021-11-29")+days(as.numeric(substr(Date, 4, 6))-3))

trustCOVID <- COVIDraw[c(11:148),] %>% 
  gather(Date, COVID, c(3:ncol(.))) %>% 
  rename(Trust=`...2`, TrustCode=`...1`) %>% 
  mutate(Date=as.Date("2021-11-29")+days(as.numeric(substr(Date, 4, 6))-3))

trustdata <- merge(trusttotals, trustCOVID) %>% 
  mutate(Other=Total-COVID) %>% 
  gather(Cause, Count, c(4:6)) %>% 
  group_by(TrustCode, Trust, Cause) %>% 
  mutate(Count_roll=roll_mean(Count, 7, align="center", fill=NA)) %>% 
  ungroup()

#Bring in some denominators
#Source https://digital.nhs.uk/data-and-information/publications/statistical/nhs-workforce-statistics/september-2021
source <- "https://files.digital.nhs.uk/18/78CB98/NHS%20Workforce%20Statistics%2C%20September%202021%20England%20and%20Organisation.xlsx"
temp <- tempfile()
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

staffpops <- read_excel(temp, sheet="2. NHSE, Org & SG - HC", range="C12:E329", col_names=FALSE) %>% 
  filter(!is.na(`...2`)) %>% 
  set_names("TrustName", "TrustCode", "Staff") %>% 
  mutate(TrustCode=if_else(TrustCode=="RW6", "RM3", TrustCode),
         TrustName=if_else(TrustCode=="RM3", "Northern Healthcare Alliance NHS Foundation Trust",
                            TrustName)) %>% 
  group_by(TrustName, TrustCode) %>% 
  summarise(Staff=sum(Staff)) %>% 
  ungroup()

combined <- merge(trustdata, staffpops) %>% 
  mutate(AbsProp=Count/Staff, AbsProp_roll=Count_roll/Staff) %>% 
  merge(lookup)

combinedreg <- combined %>% 
  group_by(Date, Region, Cause) %>% 
  summarise(Count=sum(Count), Staff=sum(Staff), Count_roll=sum(Count_roll)) %>% 
  ungroup() %>% 
  mutate(AbsProp=Count/Staff, AbsProp_roll=Count_roll/Staff)

agg_tiff("Outputs/COVIDNHSAbsencePropxReg.tiff", units="in", width=8, height=6, res=500)
ggplot(combinedreg %>% filter(Cause=="Total"), aes(x=Date, y=AbsProp_roll, colour=Region))+
  geom_line()+
  scale_x_date(name="")+
  scale_y_continuous(name="Proportion of staff absent", limits=c(0,NA),
                     labels=label_percent(accuracy=1), breaks=c(0,0.02,0.04,0.06,0.08,0.1, 0.12))+
  scale_colour_paletteer_d("colorblindr::OkabeIto")+
  theme_custom()+
  labs(title="The Midlands and the North have the highest levels of NHS staff absence",
       subtitle="Proportion of NHS staff currently absent through sickness or isolation in acute trusts in England",
       caption="Data from NHS England | Plot by @VictimOfMaths")

dev.off()

agg_tiff("Outputs/COVIDNHSAbsencePropxTrust.tiff", units="in", width=8, height=6, res=500)
ggplot(combined %>% filter(Cause=="Total" & Date==as.Date("2022-01-16")), 
       aes(x=AbsProp, y=fct_reorder(Trust, AbsProp), fill=Region))+
  geom_col()+
  scale_x_continuous(labels=label_percent(accuracy=1), name="Proportion of staff absent")+
  scale_y_discrete(name="")+
  scale_fill_paletteer_d("colorblindr::OkabeIto")+
  theme_custom()+
  theme(axis.text.y=element_blank(), axis.ticks.y=element_blank())+
  labs(title="Some NHS trusts have more than 10% of staff off work",
       subtitle="Proportion of NHS staff absent for any reason as of 16th January, by trust (acute trusts only)",
       caption="Data from NHS England | Plot by @VictimOfMaths")
dev.off()

#Bring in bed occupancy data
source <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2022/01/Weekly-covid-admissions-and-beds-publication-220120.xlsx"
temp <- tempfile()
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

weeklyrange <- "DJ"

GACV19 <- read_excel(temp, sheet="Adult G&A Beds Occupied COVID", 
                          range=paste0("B25:", weeklyrange, "164"), col_names=FALSE)[-c(2),] %>% 
  gather(date, GACV19, c(4:ncol(.))) %>% 
  mutate(date=as.Date("2021-10-01")+days(as.numeric(substr(date, 4,7))-4)) %>% 
  rename(region=`...1`, code=`...2`, trust=`...3`)

GAOther <- read_excel(temp, sheet="Adult G&A Bed Occupied NonCOVID", 
                     range=paste0("B25:", weeklyrange, "164"), col_names=FALSE)[-c(2),] %>% 
  gather(date, GAOther, c(4:ncol(.))) %>% 
  mutate(date=as.Date("2021-10-01")+days(as.numeric(substr(date, 4,7))-4)) %>% 
  rename(region=`...1`, code=`...2`, trust=`...3`)

GAUnocc <- read_excel(temp, sheet="Adult G&A Beds Unoccupied", 
                     range=paste0("B25:", weeklyrange, "164"), col_names=FALSE)[-c(2),] %>% 
  gather(date, GAUnocc, c(4:ncol(.))) %>% 
  mutate(date=as.Date("2021-10-01")+days(as.numeric(substr(date, 4,7))-4)) %>% 
  rename(region=`...1`, code=`...2`, trust=`...3`)

CCCV19 <- read_excel(temp, sheet="Adult CC Beds Occupied COVID", 
                     range=paste0("B25:", weeklyrange, "164"), col_names=FALSE)[-c(2),] %>% 
  gather(date, CCCV19, c(4:ncol(.))) %>% 
  mutate(date=as.Date("2021-10-01")+days(as.numeric(substr(date, 4,7))-4)) %>% 
  rename(region=`...1`, code=`...2`, trust=`...3`)

CCOther <- read_excel(temp, sheet="Adult CC Bed Occupied NonCOVID", 
                     range=paste0("B25:", weeklyrange, "164"), col_names=FALSE)[-c(2),] %>% 
  gather(date, CCOther, c(4:ncol(.))) %>% 
  mutate(date=as.Date("2021-10-01")+days(as.numeric(substr(date, 4,7))-4)) %>% 
  rename(region=`...1`, code=`...2`, trust=`...3`)

CCUnocc <- read_excel(temp, sheet="Adult CC Beds Unoccupied", 
                     range=paste0("B25:", weeklyrange, "164"), col_names=FALSE)[-c(2),] %>% 
  gather(date, CCUnocc, c(4:ncol(.))) %>% 
  mutate(date=as.Date("2021-10-01")+days(as.numeric(substr(date, 4,7))-4)) %>% 
  rename(region=`...1`, code=`...2`, trust=`...3`)

#Merge and calculate % of beds currently occupied
occdata <- GACV19 %>% 
  merge(GAOther %>% select(date, code, GAOther), by=c("date", "code")) %>% 
  merge(GAUnocc %>% select(date, code, GAUnocc), by=c("date", "code")) %>% 
  merge(CCCV19 %>% select(date, code, CCCV19), by=c("date", "code")) %>% 
  merge(CCOther %>% select(date, code, CCOther), by=c("date", "code")) %>% 
  merge(CCUnocc %>% select(date, code, CCUnocc), by=c("date", "code")) %>% 
  mutate(GABeds=GAUnocc+GACV19+GAOther, 
         CCBeds=CCUnocc+CCCV19+CCOther, 
         GAOccprop=(GACV19+GAOther)/GABeds,
         CCOccprop=(CCCV19+CCOther)/CCBeds)

#Download PHE's trust  catchment data from and save as a csv.
#https://app.box.com/s/qh8gzpzeo1firv1ezfxx2e6c4tgtrudl
catchments <- read_csv("COVID_LA_Plots/2020 Trust Catchment Populations Worksheet.csv") %>% 
  filter(CatchmentYear==2018) %>% 
  group_by(TrustCode) %>% 
  summarise(catchpop=sum(Catchment)) %>% 
  ungroup()


alldata <- merge(combined, occdata, by.x=c("Date", "TrustCode"),
                 by.y=c("date", "code")) %>% 
  merge(catchments)

plotdata <- alldata %>% 
  filter(Date>=max(Date)-days(7) & Cause=="Total" & CCBeds>=10) %>% 
  mutate(trust=str_replace(trust, " NHS TRUST", ""),
         trust=str_replace(trust, "NHS FOUNDATION TRUST", ""),
         trust=to_any_case(trust, case="title"),
         trust=str_replace(trust, "King s", "King's"),
         trust=str_replace(trust, "Guy s", "Guy's"),
         trust=str_replace(trust, "George s", "George's"),
         trust=str_replace(trust, "Women s", "Women's"),
         trust=str_replace(trust, "Children s", "Children's"),
         trust=str_replace(trust, "Peter s", "Peter's"),
         trust=str_replace(trust, " Nhs Ft", ""))
         
agg_tiff("Outputs/COVIDCCBedsvsAbsences.tiff", units="in", width=9, height=7, res=500)
ggplot()+
  geom_hline(yintercept=0)+
  geom_vline(xintercept=0)+
  #geom_path(data=plotdata,
  #          aes(x=AbsProp, y=CCOccprop, group=trust, alpha=7-as.integer(max(Date)-Date)),
  #          colour="Grey50", show.legend=FALSE)+
  geom_point(data=plotdata %>% filter(Date==max(Date)),
             aes(x=AbsProp, y=CCOccprop, size=CCBeds, fill=Region), shape=21, alpha=0.7)+
  geom_text_repel(data=plotdata %>% filter(Date==max(Date)), 
                  aes(x=AbsProp, y=CCOccprop, label=trust), size=rel(3),
                  box.padding=0.7, point.padding=0)+
  scale_x_continuous(name="Proportion of NHS staff absent", limits=c(0,NA), 
                     label=label_percent(accuracy=1))+
  scale_y_continuous(name="Proportion of Critical Care beds occupied", limits=c(0,NA), 
                     label=label_percent(accuracy=1))+
  scale_fill_paletteer_d("colorblindr::OkabeIto", name="")+
  scale_size(guide="none")+
  theme_custom()+
  theme(axis.line=element_blank())+
  labs(title="Critical care capacity is at risk in many hospitals",
       subtitle=paste0("Current proportion of Critical Care beds which are occupied compared with the proportion of staff who are absent.\nBubbles are sized by total Critical Care bed capacity. Non-acute trusts and those with fewer than 10 CC beds are excluded.\nData up to ",
                       max(plotdata$Date)),
       caption="Data from NHS England, PHE & ONS\nPlot by @VictimOfMaths")

dev.off()

agg_tiff("Outputs/COVIDGABedsvsAbsences.tiff", units="in", width=9, height=7, res=500)
ggplot()+
  geom_hline(yintercept=0)+
  geom_vline(xintercept=0)+
  #geom_path(data=plotdata,
  #          aes(x=AbsProp, y=GAOccprop, group=trust, alpha=7-as.integer(max(Date)-Date)),
  #          colour="Grey50", show.legend=FALSE)+
  geom_point(data=plotdata %>% filter(Date==max(Date)),
             aes(x=AbsProp, y=GAOccprop, size=GABeds, fill=Region), shape=21, alpha=0.7)+
  geom_text_repel(data=plotdata %>% filter(Date==max(Date)), 
                  aes(x=AbsProp, y=GAOccprop, label=trust), size=rel(3),
                  box.padding=0.7, point.padding=0)+
  scale_x_continuous(name="Proportion of NHS staff absent", limits=c(0,NA), 
                     label=label_percent(accuracy=1))+
  scale_y_continuous(name="Proportion of General & Acute beds occupied", limits=c(0.5,1),
                     label=label_percent(accuracy=1))+
  scale_fill_paletteer_d("colorblindr::OkabeIto", name="")+
  scale_size(guide="none")+
  theme_custom()+
  theme(axis.line=element_blank())+
  labs(title="Most English hospitals have less than 10% of beds unoccupied",
       subtitle=paste0("Current proportion of General & Acute beds which are occupied compared with the proportion of staff who are absent.\nBubbles are sized by total Critical Care bed capacity. Non-acute trusts and those with fewer than 10 CC beds are excluded.\nData up to ",
                       max(plotdata$Date)),
       caption="Data from NHS England, PHE & ONS\nPlot by @VictimOfMaths")

dev.off()






