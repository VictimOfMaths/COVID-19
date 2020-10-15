rm(list=ls())

library(tidyverse)
library(curl)
library(readxl)
library(ggtext)
library(ggridges)
library(ggstream)
library(paletteer)
library(lubridate)

#Surveillance reports taken from:
#https://www.gov.uk/government/statistics/national-flu-and-covid-19-surveillance-reports

#Confusingly, PHE number them by the week of publication, which is 1 week later
#than the data relates to. Numbers here relate to actual weeks of data.

temp1 <- tempfile()
source1 <- "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/925095/Weekly_Influenza_and_COVID19_report_data_w41.xlsx"
temp1 <- curl_download(url=source1, destfile=temp1, quiet=FALSE, mode="wb")

data.age <- read_excel(temp1, sheet=5, range="B9:L23")
colnames(data.age) <- c("week", "0-4", "5-9", "10-19", "20-29", "30-39", "40-49", "50-59",
                        "60-69", "70-79", "80+")

data.age <- gather(data.age, age, caserate, c(2:11))

#Bring in populations
temp2 <- tempfile()
source2 <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fpopulationandmigration%2fpopulationestimates%2fdatasets%2fpopulationestimatesforukenglandandwalesscotlandandnorthernireland%2fmid20182019laboundaries/ukmidyearestimates20182019ladcodes.xls"
temp2 <- curl_download(url=source2, destfile=temp2, quiet=FALSE, mode="wb")
pop <- as.data.frame(t(read_excel(temp2, sheet="MYE2-All", range="E9:CQ9", col_names=FALSE)))
pop$age <- c(0:90)
pop$ageband <- case_when(
  pop$age<5 ~ "0-4",
  pop$age<10 ~ "5-9",
  pop$age<20 ~ "10-19",
  pop$age<30 ~ "20-29",
  pop$age<40 ~ "30-39",
  pop$age<50 ~ "40-49",
  pop$age<60 ~ "50-59",
  pop$age<70 ~ "60-69",
  pop$age<80 ~ "70-79",
  TRUE ~ "80+"
)

pop <- pop %>% 
  group_by(ageband) %>% 
  summarise(pop=sum(V1))

data.age <- merge(data.age, pop, by.x="age", by.y="ageband", all.x=TRUE)

data.age$cases <- data.age$caserate*data.age$pop/100000

data.age$age <- factor(data.age$age, levels=c("0-4", "5-9", "10-19", "20-29", "30-39", "40-49", 
                                              "50-59", "60-69", "70-79", "80+"))

data.age$weeklab <- as.Date("2020-01-03")+days(7*(data.age$week-1))

tiff("Outputs/COVIDNewCasesHeatmapRecent.tiff", units="in", width=7, height=5, res=500)
ggplot(data.age)+
  geom_tile(aes(x=weeklab, y=age, fill=caserate))+
  scale_y_discrete(name="Age")+
  scale_fill_paletteer_c("viridis::magma", name="Cases\nper 100,000")+ 
  scale_x_date(name="Week ending", breaks=seq.Date(from=as.Date("2020-07-03"), 
                                                        to=max(data.age$weeklab), by="week"),
               date_labels="%d %b")+
  theme_classic()+ 
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  labs(title="Cases are concentrated in young people, but now rising in older age groups",
       subtitle="Rates of new COVID-19 cases in England by age during the pandemic",
       caption="Data from Public Health England | Plot by @VictimOfMaths")
dev.off()

#Absolute numbers
tiff("Outputs/COVIDNewCasesHeatmapAbsRecent.tiff", units="in", width=7, height=5, res=500)
ggplot(data.age)+
  geom_tile(aes(x=weeklab, y=age, fill=cases))+
  scale_y_discrete(name="Age")+
  scale_fill_paletteer_c("viridis::magma", name="Cases per day")+ 
  scale_x_date(name="Week ending", breaks=seq.Date(from=as.Date("2020-07-03"), 
                                                   to=max(data.age$weeklab), by="week"),
               date_labels="%d %b")+
  theme_classic()+ 
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  labs(title="Cases are rising fastest in 10-29 year olds",
       subtitle="Rates of new COVID-19 cases in England by age during the pandemic",
       caption="Data from Public Health England | Plot by @VictimOfMaths")
dev.off()

#Streamgraph
tiff("Outputs/COVIDCasesStreamgraphRecent.tiff", units="in", width=10, height=6, res=500)
ggplot(data.age, aes(x=weeklab, y=cases, fill=age))+
  geom_stream(bw=0.5)+
  scale_y_continuous(name="New cases per week", 
                     breaks=c(-30000, -25000, -20000, -15000,-10000,-5000,0,5000,10000,15000,
                              20000,25000,30000),
                     labels=c("30,000", "25,000", "20,000", "15,000", "10,000", "5,000", "0", 
                              "5,000", "10,000", "15,000", "20,000", "25,000", "30,000"))+
  scale_fill_paletteer_d("RColorBrewer::RdYlGn", name="Age", direction=-1)+ 
  scale_x_date(name="Week ending", breaks=seq.Date(from=as.Date("2020-07-03"), 
                                                   to=max(data.age$weeklab), by="week"),
               date_labels="%d %b")+
  theme_classic()+ 
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  labs(title="Cases are rising fastest in 10-29 year olds",
       subtitle="Confirmed new COVID-19 cases in England by age band",
       caption="Data from Public Health England | Plot by @VictimOfMaths")
dev.off()

#Analysis of positivity data
#By age and sex
pos.age.m <- read_excel(temp1, sheet="Figure 6. Positivity by agegrp", range="B44:L57",
                        col_names=FALSE)
colnames(pos.age.m) <- c("Week", "0-4", "5-9", "10-19", "20-29", "30-39", "40-49", "50-59", 
                         "60-69", "70-79", "80+")
pos.age.m <- gather(pos.age.m, age, posrate, c(2:11))
pos.age.m$sex <- "Male"

pos.age.f <- read_excel(temp1, sheet="Figure 6. Positivity by agegrp", range="B61:L74",
                        col_names=FALSE)
colnames(pos.age.f) <- c("Week", "0-4", "5-9", "10-19", "20-29", "30-39", "40-49", "50-59", 
                         "60-69", "70-79", "80+")
pos.age.f <- gather(pos.age.f, age, posrate, c(2:11))
pos.age.f$sex <- "Female"

pos.age <- bind_rows(pos.age.m, pos.age.f)

pos.age$age <- factor(pos.age$age, levels=c("0-4", "5-9", "10-19", "20-29", "30-39", "40-49", 
                                            "50-59", "60-69", "70-79", "80+"))

tiff("Outputs/COVIDPosRatexAge.tiff", units="in", width=10, height=6, res=500)
ggplot(pos.age, aes(x=Week, y=posrate/100, colour=age))+
  geom_line()+
  scale_colour_paletteer_d("RColorBrewer::RdYlGn", name="Age", direction=-1)+
  scale_y_continuous(name="Proportion of tests which are positive", 
                     labels = scales::percent_format(accuracy = 1))+  
  xlim(c(26,max(pos.age$Week)+1))+
  facet_wrap(~sex)+
  theme_classic()+
  theme(strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)))+
  labs(title="The positivity rate of tests is rising in adults",
       subtitle="Weekly positivity rates for Pillar 2 tests in England by age group",
       caption="Date from PHE | Visualisation by @VictimOfMaths")
dev.off()
