rm(list=ls())

library(tidyverse)
library(curl)
library(readxl)
library(ggtext)
library(ggridges)
library(paletteer)
library(ggstream)

#Scottish age data
temp <- tempfile()
source <- "https://www.opendata.nhs.scot/dataset/b318bddf-a4dc-4262-971f-0ba329e09b87/resource/9393bd66-5012-4f01-9bc5-e7a10accacf4/download/trend_agesex_20200912.csv"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

data <- read.csv(temp)

data <- data %>% 
  filter(!AgeGroup %in% c("Total", "Unknown")) %>% 
  mutate(date=as.Date(as.character(Date), format="%Y%m%d"))

#Bring in populations
#Bring in LA populations
temp2 <- tempfile()
source2 <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fpopulationandmigration%2fpopulationestimates%2fdatasets%2fpopulationestimatesforukenglandandwalesscotlandandnorthernireland%2fmid2019april2020localauthoritydistrictcodes/ukmidyearestimates20192020ladcodes.xls"
temp2 <- curl_download(url=source2, destfile=temp2, quiet=FALSE, mode="wb")
pop.m <- as.data.frame(t(read_excel(temp2, sheet="MYE2 - Males", range="E387:CQ387", col_names=FALSE)))
pop.m$age <- c(0:90)
pop.m$Sex <- "Male"
pop.f <- as.data.frame(t(read_excel(temp2, sheet="MYE2 - Females", range="E387:CQ387", col_names=FALSE)))
pop.f$age <- c(0:90)
pop.f$Sex <- "Female"
pop <- bind_rows(pop.m, pop.f)

pop$age <- c(0:90)
pop$AgeGroup <- case_when(
  pop$age<15 ~ "Under 15",
  pop$age<20 ~ "15 to 19",
  pop$age<25 ~ "20 to 24",
  pop$age<45 ~ "25 to 44",
  pop$age<65 ~ "45 to 64",
  pop$age<75 ~ "65 to 74",
  pop$age<85 ~ "75 to 84",
  TRUE ~ "85plus"
)

pop1 <- pop %>% 
  group_by(AgeGroup, Sex) %>% 
  summarise(pop=sum(V1))

pop2 <- pop %>% 
  group_by(AgeGroup) %>%
  summarise(pop=sum(V1)) %>% 
  mutate(Sex="Total")

pop <- bind_rows(pop1, pop2)

data <- merge(data, pop, by=c("Sex", "AgeGroup"), all.x=TRUE)

data$posrate <- data$DailyPositive*100000/data$pop

tiff("Outputs/COVIDCasesStreamgraphScotlandxSex.tiff", units="in", width=10, height=6, res=500)
ggplot(subset(data, Sex!="Total"), aes(x=date, y=DailyPositive, fill=AgeGroup))+
  geom_stream(bw=0.2)+
  scale_fill_paletteer_d("awtools::a_palette", name="Age",
                         labels=c("15-19", "20-24", "25-44", "45-64", "65-74", "75-84", "85+"))+
  scale_y_continuous(name="New cases per day", breaks=c(-100,-50,0,50,100),
                     labels=c("100", "50", "0", "50", "100"))+
  scale_x_date(name="")+
  facet_wrap(~Sex)+
  theme_classic()+
  theme(strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)))+
  labs(title="The resurgence in COVID-19 cases in Scotland is among the under-45s",
       subtitle="Confirmed new cases in Scotland by sex and age",
       caption="Date from Public Health Scotland | Plot by @VictimOfMaths")
dev.off()

tiff("Outputs/COVIDCasesStreamgraphScotland.tiff", units="in", width=10, height=6, res=500)
ggplot(subset(data, Sex=="Total"), aes(x=date, y=DailyPositive, fill=AgeGroup))+
  geom_stream(bw=0.2)+
  scale_fill_paletteer_d("awtools::a_palette", name="Age",
                         labels=c("15-19", "20-24", "25-44", "45-64", "65-74", "75-84", "85+"))+
  scale_y_continuous(name="New cases per day", breaks=c(-150,-100,-50,0,50,100,150),
                     labels=c("150", "100", "50", "0", "50", "100","150"))+
  scale_x_date(name="")+
  theme_classic()+
  theme(strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)))+
  labs(title="The resurgence in COVID-19 cases in Scotland is among the under-45s",
       subtitle="Confirmed new cases in Scotland by age",
       caption="Date from Public Health Scotland | Plot by @VictimOfMaths")
dev.off()

#Heatmap
tiff("Outputs/COVIDCasesHeatmapScotland.tiff", units="in", width=10, height=3, res=500)
ggplot(subset(data, Sex=="Total" & date>=as.Date("2020-07-01") & date<max(data$date)), 
       aes(x=date, y=AgeGroup, fill=DailyPositive))+
  geom_tile()+
  scale_x_date(name="")+
  scale_y_discrete(name="Age group",
                   labels=c("15-19", "20-24", "25-44", "45-64", "65-74", "75-84", "85+"))+
  scale_fill_paletteer_c("viridis::magma", name="New cases")+
  theme_classic()+
  labs(title="The rise in new COVID-19 cases in Scotland hasn't affected pensioners yet",
       subtitle="Confirmed daily new cases in Scotland by age",
       caption="Date from Public Health Scotland | Plot by @VictimOfMaths")
dev.off()

tiff("Outputs/COVIDCasesHeatmapScotlandRate.tiff", units="in", width=10, height=3, res=500)
ggplot(subset(data, Sex=="Total" & date>=as.Date("2020-07-01") & date<max(data$date)), 
       aes(x=date, y=AgeGroup, fill=posrate))+
  geom_tile()+
  scale_x_date(name="")+
  scale_y_discrete(name="Age group",
                   labels=c("15-19", "20-24", "25-44", "45-64", "65-74", "75-84", "85+"))+
  scale_fill_paletteer_c("viridis::magma", name="New cases\nper 100,000")+
  theme_classic()+
  labs(title="The rise in new COVID-19 cases in Scotland hasn't affected pensioners yet",
       subtitle="Confirmed daily new case rates per 100,000 in Scotland by age",
       caption="Date from Public Health Scotland | Plot by @VictimOfMaths")
dev.off()

#By deprivation
temp <- tempfile()
source <- "https://www.opendata.nhs.scot/dataset/b318bddf-a4dc-4262-971f-0ba329e09b87/resource/a38a4c21-7c75-4ecd-a511-3f83e0e8f0c3/download/trend_simd_20200912.csv"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

data.simd <- read.csv(temp)

data.simd <- data.simd %>% mutate(date=as.Date(as.character(Date), format="%Y%m%d"))

tiff("Outputs/COVIDCasesHeatmapScotlandxIMD.tiff", units="in", width=10, height=3, res=500)
ggplot(subset(data.simd, date>=as.Date("2020-07-01") & date<max(data$date)), 
       aes(x=date, y=as.factor(SIMDQuintile), fill=DailyPositive))+
  geom_tile()+
  scale_x_date(name="")+
  scale_y_discrete(name="Deprivation quintile",
                   labels=c("1 - most deprived", "2", "3", "4", "5 - least deprived"))+
  scale_fill_paletteer_c("viridis::magma", name="New cases")+
  theme_classic()+
  labs(title="More deprived parts of Scotland aren't seeing bigger rises in COVID-19 cases",
       subtitle="Confirmed daily new cases in Scotland by quintiles of the Scottish Index of Multiple Deprivation",
       caption="Date from Public Health Scotland | Plot by @VictimOfMaths")
dev.off()

tiff("Outputs/COVIDDeathsHeatmapScotlandxIMD.tiff", units="in", width=10, height=3, res=500)
ggplot(data.simd, aes(x=date, y=as.factor(SIMDQuintile), fill=DailyDeaths))+
  geom_tile()+
  scale_x_date(name="")+
  scale_y_discrete(name="Deprivation quintile",
                   labels=c("1 - most deprived", "2", "3", "4", "5 - least deprived"))+
  scale_fill_paletteer_c("viridis::magma", name="Deaths per day")+
  theme_classic()+
  labs(title="Deaths from confirmed COVID-19 in Scotland were concentrated in more deprived areas",
       subtitle="Confirmed daily deaths in Scotland by quintiles of the Scottish Index of Multiple Deprivation",
       caption="Date from Public Health Scotland | Plot by @VictimOfMaths")
dev.off()