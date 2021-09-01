rm(list=ls())

library(tidyverse)
library(curl)
library(readxl)
library(ggtext)
library(ggridges)
library(paletteer)
library(ggstream)
library(RcppRoll)
library(extrafont)

#Scottish age data
#https://www.opendata.nhs.scot/dataset/covid-19-in-scotland
temp <- tempfile()
source <- "https://www.opendata.nhs.scot/dataset/b318bddf-a4dc-4262-971f-0ba329e09b87/resource/9393bd66-5012-4f01-9bc5-e7a10accacf4/download/trend_agesex_20210831.csv"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

data <- read.csv(temp)

data <- data %>% 
  filter(!AgeGroup %in% c("Total", "0 to 59", "60+")) %>% 
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
  pop$age<15 ~ "0 to 14",
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

#Take rolling 7-day averages
data <- data %>% 
  group_by(Sex, AgeGroup) %>% 
  arrange(date) %>% 
  mutate(cases_avg=roll_mean(DailyPositive, 7, align="right", fill=0),
         posrate_avg=roll_mean(posrate, 7, align="right", fill=0))

tiff("Outputs/COVIDCasesStreamgraphScotlandxSex.tiff", units="in", width=10, height=6, res=500)
ggplot(subset(data, Sex!="Total"), aes(x=date, y=cases_avg, fill=AgeGroup))+
  geom_stream(bw=0.2)+
  scale_fill_paletteer_d("awtools::a_palette", name="Age",
                         labels=c("Under 15", "15-19", "20-24", "25-44", "45-64", "65-74", "75-84", "85+"))+
  scale_y_continuous(name="New cases per day", labels=abs)+
  scale_x_date(name="")+
  facet_wrap(~Sex)+
  theme_classic()+
  theme(strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)),
        text=element_text(family="Lato"), plot.title=element_text(face="bold", size=rel(1.6)))+
  labs(title="Cases in Scotland are rising in the under 45s",
       subtitle="Confirmed new COVID-19 cases in Scotland by sex and age",
       caption="Date from Public Health Scotland | Plot by @VictimOfMaths")
dev.off()

tiff("Outputs/COVIDCasesStreamgraphScotland.tiff", units="in", width=10, height=6, res=500)
ggplot(subset(data, Sex=="Total"), aes(x=date, y=cases_avg, fill=AgeGroup))+
  geom_stream(bw=0.2)+
  scale_fill_paletteer_d("awtools::a_palette", name="Age",
                         labels=c("Under 15", "15-19", "20-24", "25-44", "45-64", "65-74", "75-84", "85+"))+
  scale_y_continuous(name="New cases per day", labels=abs)+
  scale_x_date(name="")+
  theme_classic()+
  theme(strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)),
        plot.title=element_text(face="bold", size=rel(1.6)),
        text=element_text(family="Lato"))+
  labs(title="COVID-19 cases in Scotland have risen across all age groups",
       subtitle="Confirmed new cases in Scotland by age",
       caption="Date from Public Health Scotland | Plot by @VictimOfMaths")
dev.off()

#Heatmap
tiff("Outputs/COVIDCasesHeatmapScotland.tiff", units="in", width=10, height=3, res=500)
ggplot(subset(data, Sex=="Total" & date>=as.Date("2020-07-01") & date<max(data$date)), 
       aes(x=date, y=AgeGroup, fill=cases_avg))+
  geom_tile()+
  scale_x_date(name="")+
  scale_y_discrete(name="Age group",
                   labels=c("Under 15", "15-19", "20-24", "25-44", "45-64", "65-74", "75-84", "85+"))+
  scale_fill_paletteer_c("viridis::magma", name="New cases")+
  theme_classic()+
  theme(plot.title=element_text(face="bold", size=rel(1.2)),
        text=element_text(family="Lato"))+
  labs(title="There are more COVID cases in adults than children in Scotland",
       subtitle="Rolling 7-day average of daily confirmed new cases in Scotland by age",
       caption="Date from Public Health Scotland | Plot by @VictimOfMaths")
dev.off()

CaseratexAge <- ggplot(subset(data, Sex=="Total" & date>=as.Date("2020-07-01") & date<max(data$date)), 
       aes(x=date, y=AgeGroup, fill=posrate_avg))+
  geom_tile()+
  scale_x_date(name="")+
  scale_y_discrete(name="Age group",
                   labels=c("Under 15", "15-19", "20-24", "25-44", "45-64", "65-74", "75-84", "85+"))+
  scale_fill_paletteer_c("viridis::magma", name="New cases\nper 100,000")+
  theme_classic()+
  theme(plot.title=element_text(face="bold", size=rel(1.5)),
        text=element_text(family="Lato"))+
  labs(title="COVID-19 case rates are only rising in the under 45s",
       subtitle="Confirmed daily new COVID-19 case rates per 100,000 in Scotland by age",
       caption="Date from Public Health Scotland | Plot by @VictimOfMaths")

tiff("Outputs/COVIDCasesHeatmapScotlandRate.tiff", units="in", width=10, height=3, res=500)
CaseratexAge
dev.off()

#Line graph version
tiff("Outputs/COVIDCasesLineScotlandRate.tiff", units="in", width=8, height=6, res=500)
ggplot(subset(data, Sex=="Total" & date>=as.Date("2021-01-01") & date<max(data$date)), 
       aes(x=date, y=posrate_avg, colour=AgeGroup))+
  geom_line()+
  scale_x_date(name="")+
  scale_y_continuous(name="Daily new cases per 100,000")+
  scale_colour_paletteer_d("awtools::a_palette", name="Age")+
  theme_classic()+
  theme(plot.title=element_text(face="bold", size=rel(1.5)),
        text=element_text(family="Lato"))+
  labs(title="COVID-19 case rates are highest in 15-24 year olds",
       subtitle="Confirmed daily new COVID-19 case rates per 100,000 in Scotland by age",
       caption="Data from Public Health Scotland | Plot by @VictimOfMaths")
dev.off()

#By deprivation
temp <- tempfile()
source <- "https://www.opendata.nhs.scot/dataset/b318bddf-a4dc-4262-971f-0ba329e09b87/resource/a38a4c21-7c75-4ecd-a511-3f83e0e8f0c3/download/trend_simd_20210831.csv"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

data.simd <- read.csv(temp)

data.simd <- data.simd %>% mutate(date=as.Date(as.character(Date), format="%Y%m%d"))

#Take rolling 7-day averages
data.simd <- data.simd %>% 
  group_by(SIMDQuintile) %>% 
  arrange(date) %>% 
  mutate(cases_avg=roll_mean(DailyPositive, 7, align="right", fill=0),
         deaths_avg=roll_mean(DailyDeaths, 7, align="right", fill=0))


tiff("Outputs/COVIDCasesHeatmapScotlandxIMD.tiff", units="in", width=10, height=3, res=500)
ggplot(subset(data.simd, date>=as.Date("2020-07-01") & date<max(data$date)), 
       aes(x=date, y=as.factor(SIMDQuintile), fill=cases_avg))+
  geom_tile()+
  scale_x_date(name="")+
  scale_y_discrete(name="Deprivation quintile",
                   labels=c("1 - most deprived", "2", "3", "4", "5 - least deprived"))+
  scale_fill_paletteer_c("viridis::magma", name="Daily new cases", limits=c(0,NA))+
  theme_classic()+
  theme(plot.title=element_text(face="bold", size=rel(1.4)),
        text=element_text(family="Lato"), plot.title.position = "plot",
        plot.caption.position = "plot" , legend.position = "top")+
  guides(fill = guide_colorbar(title.position = 'top', title.hjust = .5,
                                barwidth = unit(20, 'lines'), 
                                barheight = unit(.5, 'lines')))+
  labs(title="COVID cases are highest in the most deprived areas in Scotland",
       subtitle="Rolling 7-day average of confirmed daily new cases in Scotland by quintiles of the Scottish Index of Multiple Deprivation",
       caption="Date from Public Health Scotland | Plot by @VictimOfMaths")
dev.off()

COVIDDeathsHeatmapScotlandxIMD <- ggplot(data.simd, aes(x=date, y=as.factor(SIMDQuintile), fill=deaths_avg))+
  geom_tile()+
  scale_x_date(name="")+
  scale_y_discrete(name="Deprivation quintile",
                   labels=c("1 - most deprived", "2", "3", "4", "5 - least deprived"))+
  scale_fill_paletteer_c("viridis::magma", name="Deaths per day")+
  theme_classic()+
  theme(plot.title=element_text(face="bold", size=rel(1.4)),
        text=element_text(family="Lato"), plot.title.position = "plot",
        plot.caption.position = "plot" , legend.position = "top")+
  guides(fill = guide_colorbar(title.position = 'top', title.hjust = .5,
                               barwidth = unit(20, 'lines'), 
                               barheight = unit(.5, 'lines')))+
  labs(title="COVID deaths in both 'waves' have come disproportionately from the most deprived areas",
       subtitle="Rolling 7-day average of confirmed daily deaths in Scotland by quintiles of the Scottish Index of Multiple Deprivation",
       caption="Date from Public Health Scotland | Plot by @VictimOfMaths")

tiff("Outputs/COVIDDeathsHeatmapScotlandxIMD.tiff", units="in", width=10, height=3, res=500)
COVIDDeathsHeatmapScotlandxIMD
dev.off()

#Save jpeg for SIPHER blog
ggsave("Outputs/JPEGs/MortIneqBlog6.jpeg", plot=COVIDDeathsHeatmapScotlandxIMD, 
       units="in", width=10, height=3)

#Rayshader version
library(rayshader)
library(rayrender)

plot_gg(CaseratexAge, width=10, height=3, multicore = TRUE, windowsize = c(1000, 600), 
        zoom = 0.65, phi = 35, theta = 40, sunangle = 225, soliddepth = -100) 

