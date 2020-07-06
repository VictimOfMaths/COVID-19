rm(list=ls())

library(tidyverse)
library(curl)
library(readxl)
library(ggtext)
library(paletteer)
library(lubridate)
library(forcats)

#Bring in 2020 data (released by ONS on a Tuesday)
temp <- tempfile()
source <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fhealthandsocialcare%2fcausesofdeath%2fdatasets%2fdeathregistrationsandoccurrencesbylocalauthorityandhealthboard%2f2020/lahbtablesweek25.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
data20 <- read_excel(temp, sheet=6, col_names=FALSE)[-c(1:4),]
colnames(data20) <- c("code", "type", "name", "cause", "week", "location", "deaths")

data20$deaths <- as.numeric(data20$deaths)
data20$week <- as.numeric(data20$week)

maxweek <- max(data20$week)
enddate <- as.Date("2020-01-03")+weeks(maxweek-1)

#Read in 2015-19 historic data
temp <- tempfile()
source <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/adhocs/11826fiveyearaverageweeklydeathsbylocalauthorityandplaceofoccurrenceenglandandwalesdeathsregistered2015to2019/weeklyfiveyearaveragesbylaandplaceofoccurrence20152019.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
data1519 <- read_excel(temp, sheet=2, col_names=FALSE)[-c(1:3),]
colnames(data1519) <- c("code", "name", "week", "location", "deaths")

data1519$deaths <- as.numeric(data1519$deaths)
data1519$week <- as.numeric(data1519$week)
data1519 <- data1519 %>% drop_na(name)

#Combine Cornwall & Isles of Scilly in both datasets
data20$code <- if_else(data20$code=="E06000053", "E06000052", data20$code)
data1519$code <- if_else(data1519$code=="E06000053", "E06000052", data1519$code)
data20$name <- if_else(data20$name=="Isles of Scilly", "Cornwall", data20$name)
data1519$name <- if_else(data1519$name=="Isles of Scilly", "Cornwall", data1519$name)

#Extract location data
data20$location <- case_when(
  data20$location %in% c("Elsewhere", "Home", "Hospice", "Other communal establishment") ~ "Home/Other",
  TRUE ~ data20$location)

data1519$location <- case_when(
  data1519$location %in% c("Elsewhere", "Home", "Hospice", "Other communal establishment") ~ "Home/Other",
  TRUE ~ data1519$location)

data20.loc <- data20 %>% 
  group_by(code, name, location, week) %>% 
  summarise(deaths20=sum(deaths))

data1519.loc <- data1519 %>% 
  group_by(code, name, location, week) %>% 
  summarise(deaths1519=sum(deaths))

data.loc <- merge(data1519.loc, data20.loc, by=c("name", "code", "location", "week"), all.x=TRUE)

#Calculate location-specific excess deaths
data.loc$excess <- data.loc$deaths20-data.loc$deaths1519
data.loc$location <- factor(data.loc$location, levels=c("Hospital", "Care home", "Home/Other"))

#Collapse location data
data20 <- data20 %>%
  group_by(code, name, cause, week) %>%
  summarise(deaths=sum(deaths)) %>%
  mutate(year="2020")

data1519 <- data1519 %>% 
  group_by(code, name, week) %>% 
  summarise(deaths=sum(deaths)) %>% 
  mutate(year="201519", cause="All")

data20.all <- data20 %>% 
  group_by(code, name, week) %>% 
  summarise(deaths=sum(deaths)) %>% 
  mutate(year="2020", cause="All")

#Cause-specific dataset
data20_wide <- spread(data20, cause, deaths)
data20_wide <- merge(data20_wide, data1519, by=c("code", "name", "week"))[,-c(4,8,9)]
data20_wide$Other <- data20_wide$`All causes`-data20_wide$deaths
data_bar <- gather(data20_wide, excesscause, deaths, c(5,7))

#Summary figures
data_summary <- data20_wide %>% 
  group_by(code, name) %>% 
  summarise(hist=sum(deaths), curr=sum(`All causes`)) %>% 
  mutate(excess=curr-hist, excessprop=excess/hist)

data_summary$country <- if_else(substr(data_summary$code,1,1)=="E", "England", "Wales")

ggplot(data_summary, aes(x=excessprop*100, y=fct_reorder(name, excessprop), fill=country))+
  geom_col()+
  scale_x_continuous(name="Change in all-cause deaths in 2020 vs. 2015-19 mean", 
                     breaks=c(-25,0,25,50), labels=c("-25%", "0%", "+25%", "+50%"))+
  scale_y_discrete(name="Local Authority")+
  theme_classic()

#All-cause only dataset
data.all <- merge(data1519, data20.all, by=c("code", "week", "name"), all.x=TRUE)[,c(1:4,7)]
colnames(data.all) <- c("code", "week", "name", "hist", "2020")
data.all <- data.all %>% drop_na(name)

#LA level excess deaths
LA <- "Sheffield"
excess <- data_summary %>% filter(name==LA) 

labpos <- max(data.all$`2020`[data.all$week==maxweek & data.all$name==LA],
              data.all$hist[data.all$week==maxweek & data.all$name==LA])

lab <- if_else(excess[1,5]<0, 
               paste0(round(excess[1,5],0), " (",round(excess[1,6]*100,0), "%) deaths in 2020\ncompared to the average in 2015-19"),
               paste0("+", round(excess[1,5],0), " (+",round(excess[1,6]*100,0), "%) deaths in 2020\ncompared to the average in 2015-19"))

#Excess deaths graph
ggplot(subset(data.all, name==LA))+
  geom_line(aes(x=week, y=hist), colour="skyblue4")+
  geom_line(aes(x=week, y=`2020`), colour="red")+
  scale_x_continuous(name="Week")+
  scale_y_continuous(name="Deaths", limits=c(0,NA))+
  theme_classic()+
  theme(plot.subtitle=element_markdown())+
  annotate("text", x=maxweek-2, y=max(labpos*1.5, labpos+20), 
           label=lab,
           hjust=0, colour="red", size=3)+
  labs(title=paste0("Excess deaths in ", LA, " during the pandemic"),
       subtitle=paste0("Weekly deaths in <span style='color:red;'>2020</span> compared to <span style='color:Skyblue4;'>the average in 2015-19</span> by date of occurance<br>Data up to ", enddate),
       caption="Data from ONS | Plot by @VictimOfMaths")

#Excess deaths by cause
ggplot(subset(data_bar, name==LA), aes(x=week, y=deaths, fill=excesscause))+
  geom_bar(stat="identity")+
  geom_segment(aes(x=0.5, xend=maxweek+0.5, y=0, yend=0), colour="Grey30")+
  scale_x_continuous(name="Week")+
  scale_y_continuous(name="Excess deaths vs. 2015-19 average")+
  scale_fill_paletteer_d("LaCroixColoR::PinaFraise", name="Cause", labels=c("COVID-19", "Other causes"))+
  theme_classic()+
  labs(title=paste0("Excess deaths in ", LA, " during the pandemic"),
       subtitle=paste0("Excess deaths by date of occurance in 2020 vs. 2015-19 average by cause\nData up to ", enddate),
       caption="Data from ONS | Plot by @VictimOfMaths")

#Excess deaths by location
ggplot(subset(data.loc, name==LA), aes(x=week, y=excess, fill=location))+
  geom_col()+
  geom_segment(aes(x=0.5, xend=maxweek+0.5, y=0, yend=0), colour="Grey30")+
  scale_x_continuous(name="Week")+
  scale_y_continuous(name="Excess deaths vs. 2015-19 average")+
  scale_fill_paletteer_d("ggsci::planetexpress_futurama", name="Place of death")+
  theme_classic()+
  labs(title=paste0("Excess deaths in ", LA, " during the pandemic"),
       subtitle=paste0("Excess deaths by place of death in 2020 vs. 2015-19 average by cause\nData up to ", enddate),
       caption="Data from ONS | Plot by @VictimOfMaths")
