rm(list=ls())

library(tidyverse)
library(curl)
library(readxl)
library(scales)
library(extrafont)
library(ragg)
library(ggtext)
library(paletteer)
library(scales)


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

#Download 2020 COVID deaths data by age
temp <- tempfile()
url2020 <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fbirthsdeathsandmarriages%2fdeaths%2fdatasets%2fweeklyprovisionalfiguresondeathsregisteredinenglandandwales%2f2020/publishedweek532020.xlsx"
temp <- curl_download(url=url2020, destfile=temp, quiet=FALSE, mode="wb")

data20m <- read_excel(temp, sheet="Covid-19 - Weekly registrations", range="B34:BC53", 
                     col_names=FALSE) %>% 
  gather(Week, Deaths, c(2:ncol(.))) %>% 
  set_names("Age", "Week", "Deaths") %>% 
  mutate(Week=as.numeric(substr(Week, 4,5))-1,
         Year=2020, Sex="Male")

data20f <- read_excel(temp, sheet="Covid-19 - Weekly registrations", range="B56:BC75", 
                      col_names=FALSE) %>% 
  gather(Week, Deaths, c(2:ncol(.))) %>% 
  set_names("Age", "Week", "Deaths") %>% 
  mutate(Week=as.numeric(substr(Week, 4,5))-1,
         Year=2020, Sex="Female")

#All cause deaths
data20m_ac <- read_excel(temp, sheet="Weekly figures 2020", range="B44:BC63", col_names=FALSE)%>% 
  gather(Week, Deaths, c(2:ncol(.))) %>% 
  set_names("Age", "Week", "ACDeaths") %>% 
  mutate(Week=as.numeric(substr(Week, 4,5))-1,
         Year=2020, Sex="Male")

data20f_ac <- read_excel(temp, sheet="Weekly figures 2020", range="B66:BC85", col_names=FALSE)%>% 
  gather(Week, Deaths, c(2:ncol(.))) %>% 
  set_names("Age", "Week", "ACDeaths") %>% 
  mutate(Week=as.numeric(substr(Week, 4,5))-1,
         Year=2020, Sex="Female")

#2021
url2021 <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fbirthsdeathsandmarriages%2fdeaths%2fdatasets%2fweeklyprovisionalfiguresondeathsregisteredinenglandandwales%2f2021/publishedweek522021.xlsx"
temp <- curl_download(url=url2021, destfile=temp, quiet=FALSE, mode="wb")

data21m <- read_excel(temp, sheet="Covid-19 - Weekly registrations", range="B34:BB53", 
                      col_names=FALSE) %>% 
  gather(Week, Deaths, c(2:ncol(.))) %>% 
  set_names("Age", "Week", "Deaths") %>% 
  mutate(Week=as.numeric(substr(Week, 4,5))-1,
         Year=2021, Sex="Male")

data21f <- read_excel(temp, sheet="Covid-19 - Weekly registrations", range="B56:BB75", 
                       col_names=FALSE) %>% 
  gather(Week, Deaths, c(2:ncol(.))) %>% 
  set_names("Age", "Week", "Deaths") %>% 
  mutate(Week=as.numeric(substr(Week, 4,5))-1,
         Year=2021, Sex="Female")

#All cause deaths
data21m_ac <- read_excel(temp, sheet="Weekly figures 2021", range="B40:BC59", col_names=FALSE)%>% 
  gather(Week, Deaths, c(2:ncol(.))) %>% 
  set_names("Age", "Week", "ACDeaths") %>% 
  mutate(Week=as.numeric(substr(Week, 4,5))-1,
         Year=2021, Sex="Male")

data21f_ac <- read_excel(temp, sheet="Weekly figures 2021", range="B62:BC81", col_names=FALSE)%>% 
  gather(Week, Deaths, c(2:ncol(.))) %>% 
  set_names("Age", "Week", "ACDeaths") %>% 
  mutate(Week=as.numeric(substr(Week, 4,5))-1,
         Year=2021, Sex="Female")

#2022 (in a different format, thanks for that ONS)
url2022 <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/datasets/weeklyprovisionalfiguresondeathsregisteredinenglandandwales/2022/publicationfileweek212022.xlsx"
temp <- curl_download(url=url2022, destfile=temp, quiet=FALSE, mode="wb")

data22m <- read_excel(temp, sheet="4", range="A31:W52") %>% 
  select(-c(2,3)) %>% 
  gather(Age, Deaths, c(2:ncol(.))) %>% 
  set_names(c("Week", "Age", "Deaths")) %>% 
  mutate(Year=2022, Sex="Male")

data22f <- read_excel(temp, sheet="4", range="A55:W76") %>% 
  select(-c(2,3)) %>% 
  gather(Age, Deaths, c(2:ncol(.))) %>% 
  set_names(c("Week", "Age", "Deaths")) %>% 
  mutate(Year=2022, Sex="Female")

#All cause deaths
data22m_ac <- read_excel(temp, sheet="2", range="A31:W52") %>% 
  select(-c(2,3)) %>% 
  gather(Age, Deaths, c(2:ncol(.))) %>% 
  set_names(c("Week", "Age", "ACDeaths")) %>% 
  mutate(Year=2022, Sex="Male")

data22f_ac <- read_excel(temp, sheet="2", range="A55:W76") %>% 
  select(-c(2,3)) %>% 
  gather(Age, Deaths, c(2:ncol(.))) %>% 
  set_names(c("Week", "Age", "ACDeaths")) %>% 
  mutate(Year=2022, Sex="Female")

alldata <- bind_rows(data20f, data20m, data21f, data21m, data22f, data22m) %>% 
  merge(bind_rows(data20f_ac, data20m_ac, data21f_ac, data21m_ac, data22f_ac, data22m_ac)) %>% 
  mutate(Age=case_when(
    Age=="01-04" ~ "1-4",
    Age=="05-09" ~ "5-9",
    TRUE ~Age),
    COVIDprop=Deaths/ACDeaths) 

data1 <- alldata %>%
  filter(!(Year==2020 & Week<11)) %>% 
  group_by(Age, Sex) %>% 
  summarise(Deaths=sum(Deaths), ACDeaths=sum(ACDeaths)) %>% 
  ungroup() %>% 
  mutate(plotdeaths=if_else(Sex=="Male", -Deaths, Deaths),
         Age=factor(Age, levels=c("<1", "1-4", "5-9", "10-14", "15-19", "20-24", "25-29",
                                  "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64",
                                  "65-69", "70-74", "75-79", "80-84", "85-89", "90+")),
         COVIDprop=Deaths/ACDeaths,
         plotCOVIDprop=if_else(Sex=="Male", -COVIDprop, COVIDprop))

agg_png("Outputs/COVIDTotalDeathsxAgexSex.png", units="in", width=8, height=8, res=800)
ggplot(data1, aes(x=plotdeaths, y=Age, fill=Sex))+
  geom_col()+
  geom_vline(xintercept=0, colour="Grey40")+
  scale_x_continuous(name="Total COVID deaths", limits=c(-25000, 25000),
                     breaks=c(-20000, -10000, 0, 10000, 20000),
                     labels=c("20,000", "10,000", "0", "10,000", "20,000"))+
  scale_fill_manual(values=c("#00cc99", "#6600cc"), name="", guide = guide_legend(reverse = TRUE))+
  theme_custom()+
  theme(legend.position="top")+
  labs(title="Total COVID-19 deaths by age and sex",
       subtitle="Deaths in England & Wales registered up to 27th May 2022 where COVID-19 was mentioned on the death certificate",
       caption="Data from ONS | Plot by @VictimOfMaths")

dev.off()

agg_png("Outputs/COVIDTotalDeathPropxAgexSex.png", units="in", width=8, height=8, res=800)
ggplot(data1, aes(x=plotCOVIDprop, y=Age, fill=Sex))+
  geom_col()+
  geom_vline(xintercept=0, colour="Grey40")+
  scale_x_continuous(name="Proportion of deaths involving COVID", limits=c(-0.17, 0.17),
                     breaks=c(-0.15, -0.1, -0.05, 0, 0.05, 0.1, 0.15),
                     labels=c("15%", "10%", "5%", "0%", "5%", "10%", "15%"))+
  scale_fill_manual(values=c("#00cc99", "#6600cc"), name="", guide = guide_legend(reverse = TRUE))+
  theme_custom()+
  theme(legend.position="top")+
  labs(title="The proportional impact of COVID on mortality",
       subtitle="Proportion of all deaths in England & Wales registered between 7th March 2020 and 27th May 2022\nwhere COVID-19 was mentioned on the death certificate",
       caption="Data from ONS | Plot by @VictimOfMaths")

dev.off()

data2 <- alldata %>% 
  group_by(Age, Sex, Year) %>% 
  summarise(Deaths=sum(Deaths), ACDeaths=sum(ACDeaths)) %>% 
  ungroup() %>% 
  mutate(plotdeaths=if_else(Sex=="Male", -Deaths, Deaths),
         Age=factor(Age, levels=c("<1", "1-4", "5-9", "10-14", "15-19", "20-24", "25-29",
                                  "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64",
                                  "65-69", "70-74", "75-79", "80-84", "85-89", "90+")),
         COVIDprop=Deaths/ACDeaths,
         plotCOVIDprop=if_else(Sex=="Male", -COVIDprop, COVIDprop))

agg_png("Outputs/COVIDTotalDeathsxAgexSexxYear.png", units="in", width=8, height=8, res=800)
ggplot(data2, aes(x=plotdeaths, y=Age, fill=as.factor(Year)))+
  geom_col(position="dodge")+
  geom_vline(xintercept=0, colour="Grey40")+
  scale_x_continuous(name="Total COVID deaths", limits=c(-11000, 11000),
                     breaks=c(-10000, -5000, 0, 5000, 10000),
                     labels=c("10,000", "5,000", "0", "5,000", "10,000"))+
  scale_fill_paletteer_d("calecopal::superbloom3", name="")+
  theme_custom()+
  theme(legend.position="top")+
  labs(title="COVID-19 deaths by age, sex and year",
       subtitle="Deaths in England & Wales registered up to 27th May 2022 where COVID-19 was mentioned on the death certificate",
       caption="Data from ONS | Plot by @VictimOfMaths")

dev.off()

agg_png("Outputs/COVIDTotalDeathPropxAgexSexxYear.png", units="in", width=8, height=8, res=800)
ggplot(data2, aes(x=plotCOVIDprop, y=Age, fill=as.factor(Year)))+
  geom_col(position="dodge")+
  geom_vline(xintercept=0, colour="Grey40")+
  scale_x_continuous(name="Proportion of deaths involving COVID", limits=c(-0.17, 0.17),
                     breaks=c(-0.15, -0.1, -0.05, 0, 0.05, 0.1, 0.15),
                     labels=c("15%", "10%", "5%", "0%", "5%", "10%", "15%"))+
  scale_fill_paletteer_d("calecopal::superbloom3", name="")+
  theme_custom()+
  theme(legend.position="top")+
  labs(title="The proportional impact of COVID on mortality",
       subtitle="Proportion of all deaths in England & Wales registered between 1st Jan 2020 and 27th May 2022,\n where COVID-19 was mentioned on the death certificate",
       caption="Data from ONS | Plot by @VictimOfMaths")

dev.off()

data3 <- alldata %>% 
  group_by(Age, Sex, Year, Week) %>% 
  summarise(Deaths=sum(Deaths), ACDeaths=sum(ACDeaths)) %>% 
  ungroup() %>% 
  mutate(plotdeaths=if_else(Sex=="Male", -Deaths, Deaths),
         Age=factor(Age, levels=c("<1", "1-4", "5-9", "10-14", "15-19", "20-24", "25-29",
                                  "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64",
                                  "65-69", "70-74", "75-79", "80-84", "85-89", "90+")),
         COVIDprop=Deaths/ACDeaths,
         plotCOVIDprop=if_else(Sex=="Male", -COVIDprop, COVIDprop),
         Date=case_when(
           Year==2020 ~ as.Date("2020-01-03")+Week-1,
           Year==2021 ~ as.Date("2020-01-03")+Week+53,
           Year==2022 ~ as.Date("2020-01-03")+Week+53+52))

ggplot(data3, aes(x=Date, y=COVIDprop, colour=Sex, group=Sex))+
  geom_line()+
  scale_colour_manual(values=c("#00cc99", "#6600cc"), name="", guide = guide_legend(reverse = TRUE))+
  facet_wrap(~Age)+
  theme_custom()

