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

#2022 (in a different format, thanks for that ONS)
url2022 <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fbirthsdeathsandmarriages%2fdeaths%2fdatasets%2fweeklyprovisionalfiguresondeathsregisteredinenglandandwales%2f2022/publicationfileweek182022.xlsx"
temp <- curl_download(url=url2022, destfile=temp, quiet=FALSE, mode="wb")

data22m <- read_excel(temp, sheet="4", range="A28:W46") %>% 
  select(-c(2,3)) %>% 
  gather(Age, Deaths, c(2:ncol(.))) %>% 
  set_names(c("Week", "Age", "Deaths")) %>% 
  mutate(Year=2022, Sex="Male")

data22f <- read_excel(temp, sheet="4", range="A49:W67") %>% 
  select(-c(2,3)) %>% 
  gather(Age, Deaths, c(2:ncol(.))) %>% 
  set_names(c("Week", "Age", "Deaths")) %>% 
  mutate(Year=2022, Sex="Female")

data <- bind_rows(data20f, data20m, data21f, data21m, data22f, data22m) %>% 
  mutate(Age=case_when(
    Age=="01-04" ~ "1-4",
    Age=="05-09" ~ "5-9",
    TRUE ~Age)) %>% 
  group_by(Age, Sex) %>% 
  summarise(Deaths=sum(Deaths)) %>% 
  ungroup() %>% 
  mutate(plotdeaths=if_else(Sex=="Male", -Deaths, Deaths),
         Age=factor(Age, levels=c("<1", "1-4", "5-9", "10-14", "15-19", "20-24", "25-29",
                                  "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64",
                                  "65-69", "70-74", "75-79", "80-84", "85-89", "90+")))

agg_png("Outputs/COVIDTotalDeathsxAgexSex.png", units="in", width=8, height=8, res=800)
ggplot(data, aes(x=plotdeaths, y=Age, fill=Sex))+
  geom_col()+
  geom_vline(xintercept=0, colour="Grey40")+
  scale_x_continuous(name="Total COVID deaths", limits=c(-25000, 25000),
                     breaks=c(-20000, -10000, 0, 10000, 20000),
                     labels=c("20,000", "10,000", "0", "10,000", "20,000"))+
  scale_fill_manual(values=c("#00cc99", "#6600cc"), name="", guide = guide_legend(reverse = TRUE))+
  theme_custom()+
  theme(legend.position="top")+
  labs(title="Total COVID-19 deaths by age and sex",
       subtitle="Deaths in England & Wales registered up to 6th May 2022 where COVID-19 was mentioned on the death certificate",
       caption="Data from ONS | Plot by @VictimOfMaths")

dev.off()

data2 <- bind_rows(data20f, data20m, data21f, data21m, data22f, data22m) %>% 
  mutate(Age=case_when(
    Age=="01-04" ~ "1-4",
    Age=="05-09" ~ "5-9",
    TRUE ~Age)) %>% 
  group_by(Age, Sex, Year) %>% 
  summarise(Deaths=sum(Deaths)) %>% 
  ungroup() %>% 
  mutate(plotdeaths=if_else(Sex=="Male", -Deaths, Deaths),
         Age=factor(Age, levels=c("<1", "1-4", "5-9", "10-14", "15-19", "20-24", "25-29",
                                  "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64",
                                  "65-69", "70-74", "75-79", "80-84", "85-89", "90+")))

agg_png("Outputs/COVIDTotalDeathsxAgexSexxYear.png", units="in", width=8, height=8, res=800)
ggplot(data2, aes(x=plotdeaths, y=Age, fill=Sex))+
  geom_col()+
  geom_vline(xintercept=0, colour="Grey40")+
  scale_x_continuous(name="Total COVID deaths", limits=c(-25000, 25000),
                     breaks=c(-20000, -10000, 0, 10000, 20000),
                     labels=c("20,000", "10,000", "0", "10,000", "20,000"))+
  scale_fill_manual(values=c("#00cc99", "#6600cc"), name="", guide = guide_legend(reverse = TRUE))+
  facet_wrap(~Year)+
  theme_custom()+
  theme(legend.position="top")+
  labs(title="Total COVID-19 deaths by age and sex",
       subtitle="Deaths in England & Wales registered up to 6th May 2022 where COVID-19 was mentioned on the death certificate",
       caption="Data from ONS | Plot by @VictimOfMaths")

dev.off()

