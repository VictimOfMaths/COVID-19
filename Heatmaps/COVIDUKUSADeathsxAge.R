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

#Download US data from CDC website
USurl <- "https://data.cdc.gov/api/views/vsak-wrfu/rows.csv?accessType=DOWNLOAD"

temp <- tempfile()
temp <- curl_download(url=USurl, destfile=temp, quiet=FALSE, mode="wb")

USdata <- read.csv(temp) %>% 
  mutate(Country="USA", End.Week=as.Date(End.Week, format="%m/%d/%Y")) %>% 
  filter(Age.Group!="All Ages")

#US population data
USpopurl <- "https://www2.census.gov/programs-surveys/popest/technical-documentation/file-layouts/2010-2019/nc-est2019-agesex-res.csv"

temp <- tempfile()
temp <- curl_download(url=USpopurl, destfile=temp, quiet=FALSE, mode="wb")

USpop <- read.csv(temp) %>% 
  mutate(Sex=case_when(
    SEX==0 ~ "All Sex",
    SEX==1 ~ "Male",
    TRUE ~ "Female")) %>% 
  select(Sex, AGE, POPESTIMATE2019) %>% 
  filter(AGE!=999) %>% 
  mutate(Age.Group=case_when(
    AGE==0 ~ "Under 1 year", AGE<5 ~ "1-4 Years", AGE<15 ~ "5-14 Years", AGE<25 ~ "15-24 Years",
    AGE<35 ~ "25-34 Years", AGE<45 ~ "35-44 Years", AGE<55 ~ "45-54 Years", AGE<65 ~ "55-64 Years", 
    AGE<75 ~ "65-74 Years", AGE<85 ~ "75-84 Years", TRUE ~ "85 Years and Over")) %>%
  group_by(Age.Group, Sex) %>% 
  summarise(Pop=sum(POPESTIMATE2019)) %>% 
  ungroup()
  
#Read in UK data year by year

UKurl2021 <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fbirthsdeathsandmarriages%2fdeaths%2fdatasets%2fweeklyprovisionalfiguresondeathsregisteredinenglandandwales%2f2021/publishedweek522021.xlsx"

temp <- curl_download(url=UKurl2021, destfile=temp, quiet=FALSE, mode="wb")

UKdata2021 <- read_excel(temp, sheet="Covid-19 - Weekly occurrences", range="B12:DC75", 
                         col_names=FALSE) %>% 
  na.omit() %>% 
  mutate(Sex=rep(c("All Sex", "Male", "Female"), each=20)) %>% 
  gather(End.Week, COVID.19.Deaths, c(2:(ncol(.)-1))) %>% 
  mutate(End.Week=as.Date("2020-01-03")+weeks(as.numeric(substr(End.Week,4,6))-2)) %>% 
  rename(Age=`...1`) %>% 
  mutate(Country="UK", Age.Group=case_when(
    Age=="<1" ~ "Under 1 year",
    Age=="1-4" ~ "1-4 Years",
    Age %in% c("5-9", "10-14") ~ "5-14 Years",
    Age %in% c("15-19", "20-24") ~ "15-24 Years",
    Age %in% c("25-29", "30-34") ~ "25-34 Years",
    Age %in% c("35-39", "40-44") ~ "35-44 Years",
    Age %in% c("45-49", "50-54") ~ "45-54 Years",
    Age %in% c("55-59", "60-64") ~ "55-64 Years",
    Age %in% c("65-69", "70-74") ~ "65-74 Years",
    Age %in% c("75-79", "80-84") ~ "75-84 Years",
    TRUE ~ "85 Years and Over")) %>% 
  group_by(End.Week, Age.Group, Country, Sex) %>% 
  summarise(COVID.19.Deaths=sum(COVID.19.Deaths)) %>% 
  ungroup()

#UK population data

EWpopurl <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fpopulationandmigration%2fpopulationestimates%2fdatasets%2fpopulationestimatesforukenglandandwalesscotlandandnorthernireland%2fmid2020/ukpopestimatesmid2020on2020geography.xls"

temp <- curl_download(url=EWpopurl, destfile=temp, quiet=FALSE, mode="wb")

EWpop.m <- as.data.frame(t(read_excel(temp, sheet="MYE2 - Males", range="E11:CQ11", col_names=FALSE))) %>% 
  mutate(Age=c(0:90)) %>% 
  rename(Male=V1)

EWpop.f <- as.data.frame(t(read_excel(temp, sheet="MYE2 - Females", range="E11:CQ11", col_names=FALSE))) %>% 
  mutate(Age=c(0:90)) %>% 
  rename(Female=V1)

EWpop <- merge(EWpop.m, EWpop.f) %>% 
  mutate(`All Sex`=Male+Female) %>% 
  mutate(Age.Group=case_when(
    Age==0 ~ "Under 1 year", Age<5 ~ "1-4 Years", Age<15 ~ "5-14 Years", Age<25 ~ "15-24 Years",
    Age<35 ~ "25-34 Years", Age<45 ~ "35-44 Years", Age<55 ~ "45-54 Years", Age<65 ~ "55-64 Years", 
    Age<75 ~ "65-74 Years", Age<85 ~ "75-84 Years", TRUE ~ "85 Years and Over")) %>%
  group_by(Age.Group) %>% 
  summarise(Male=sum(Male), Female=sum(Female), `All Sex`=sum(`All Sex`)) %>% 
  ungroup() %>% 
  gather(Sex, Pop, c(2:4))
  

data <- bind_rows(UKdata2021 %>% merge(EWpop), USdata %>% merge(USpop)) %>% 
  select(End.Week, COVID.19.Deaths, Country, Age.Group, Sex, Pop) %>% 
  mutate(Age.Group=case_when(
    Age.Group %in% c("Under 1 year", "1-4 Years", "5-14 Years") ~ "0-14 Years",
    TRUE ~ Age.Group)) %>% 
  group_by(Age.Group, Sex, End.Week, Country) %>% 
  summarise(COVID.19.Deaths=sum(COVID.19.Deaths), Pop=sum(Pop)) %>% 
  ungroup() %>% 
  mutate(Age.Group=factor(Age.Group, levels=c("0-14 Years",
                                              "15-24 Years", "25-34 Years", "35-44 Years",
                                              "45-54 Years", "55-64 Years", "65-74 Years",
                                              "75-84 Years", "85 Years and Over")),
         MortRate=COVID.19.Deaths*100000/Pop) 

agg_tiff("Outputs/COVIDDeathsUKUSA.tiff", units="in", width=10, height=8, res=500)
ggplot(data %>% filter(Sex=="All Sex"), aes(x=End.Week, y=MortRate, colour=Country))+
  geom_line(show.legend=FALSE)+
  scale_x_date(name="")+
  scale_y_continuous(name="Weekly deaths per 100,000")+
  scale_colour_paletteer_d("ggsci::uniform_startrek")+
  facet_wrap(~Age.Group, scales="free_y")+
  theme_custom()+
  theme(plot.title=element_markdown())+
  labs(title="Recent months have seen more COVID deaths in younger adults in <span style='color:#5C88DA;'>the US</span> than <span style='color:#CC0C00;'>England & Wales</span>",
       subtitle="Registered COVID-19 deaths by date of occurrence in England & Wales and the USA",
       caption="Data from ONS, CDC and the US Census Bureau")

dev.off()

collapse <- data %>% 
  filter(End.Week>=as.Date("2021-07-01") & Sex=="All Sex" & End.Week<=as.Date("2022-01-01")) %>% 
  group_by(Country, Age.Group) %>% 
  summarise(COVID.19.Deaths=sum(COVID.19.Deaths), Pop=unique(Pop)) %>% 
  ungroup() %>% 
  group_by(Country) %>% 
  mutate(AllDeaths=sum(COVID.19.Deaths), AllPop=sum(Pop)) %>% 
  ungroup() %>% 
  mutate(Deathprop=COVID.19.Deaths/AllDeaths, AllRate=AllDeaths/AllPop, 
         MortRate=COVID.19.Deaths*100000/Pop)

ratio <- collapse %>% 
  select(Country, Age.Group, MortRate) %>% 
  spread(Country, MortRate) %>% 
  mutate(ratio=USA/UK)

agg_png("Outputs/COVIDDeathsUKUSAProp.png", units="in", width=8, height=6, res=500)
ggplot(collapse, aes(x=Deathprop, y=Country, fill=Age.Group))+
  geom_col()+
  scale_x_continuous(name="Proportion of COVID deaths", labels=label_percent(accuracy=1))+
  scale_y_discrete(name="", labels=c("E&W", "USA"))+
  scale_fill_paletteer_d("RColorBrewer::Spectral", name="")+
  theme_custom()+
  labs(title="Recent COVID-19 deaths in the USA have been younger than England & Wales",
       subtitle="Proportion of all COVID-19 deaths since 1st July 2021 by age group",
       caption="Data from ONS & CDC")

dev.off()

agg_png("Outputs/COVIDDeathsUKUSABars.png", units="in", width=8, height=6, res=500)
ggplot(collapse, aes(y=Age.Group, x=MortRate, fill=Country))+
  geom_col(position="dodge", show.legend=FALSE)+
  scale_x_continuous(name="COVID-19 deaths per 100,000")+
  scale_y_discrete(name="")+
  scale_fill_paletteer_d("ggsci::uniform_startrek", name="")+
  theme_custom()+
  theme(plot.title=element_markdown())+
  labs(title="Recent months have seen more COVID deaths in younger adults<br>in <span style='color:#5C88DA;'>the US</span> than <span style='color:#CC0C00;'>England & Wales</span>",
       subtitle="Registered COVID-19 deaths in July-December 2021 by date of occurrence in England & Wales and the USA",
       caption="Data from ONS, CDC and the US Census Bureau")

dev.off()

agg_png("Outputs/COVIDDeathsUKUSABarsAnnotated.png", units="in", width=8, height=6, res=500)
ggplot(collapse, aes(y=Age.Group, x=MortRate, fill=Country))+
  geom_col(position="dodge", show.legend=FALSE)+
  scale_x_continuous(name="COVID-19 deaths per 100,000")+
  scale_y_discrete(name="")+
  scale_fill_paletteer_d("ggsci::uniform_startrek", name="")+
  theme_custom()+
  theme(plot.title=element_markdown())+
  labs(title="Recent months have seen more COVID deaths in younger adults<br>in <span style='color:#5C88DA;'>the US</span> than <span style='color:#CC0C00;'>England & Wales</span>",
       subtitle="Registered COVID-19 deaths in July-December 2021 by date of occurrence in England & Wales and the USA",
       caption="Data from ONS, CDC and the US Census Bureau")+
  geom_richtext(aes(x=ratio$UK[ratio$Age.Group=="85 Years and Over"]+10, y="85 Years and Over",
           label=paste0("The <span style='color:#5C88DA;'>US COVID death rate</span> is <span style='color:Black;'>", 
                        round(ratio$ratio[ratio$Age.Group=="85 Years and Over"], 1),
                        "</span> <br>times higher than <span style='color:#CC0C00;'>England & Wales")),
           text.colour="Grey50", fill = NA, label.color = NA, hjust=0, vjust=1, family="Lato", size=rel(3))+
  geom_richtext(aes(x=ratio$UK[ratio$Age.Group=="75-84 Years"]+50, y="75-84 Years",
                    label=paste0("<span style='color:#5C88DA;'>US </span><span style='color:Black;'>", 
                                 round(ratio$ratio[ratio$Age.Group=="75-84 Years"], 1), "x</span> higher")),
                text.colour="Grey50", fill = NA, label.color = NA, hjust=0, vjust=1, family="Lato", size=rel(3))+
  geom_richtext(aes(x=ratio$UK[ratio$Age.Group=="65-74 Years"]+20, y="65-74 Years",
                    label=paste0("<span style='color:#5C88DA;'>US </span><span style='color:Black;'>", 
                                 round(ratio$ratio[ratio$Age.Group=="65-74 Years"], 1), "x</span> higher")),
                text.colour="Grey50", fill = NA, label.color = NA, hjust=0, vjust=1, family="Lato", size=rel(3))+
  geom_richtext(aes(x=ratio$UK[ratio$Age.Group=="55-64 Years"]+3, y="55-64 Years",
                    label=paste0("<span style='color:#5C88DA;'>US </span><span style='color:Black;'>", 
                                 round(ratio$ratio[ratio$Age.Group=="55-64 Years"], 1), "x</span> higher")),
                text.colour="Grey50", fill = NA, label.color = NA, hjust=0, vjust=1, family="Lato", size=rel(3))+
  geom_richtext(aes(x=ratio$UK[ratio$Age.Group=="45-54 Years"]+3, y="45-54 Years",
                    label=paste0("<span style='color:#5C88DA;'>US </span><span style='color:Black;'>", 
                                 round(ratio$ratio[ratio$Age.Group=="45-54 Years"], 1), "x</span> higher")),
                text.colour="Grey50", fill = NA, label.color = NA, hjust=0, vjust=1, family="Lato", size=rel(3))+
  geom_richtext(aes(x=ratio$UK[ratio$Age.Group=="35-44 Years"]+3, y="35-44 Years",
                    label=paste0("<span style='color:#5C88DA;'>US </span><span style='color:Black;'>", 
                                 round(ratio$ratio[ratio$Age.Group=="35-44 Years"], 1), "x</span> higher")),
                text.colour="Grey50", fill = NA, label.color = NA, hjust=0, vjust=1, family="Lato", size=rel(3))+
  geom_richtext(aes(x=ratio$UK[ratio$Age.Group=="25-34 Years"]+3, y="25-34 Years",
                    label=paste0("<span style='color:#5C88DA;'>US </span><span style='color:Black;'>", 
                                 round(ratio$ratio[ratio$Age.Group=="25-34 Years"], 1), "x</span> higher")),
                text.colour="Grey50", fill = NA, label.color = NA, hjust=0, vjust=1, family="Lato", size=rel(3))+
  geom_richtext(aes(x=ratio$UK[ratio$Age.Group=="15-24 Years"]+3, y="15-24 Years",
                    label=paste0("<span style='color:#5C88DA;'>US </span><span style='color:Black;'>", 
                                 round(ratio$ratio[ratio$Age.Group=="15-24 Years"], 1), "x</span> higher")),
                text.colour="Grey50", fill = NA, label.color = NA, hjust=0, vjust=1, family="Lato", size=rel(3))+
  geom_richtext(aes(x=ratio$UK[ratio$Age.Group=="0-14 Years"]+3, y="0-14 Years",
                    label=paste0("<span style='color:#5C88DA;'>US </span><span style='color:Black;'>", 
                                 round(ratio$ratio[ratio$Age.Group=="0-14 Years"], 1), "x</span> higher")),
                text.colour="Grey50", fill = NA, label.color = NA, hjust=0, vjust=1, family="Lato", size=rel(3))

dev.off()
