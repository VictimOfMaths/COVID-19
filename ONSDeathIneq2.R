rm(list=ls())

library(tidyverse)
library(curl)
library(paletteer)
library(readxl)
library(broom)
library(wesanderson)
library(ggtext)

temp <- tempfile()
source <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/adhocs/10929weeklydeathsregistrationsbyimdsexandagegroupenglandandwales2005to2018/wklydthsimdsexage2005to2018final.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

#Read in data
data2005.E <- read_excel(temp, sheet="Table 1", range="A4:BC144")
data2005.W <- read_excel(temp, sheet="Table 2", range="A4:BC144")
data2006.E <- read_excel(temp, sheet="Table 3", range="A4:BC144")
data2006.W <- read_excel(temp, sheet="Table 4", range="A4:BC144")
data2007.E <- read_excel(temp, sheet="Table 5", range="A4:BC144")
data2007.W <- read_excel(temp, sheet="Table 6", range="A4:BC144")
data2008.E <- read_excel(temp, sheet="Table 7", range="A4:BC144")
data2008.W <- read_excel(temp, sheet="Table 8", range="A4:BC144")
data2009.E <- read_excel(temp, sheet="Table 9", range="A4:BD144")
colnames(data2009.E)[56] <- "Week 53"
data2009.W <- read_excel(temp, sheet="Table 10", range="A4:BD144")
colnames(data2009.W)[56] <- "Week 53"
data2010.E <- read_excel(temp, sheet="Table 11", range="A4:BC144")
data2010.W <- read_excel(temp, sheet="Table 12", range="A4:BC144")
data2011.E <- read_excel(temp, sheet="Table 13", range="A4:BC144")
data2011.W <- read_excel(temp, sheet="Table 14", range="A4:BC144")
data2012.E <- read_excel(temp, sheet="Table 15", range="A4:BC144")
data2012.W <- read_excel(temp, sheet="Table 16", range="A4:BC144")
data2013.E <- read_excel(temp, sheet="Table 17", range="A4:BC144")
data2013.W <- read_excel(temp, sheet="Table 18", range="A4:BC144")
data2014.E <- read_excel(temp, sheet="Table 19", range="A4:BC144")
data2014.W <- read_excel(temp, sheet="Table 20", range="A4:BC144")
data2015.E <- read_excel(temp, sheet="Table 21", range="A4:BD144")
data2015.W <- read_excel(temp, sheet="Table 22", range="A4:BD144")
data2016.E <- read_excel(temp, sheet="Table 23", range="A4:BC144")
data2016.W <- read_excel(temp, sheet="Table 24", range="A4:BC144")
data2017.E <- read_excel(temp, sheet="Table 25", range="A4:BC144")
data2017.W <- read_excel(temp, sheet="Table 26", range="A4:BC144")
data2018.E <- read_excel(temp, sheet="Table 27", range="A4:BC144")
data2018.W <- read_excel(temp, sheet="Table 28", range="A4:BC144")

#Tidy up and stick together
data2005.E$country <- "England"
data2005.W$country <- "Wales"
data2006.E$country <- "England"
data2006.W$country <- "Wales"
data2007.E$country <- "England"
data2007.W$country <- "Wales"
data2008.E$country <- "England"
data2008.W$country <- "Wales"
data2009.E$country <- "England"
data2009.W$country <- "Wales"
data2010.E$country <- "England"
data2010.W$country <- "Wales"
data2011.E$country <- "England"
data2011.W$country <- "Wales"
data2012.E$country <- "England"
data2012.W$country <- "Wales"
data2013.E$country <- "England"
data2013.W$country <- "Wales"
data2014.E$country <- "England"
data2014.W$country <- "Wales"
data2015.E$country <- "England"
data2015.W$country <- "Wales"
data2016.E$country <- "England"
data2016.W$country <- "Wales"
data2017.E$country <- "England"
data2017.W$country <- "Wales"
data2018.E$country <- "England"
data2018.W$country <- "Wales"

data2005 <- bind_rows(data2005.E, data2005.W)
data2005$year <- 2005
data2006 <- bind_rows(data2006.E, data2006.W)
data2006$year <- 2006
data2007 <- bind_rows(data2007.E, data2007.W)
data2007$year <- 2007
data2008 <- bind_rows(data2008.E, data2008.W)
data2008$year <- 2008
data2009 <- bind_rows(data2009.E, data2009.W)
data2009$year <- 2009
data2010 <- bind_rows(data2010.E, data2010.W)
data2010$year <- 2010
data2011 <- bind_rows(data2011.E, data2011.W)
data2011$year <- 2011
data2012 <- bind_rows(data2012.E, data2012.W)
data2012$year <- 2012
data2013 <- bind_rows(data2013.E, data2013.W)
data2013$year <- 2013
data2014 <- bind_rows(data2014.E, data2014.W)
data2014$year <- 2014
data2015 <- bind_rows(data2015.E, data2015.W)
data2015$year <- 2015
data2016 <- bind_rows(data2016.E, data2016.W)
data2016$year <- 2016
data2017 <- bind_rows(data2017.E, data2017.W)
data2017$year <- 2017
data2018 <- bind_rows(data2018.E, data2018.W)
data2018$year <- 2018

data <- bind_rows(data2005, data2006, data2007, data2008, data2009, data2010, data2011, data2012,
                  data2013, data2014, data2015, data2016, data2017, data2018)

#Merge E&W data
data <- data %>%
  group_by(year, IMD, Sex, Age) %>%
  summarise_at(vars(`Week 1`:`Week 52`, `Week 53`), sum)

#Convert to long
data_long <- gather(data, week, deaths, c(5:57))

data_long$week <- as.numeric(substr(data_long$week, 6,7))

#Read in populations for rates using mid-year values for each year
#A more refined approach estimating week-on-week populations is no doubt possible,
#But seems unlikely to make much difference.
temp <- tempfile()
source <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/adhocs/009299numberofdeathsandpopulationsindeprivationdecileareasbysexandsingleyearofageenglandandwalesregisteredyears2001to2017/final.xls"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
engpop <- read_excel(temp, sheet=5, range="B6:GB175", col_names=FALSE)
walpop <- read_excel(temp, sheet=6, range="B6:GB175", col_names=FALSE)

engpop$country <- "England"
walpop$country <- "Wales"
pop <- bind_rows(engpop, walpop)

#Label years
pop$year <- rep(c(2001:2017), each=10, times=2)

colnames(pop) <- c("IMD", paste0("Male_", as.character(0:90)), 
                  paste0("Female_", as.character(0:90)), "country", "year")

#Collapse by country
pop <- pop %>%
  group_by(IMD, year) %>%
  summarise_at(vars("Male_0":"Female_90"), sum)

#Assume 2018 population=2017 population
temp <- subset(pop, year==2017)
temp$year <- 2018
pop <- bind_rows(pop, temp)

#Move sex to long
pop_long <- pivot_longer(pop, cols=c(3:184), names_to=c("Sex", "age"), names_sep="_", values_to="pop")

pop_long$age <- as.integer(pop_long$age)

pop_long$Age <- case_when(
  pop_long$age==0 ~ "Under 1 year",
  pop_long$age<15 ~ "01-14",
  pop_long$age<45 ~ "15-44",
  pop_long$age<65 ~ "45-64",
  pop_long$age<75 ~ "65-74",
  pop_long$age<85 ~ "75-84",
  TRUE ~ "85+")

pop_long <- pop_long %>%
  group_by(Age, Sex, year, IMD) %>%
  summarise(pop=sum(pop))

#Merge into deaths data
data_long <- merge(data_long, pop_long, all.x=TRUE)

data_long$mortrate <- data_long$deaths*100000/data_long$pop

#Age standardise
data_long$weight <- case_when(
  data_long$Age=="Under 1 year" ~ 0.01,
  data_long$Age=="01-14" ~ 0.15,
  data_long$Age=="15-44" ~ 0.38,
  data_long$Age=="45-64" ~ 0.265,
  data_long$Age=="65-74" ~ 0.105,
  data_long$Age=="75-84" ~ 0.065,
  data_long$Age=="85+" ~ 0.025,
  TRUE~0)

data_long <- data_long %>%
  group_by(year, IMD, Sex, week) %>%
  summarise(mortrate=weighted.mean(mortrate, weight), pop=sum(pop))

#Set up for SII calculations
data_long <- data_long %>%
  arrange(year, Sex, week, -IMD) %>%
  group_by(year, Sex, week) %>%
  mutate(cumpop=cumsum(pop), totpop=sum(pop))

data_long$cumpopprop <- data_long$cumpop/data_long$totpop

#Calculate SII & RII
SII <- data_long %>%
  group_by(year, Sex, week) %>%
  filter(week!=53) %>%
  do(tidy(lm(mortrate ~ cumpopprop, data=.))) %>%
  pivot_wider(id_cols=c("year", "Sex", "week"), names_from=term, values_from=c("estimate", "std.error"))

colnames(SII) <- c("year", "Sex", "week", "intercept", "SII", "int.SE", "SII.SE")

SII$RII <- (SII$intercept+SII$SII)/SII$intercept

#calculate mean across all years and last 5 years
SII <- SII %>%
  group_by(Sex, week) %>%
  mutate(SII.mean.all=mean(SII), RII.mean.all=mean(RII))

pal <- wes_palette("Zissou1", 14, type = "continuous")

#Plot SII trends
tiff("Outputs/SIIbyweek.tiff", units="in", width=12, height=8, res=300)
ggplot(SII, aes(x=week, y=SII, colour=as.factor(year)))+
  geom_line(show.legend=FALSE)+
  facet_wrap(~Sex)+
  theme_classic()+
  scale_y_continuous(name="Slope Index of Inequality", limits=c(0,25))+
  scale_colour_manual(values=pal, name="Year")+
  theme(strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)),
        plot.title.position="plot", plot.subtitle=element_markdown())+
  labs(title="Absolute inequalities in mortality are highest in winter",
       subtitle="Patterns in weekly Slope Index of Inequality (SII) values in age-standardised deaths in England & Wales between <span style='color:#3c99b2;'>2005 </span> and <span style='color:#f22300;'>2018",
       caption="Data from ONS | Plot by @VictimOfMaths")
dev.off()

tiff("Outputs/RIIbyweek.tiff", units="in", width=12, height=8, res=300)
ggplot(SII, aes(x=week, y=RII, colour=as.factor(year)))+
  geom_line(show.legend=FALSE)+
  facet_wrap(~Sex)+
  theme_classic()+
  scale_y_continuous(name="Relative Index of Inequality", limits=c(0,2.5))+
  scale_colour_manual(values=pal, name="Year")+
  theme(strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)),
        plot.title.position="plot", plot.subtitle=element_markdown())+
        labs(title="Relative inequalities in mortality are similar throughout the year",
             subtitle="Patterns in weekly Relative Index of Inequality (RII) values in age-standardised deaths in England & Wales between <span style='color:#3c99b2;'>2005 </span> and <span style='color:#f22300;'>2018",
             caption="Data from ONS | Plot by @VictimOfMaths")
dev.off()

##################################################################################
#Restrict analysis to England only from 1st March - 17th April 2020 (weeks 10-16)#
##################################################################################

data2 <- bind_rows(data2005, data2006, data2007, data2008, data2009, data2010, data2011, data2012,
                  data2013, data2014, data2015, data2016, data2017, data2018)

data2 <- subset(data2, country=="England")
data2_long <- gather(data2, week, deaths, c(4:55, 58))
data2_long$week <- as.numeric(substr(data2_long$week, 6,7))

#Remove data outside of weeks of interest
data2_long <- subset(data2_long, week>=10 & week<=16)

#Wollapse weeks
data2_long <- data2_long %>%
  group_by(IMD, Sex, Age, year) %>%
  summarise(deaths=sum(deaths))

engpop$year <- rep(c(2001:2017), each=10)

colnames(engpop) <- c("IMD", paste0("Male_", as.character(0:90)), 
                   paste0("Female_", as.character(0:90)), "country", "year")

#Assume 2018 population=2017 population
temp <- subset(engpop, year==2017)
temp$year <- 2018
engpop <- bind_rows(engpop, temp)

#Move sex to long
engpop_long <- pivot_longer(engpop, cols=c(2:183), names_to=c("Sex", "age"), names_sep="_", values_to="pop")

engpop_long$age <- as.integer(engpop_long$age)

engpop_long$Age <- case_when(
  engpop_long$age==0 ~ "Under 1 year",
  engpop_long$age<15 ~ "01-14",
  engpop_long$age<45 ~ "15-44",
  engpop_long$age<65 ~ "45-64",
  engpop_long$age<75 ~ "65-74",
  engpop_long$age<85 ~ "75-84",
  TRUE ~ "85+")

engpop_long <- engpop_long %>%
  group_by(Age, Sex, year, IMD) %>%
  summarise(pop=sum(pop))

#Merge into deaths data
data2_long <- merge(data2_long, engpop_long, all.x=TRUE)

data2_long$mortrate <- data2_long$deaths*100000/data2_long$pop

#Age standardise
data2_long$weight <- case_when(
  data2_long$Age=="Under 1 year" ~ 0.01,
  data2_long$Age=="01-14" ~ 0.15,
  data2_long$Age=="15-44" ~ 0.38,
  data2_long$Age=="45-64" ~ 0.265,
  data2_long$Age=="65-74" ~ 0.105,
  data2_long$Age=="75-84" ~ 0.065,
  data2_long$Age=="85+" ~ 0.025,
  TRUE~0)

data2_long <- data2_long %>%
  group_by(year, IMD, Sex) %>%
  summarise(mortrate=weighted.mean(mortrate, weight), pop=sum(pop))

#Bring in 2020 data 
temp <- tempfile()
source <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fbirthsdeathsandmarriages%2fdeaths%2fdatasets%2fdeathsinvolvingcovid19bylocalareaanddeprivation%2f1march2020to17april2020/referencetablesdraft.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

data2020 <- read_excel(temp, sheet="Table 3", range="B6:P25", col_names=FALSE)[,-c(2:7,9:12, 14,15)]

colnames(data2020) <- c("IMD", "rate.m", "rate.f")

data2020$cause <- rep(c("All cause", "COVID-19"), each=10, times=1)

#Calculate 'other cause' outcomes
data_wide2020 <- pivot_wider(data2020, names_from=cause, values_from=c(2:3))

data_wide2020$rate.m_other <- data_wide2020$`rate.m_All cause`-data_wide2020$`rate.m_COVID-19`
data_wide2020$rate.f_other <- data_wide2020$`rate.f_All cause`-data_wide2020$`rate.f_COVID-19`

data2020 <- pivot_longer(data_wide2020, cols=c(2:7), names_to=c("measure", "cause"), 
                     values_to="mortrate", names_sep="_")

data2020$Sex <- case_when(
  endsWith(data2020$measure, "m")==TRUE ~ "Male",
  endsWith(data2020$measure, "f")==TRUE ~ "Female",
  TRUE~"all"
)

data2020$year <- 2020
data2020 <- data2020[-c(2)]

data2_long$IMD <- case_when(
  data2_long$IMD==10 ~ "10 (least deprived)",
  data2_long$IMD==9 ~ "9",
  data2_long$IMD==8 ~ "8",
  data2_long$IMD==7 ~ "7",
  data2_long$IMD==6 ~ "6",
  data2_long$IMD==5 ~ "5",
  data2_long$IMD==4 ~ "4",
  data2_long$IMD==3 ~ "3",
  data2_long$IMD==2 ~ "2",
  data2_long$IMD==1 ~ "1 (most deprived)"
)

#Calculate average deaths 2014-18
temp <- data2_long %>%
              group_by(IMD, Sex) %>%
              summarise(mortrate=mean(mortrate))
temp$year <- 2019

data2_long <- bind_rows(data2_long, temp)

data2_long$cause <- "All cause"


#Merge with older data
fulldata <- bind_rows(subset(data2_long, year>2013)[-c(5)], data2020)

fulldata$Sex <- factor(fulldata$Sex, levels=c("Male", "Female", "all"))
fulldata$cause <- factor(fulldata$cause, levels=c("other", "COVID-19", "All cause"))
fulldata$IMD <- factor(fulldata$IMD, levels=c("1 (most deprived)", "2", "3", "4", "5", "6", "7", "8", "9", "10 (least deprived)"))

tiff("Outputs/COVIDIneqCompare.tiff", units="in", width=12, heigh=8, res=300)
ggplot(fulldata)+
  geom_col(data=subset(fulldata, year==2020 & cause!="All cause"), aes(x=IMD, y=mortrate, fill=cause), show.legend=FALSE)+
  geom_jitter(data=subset(fulldata, year<2019 & cause=="All cause"), aes(x=IMD, y=mortrate), colour="midnightblue")+
  scale_fill_paletteer_d("palettetown::porygon", name="Cause", labels=c("Other", "COVID-19"))+
  scale_x_discrete("Deprivation decile")+
  scale_y_continuous("Age-standardised mortality rate per 100,000")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), strip.background=element_blank(),
        strip.text=element_text(face="bold", size=rel(1)), plot.title.position="plot",
        axis.text=element_text(colour="black"), plot.subtitle=element_markdown())+
  facet_wrap(~Sex)+
  labs(title="All cause deaths are higher and more unequal than usual",
       subtitle="<span style='color:grey40;'>Deaths in England between March 1st and April 17th by deprivation decile from <span style='color:midnightblue;'>all causes in 2014-18<span style='color:grey40;'> and from <span style='color:#f89088;'>COVID-19<span style='color:grey40;'> and <span style='color:#40a0d8;'>other causes<span style='color:grey40;'> in 2020",
caption="Data from ONS | Plot by @VictimOfMaths")
dev.off()

tiff("Outputs/COVIDIneqCompare2.tiff", units="in", width=12, heigh=8, res=300)
ggplot(fulldata)+
  geom_col(data=subset(fulldata, year==2020 & cause=="other"), aes(x=IMD, y=mortrate, fill=cause), show.legend=FALSE)+
  geom_line(data=subset(fulldata, year==2019), aes(x=IMD, y=mortrate, group=Sex), colour="midnightblue")+
  scale_fill_paletteer_d("palettetown::porygon", name="Cause", labels=c("Other", "COVID-19"))+
  scale_x_discrete("Deprivation decile")+
  scale_y_continuous("Age-standardised mortality rate per 100,000")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), strip.background=element_blank(),
        strip.text=element_text(face="bold", size=rel(1)), plot.title.position="plot",
        axis.text=element_text(colour="black"), plot.subtitle=element_markdown())+
  facet_wrap(~Sex)+
  labs(title="Fewer people are dying of 'usual' causes during the pandemic",
       subtitle="<span style='color:grey40;'>Non-COVID-19 deaths in England between March 1st and April 17th by deprivation decile <span style='color:#40a0d8;'>in 2020 <span style='color:grey40;'>compared to <span style='color:midnightblue;'>the average for 2014-18",
       caption="Data from ONS | Plot by @VictimOfMaths")
dev.off()

#Set up for SII calculations
fulldata <- arrange(fulldata, year, Sex, cause, fct_rev(IMD))
fulldata$popprop <- rep(seq(0.05,0.95, 0.1), times=18)

#Calculate SII & RII
SII <- fulldata %>%
  group_by(year, Sex, cause) %>%
  do(tidy(lm(mortrate ~ popprop, data=.))) %>%
  pivot_wider(id_cols=c("year", "Sex", "cause"), names_from=term, values_from=c("estimate", "std.error"))


colnames(SII) <- c("year", "Sex", "cause", "intercept", "SII", "int.SE", "SII.SE")

SII$RII <- (SII$intercept+SII$SII)/SII$intercept

#calculate mean from 2014-18
temp <- SII %>%
  filter(year<2019) %>%
  group_by(Sex, cause) %>%
  summarise(SII=mean(SII), RII=mean(RII))

temp$year <- 2018

SII <- bind_rows(temp, subset(SII, year>2019))

tiff("Outputs/SIICompare.tiff", units="in", height=8, width=12, res=300)
ggplot()+
  geom_col(data=subset(SII, year==2020), aes(x=fct_rev(cause), y=SII, fill=fct_rev(cause)), show.legend=FALSE)+
  geom_point(data=subset(SII, year==2018), aes(x=cause, y=SII), colour="red", shape=18, size=5)+
  scale_x_discrete(name="", labels=c("All cause", "COVID-19", "Other cause"))+
  scale_y_continuous(name="Slope Index of Inequality (SII)")+
  scale_fill_manual(values=c("midnightblue", "#f89088", "#40a0d8"))+
  facet_wrap(~Sex)+
  theme_classic()+
  theme(strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)),
        plot.subtitle=element_markdown())+
  labs(title="Absolute inequality in mortality rates is higher than usual",
       subtitle = "<span style='color:grey40;'>Slope Indices of Inequality (SII) for deaths in England between 1st March and 17th April by cause<br>in 2020 (<span style='color:midnightblue;'>All cause<span style='color:grey40;'>, <span style='color:#f89088;'>COVID-19 <span style='color:grey40;'>and <span style='color:#40a0d8;'>Other causes<span style='color:grey40;'>) and the <span style='color:red;'>All-cause average for 2014-18",
       caption="Data from ONS | Plot by @VictimOfMaths")
dev.off()

tiff("Outputs/RIICompare.tiff", units="in", height=8, width=12, res=300)
ggplot()+
  geom_col(data=subset(SII, year==2020), aes(x=fct_rev(cause), y=RII, fill=fct_rev(cause)), show.legend=FALSE)+
  geom_point(data=subset(SII, year==2018), aes(x=cause, y=RII), colour="red", shape=18, size=5)+
  scale_x_discrete(name="", labels=c("All cause", "COVID-19", "Other cause"))+
  scale_y_continuous(name="Relative Index of Inequality (RII)")+
  scale_fill_manual(values=c("midnightblue", "#f89088", "#40a0d8"))+
  facet_wrap(~Sex)+
  theme_classic()+
  theme(strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)),
        plot.subtitle=element_markdown())+
  labs(title="Relative inequality in non-COVID-19 deaths is on a par with historic levels",
       subtitle = "<span style='color:grey40;'>Relative Indices of Inequality (RII) for deaths in England between 1st March and 17th April by cause<br>in 2020 (<span style='color:midnightblue;'>All cause<span style='color:grey40;'>, <span style='color:#f89088;'>COVID-19 <span style='color:grey40;'>and <span style='color:#40a0d8;'>Other causes<span style='color:grey40;'>) and the <span style='color:red;'>All-cause average for 2014-18",
       caption="Data from ONS | Plot by @VictimOfMaths")
dev.off()
