rm(list=ls())

library(tidyverse)
library(curl)
library(readxl)
library(lubridate)
library(paletteer)
library(geofacet)

temp <- tempfile()
source <- ("https://api.coronavirus.data.gov.uk/v2/data?areaType=msoa&metric=newCasesBySpecimenDateRollingRate&format=csv")
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

casedata <- read_csv(temp) 

#Download IMD data
temp <- tempfile()
source <- ("https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/833970/File_1_-_IMD2019_Index_of_Multiple_Deprivation.xlsx")
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

IMD <- read_excel(temp, sheet="IMD2019", range="A2:F32845", col_names=FALSE)[,c(1,2,5,6)]
colnames(IMD) <- c("LSOA11CD", "LSOA11NM", "IMDrank", "IMDdecile")

#Download LSOA to MSOA lookup
temp <- tempfile()
source <- ("https://opendata.arcgis.com/datasets/fe6c55f0924b4734adf1cf7104a0173e_0.csv")
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

lookup <- read.csv(temp) %>% 
  select(LSOA11CD, MSOA11CD, RGN11NM) %>% 
  unique()

#Merge into IMD data
IMD <- merge(IMD, lookup, by="LSOA11CD")

#Bring in population data
temp <- tempfile()
temp2 <- tempfile()
source <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fpopulationandmigration%2fpopulationestimates%2fdatasets%2flowersuperoutputareamidyearpopulationestimatesnationalstatistics%2fmid2019sape22dt13/sape22dt13mid2019lsoabroadagesestimatesunformatted.zip"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)

pop <- read_excel(file.path(temp2, "SAPE22DT13-mid-2019-lsoa-Broad_ages-estimates-unformatted.xlsx"),
                  sheet="Mid-2019 Persons", range="A6:G34758", col_names=FALSE)[,c(1,7)]
colnames(pop) <- c("LSOA11CD", "pop")

#Merge into IMD data
IMD <- merge(IMD, pop)

#Calculate IMD rank at MSOA level as weighted average of LSOA level ranks, weight by population
IMD_MSOA <- IMD %>% 
  group_by(MSOA11CD) %>% 
  summarise(IMDrank=weighted.mean(IMDrank, pop)) %>% 
  ungroup() %>% 
  #Then merge into COVID case data
  merge(casedata, by.x="MSOA11CD", by.y="areaCode", all=TRUE) %>% 
  rename(msoa11cd=MSOA11CD, caserate=newCasesBySpecimenDateRollingRate)

#Calculate lagged changes in 1 week, 2 weeks and 3 weeks for each MSOA
data <- IMD_MSOA %>% 
  select(caserate, date, IMDrank, regionName) %>% 
  filter(date>=max(date)-weeks(3)) %>% 
  spread(date, caserate) %>% 
  #There is definitely a better way to dynamically reference these columns, but this does work
  mutate(abs1wk=.[[ncol(.)]]-.[[ncol(.)-1]],
         abs2wk=.[[ncol(.)]]-.[[ncol(.)-2]],
         abs3wk=.[[ncol(.)]]-.[[ncol(.)-3]],
         rel1wk=abs1wk/.[[ncol(.)-1]],
         rel2wk=abs2wk/.[[ncol(.)-2]],
         rel3wk=abs3wk/.[[ncol(.)-3]],
         IMDrank=max(IMDrank)-IMDrank) %>% 
  rename(oldcases=4)

#National level plots
natrhocases <- cor(subset(data, !is.na(oldcases))$IMDrank, subset(data, !is.na(oldcases))$oldcases)
natrhoabs <- cor(subset(data, !is.na(abs2wk))$IMDrank, subset(data, !is.na(abs2wk))$abs2wk)
natrhorel <- cor(subset(data, !is.na(rel2wk))$IMDrank, subset(data, !is.na(rel2wk))$rel2wk)

tiff("Outputs/COVIDMSOACaseRatexIMDOld.tiff", units="in", width=9, height=7, res=500)
ggplot(data, aes(x=IMDrank, y=oldcases, colour=oldcases))+
  geom_point(show.legend=FALSE)+
  geom_smooth(method="lm", formula=y~x, colour="Red")+
  scale_x_continuous(name="Deprivation (higher = more deprived)")+
  scale_y_continuous(name="Change in cases per 100,000 in the past 2 weeks")+
  scale_colour_paletteer_c("scico::tokyo", direction=-1)+
  theme_classic()+
  theme(strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)),
        plot.title=element_text(face="bold", size=rel(1.2)))+
  labs(title="COVID-19 case rates were higher, on average, in more deprived areas",
       subtitle=paste0("7-day average rates of new COVID-19 cases for MSOAs in England in the week ending ", 
                       max(IMD_MSOA$date)-weeks(2)),
       caption="Data from PHE, ONS & MHCLG | Plot by @VictimOfMaths")+
  annotate("text", x=33000, y=850, label=paste0("\u03C1", "=", round(natrhocases, 2)), colour="Red")
dev.off()

tiff("Outputs/COVIDMSOACaseRatexIMDAbs.tiff", units="in", width=9, height=7, res=500)
ggplot(data, aes(x=IMDrank, y=abs2wk, colour=abs2wk))+
  geom_point()+
  geom_hline(yintercept=0, colour="Grey60")+
  geom_smooth(method="lm", formula=y~x, colour="Red")+
  scale_x_continuous(name="Deprivation (higher = more deprived)")+
  scale_y_continuous(name="Change in cases per 100,000 in the past 2 weeks")+
  scale_colour_paletteer_c("scico::roma", name="Change in\ncase rates",
                           limit=c(-1,1)*max(abs(data$abs2wk)))+
  theme_classic()+
  theme(strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)),
        plot.title=element_text(face="bold", size=rel(1.2)))+
  labs(title="The absolute fall in COVID-19 cases is very equal across the deprivation spectrum",
       subtitle=paste0("Change in rolling 7-day rates of new COVID-19 cases for MSOAs in England between ", 
                       max(IMD_MSOA$date)-weeks(2), " and ",  max(IMD_MSOA$date)),
       caption="Data from PHE, ONS & MHCLG | Plot by @VictimOfMaths")+
  annotate("text", x=33000, y=-100, label=paste0("\u03C1", "=", round(natrhoabs, 2)), colour="Red")
dev.off()

tiff("Outputs/COVIDMSOACaseRatexIMDRel.tiff", units="in", width=9, height=7, res=500)
ggplot(data, aes(x=IMDrank, y=rel2wk, colour=rel2wk))+
  geom_point(show.legend=FALSE)+
  geom_hline(yintercept=0, colour="Grey60")+
  geom_smooth(method="lm", formula=y~x, colour="Red")+
  scale_x_continuous(name="Deprivation (higher = more deprived)")+
  scale_y_continuous(name="Change in cases per 100,000 in the past 2 weeks", 
                     labels=scales::label_percent(accuracy=1))+
  scale_colour_paletteer_c("scico::roma",
                           limit=c(-1,1)*max(abs(subset(data, !is.na(rel2wk))$rel2wk)))+
  theme_classic()+
  theme(strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)),
        plot.title=element_text(face="bold", size=rel(1.2)))+
  labs(title="Relative reductions in COVID-19 cases are smaller in more deprived areas",
       subtitle=paste0("Change in rolling 7-day rates of new COVID-19 cases for MSOAs in England between ", 
                       max(IMD_MSOA$date)-weeks(2), " and ",  max(IMD_MSOA$date)),
       caption="Data from PHE, ONS & MHCLG | Plot by @VictimOfMaths")+
  annotate("text", x=33000, y=-0.3, label=paste0("\u03C1", "=", round(natrhorel, 2)), colour="Red")
dev.off()


mygrid <- data.frame(name=c("North East", "North West", "Yorkshire and The Humber",
                            "West Midlands", "East Midlands", "East of England",
                            "South West", "London", "South East"),
                     row=c(1,2,2,3,3,3,4,4,4), col=c(2,1,2,1,2,3,1,2,3),
                     code=c(1:9))

rhoold <- data %>% 
  filter(!is.na(oldcases)) %>% 
  group_by(regionName) %>% 
  mutate(rho=cor(IMDrank, oldcases), 
         IMDrank=27000, oldcases=2200) %>% 
  ungroup() %>% 
  select(regionName, rho, IMDrank, oldcases) %>% 
  distinct() %>% 
  mutate(label=paste0("\u03C1", "=", round(rho, 2)))

rhoabs <- data %>% 
  filter(!is.na(abs2wk)) %>% 
  group_by(regionName) %>% 
  mutate(rho=cor(IMDrank, abs2wk), 
         IMDrank=25000, abs2wk=1500) %>% 
  ungroup() %>% 
  select(regionName, rho, IMDrank, abs2wk) %>% 
  distinct() %>% 
  mutate(label=paste0("\u03C1", "=", round(rho, 2)))

rhorel <- data %>% 
  filter(!is.na(rel2wk)) %>% 
  group_by(regionName) %>% 
  mutate(rho=cor(IMDrank, rel2wk), 
         IMDrank=25000, rel2wk=3) %>% 
  ungroup() %>% 
  select(regionName, rho, IMDrank, rel2wk) %>% 
  distinct() %>% 
  mutate(label=paste0("\u03C1", "=", round(rho, 2)))

tiff("Outputs/COVIDMSOACaseRatexIMDOldxReg.tiff", units="in", width=10, height=10, res=500)
ggplot(data, aes(x=IMDrank, y=oldcases, colour=oldcases))+
  geom_point(show.legend=FALSE)+
  geom_hline(yintercept=0, colour="Grey60")+
  geom_smooth(method="lm", formula=y~x, colour="Red")+
  geom_text(data=rhoold, aes(label=label), colour="Red")+
  scale_x_continuous(name="Deprivation (higher = more deprived)")+
  scale_y_continuous(name="Change in cases per 100,000 in the past 2 weeks")+
  scale_colour_paletteer_c("scico::tokyo", direction=-1)+
  facet_geo(~regionName, grid=mygrid)+  
  theme_classic()+
  theme(strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)),
        plot.title=element_text(face="bold", size=rel(1.2)))+
  labs(title="COVID-19 case rates were higher, on average, in more deprived areas",
       subtitle=paste0("7-day average rates of new COVID-19 cases for MSOAs in England in the week ending ", 
                       max(IMD_MSOA$date)-weeks(2)),
       caption="Data from PHE, ONS & MHCLG | Plot by @VictimOfMaths")
dev.off()


tiff("Outputs/COVIDMSOACaseRatexIMDAbsxReg.tiff", units="in", width=10, height=10, res=500)
ggplot(data, aes(x=IMDrank, y=abs2wk, colour=abs2wk))+
  geom_point()+
  geom_hline(yintercept=0, colour="Grey60")+
  geom_smooth(method="lm", formula=y~x, colour="Red")+
  geom_text(data=rhoabs, aes(label=label), colour="Red")+
  scale_x_continuous(name="Deprivation (higher = more deprived)")+
  scale_y_continuous(name="Change in cases per 100,000 in the past 2 weeks")+
  scale_colour_paletteer_c("scico::roma", name="Change in\ncase rates",
                           limit=c(-1,1)*max(abs(data$abs2wk)))+
  facet_geo(~regionName, grid=mygrid)+  
  theme_classic()+
  theme(strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)),
        plot.title=element_text(face="bold", size=rel(1.2)))+
  labs(title="Absolute falls in COVID-19 deaths are bigger in more deprived parts of London and the South East",
       subtitle=paste0("Change in rolling 7-day rates of new COVID-19 cases for MSOAs in England between ", 
                       max(IMD_MSOA$date)-weeks(2), " and ",  max(IMD_MSOA$date)),
       caption="Data from PHE, ONS & MHCLG | Plot by @VictimOfMaths")
dev.off()

tiff("Outputs/COVIDMSOACaseRatexIMDRelxReg.tiff", units="in", width=10, height=10, res=500)
ggplot(data, aes(x=IMDrank, y=rel2wk, colour=rel2wk))+
  geom_point(show.legend=FALSE)+
  geom_hline(yintercept=0, colour="Grey60")+
  geom_smooth(method="lm", formula=y~x, colour="Red")+
  geom_text(data=rhorel, aes(label=label), colour="Red")+
  scale_x_continuous(name="Deprivation (higher = more deprived)")+
  scale_y_continuous(name="Change in cases per 100,000 in the past 2 weeks",
                     labels=scales::label_percent(accuracy=1))+
  scale_colour_paletteer_c("scico::roma",
                           limit=c(-1,1)*max(abs(subset(data, !is.na(rel2wk))$rel2wk)))+
  facet_geo(~regionName, grid=mygrid)+  
  theme_classic()+
  theme(strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)),
        plot.title=element_text(face="bold", size=rel(1.2)))+
  labs(title="The relative fall in COVID-19 cases is most unequal in Yorkshire and the North West",
       subtitle=paste0("Change in rolling 7-day rates of new COVID-19 cases for MSOAs in England between ", 
                       max(IMD_MSOA$date)-weeks(2), " and ",  max(IMD_MSOA$date)),
       caption="Data from PHE, ONS & MHCLG | Plot by @VictimOfMaths")
dev.off()


