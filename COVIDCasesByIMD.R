rm(list=ls())

library(tidyverse)
library(curl)
library(readxl)
library(paletteer)
library(sf)
library(gganimate)
library(cowplot)
library(lubridate)
library(data.table)
library(forcats)
library(RcppRoll)

options(scipen = 999)

#Read in COVID dase data
temp <- tempfile()
source <- "https://coronavirus.data.gov.uk/downloads/csv/coronavirus-cases_latest.csv"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
data <- fread(temp)[,c(1,2,3,4,5,8)]
colnames(data) <- c("name", "code", "type", "date", "cases", "cumul_cases")
data$date <- as.Date(data$date)

#split out regional data
reg_data <- subset(data, type=="Region")
data <- subset(data, type=="Upper tier local authority")

#Read in IMD data
temp <- tempfile()
source <- "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/834001/File_11_-_IoD2019_Local_Authority_District_Summaries__upper-tier__.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
IMDdata <- read_excel(temp, sheet="IMD", range="A1:D152", col_names=TRUE)[,-c(2)]
colnames(IMDdata) <-  c("code", "IMDrank", "IMDorder")
IMDdata$quintile <- ntile(IMDdata$IMDrank, 5)

data <- merge(data, IMDdata, by="code")

#Bring in region data (probably a better source for this)
temp <- tempfile()
source <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/healthandsocialcare/healthandlifeexpectancies/datasets/healthylifeexpectancyatbirthforuppertierlocalauthoritiesenglandreferencetables/20102012/hleatbirthforuppertierlocalauthorities201012final.xls"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
RegionLookup <- read_excel(temp, sheet="UTLA males", range="A10:E160", col_names=TRUE)[,c(1,5)]
colnames(RegionLookup) <- c("code", "region")

#Combine
data <- merge(data, RegionLookup, all.x=TRUE)

#Sort out regions with boundary changes since the region list was compiled
data$region <- case_when(
  data$name=="Dorset" ~ "South West",
  data$name=="Bournemouth, Christchurch and Poole" ~ "South West",
  data$name=="Gateshead" ~ "North East",
  data$name=="City of London" ~ "London",
  TRUE ~ data$region
)

#Bring in LA-level population data
temp <- tempfile()
source <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fpopulationandmigration%2fpopulationestimates%2fdatasets%2fpopulationestimatesforukenglandandwalesscotlandandnorthernireland%2fmid20182019laboundaries/ukmidyearestimates20182019ladcodes.xls"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
LApop <- read_excel(temp, sheet="MYE2-All", range="A5:D367", col_names=TRUE)
colnames(LApop) <- c("code2", "name", "geography", "pop")

LApop_long <- gather(LApop, age, pop, pop)

#deal with Cornwall & Scilly + Hackney & City of London which are merged in the PHE data
LApop_long$code <- case_when(
  LApop_long$code2=="E06000052" ~ "E06000052",
  TRUE ~ LApop_long$code2
)

LApop_long <- LApop_long %>%
  group_by(code) %>%
  summarise(pop=sum(pop))

#Combine
data <- merge(data, LApop_long, all.x=TRUE)

#Set up skepeton dataframe with dates
LAcodes <- unique(data$code)
min <- min(data$date)
max <- max(data$date)

skeleton <- data.frame(code=rep(LAcodes, each=(max-min+1), times=1), date=rep(seq.Date(from=min, to=max, by="day"), each=1, times=length(LAcodes)))

#Map data onto skeleton
fulldata <- merge(skeleton, data[,c(1,4:6)], by=c("code", "date"), all.x=TRUE, all.y=TRUE)

#Fill in blank days
fulldata$cases <- ifelse(is.na(fulldata$cases), 0, fulldata$cases)

#Calculate cumulative sums so far
fulldata <- fulldata %>%
  group_by(code) %>%
  mutate(cumul_cases=cumsum(cases))

#Bring in LA factors
LAdata <- unique(data[,c(1,2,7:11)])
fulldata <- merge(fulldata, LAdata)
  
#Calculate rates by LA
fulldata$caserate <- fulldata$cases*100000/fulldata$pop

#Collapse to quintiles
quintiles <- fulldata %>%
  group_by(quintile, date) %>%
  summarise(cases=sum(cases), cumul_cases=sum(cumul_cases), pop=sum(pop))

#Calculate rates
quintiles$caserate <- quintiles$cumul_cases*100000/quintiles$pop

#Set up lines for log breaks
logbreaks <- data.frame(breaks=c(1,2,5,10, 20, 50, 100, 200, 500, 1000, 2000, 5000, 10000))
ratebreaks <- data.frame(breaks=c(1,2,5,10,20,50))

tiff("Outputs/COVIDQuintiles.tiff", units="in", res=300, width=8, height=6)
ggplot(quintiles, aes(x=date, y=cumul_cases, colour=as.factor(quintile)))+
  geom_line()+
  theme_classic()+
  scale_colour_manual(values=c("#fcc5c0", "#fa9fb5", "#f768a1", "#c51b8a", "#7a0177"), name="IMD quintile",
                      labels=c("Q1 (least deprived)", "Q2", "Q3", "Q4", "Q5 (most deprived)"))+
  scale_x_date(name="Date")+
  scale_y_continuous(name="Cumulative known cases")+
  labs(title="Cumulative COVID-19 cases by deprivation quintile", subtitle="Deprivation estimated using mean IMD level across each UTLA",
       caption="Case data from PHE | IMD2019 data from DHCLG | Plot by @VictimOfMaths")
dev.off()

tiff("Outputs/COVIDQuintilesLog.tiff", units="in", res=300, width=8, height=6)
ggplot(subset(quintiles, cumul_cases>0), aes(x=date, y=cumul_cases, colour=as.factor(quintile)))+
  geom_segment(data=logbreaks, aes(x=as.Date(-Inf, origin="2018-01-01"), xend=as.Date(Inf, origin="2018-01-01"), 
                                   y=breaks, yend=breaks), colour="gray80")+
  geom_line()+
  theme_classic()+
  scale_colour_manual(values=c("#fcc5c0", "#fa9fb5", "#f768a1", "#c51b8a", "#7a0177"), name="IMD quintile",
                      labels=c("Q1 (least deprived)", "Q2", "Q3", "Q4", "Q5 (most deprived)"))+
  scale_x_date(name="Date")+
  scale_y_continuous(name="Cumulative known cases (log scale)", trans="log10", breaks=logbreaks$breaks)+
  labs(title="Cumulative COVID-19 cases by deprivation quintile", subtitle="Deprivation estimated using mean IMD level across each UTLA",
       caption="Case data from PHE | IMD2019 data from DHCLG | Plot by @VictimOfMaths")
dev.off()

tiff("Outputs/COVIDQuintilesRate.tiff", units="in", res=300, width=8, height=6)
ggplot(quintiles, aes(x=date, y=caserate, colour=as.factor(quintile)))+
  geom_line()+
  theme_classic()+
  scale_colour_manual(values=c("#fcc5c0", "#fa9fb5", "#f768a1", "#c51b8a", "#7a0177"), name="IMD quintile",
                      labels=c("Q1 (least deprived)", "Q2", "Q3", "Q4", "Q5 (most deprived)"))+
  scale_x_date(name="Date")+
  scale_y_continuous(name="Cumulative known cases per 100,000 population")+
  labs(title="Cumulative COVID-19 cases by deprivation quintile", subtitle="Deprivation estimated using mean IMD level across each UTLA",
       caption="Case data from PHE | IMD2019 data from DHCLG | Plot by @VictimOfMaths")
dev.off()

tiff("Outputs/COVIDQuintilesRateLog.tiff", units="in", res=300, width=8, height=6)
ggplot(subset(quintiles, cumul_cases>0), aes(x=date, y=caserate, colour=as.factor(quintile)))+
  geom_segment(data=ratebreaks, aes(x=as.Date(-Inf, origin="2018-01-01"), xend=as.Date(Inf, origin="2018-01-01"), 
                                    y=breaks, yend=breaks), colour="gray80")+  geom_line()+
  theme_classic()+
  scale_colour_manual(values=c("#fcc5c0", "#fa9fb5", "#f768a1", "#c51b8a", "#7a0177"), name="IMD quintile",
                      labels=c("Q1 (least deprived)", "Q2", "Q3", "Q4", "Q5 (most deprived)"))+
  scale_x_date(name="Date")+
  scale_y_continuous(name="Cumulative known cases per 100,000 population (log scale)", trans="log10",
                     breaks=ratebreaks$breaks)+
  labs(title="Cumulative COVID-19 cases by deprivation quintile", subtitle="Deprivation estimated using mean IMD level across each UTLA",
       caption="Case data from PHE | IMD2019 data from DHCLG | Plot by @VictimOfMaths")
dev.off()

#Split into London/not London
fulldata$London <- case_when(
  fulldata$region=="London" ~ "London",
  TRUE ~ "Rest of England"
)

#Collapse to quintiles
Lonquintiles<- fulldata %>%
  group_by(quintile, London, date) %>%
  summarise(cases=sum(cases), cumul_cases=sum(cumul_cases), pop=sum(pop))

#Calculate rates
Lonquintiles$caserate <- Lonquintiles$cumul_cases*100000/Lonquintiles$pop

tiff("Outputs/COVIDQuintilesLon.tiff", units="in", res=300, width=12, height=6)
ggplot(Lonquintiles, aes(x=date, y=cumul_cases, colour=as.factor(quintile)))+
  geom_line()+
  facet_wrap(~London)+
  theme_classic()+
  scale_colour_manual(values=c("#fcc5c0", "#fa9fb5", "#f768a1", "#c51b8a", "#7a0177"), name="IMD quintile",
                      labels=c("Q1 (least deprived)", "Q2", "Q3", "Q4", "Q5 (most deprived)"))+
  scale_x_date(name="Date")+
  scale_y_continuous(name="Cumulative known cases")+
  labs(title="Cumulative COVID-19 cases by deprivation quintile", subtitle="Deprivation estimated using mean IMD level across each UTLA",
       caption="Case data from PHE | IMD2019 data from DHCLG | Plot by @VictimOfMaths")
dev.off()

tiff("Outputs/COVIDQuintilesLonLog.tiff", units="in", res=300, width=12, height=6)
ggplot(subset(Lonquintiles, cumul_cases>0), aes(x=date, y=cumul_cases, colour=as.factor(quintile)))+
  geom_segment(data=logbreaks, aes(x=as.Date(-Inf, origin="2018-01-01"), xend=as.Date(Inf, origin="2018-01-01"), 
                                   y=breaks, yend=breaks), colour="gray80")+  geom_line()+  geom_line()+
  facet_wrap(~London)+
  theme_classic()+
  scale_colour_manual(values=c("#fcc5c0", "#fa9fb5", "#f768a1", "#c51b8a", "#7a0177"), name="IMD quintile",
                      labels=c("Q1 (least deprived)", "Q2", "Q3", "Q4", "Q5 (most deprived)"))+
  scale_x_date(name="Date")+
  scale_y_continuous(name="Cumulative known cases", trans="log10",
                     breaks=logbreaks$breaks)+
  labs(title="Cumulative COVID-19 cases by deprivation quintile", subtitle="Deprivation estimated using mean IMD level across each UTLA",
       caption="Case data from PHE | IMD2019 data from DHCLG | Plot by @VictimOfMaths")
dev.off()

tiff("Outputs/COVIDQuintilesLonRate.tiff", units="in", res=300, width=12, height=6)
ggplot(Lonquintiles, aes(x=date, y=caserate, colour=as.factor(quintile)))+
  geom_line()+
  facet_wrap(~London)+
  theme_classic()+
  scale_colour_manual(values=c("#fcc5c0", "#fa9fb5", "#f768a1", "#c51b8a", "#7a0177"), name="IMD quintile",
                      labels=c("Q1 (least deprived)", "Q2", "Q3", "Q4", "Q5 (most deprived)"))+
  scale_x_date(name="Date")+
  scale_y_continuous(name="Cumulative known cases per 100,000 population")+
  labs(title="Cumulative COVID-19 cases by deprivation quintile", subtitle="Deprivation estimated using mean IMD level across each UTLA",
       caption="Case data from PHE | IMD2019 data from DHCLG | Plot by @VictimOfMaths")
dev.off()

tiff("Outputs/COVIDQuintilesLonRateLog.tiff", units="in", res=300, width=12, height=6)
ggplot(subset(Lonquintiles, cumul_cases>0), aes(x=date, y=caserate, colour=as.factor(quintile)))+
  geom_segment(data=ratebreaks, aes(x=as.Date(-Inf, origin="2018-01-01"), xend=as.Date(Inf, origin="2018-01-01"), 
                                    y=breaks, yend=breaks), colour="gray80")+  geom_line()+  geom_line()+
  facet_wrap(~London)+
  theme_classic()+
  scale_colour_manual(values=c("#fcc5c0", "#fa9fb5", "#f768a1", "#c51b8a", "#7a0177"), name="IMD quintile",
                      labels=c("Q1 (least deprived)", "Q2", "Q3", "Q4", "Q5 (most deprived)"))+
  scale_x_date(name="Date")+
  scale_y_continuous(name="Cumulative known cases per 100,000 population", trans="log10",
                     breaks=ratebreaks$breaks)+
  labs(title="Cumulative COVID-19 cases by deprivation quintile", subtitle="Deprivation estimated using mean IMD level across each UTLA",
       caption="Case data from PHE | IMD2019 data from DHCLG | Plot by @VictimOfMaths")
dev.off()

#Map of most cumulative case rates
#Read in shapefile of LA boundaries, which can be downloaded from:
#http://geoportal.statistics.gov.uk/datasets/counties-and-unitary-authorities-december-2017-full-clipped-boundaries-in-uk/data
shapefile <- st_read("Shapefiles/Counties_and_Unitary_Authorities_December_2017_Full_Clipped_Boundaries_in_UK.shp")
names(shapefile)[names(shapefile) == "ctyua17cd"] <- "code"

fulldata$caserate <- fulldata$cumul_cases*100000/fulldata$pop

#Duplicate data to account for shapefile using pre-2019 codes
int1 <- filter(fulldata, name=="Bournemouth, Christchurch and Poole")
int1$code <- "E06000028"
int2 <- filter(fulldata, name=="Bournemouth, Christchurch and Poole")
int2$code <- "E06000029"
int3 <- filter(fulldata, name=="Bournemouth, Christchurch and Poole")
int3$code <- "E10000009"
int4 <- filter(fulldata, name=="Cornwall and Isles of Scilly")
int4$code <- "E06000053"

temp <- rbind(fulldata, int1, int2, int3, int4)

#Bring in data
map.data <- full_join(shapefile, temp, by="code", all.y=TRUE)

#remove areas with no HLE data (i.e. Scotland, Wales & NI)
map.data <- subset(map.data, substr(code, 1, 1)=="E")

#Whole country
England <- ggplot(data=subset(map.data, date==max(date)), aes(fill=caserate, geometry=geometry))+
  geom_sf(colour="White")+
  xlim(10000,655644)+
  ylim(5337,700000)+
  theme_classic()+
  scale_fill_paletteer_c("pals::ocean.tempo", name="Total confirmed\ncases per 100,000")+
  theme(axis.line=element_blank(), axis.ticks=element_blank(), axis.text=element_blank(),
        axis.title=element_blank(),  plot.title=element_text(face="bold"))+
  labs(title="Confirmed COVID-19 case rates for Local Authorities in England",
       caption="Data from Public Health England | Plot by @VictimOfMaths")+
  geom_rect(aes(xmin=500000, xmax=560000, ymin=156000, ymax=200000), fill="transparent",
            colour="gray50")+
  geom_rect(aes(xmin=310000, xmax=405000, ymin=370000, ymax=430000), fill="transparent",
            colour="gray50")+
  geom_rect(aes(xmin=405000, xmax=490000, ymin=505000, ymax=580000), fill="transparent",
            colour="gray50")
#Add zoomed in areas
#London
London <- ggplot(data=subset(map.data, date==max(date)), aes(fill=caserate, geometry=geometry))+
  geom_sf(colour="White", show.legend=FALSE)+
  scale_x_continuous(limits=c(500000,560000), expand=c(0,0))+
  scale_y_continuous(limits=c(156000,200000), expand=c(0,0))+
  theme_classic()+
  scale_fill_paletteer_c("pals::ocean.tempo", name="Total confirmed\ncases per 100,000",
  )+
  labs(title="Greater London")+
  theme(axis.line=element_blank(), axis.ticks=element_blank(), axis.text=element_blank(),
        axis.title=element_blank())

#North-West England
NWEng <- ggplot(data=subset(map.data, date==max(date)), aes(fill=caserate, geometry=geometry))+
  geom_sf(colour="White", show.legend=FALSE)+
  scale_x_continuous(limits=c(310000,405000), expand=c(0,0))+
  scale_y_continuous(limits=c(370000,430000), expand=c(0,0))+
  theme_classic()+
  scale_fill_paletteer_c("pals::ocean.tempo", name="Total confirmed\ncases per 100,000")+
  labs(title="The North West")+
  theme(axis.line=element_blank(), axis.ticks=element_blank(), axis.text=element_blank(),
        axis.title=element_blank())

#Tyne/Tees  
NEEng <- ggplot(data=subset(map.data, date==max(date)), aes(fill=caserate, geometry=geometry))+
  geom_sf(colour="White", show.legend=FALSE)+
  scale_x_continuous(limits=c(405000,490000), expand=c(0,0))+
  scale_y_continuous(limits=c(505000,580000), expand=c(0,0))+
  theme_classic()+
  scale_fill_paletteer_c("pals::ocean.tempo", name="Total confirmed\ncases per 100,000")+
  labs(title="The North East")+
  theme(axis.line=element_blank(), axis.ticks=element_blank(), axis.text=element_blank(),
        axis.title=element_blank())

tiff("Outputs/COVIDMap.tiff", units="in", width=10, height=12, res=300)
ggdraw()+
  draw_plot(England)+
  draw_plot(London, 0.01,0.32,0.35,0.24)+
  draw_plot(NWEng, 0.01,0.62, 0.32, 0.24)+
  draw_plot(NEEng, 0.57, 0.67, 0.22, 0.22)
dev.off()
