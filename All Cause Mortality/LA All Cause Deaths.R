rm(list=ls())

library(tidyverse)
library(curl)
library(readxl)
library(ggtext)
library(paletteer)
library(lubridate)
library(forcats)
library(RcppRoll)

###################################################################################
#Weekly data

#Read in 2020 data for England
temp <- tempfile()
source <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fhealthandsocialcare%2fcausesofdeath%2fdatasets%2fdeathregistrationsandoccurrencesbylocalauthorityandhealthboard%2f2020/lahbtablesweek27.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
data20 <- read_excel(temp, sheet=6, col_names=FALSE)[-c(1:4),]
colnames(data20) <- c("code", "type", "name", "cause", "week", "location", "deaths.20")
data20 <- subset(data20, type=="Local Authority")[,-c(2)]

data20$deaths.20 <- as.numeric(data20$deaths.20)
data20$week <- as.numeric(data20$week)

maxweek.ew <- max(data20$week)
enddate.ew <- as.Date("2020-01-03")+weeks(maxweek.ew-1)

#Spread causes
data20 <- pivot_wider(data20, names_from="cause", values_from="deaths.20")

#Read in 2015-19 historic data for England & Wales
temp <- tempfile()
source <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/adhocs/11826fiveyearaverageweeklydeathsbylocalauthorityandplaceofoccurrenceenglandandwalesdeathsregistered2015to2019/weeklyfiveyearaveragesbylaandplaceofoccurrence20152019.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
data1519 <- read_excel(temp, sheet=2, col_names=FALSE)[-c(1:3),]
colnames(data1519) <- c("code", "name", "week", "location", "deaths.1519")

data1519$deaths.1519 <- as.numeric(data1519$deaths.1519)
data1519$week <- as.numeric(data1519$week)
data1519 <- data1519 %>% drop_na(name)

#Address merging of Aylesbury Vale, Chiltern and South Bucks into Bucks
data1519$name <- if_else(data1519$name %in% c("Aylesbury Vale", "Chiltern", "South Bucks", "Wycombe"), 
                         "Buckinghamshire", data1519$name)
data1519$code <- if_else(data1519$code %in% c("E07000004", "E07000005", "E07000006", "E07000007"), 
                         "E06000060", data1519$code)

data1519 <- data1519 %>% 
  group_by(week, location, name, code) %>% 
  summarise(deaths.1519=sum(deaths.1519)) %>% 
  ungroup()

data.ew <- merge(data1519, data20, all.x=TRUE)

#Combine Cornwall & Isles of Scilly
data.ew$code <- if_else(data.ew$code=="E06000053", "E06000052", data.ew$code)
data.ew$name <- if_else(data.ew$name=="Isles of Scilly", "Cornwall", data.ew$name)

#Combine Hackney & City of London
data.ew$code <- if_else(data.ew$code=="E09000001", "E09000012", data.ew$code)
data.ew$name <- if_else(data.ew$name=="City of London", "Hackney and City of London", data.ew$name)
data.ew$name <- if_else(data.ew$name=="Hackney", "Hackney and City of London", data.ew$name)

#Compress locations
data.ew$location <- case_when(
  data.ew$location %in% c("Elsewhere", "Home", "Hospice", "Other communal establishment") ~ "Home/Other",
  TRUE ~ data.ew$location)

data.ew <- data.ew %>% 
  group_by(code, name, location, week) %>% 
  summarise(deaths.1519=sum(deaths.1519), AllCause.20=sum(`All causes`), COVID.20=sum(`COVID 19`)) %>% 
  mutate(Other.20=AllCause.20-COVID.20) %>% 
  ungroup()

#Bring in Scottish deaths data (released by NRS on a Wednesday)
#2020 data

#Need to update link and range each week
temp <- tempfile()
source <- "https://www.nrscotland.gov.uk/files//statistics/covid19/weekly-deaths-by-date-council-area-location.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
data20.s <- read_excel(temp, sheet=2, range="A5:E3389", col_names=FALSE)
colnames(data20.s) <- c("week", "name", "location", "cause", "deaths")
data20.s$week <- as.numeric(data20.s$week)

maxweek.s <- max(data20.s$week)
enddate.s <- as.Date("2020-01-04")+weeks(maxweek.s-1)

data20.s$cause <- if_else(data20.s$cause=="Non-COVID-19", "Other.20", "COVID.20")

data20.s <- spread(data20.s, cause, deaths)
data20.s$COVID.20 <- replace_na(data20.s$COVID.20, 0)
data20.s$Other.20 <- replace_na(data20.s$Other.20, 0)

#2015-19 data
temp <- tempfile()
source <- "https://www.nrscotland.gov.uk/files//statistics/covid19/weekly-deaths-by-date-council-area-location-15-19.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
data1519.s <- read_excel(temp, sheet=2, range="A5:E25207", col_names=FALSE)
colnames(data1519.s) <- c("week", "name", "location", "year", "deaths")
data1519.s$week <- as.numeric(data1519.s$week)

#Take 5 year averages
data1519.s <- data1519.s %>% 
  group_by(week, name, location) %>% 
  summarise(deaths.1519=mean(deaths)) %>% 
  ungroup()

#Merge years
data.s <- merge(data1519.s, data20.s, all=TRUE)

data.s$deaths.1519 <- replace_na(data.s$deaths.1519)
data.s$COVID.20 <- if_else(is.na(data.s$COVID.20) & data.s$week<=maxweek.s, 0, data.s$COVID.20)
data.s$Other.20 <- if_else(is.na(data.s$Other.20) & data.s$week<=maxweek.s, 0, data.s$Other.20)

#Compress locations to match EW
data.s$location <- case_when(
  data.s$location=="Care Home" ~ "Care home",
  data.s$location %in% c("Home / Non-institution", "Other institution") ~ "Home/Other",
  TRUE ~ "Hospital"
)

data.s <- data.s %>% 
  group_by(week, name, location) %>% 
  summarise(deaths.1519=sum(deaths.1519, na.rm=TRUE), 
            across(c("COVID.20", "Other.20"), sum)) %>% 
  mutate(AllCause.20=COVID.20+Other.20) %>% 
  ungroup()

#Bring in Scottish LA codes
temp <- tempfile()
source <- "https://opendata.arcgis.com/datasets/35de30c6778b463a8305939216656132_0.csv"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
codelookup <- read.csv(temp)[,c(2,3)]
colnames(codelookup) <- c("code", "name")

data.s <- merge(data.s, codelookup, all.x=TRUE)

#Merge countries
data <- bind_rows(data.ew, data.s)

data$country <- case_when(
  substr(data$code,1,1)=="E" ~ "England",
  substr(data$code,1,1)=="W" ~ "Wales",
  substr(data$code,1,1)=="S" ~ "Scotland")

#Bring in LA populations
temp <- tempfile()
source <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fpopulationandmigration%2fpopulationestimates%2fdatasets%2fpopulationestimatesforukenglandandwalesscotlandandnorthernireland%2fmid20182019laboundaries/ukmidyearestimates20182019ladcodes.xls"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
LApop <- read_excel(temp, sheet="MYE2-All", range="A5:D423", col_names=TRUE)
colnames(LApop) <- c("code", "name", "geography", "pop")

#Merge isles of Scilly in with Cornwall
LApop$code <- if_else(LApop$code=="E06000053", "E06000052", LApop$code)
LApop$name <- if_else(LApop$name=="Isles of Scilly", "Cornwall", LApop$name)

#Address merging of Aylesbury Vale, Chiltern and South Bucks into Bucks
LApop$name <- if_else(LApop$name %in% c("Aylesbury Vale", "Chiltern", "South Bucks", "Wycombe"), 
                         "Buckinghamshire", LApop$name)
LApop$code <- if_else(LApop$code %in% c("E07000004", "E07000005", "E07000006", "E07000007"), 
                         "E06000060", LApop$code)

LApop <- LApop %>% 
  group_by(name, code) %>% 
  summarise(pop=sum(pop)) %>% 
  ungroup()

data <- merge(data, LApop, all.x=TRUE)

#Bring in Regions
temp <- tempfile()
source <- "https://opendata.arcgis.com/datasets/0c3a9643cc7c4015bb80751aad1d2594_0.csv"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
LADtoRegion <- read.csv(temp)[,c(1,4)]
colnames(LADtoRegion) <- c("code", "Region")

data <- merge(data, LADtoRegion,all.x=TRUE)

data$Region <- case_when(
  is.na(data$Region) & data$country=="Scotland" ~ "Scotland",
  is.na(data$Region) & data$country=="Wales" ~ "Wales",
  is.na(data$Region) & data$code %in% c("E06000058", "E06000059", "E07000246") ~ "South West",
  is.na(data$Region) & data$code %in% c("E07000244", "E07000245") ~ "East of England",
  is.na(data$Region) & data$code=="E06000060" ~ "South East",
  TRUE ~ as.character(data$Region))

#Generate national summaries
data.nat <- data %>% 
  group_by(week, country, location) %>% 
  summarise(across(c("deaths.1519", "AllCause.20", "COVID.20", "Other.20"), sum)) %>% 
  mutate(name=country, Region="Nation") %>% 
  ungroup()

data <- bind_rows(data, data.nat)

#Calculate excesses
data$allexcess <- case_when(
  data$country=="Scotland" & data$week<=maxweek.s ~ data$AllCause.20-data$deaths.1519,
  data$country!="Scotland" & data$week<=maxweek.ew ~ data$AllCause.20-data$deaths.1519)
data$excessrate <- data$allexcess*100000/data$pop
data$othexcess <- case_when(
  data$country=="Scotland" & data$week<=maxweek.s ~ data$Other.20-data$deaths.1519,
  data$country!="Scotland" & data$week<=maxweek.ew ~ data$Other.20-data$deaths.1519)
data$COVIDrate <- data$COVID.20*100000/data$pop

#############################################################
#Daily data

#Set up daily dataframe
#Bring in case data
#Read in cases data for England
temp <- tempfile()
source <- "https://coronavirus.data.gov.uk/downloads/csv/coronavirus-cases_latest.csv"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

casedata.E <- read.csv(temp)[,c(1:5)]
colnames(casedata.E) <- c("name", "code", "geography", "date", "cases")
casedata.E <- casedata.E %>% filter(geography=="Lower tier local authority")

mindate <- min(as.Date(casedata.E$date))
maxdate <- max(as.Date(casedata.E$date))

#Address merging of Aylesbury Vale, Chiltern and South Bucks into Bucks
casedata.E$name <- if_else(casedata.E$name %in% c("Aylesbury Vale", "Chiltern", "South Bucks", "Wycombe"), 
                      "Buckinghamshire", as.character(casedata.E$name))
casedata.E$code <- if_else(casedata.E$code %in% c("E07000004", "E07000005", "E07000006", "E07000007"), 
                      "E06000060", as.character(casedata.E$code))

casedata.E <- casedata.E %>% 
  group_by(name, code, date) %>% 
  summarise(cases=sum(cases)) %>% 
  ungroup()


#Set up skeleton dataframe, merging City of London and Hackney
daydata <- data.frame(code=rep(unique(subset(data, !name %in% c("England", "Scotland", "Wales"))$code),
                               each=maxdate-mindate+1),
                      name=rep(unique(subset(data, !name %in% c("England", "Scotland", "Wales"))$name),
                               each=maxdate-mindate+1),
                      date=rep(seq.Date(from=mindate, to=maxdate, by="day"), 
                               times=length(unique(subset(data, !name %in% c("England", "Scotland", "Wales"))$code))))

#merge in English cases
daydata <- merge(daydata, casedata.E, by=c("name", "code", "date"), all.x=TRUE)

#Bring in Welsh case data
temp <- tempfile()
source <- "http://www2.nphs.wales.nhs.uk:8080/CommunitySurveillanceDocs.nsf/3dc04669c9e1eaa880257062003b246b/77fdb9a33544aee88025855100300cab/$FILE/Rapid%20COVID-19%20surveillance%20data.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
casedata.W <- read_excel(temp, sheet=3)[,c(1:3)]

colnames(casedata.W) <- c("name", "date", "cases")

daydata <- merge(daydata, casedata.W, by=c("name", "date"), all.x=TRUE)

daydata$country <- case_when(
  substr(daydata$code,1,1)=="E" ~ "England",
  substr(daydata$code,1,1)=="W" ~ "Wales",
  substr(daydata$code,1,1)=="S" ~ "Scotland")

#Fill in blanks
daydata$cases <- coalesce(daydata$cases.x, daydata$cases.y)
daydata <- daydata[,-c(4:5)]
daydata$cases <- if_else(
  is.na(daydata$cases) & daydata$country!="Scotland", 0, daydata$cases)

#Experimental pillar 1 & 2 separation - England only and only up to end of June
#Archive files from 1st & 2nd July - either side of pillar 2 addition to data
#Available from https://coronavirus.data.gov.uk/archive
#Pillar 1 data
temp <- tempfile()
source <- "https://coronavirus.data.gov.uk/downloads/csv/dated/coronavirus-cases_202007011400.csv"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
p1data <- read.csv(temp)[,c(1:5)]
colnames(p1data) <- c("name", "code", "geography", "date", "p1cases")
p1data$date <- as.Date(p1data$date)
p1data <- subset(p1data, geography=="Lower tier local authority" & date<"2020-07-01")
p1data$code <- if_else(p1data$code %in% c("E09000001", "E09000012"), 
                       "E09000012", as.character(p1data$code))
p1data$code <- case_when(
  p1data$code %in% c("E09000001", "E09000012") ~ "E09000012",
  p1data$code %in% c("E07000004", "E07000005", "E07000006", "E07000007") ~ "E06000060",
  TRUE ~ p1data$code
)

p1data <- p1data %>% 
  group_by(code, date) %>% 
  summarise(p1cases=sum(p1cases)) %>% 
  ungroup()

#Pillar 1 & 2 combined
temp <- tempfile()
source <- "https://coronavirus.data.gov.uk/downloads/csv/dated/coronavirus-cases_202007021618.csv"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
p12data <- read.csv(temp)[,c(1:5)]
colnames(p12data) <- c("name", "code", "geography", "date", "p12cases")
p12data$date <- as.Date(p12data$date)
p12data <- subset(p12data, geography=="Lower tier local authority" & date<"2020-07-01")
p12data$code <- case_when(
  p1data$code %in% c("E09000001", "E09000012") ~ "E09000012",
  p1data$code %in% c("E07000004", "E07000005", "E07000006", "E07000007") ~ "E06000060",
  TRUE ~ p12data$code
)
p12data <- p12data %>% 
  group_by(code, date) %>% 
  summarise(p12cases=sum(p12cases)) %>% 
  ungroup()

daydata <- merge(daydata, p1data, by=c("date", "code"), all.x=TRUE)
daydata <- merge(daydata, p12data, by=c("date", "code"), all.x=TRUE)

daydata$p1cases <- if_else(is.na(daydata$p1cases) & daydata$country=="England" & daydata$date<as.Date("2020-07-01"),
                           0, daydata$p1cases)
daydata$p12cases <- if_else(is.na(daydata$p12cases) & daydata$country=="England" & daydata$date<as.Date("2020-07-01"), 
                            0, daydata$p12cases)

#Estimate Pillar 2 cases
daydata$p2cases <- daydata$p12cases-daydata$p1cases
daydata$p2cases <- if_else(daydata$p2cases<0, 0, daydata$p2cases)

daydata$date <- as.Date(daydata$date)

#National summary (E&W only)
daydata.nat <- daydata %>% 
  group_by(date, country) %>% 
  summarise(across(c("cases", "p1cases", "p12cases", "p2cases"), sum)) %>% 
  mutate(name=country) %>% 
  ungroup()

daydata <- bind_rows(daydata, daydata.nat)

daydata <- daydata %>% 
  group_by(name) %>% 
  arrange(date) %>% 
  mutate(casesroll_avg=roll_mean(cases, 7, align="right", fill=0)) %>% 
  ungroup()

daydata$date <- as.Date(daydata$date)

#Calculate weekly cases
daydata$week <- week(as.Date(daydata$date)-days(4))

daydata.week <- daydata %>% 
  group_by(name, week) %>% 
  summarise(cases=sum(cases), p1cases=sum(p1cases), p2cases=sum(p2cases)) %>% 
  ungroup()

data <- merge(data, daydata.week, all.x=TRUE)

#Calculate total excess deaths
excess.ew <- data %>% 
  filter(country!="Scotland" & week<=maxweek.ew) %>% 
  group_by(name) %>% 
  summarise(excess=sum(allexcess, na.rm=TRUE), hist=sum(deaths.1519), excessprop=excess/hist) %>% 
  ungroup()

excess.s <-  data %>% 
  filter(country=="Scotland" & week<=maxweek.s) %>% 
  group_by(name) %>% 
  summarise(excess=sum(allexcess, na.rm=TRUE), hist=sum(deaths.1519), excessprop=excess/hist) %>% 
  ungroup()

excess <- bind_rows(excess.ew, excess.s)

#Save master data
write.csv(data, "COVID_LA_Plots/LAExcess.csv")
write.csv(excess, "COVID_LA_Plots/LAExcessSummary.csv")
write.csv(daydata, "COVID_LA_Plots/LACases.csv")

####################################################################

data <- read.csv("COVID_LA_Plots/LAExcess.csv")
excess <- read.csv("COVID_LA_Plots/LAExcessSummary.csv")
daydata <- read.csv("COVID_LA_Plots/LACases.csv")

###################
#LA-specific plots#
###################

LA <- "Sheffield"

LAdata <- data %>% filter(name==LA) 
LAexcess <- excess %>% filter(name==LA) 

enddate <- if_else(LAdata$country[1]=="Scotland", enddate.s, enddate.ew)
source <- if_else(LAdata$country[1]=="Scotland", "NRS", "ONS")

maxweek <- week(enddate)

labpos <- max(sum(LAdata$AllCause.20[LAdata$week==maxweek]),
              sum(LAdata$deaths.1519[LAdata$week==maxweek]))

lab <- if_else(LAexcess[2]<0, 
               paste0(round(LAexcess[2],0), " (",round(LAexcess[4]*100,0), "%) deaths in 2020\ncompared to the average in 2015-19"),
               paste0("+", round(LAexcess[2],0), " (+",round(LAexcess[4]*100,0), "%) deaths in 2020\ncompared to the average in 2015-19"))

#Excess deaths graph
png("Outputs/COVID_LA_Plots_1.png", units="in", width=8, height=6, res=500)
LAdata %>% 
  group_by(week) %>% 
  summarise(deaths.1519=sum(deaths.1519), AllCause.20=sum(AllCause.20)) %>% 
ggplot()+
  geom_line(aes(x=week, y=deaths.1519), colour="skyblue4")+
  geom_line(aes(x=week, y=AllCause.20), colour="red")+
  scale_x_continuous(name="Week")+
  scale_y_continuous(name="Deaths", limits=c(0,NA))+
  theme_classic()+
  theme(plot.subtitle=element_markdown())+
  annotate("text", x=week(enddate)-2, y=max(labpos*1.5, labpos+20), 
           label=lab,
           hjust=0, colour="red", size=3)+
  labs(title=paste0("Excess deaths in ", LA, " during the pandemic"),
       subtitle=paste0("Weekly deaths in <span style='color:red;'>2020</span> compared to <span style='color:Skyblue4;'>the average in 2015-19</span> by date of occurence<br>Data up to ", enddate, ". Data for recent weeks is likely to be an undercount due to deaths<br>not yet having been fully processed."),
       caption=paste0("Data from ", source," | Plot by @VictimOfMaths"))
dev.off()

#Excess deaths by cause
png("Outputs/COVID_LA_Plots_2.png", units="in", width=8, height=6, res=500)
LAdata %>% 
  gather(cause, excess, c(7,14)) %>% 
  group_by(week, cause) %>% 
  summarise(excess=sum(excess)) %>% 
  mutate(cause=fct_relevel(cause, "COVID.20")) %>% 
ggplot(aes(x=week, y=excess, fill=cause))+
  geom_bar(stat="identity")+
  geom_segment(aes(x=0.5, xend=maxweek+0.5, y=0, yend=0), colour="Grey30")+
  scale_x_continuous(name="Week")+
  scale_y_continuous(name="Excess deaths vs. 2015-19 average")+
  scale_fill_paletteer_d("LaCroixColoR::PinaFraise", name="Cause", labels=c("COVID-19", "Other causes"))+
  theme_classic()+
  labs(title=paste0("Excess deaths in ", LA, " during the pandemic"),
       subtitle=paste0("Excess deaths by date of occurence in 2020 vs. 2015-19 average by cause.\nData up to ", enddate, ". Data for recent weeks is likely to be an undercount due to deaths\nnot yet having been fully processed."),
       caption=paste0("Data from ", source," | Plot by @VictimOfMaths"))
dev.off()

#Excess deaths by location
png("Outputs/COVID_LA_Plots_3.png", units="in", width=8, height=6, res=500)
ggplot(LAdata, aes(x=week, y=allexcess, fill=location))+
  geom_col()+
  geom_segment(aes(x=0.5, xend=maxweek+0.5, y=0, yend=0), colour="Grey30")+
  scale_x_continuous(name="Week")+
  scale_y_continuous(name="Excess deaths vs. 2015-19 average")+
  scale_fill_paletteer_d("ggsci::planetexpress_futurama", name="Place of death")+
  theme_classic()+
  labs(title=paste0("Excess deaths in ", LA, " during the pandemic"),
       subtitle=paste0("Excess deaths by occurence in 2020 vs. 2015-19 average by location.\nData up to ", enddate, ". Data for recent weeks is likely to be an undercount due to deaths\nnot yet having been fully processed."),
       caption=paste0("Data from ", source," | Plot by @VictimOfMaths"))
dev.off()

#Cases vs. deaths
png("Outputs/COVID_LA_Plots_4.png", units="in", width=8, height=6, res=500)
LAdata %>% 
  group_by(week) %>% 
  summarise(excess=sum(COVID.20), cases=unique(cases)) %>% 
ggplot()+
  geom_segment(aes(x=0.5, xend=maxweek+0.5, y=0, yend=0), colour="Grey30")+
  geom_line(aes(x=week, y=cases), colour="#B25D91")+
  geom_line(aes(x=week, y=excess), colour="#1BB6AF")+
  scale_x_continuous(name="Week", limits=c(0,maxweek+1))+
  scale_y_continuous(name="")+
  theme_classic()+
  theme(plot.subtitle=element_markdown())+
  labs(title=paste0("Timeline of COVID-19 in ", LA),
       subtitle=paste0("Confirmed new COVID-19 <span style='color:#B25D91;'>cases</span> compared to confirmed COVID-19 <span style='color:#1BB6AF;'>deaths</span> by week of occurence.<br>Data up to ", enddate),
       caption=paste0("Data from ", source," | Plot by @VictimOfMaths"))
dev.off()

#cases plot
#England & Wales only
tiff(paste0("Outputs/COVIDNewCases", LA, ".tiff"), units="in", width=8, height=6, res=500)
#png("Outputs/COVID_LA_Plots_5.png", units="in", width=8, height=6, res=500)
daydata %>% 
  filter(name==LA) %>% 
ggplot()+
  geom_col(aes(x=date, y=cases), fill="skyblue2")+
  geom_line(aes(x=date, y=casesroll_avg), colour="red")+
  scale_x_date(name="Date")+
  scale_y_continuous(name="Daily confirmed new cases")+
  theme_classic()+
  theme(plot.subtitle=element_markdown())+
  labs(title=paste0("Confirmed new COVID cases in ",LA),
       subtitle="Confirmed new COVID-19 cases identified through combined pillar 1 & 2 testing<br>and the <span style='color:Red;'>7-day rolling average",
       caption="Data from PHE | Plot by @VictimOfMaths")
dev.off()

#Experimental pillar 1 vs. 2 tests numbers
#ENGLAND ONLY
#tiff(paste0("Outputs/COVIDNewCasesPillars", LA, ".tiff"), units="in", width=8, height=6, res=500)
png("Outputs/COVID_LA_Plots_6.png", units="in", width=8, height=6, res=500)
daydata %>% 
  filter(name==LA) %>% 
ggplot()+
  geom_line(aes(x=date, y=p1cases), colour="#FF4E86")+
  geom_line(aes(x=date, y=p2cases), colour="#FF9E44")+
  geom_line(aes(x=date, y=casesroll_avg), colour="navyblue")+
  scale_x_date(name="Date")+
  scale_y_continuous(name="Daily confirmed new cases")+
  theme_classic()+
  theme(plot.subtitle=element_markdown())+
  labs(title=paste0("Confirmed new COVID cases in ",LA),
       subtitle="Confirmed new COVID-19 cases identified through <span style='color:#FF4E86;'>Pillar 1</span> and <span style='color:#FF9E44;'>Pillar 2</span> testing and the <span style='color:navyblue;'>7-day rolling average</span>.<br>PHE changed their methodology on 1st July and so pillar-specific data is not available since then.<br>Rolling average based on new approach.<br>Pillar-specific figures are estimated from the old approach and may be subject to some double-counting",
       caption="Data from PHE | Plot by @VictimOfMaths")
dev.off()

#################################################
#Analysis of cumulative excess deaths
