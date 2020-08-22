rm(list=ls())

library(tidyverse)
library(curl)
library(readxl)
library(ggtext)
library(ggridges)
library(paletteer)

#Surveillance reports taken from:
#https://www.gov.uk/government/publications/national-covid-19-surveillance-reports

#Read in latest data
temp <- tempfile()
source <- "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/910970/Weekly_COVID19_report_data_w34.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

week33 <- read_excel(temp, sheet="Figure 2. Age sex pyramids", range="B10:D19", col_names=FALSE)
colnames(week33) <- c("Age", "Male", "Female")
week33 <- gather(week33, key=Sex, value=Week33, c(2:3))

case.demog <- read_excel(temp, sheet="Figure 4. Case rates by agegrp", range="B10:I38", col_names=FALSE)
colnames(case.demog) <- c("Week", "0-4", "5-14", "15-44", "45-64", "65-74", "75-84", "85+")
case.demog <- gather(case.demog, age, caserate, c(2:8))

#Week 32
temp <- tempfile()
source <- "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/909421/Weekly_COVID19_report_data_w33.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

week32 <- read_excel(temp, sheet="Figure 2. Age sex pyramids", range="B10:D19", col_names=FALSE)
colnames(week32) <- c("Age", "Male", "Female")
week32 <- gather(week32, key=Sex, value=Week32, c(2:3))

#Week 31
temp <- tempfile()
source <- "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/907704/Weekly_COVID19_report_data_w32.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

week31 <- read_excel(temp, sheet="Figure 2. Age sex pyramids", range="B10:D19", col_names=FALSE)
colnames(week31) <- c("Age", "Male", "Female")
week31 <- gather(week31, key=Sex, value=Week31, c(2:3))

#Week 30
temp <- tempfile()
source <- "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/905854/Weekly_COVID19_report_data_w31.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

week30 <- read_excel(temp, sheet="Figure 2. Age sex pyramids", range="B10:D19", col_names=FALSE)
colnames(week30) <- c("Age", "Male", "Female")
week30 <- gather(week30, key=Sex, value=Week30, c(2:3))

#Week 29
temp <- tempfile()
source <- "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/903436/Weekly_COVID19_report_data_w30.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

week29 <- read_excel(temp, sheet="Figure 2. Age sex pyramids", range="B10:D19", col_names=FALSE)
colnames(week29) <- c("Age", "Male", "Female")
week29 <- gather(week29, key=Sex, value=Week29, c(2:3))

#Week 28
temp <- tempfile()
source <- "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/901804/Weekly_COVID19_report_data_w29.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

week28 <- read_excel(temp, sheet="Figure 2. Age sex pyramids", range="B10:D19", col_names=FALSE)
colnames(week28) <- c("Age", "Male", "Female")
week28 <- gather(week28, key=Sex, value=Week28, c(2:3))

#Week 27
temp <- tempfile()
source <- "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/899255/Weekly_COVID19_report_data_w28.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

week27 <- read_excel(temp, sheet="Figure 2. Age sex pyramids", range="B10:D19", col_names=FALSE)
colnames(week27) <- c("Age", "Male", "Female")
week27 <- gather(week27, key=Sex, value=Week27, c(2:3))

#Week 26
temp <- tempfile()
source <- "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/897200/Weekly_COVID19_report_data_w27.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

week26 <- read_excel(temp, sheet="Figure 2. Age sex pyramids", range="B10:D19", col_names=FALSE)
colnames(week26) <- c("Age", "Male", "Female")
week26 <- gather(week26, key=Sex, value=Week26, c(2:3))

#Week 25
temp <- tempfile()
source <- "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/896777/Weekly_COVID19_report_data_current.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

week25 <- read_excel(temp, sheet="Figure 2. Age sex pyramids", range="B10:D19", col_names=FALSE)
colnames(week25) <- c("Age", "Male", "Female")
week25 <- gather(week25, key=Sex, value=Week25, c(2:3))

#Join them together
data <- merge(week32, week33)
data <- merge(data, week31)
data <- merge(data, week30)
data <- merge(data, week29)
data <- merge(data, week28)
data <- merge(data, week27)
data <- merge(data, week26)
data <- merge(data, week25)

#Calculate weekly counts
data$w33 <- data$Week33-data$Week32
data$w32 <- data$Week32-data$Week31
data$w31 <- data$Week31-data$Week30
data$w30 <- data$Week30-data$Week29
data$w29 <- data$Week29-data$Week28
data$w28 <- data$Week28-data$Week27
data$w27 <- data$Week27-data$Week26
data$w26 <- data$Week26-data$Week25

data$Age <- factor(data$Age, levels=c("<5 years", "5-9 years", "10-19 years", "20-29 years", "30-39 years",
                                      "40-49 years", "50-59 years", "60-69 years", "70-79 years", "80+ years"))

case.demog$age <- factor(case.demog$age, levels=c("0-4", "5-14", "15-44", "45-64", "65-74", "75-84", "85+"))

#Data isn't available prior to week 25, only the pdfs of the surveillance reports
#Also, data before week 29 is pillar 1 only, week 29 includes all historic pillar 2 
#cases and week 30+ is both, so comparability is tricky.

#So, this replicates the Figures from the early surveillance reports to visually
#illustrate the difference. Oldest available report is here (AFAIK) 
#https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/880925/COVID19_Epidemiological_Summary_w17.pdf

#Plot most recent week
tiff("Outputs/COVIDNewCasesxAgexSex.tiff", units="in", width=8, height=6, res=500)
ggplot()+
  geom_col(data=subset(data, Sex=="Male"), aes(x=-w33, y=Age), fill="#98002d")+
  geom_col(data=subset(data, Sex=="Female"), aes(x=w33, y=Age), fill="#00af9f")+
  geom_segment(aes(x=0, xend=0, y=0.5, yend=10.5))+
  scale_x_continuous(breaks=c(-600, -300, 0, 300, 600), 
                     labels=c("600", "300", "0", "300", "600"),
                     name="Confirmed new COVID-19 cases")+
  theme_classic()+
  labs(title="New COVID-19 cases are younger than they were",
       subtitle="Age breakdown of new confirmed cases in week 33 for <span style='color:#98002d;'>men</span> and <span style='color:#00af9f;'>women</span>",
       caption="Data from PHE | Plot by @VictimOfMaths")+
  theme(plot.subtitle=element_markdown())
dev.off()

#Bring in populations to back numbers out of the rates
#Bring in LA populations
temp <- tempfile()
source <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fpopulationandmigration%2fpopulationestimates%2fdatasets%2fpopulationestimatesforukenglandandwalesscotlandandnorthernireland%2fmid20182019laboundaries/ukmidyearestimates20182019ladcodes.xls"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
pop <- as.data.frame(t(read_excel(temp, sheet="MYE2-All", range="E9:CQ9", col_names=FALSE)))
pop$age <- c(0:90)
pop$ageband1 <- case_when(
  pop$age<5 ~ "<5 years",
  pop$age<10 ~ "5-9 years",
  pop$age<20 ~ "10-19 years",
  pop$age<30 ~ "20-29 years",
  pop$age<40 ~ "30-39 years",
  pop$age<50 ~ "40-49 years",
  pop$age<60 ~ "50-59 years",
  pop$age<70 ~ "60-69 years",
  pop$age<80 ~ "70-79 years",
  TRUE ~ "80+ years"
)

pop$ageband2 <- case_when(
  pop$age<5 ~ "0-4",
  pop$age<15 ~ "5-14",
  pop$age<45 ~ "15-44",
  pop$age<65 ~ "45-64",
  pop$age<75 ~ "65-74",
  pop$age<85 ~ "75-84",
  TRUE ~ "85+"
)

pop1 <- pop %>% 
  group_by(ageband1) %>% 
  summarise(pop=sum(V1))

data <- merge(data, pop1, by.x="Age", by.y="ageband1", all.x=TRUE)

pop2 <- pop %>% 
  group_by(ageband2) %>% 
  summarise(pop=sum(V1))

case.demog <- merge(case.demog, pop2, by.x="age", by.y="ageband2", all.x=TRUE)

case.demog$cases <- case.demog$caserate*case.demog$pop/100000

#Heatmaps of cases by age: rates
tiff("Outputs/COVIDNewCasesHeatmap.tiff", units="in", width=13, height=4, res=500)
ggplot(case.demog)+
  geom_tile(aes(x=Week, y=age, fill=caserate))+
  scale_y_discrete(name="Age")+
  scale_fill_paletteer_c("viridis::magma", name="Cases per 100,000")+ 
  theme_classic()+
  labs(title="New COVID-19 cases by age during the pandemic",
       caption="Data from Public Health England | Plot by @VictimOfMaths")
dev.off()

#Absolute numbers
tiff("Outputs/COVIDNewCasesHeatmapAbs.tiff", units="in", width=13, height=4, res=500)
ggplot(case.demog)+
  geom_tile(aes(x=Week, y=age, fill=cases))+
  scale_y_discrete(name="Age")+
  scale_fill_paletteer_c("viridis::magma", name="Confirmed cases")+ 
  theme_classic()+
  labs(title="COVID-19 cases are currently concentrated in working ages",
    subtitle="New confirmed COVID-19 cases by age in England",
       caption="Data from Public Health England | Plot by @VictimOfMaths")
dev.off()

#Streamgraph
library(streamgraph) #devtools::install_github("hrbrmstr/streamgraph")
library(lubridate)
library(htmlwidgets)

case.demog$date <- as.Date("2020-01-01")+weeks(case.demog$Week-1)

streamgraph <- case.demog %>% 
  mutate(cases=round(cases, 0)) %>% 
  streamgraph("age", "cases", "date") %>% 
  sg_fill_manual(values=palettes_d$awtools$a_palette[c(1:7)]) %>% 
  sg_annotate(label="The changing age distribution of new COVID-19 cases in England",
              x=as.Date("2020-01-20"), y=28000, size=16) %>% 
  sg_annotate(label="Data from PHE | Plot by @VictimOfMaths", x=as.Date("2020-06-20"),
              y=0) 

saveWidget(streamgraph, "COVIDAgeStreamgraph.html")

#This is now published here: https://victimofmaths.github.io/COVID-Streamgraph/COVIDAgeStreamgraph.html
