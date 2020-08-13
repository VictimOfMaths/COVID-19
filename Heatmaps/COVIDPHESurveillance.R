rm(list=ls())

library(tidyverse)
library(curl)
library(readxl)
library(ggtext)

#Surveillance reports taken from:
#https://www.gov.uk/government/publications/national-covid-19-surveillance-reports

#Read in latest data
temp <- tempfile()
source <- "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/907704/Weekly_COVID19_report_data_w32.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

week31 <- read_excel(temp, sheet="Figure 2. Age sex pyramids", range="B10:D19", , col_names=FALSE)
colnames(week31) <- c("Age", "Male", "Female")
week31 <- gather(week31, key=Sex, value=Week31, c(2:3))

#Week 30
temp <- tempfile()
source <- "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/905854/Weekly_COVID19_report_data_w31.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

week30 <- read_excel(temp, sheet="Figure 2. Age sex pyramids", range="B10:D19", , col_names=FALSE)
colnames(week30) <- c("Age", "Male", "Female")
week30 <- gather(week30, key=Sex, value=Week30, c(2:3))

#Week 29
temp <- tempfile()
source <- "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/903436/Weekly_COVID19_report_data_w30.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

week29 <- read_excel(temp, sheet="Figure 2. Age sex pyramids", range="B10:D19", , col_names=FALSE)
colnames(week29) <- c("Age", "Male", "Female")
week29 <- gather(week29, key=Sex, value=Week29, c(2:3))

#Week 28
temp <- tempfile()
source <- "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/901804/Weekly_COVID19_report_data_w29.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

week28 <- read_excel(temp, sheet="Figure 2. Age sex pyramids", range="B10:D19", , col_names=FALSE)
colnames(week28) <- c("Age", "Male", "Female")
week28 <- gather(week28, key=Sex, value=Week28, c(2:3))

#Week 27
temp <- tempfile()
source <- "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/899255/Weekly_COVID19_report_data_w28.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

week27 <- read_excel(temp, sheet="Figure 2. Age sex pyramids", range="B10:D19", , col_names=FALSE)
colnames(week27) <- c("Age", "Male", "Female")
week27 <- gather(week27, key=Sex, value=Week27, c(2:3))

#Week 26
temp <- tempfile()
source <- "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/897200/Weekly_COVID19_report_data_w27.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

week26 <- read_excel(temp, sheet="Figure 2. Age sex pyramids", range="B10:D19", , col_names=FALSE)
colnames(week26) <- c("Age", "Male", "Female")
week26 <- gather(week26, key=Sex, value=Week26, c(2:3))

#Week 25
temp <- tempfile()
source <- "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/896777/Weekly_COVID19_report_data_current.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

week25 <- read_excel(temp, sheet="Figure 2. Age sex pyramids", range="B10:D19", , col_names=FALSE)
colnames(week25) <- c("Age", "Male", "Female")
week25 <- gather(week25, key=Sex, value=Week25, c(2:3))

#Join them together
data <- merge(week30, week31)
data <- merge(data, week29)
data <- merge(data, week28)
data <- merge(data, week27)
data <- merge(data, week26)
data <- merge(data, week25)

#Calculate weekly counts
data$w31 <- data$Week31-data$Week30
data$w30 <- data$Week30-data$Week29
data$w29 <- data$Week29-data$Week28
data$w28 <- data$Week28-data$Week27
data$w27 <- data$Week27-data$Week26
data$w26 <- data$Week26-data$Week25

data$Age <- factor(data$Age, levels=c("<5 years", "5-9 years", "10-19 years", "20-29 years", "30-39 years",
                                      "40-49 years", "50-59 years", "60-69 years", "70-79 years", "80+ years"))

#Data isn't available prior to week 25, only the pdfs of the surveillance reports
#Also, data before week 29 is pillar 1 only, week 29 includes all historic pillar 2 
#cases and week 30+ is both, so comparability is tricky.

#So, this replicates the Figures from the early surveillance reports to visually
#illustrate the difference. Oldest available report is here (AFAIK) 
#https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/880925/COVID19_Epidemiological_Summary_w17.pdf

#Plot most recent week
tiff("Outputs/COVIDNewCasesxAgexSex.tiff", units="in", width=8, height=6, res=500)
ggplot()+
  geom_col(data=subset(data, Sex=="Male"), aes(x=-w31, y=Age), fill="#98002d")+
  geom_col(data=subset(data, Sex=="Female"), aes(x=w31, y=Age), fill="#00af9f")+
  geom_segment(aes(x=0, xend=0, y=0.5, yend=10.5))+
  scale_x_continuous(breaks=c(-600, -300, 0, 300, 600), 
                     labels=c("600", "300", "0", "300", "600"),
                     name="Confirmed new COVID-19 cases")+
  theme_classic()+
  labs(title="New COVID-19 cases are younger than they were",
       subtitle="Age breakdown of new confirmed cases in week 31 for <span style='color:#98002d;'>men</span> and <span style='color:#00af9f;'>women</span>",
       caption="Data from PHE | Plot by @VictimOfMaths")+
  theme(plot.subtitle=element_markdown())
dev.off()




