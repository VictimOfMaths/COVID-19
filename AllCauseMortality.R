rm(list=ls())

library(tidyverse)
library(paletteer)
library(curl)
library(readxl)
library(lubridate)
library(forcats)

#A gold star to anyone who can make the range updates for the 3 different Excel files for E&W, Scotland & NI automatic.

####################################
#Read in English & Welsh data first#
####################################

#The ONS published weekly all-cause deaths data into a new Excel file each week on a Tuesday,
#So need to manually update the 2020 link below and the corresponding range selection

#Start with 2020 - data up to 17th April, updated on 28th April
temp <- tempfile()
source <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fbirthsdeathsandmarriages%2fdeaths%2fdatasets%2fweeklyprovisionalfiguresondeathsregisteredinenglandandwales%2f2020/publishedweek1620201.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

#By age
data2020.age.EW <- read_excel(temp, sheet="Weekly figures 2020", range="B22:R41", col_names=FALSE)
colnames(data2020.age.EW) <- c("Age", format(seq.Date(from=as.Date("2020-01-03"), by="7 days", 
                                                      length.out=ncol(data2020.age.EW)-1), "%d/%m/%y"))

#Compress age bands to match earlier years
data2020.age.EW$age <- case_when(
  data2020.age.EW$Age=="<1" ~ "Under 1 year",
  data2020.age.EW$Age %in% c("1-4", "5-9", "10-14") ~ "01-14",
  data2020.age.EW$Age %in% c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44") ~ "15-44",
  data2020.age.EW$Age %in% c("45-49", "50-54", "55-59", "60-64") ~ "45-64",
  data2020.age.EW$Age %in% c("65-69", "70-74") ~ "65-74",
  data2020.age.EW$Age %in% c("75-79", "80-84") ~ "75-84",
  TRUE ~ "85+"
)

data2020.age.EW <- data2020.age.EW %>%
  group_by(age) %>%
  summarise_at(c(2:(ncol(data2020.age.EW)-1)), sum)

data2020.age.EW$age <- factor(data2020.age.EW$age, levels=c("Under 1 year", "01-14", "15-44", "45-64", 
                                                            "65-74", "75-84", "85+"))
data2020.age.EW <- arrange(data2020.age.EW, age)

#By sex
data2020.male.EW <- read_excel(temp, sheet="Weekly figures 2020", range="B44:R63", col_names=FALSE)
colnames(data2020.male.EW) <- c("Age", format(seq.Date(from=as.Date("2020-01-03"), by="7 days", 
                                                       length.out=ncol(data2020.male.EW)-1), "%d/%m/%y"))
data2020.male.EW$sex <- "Male"

data2020.female.EW <- read_excel(temp, sheet="Weekly figures 2020", range="B66:R85", col_names=FALSE)
colnames(data2020.female.EW) <- c("Age", format(seq.Date(from=as.Date("2020-01-03"), by="7 days", 
                                                         length.out=ncol(data2020.female.EW)-1), "%d/%m/%y"))
data2020.female.EW$sex <- "Female"

data2020.sex.EW <- bind_rows(data2020.male.EW, data2020.female.EW)

#Compress age bands to match earlier years
data2020.sex.EW$age <- case_when(
  data2020.sex.EW$Age=="<1" ~ "Under 1 year",
  data2020.sex.EW$Age %in% c("1-4", "5-9", "10-14") ~ "01-14",
  data2020.sex.EW$Age %in% c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44") ~ "15-44",
  data2020.sex.EW$Age %in% c("45-49", "50-54", "55-59", "60-64") ~ "45-64",
  data2020.sex.EW$Age %in% c("65-69", "70-74") ~ "65-74",
  data2020.sex.EW$Age %in% c("75-79", "80-84") ~ "75-84",
  TRUE ~ "85+"
)

data2020.sex.EW <- data2020.sex.EW %>%
  group_by(age, sex) %>%
  summarise_at(c(2:(ncol(data2020.sex.EW)-2)), sum)

data2020.sex.EW$age <- factor(data2020.sex.EW$age, levels=c("Under 1 year", "01-14", "15-44", "45-64", 
                                                            "65-74", "75-84", "85+"))
data2020.sex.EW$sex <- factor(data2020.sex.EW$sex, levels=c("Male", "Female"))
data2020.sex.EW <- arrange(data2020.sex.EW, age, sex)

#Add total rows
data2020.sex.EW <- data2020.sex.EW %>%
  group_by(sex) %>%
  summarise_at(c(2:(ncol(data2020.sex.EW)-1)), sum) %>%
  mutate(age="Total") %>%
  bind_rows(data2020.sex.EW)

#By region
data2020.reg.EW <- read_excel(temp, sheet="Weekly figures 2020", range="B87:R96", col_names=FALSE)
colnames(data2020.reg.EW) <- c("reg", format(seq.Date(from=as.Date("2020-01-03"), by="7 days", 
                                                      length.out=ncol(data2020.reg.EW)-1), "%d/%m/%y"))

#2019 data
temp <- tempfile()
source <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fbirthsdeathsandmarriages%2fdeaths%2fdatasets%2fweeklyprovisionalfiguresondeathsregisteredinenglandandwales%2f2019/publishedweek522019.xls"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

#By age
data2019.age.EW <- read_excel(temp, sheet="Weekly figures 2019", range="B16:BB22", col_names=FALSE)
colnames(data2019.age.EW) <- c("age", format(seq.Date(from=as.Date("2019-01-04"), by="7 days", 
                                                      length.out=ncol(data2019.age.EW)-1), "%d/%m/%y"))
data2019.age.EW$age <- factor(data2019.age.EW$age, levels=c("Under 1 year", "01-14", "15-44", "45-64", "65-74", 
                                                            "75-84", "85+"))
data2019.age.EW <- arrange(data2019.age.EW, age)

#By sex
data2019.male.EW <- read_excel(temp, sheet="Weekly figures 2019", range="B25:BB31", col_names=FALSE)
colnames(data2019.male.EW) <- c("age", format(seq.Date(from=as.Date("2019-01-04"), by="7 days", 
                                                       length.out=ncol(data2019.male.EW)-1), "%d/%m/%y"))
data2019.male.EW$sex <- "Male"

data2019.female.EW <- read_excel(temp, sheet="Weekly figures 2019", range="B34:BB40", col_names=FALSE)
colnames(data2019.female.EW) <- c("age", format(seq.Date(from=as.Date("2019-01-04"), by="7 days", 
                                                         length.out=ncol(data2019.female.EW)-1), "%d/%m/%y"))
data2019.female.EW$sex <- "Female"

data2019.sex.EW <- bind_rows(data2019.male.EW, data2019.female.EW)
data2019.sex.EW$age <- factor(data2019.sex.EW$age, levels=c("Under 1 year", "01-14", "15-44", "45-64", 
                                                            "65-74", "75-84", "85+"))
data2019.sex.EW$sex <- factor(data2019.sex.EW$sex, levels=c("Male", "Female"))
data2019.sex.EW <- arrange(data2019.sex.EW, age, sex)

#Add total rows
data2019.sex.EW <- data2019.sex.EW %>%
  group_by(sex) %>%
  summarise_at(c(2:(ncol(data2019.sex.EW)-1)), sum) %>%
  mutate(age="Total") %>%
  bind_rows(data2019.sex.EW)

#By region
data2019.reg.EW <- read_excel(temp, sheet="Weekly figures 2019", range="B43:BB52", col_names=FALSE)
colnames(data2019.reg.EW) <- c("reg", format(seq.Date(from=as.Date("2019-01-04"), by="7 days", 
                                                      length.out=ncol(data2019.reg.EW)-1), "%d/%m/%y"))

#2018 data
temp <- tempfile()
source <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fbirthsdeathsandmarriages%2fdeaths%2fdatasets%2fweeklyprovisionalfiguresondeathsregisteredinenglandandwales%2f2018/publishedweek522018withupdatedrespiratoryrow.xls"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

#By age
data2018.age.EW <- read_excel(temp, sheet="Weekly figures 2018", range="B16:BB22", col_names=FALSE)
colnames(data2018.age.EW) <- c("age", format(seq.Date(from=as.Date("2018-01-05"), by="7 days", 
                                                      length.out=ncol(data2018.age.EW)-1), "%d/%m/%y"))
data2018.age.EW$age <- factor(data2018.age.EW$age, levels=c("Under 1 year", "01-14", "15-44", "45-64", "65-74", 
                                                            "75-84", "85+"))
data2018.age.EW <- arrange(data2018.age.EW, age)

#By sex
data2018.male.EW <- read_excel(temp, sheet="Weekly figures 2018", range="B25:BB31", col_names=FALSE)
colnames(data2018.male.EW) <- c("age", format(seq.Date(from=as.Date("2018-01-05"), by="7 days", 
                                                       length.out=ncol(data2018.male.EW)-1), "%d/%m/%y"))
data2018.male.EW$sex <- "Male"

data2018.female.EW <- read_excel(temp, sheet="Weekly figures 2018", range="B34:BB40", col_names=FALSE)
colnames(data2018.female.EW) <- c("age", format(seq.Date(from=as.Date("2018-01-05"), by="7 days", 
                                                         length.out=ncol(data2018.female.EW)-1), "%d/%m/%y"))
data2018.female.EW$sex <- "Female"

data2018.sex.EW <- bind_rows(data2018.male.EW, data2018.female.EW)
data2018.sex.EW$age <- factor(data2018.sex.EW$age, levels=c("Under 1 year", "01-14", "15-44", "45-64", 
                                                            "65-74", "75-84", "85+"))
data2018.sex.EW$sex <- factor(data2018.sex.EW$sex, levels=c("Male", "Female"))
data2018.sex.EW <- arrange(data2018.sex.EW, age, sex)

#Add total rows
data2018.sex.EW <- data2018.sex.EW %>%
  group_by(sex) %>%
  summarise_at(c(2:(ncol(data2018.sex.EW)-1)), sum) %>%
  mutate(age="Total") %>%
  bind_rows(data2018.sex.EW)

#By region
data2018.reg.EW <- read_excel(temp, sheet="Weekly figures 2018", range="B43:BB52", col_names=FALSE)
colnames(data2018.reg.EW) <- c("reg", format(seq.Date(from=as.Date("2018-01-05"), by="7 days", 
                                                      length.out=ncol(data2018.reg.EW)-1), "%d/%m/%y"))

#2017 data
temp <- tempfile()
source <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fbirthsdeathsandmarriages%2fdeaths%2fdatasets%2fweeklyprovisionalfiguresondeathsregisteredinenglandandwales%2f2017/publishedweek522017.xls"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

#By age
data2017.age.EW <- read_excel(temp, sheet="Weekly figures 2017", range="B16:BB22", col_names=FALSE)
colnames(data2017.age.EW) <- c("age", format(seq.Date(from=as.Date("2017-01-06"), by="7 days", 
                                                      length.out=ncol(data2017.age.EW)-1), "%d/%m/%y"))
data2017.age.EW$age <- factor(data2017.age.EW$age, levels=c("Under 1 year", "01-14", "15-44", "45-64", "65-74", 
                                                            "75-84", "85+"))
data2017.age.EW <- arrange(data2017.age.EW, age)

#By sex
data2017.male.EW <- read_excel(temp, sheet="Weekly figures 2017", range="B25:BB31", col_names=FALSE)
colnames(data2017.male.EW) <- c("age", format(seq.Date(from=as.Date("2017-01-06"), by="7 days", 
                                                       length.out=ncol(data2017.male.EW)-1), "%d/%m/%y"))
data2017.male.EW$sex <- "Male"

data2017.female.EW <- read_excel(temp, sheet="Weekly figures 2017", range="B34:BB40", col_names=FALSE)
colnames(data2017.female.EW) <- c("age", format(seq.Date(from=as.Date("2017-01-06"), by="7 days", 
                                                         length.out=ncol(data2017.female.EW)-1), "%d/%m/%y"))
data2017.female.EW$sex <- "Female"

data2017.sex.EW <- bind_rows(data2017.male.EW, data2017.female.EW)
data2017.sex.EW$age <- factor(data2017.sex.EW$age, levels=c("Under 1 year", "01-14", "15-44", "45-64", 
                                                            "65-74", "75-84", "85+"))
data2017.sex.EW$sex <- factor(data2017.sex.EW$sex, levels=c("Male", "Female"))
data2017.sex.EW <- arrange(data2017.sex.EW, age, sex)

#Add total rows
data2017.sex.EW <- data2017.sex.EW %>%
  group_by(sex) %>%
  summarise_at(c(2:(ncol(data2017.sex.EW)-1)), sum) %>%
  mutate(age="Total") %>%
  bind_rows(data2017.sex.EW)

#By region
data2017.reg.EW <- read_excel(temp, sheet="Weekly figures 2017", range="B43:BB52", col_names=FALSE)
colnames(data2017.reg.EW) <- c("reg", format(seq.Date(from=as.Date("2017-01-06"), by="7 days", 
                                                      length.out=ncol(data2017.reg.EW)-1), "%d/%m/%y"))

#2016 data
temp <- tempfile()
source <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fbirthsdeathsandmarriages%2fdeaths%2fdatasets%2fweeklyprovisionalfiguresondeathsregisteredinenglandandwales%2f2016/publishedweek522016.xls"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

#By age
data2016.age.EW <- read_excel(temp, sheet="Weekly figures 2016", range="B16:BB22", col_names=FALSE)
colnames(data2016.age.EW) <- c("age", format(seq.Date(from=as.Date("2016-01-08"), by="7 days", 
                                                      length.out=ncol(data2016.age.EW)-1), "%d/%m/%y"))
data2016.age.EW$age <- factor(data2016.age.EW$age, levels=c("Under 1 year", "01-14", "15-44", "45-64", "65-74", 
                                                            "75-84", "85+"))
data2016.age.EW <- arrange(data2016.age.EW, age)

#By sex
data2016.male.EW <- read_excel(temp, sheet="Weekly figures 2016", range="B25:BB31", col_names=FALSE)
colnames(data2016.male.EW) <- c("age", format(seq.Date(from=as.Date("2016-01-08"), by="7 days", 
                                                       length.out=ncol(data2016.male.EW)-1), "%d/%m/%y"))
data2016.male.EW$sex <- "Male"

data2016.female.EW <- read_excel(temp, sheet="Weekly figures 2016", range="B34:BB40", col_names=FALSE)
colnames(data2016.female.EW) <- c("age", format(seq.Date(from=as.Date("2016-01-08"), by="7 days", 
                                                         length.out=ncol(data2016.female.EW)-1), "%d/%m/%y"))
data2016.female.EW$sex <- "Female"

data2016.sex.EW <- bind_rows(data2016.male.EW, data2016.female.EW)
data2016.sex.EW$age <- factor(data2016.sex.EW$age, levels=c("Under 1 year", "01-14", "15-44", "45-64", 
                                                            "65-74", "75-84", "85+"))
data2016.sex.EW$sex <- factor(data2016.sex.EW$sex, levels=c("Male", "Female"))
data2016.sex.EW <- arrange(data2016.sex.EW, age, sex)

#Add total rows
data2016.sex.EW <- data2016.sex.EW %>%
  group_by(sex) %>%
  summarise_at(c(2:(ncol(data2016.sex.EW)-1)), sum) %>%
  mutate(age="Total") %>%
  bind_rows(data2016.sex.EW)

#By region
data2016.reg.EW <- read_excel(temp, sheet="Weekly figures 2016", range="B43:BB52", col_names=FALSE)
colnames(data2016.reg.EW) <- c("reg", format(seq.Date(from=as.Date("2016-01-08"), by="7 days", 
                                                      length.out=ncol(data2016.reg.EW)-1), "%d/%m/%y"))

#2015 data
temp <- tempfile()
source <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fbirthsdeathsandmarriages%2fdeaths%2fdatasets%2fweeklyprovisionalfiguresondeathsregisteredinenglandandwales%2f2015/publishedweek2015.xls"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

#By age
data2015.age.EW <- read_excel(temp, sheet="Weekly Figures 2015", range="A16:BB22", col_names=FALSE)
colnames(data2015.age.EW) <- c("age", format(seq.Date(from=as.Date("2015-01-02"), by="7 days", 
                                                      length.out=ncol(data2015.age.EW)-1), "%d/%m/%y"))
data2015.age.EW$age <- factor(data2015.age.EW$age, levels=c("Under 1 year", "01-14", "15-44", "45-64", "65-74", 
                                                            "75-84", "85+"))
data2015.age.EW <- arrange(data2015.age.EW, age)

#By sex
data2015.male.EW <- read_excel(temp, sheet="Weekly Figures 2015", range="A25:BB31", col_names=FALSE)
colnames(data2015.male.EW) <- c("age", format(seq.Date(from=as.Date("2015-01-02"), by="7 days", 
                                                       length.out=ncol(data2015.male.EW)-1), "%d/%m/%y"))
data2015.male.EW$sex <- "Male"

data2015.female.EW <- read_excel(temp, sheet="Weekly Figures 2015", range="A34:BB40", col_names=FALSE)
colnames(data2015.female.EW) <- c("age", format(seq.Date(from=as.Date("2015-01-02"), by="7 days", 
                                                         length.out=ncol(data2015.female.EW)-1), "%d/%m/%y"))
data2015.female.EW$sex <- "Female"

data2015.sex.EW <- bind_rows(data2015.male.EW, data2015.female.EW)
data2015.sex.EW$age <- factor(data2015.sex.EW$age, levels=c("Under 1 year", "01-14", "15-44", "45-64", 
                                                            "65-74", "75-84", "85+"))
data2015.sex.EW$sex <- factor(data2015.sex.EW$sex, levels=c("Male", "Female"))
data2015.sex.EW <- arrange(data2015.sex.EW, age, sex)

#Add total rows
data2015.sex.EW <- data2015.sex.EW %>%
  group_by(sex) %>%
  summarise_at(c(2:(ncol(data2015.sex.EW)-1)), sum) %>%
  mutate(age="Total") %>%
  bind_rows(data2015.sex.EW)

#By region
data2015.reg.EW <- read_excel(temp, sheet="Weekly Figures 2015", range="A43:BB52", col_names=FALSE)
colnames(data2015.reg.EW) <- c("reg", format(seq.Date(from=as.Date("2015-01-02"), by="7 days", 
                                                      length.out=ncol(data2015.reg.EW)-1), "%d/%m/%y"))

#2014 data
temp <- tempfile()
source <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fbirthsdeathsandmarriages%2fdeaths%2fdatasets%2fweeklyprovisionalfiguresondeathsregisteredinenglandandwales%2f2014/publishedweek2014.xls"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

#By age
data2014.age.EW <- read_excel(temp, sheet="Weekly Figures 2014", range="A16:BA22", col_names=FALSE)
colnames(data2014.age.EW) <- c("age", format(seq.Date(from=as.Date("2014-01-03"), by="7 days", 
                                                      length.out=ncol(data2014.age.EW)-1), "%d/%m/%y"))
data2014.age.EW$age <- factor(data2014.age.EW$age, levels=c("Under 1 year", "01-14", "15-44", "45-64", "65-74", 
                                                            "75-84", "85+"))
data2014.age.EW <- arrange(data2014.age.EW, age)

#By sex
data2014.male.EW <- read_excel(temp, sheet="Weekly Figures 2014", range="A25:BA31", col_names=FALSE)
colnames(data2014.male.EW) <- c("age", format(seq.Date(from=as.Date("2014-01-03"), by="7 days", 
                                                       length.out=ncol(data2014.male.EW)-1), "%d/%m/%y"))
data2014.male.EW$sex <- "Male"

data2014.female.EW <- read_excel(temp, sheet="Weekly Figures 2014", range="A34:BA40", col_names=FALSE)
colnames(data2014.female.EW) <- c("age", format(seq.Date(from=as.Date("2014-01-03"), by="7 days", 
                                                         length.out=ncol(data2014.female.EW)-1), "%d/%m/%y"))
data2014.female.EW$sex <- "Female"

data2014.sex.EW <- bind_rows(data2014.male.EW, data2014.female.EW)
data2014.sex.EW$age <- factor(data2014.sex.EW$age, levels=c("Under 1 year", "01-14", "15-44", "45-64", 
                                                            "65-74", "75-84", "85+"))
data2014.sex.EW$sex <- factor(data2014.sex.EW$sex, levels=c("Male", "Female"))
data2014.sex.EW <- arrange(data2014.sex.EW, age, sex)

#Add total rows
data2014.sex.EW <- data2014.sex.EW %>%
  group_by(sex) %>%
  summarise_at(c(2:(ncol(data2014.sex.EW)-1)), sum) %>%
  mutate(age="Total") %>%
  bind_rows(data2014.sex.EW)

#By region
data2014.reg.EW <- read_excel(temp, sheet="Weekly Figures 2014", range="A43:BA52", col_names=FALSE)
colnames(data2014.reg.EW) <- c("reg", format(seq.Date(from=as.Date("2014-01-03"), by="7 days", 
                                                      length.out=ncol(data2014.reg.EW)-1), "%d/%m/%y"))

#2013 data
temp <- tempfile()
source <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fbirthsdeathsandmarriages%2fdeaths%2fdatasets%2fweeklyprovisionalfiguresondeathsregisteredinenglandandwales%2f2013/publishedweek2013.xls"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

#By age
data2013.age.EW <- read_excel(temp, sheet="Weekly Figures 2013", range="A16:BA22", col_names=FALSE)
colnames(data2013.age.EW) <- c("age", format(seq.Date(from=as.Date("2013-01-04"), by="7 days", 
                                                      length.out=ncol(data2013.age.EW)-1), "%d/%m/%y"))
data2013.age.EW$age <- factor(data2013.age.EW$age, levels=c("Under 1 year", "01-14", "15-44", "45-64", "65-74", 
                                                            "75-84", "85+"))
data2013.age.EW <- arrange(data2013.age.EW, age)

#By sex
data2013.male.EW <- read_excel(temp, sheet="Weekly Figures 2013", range="A25:BA31", col_names=FALSE)
colnames(data2013.male.EW) <- c("age", format(seq.Date(from=as.Date("2013-01-04"), by="7 days", 
                                                       length.out=ncol(data2013.male.EW)-1), "%d/%m/%y"))
data2013.male.EW$sex <- "Male"

data2013.female.EW <- read_excel(temp, sheet="Weekly Figures 2013", range="A34:BA40", col_names=FALSE)
colnames(data2013.female.EW) <- c("age", format(seq.Date(from=as.Date("2013-01-04"), by="7 days", 
                                                         length.out=ncol(data2013.female.EW)-1), "%d/%m/%y"))
data2013.female.EW$sex <- "Female"

data2013.sex.EW <- bind_rows(data2013.male.EW, data2013.female.EW)
data2013.sex.EW$age <- factor(data2013.sex.EW$age, levels=c("Under 1 year", "01-14", "15-44", "45-64", 
                                                            "65-74", "75-84", "85+"))
data2013.sex.EW$sex <- factor(data2013.sex.EW$sex, levels=c("Male", "Female"))
data2013.sex.EW <- arrange(data2013.sex.EW, age, sex)

#Add total rows
data2013.sex.EW <- data2013.sex.EW %>%
  group_by(sex) %>%
  summarise_at(c(2:(ncol(data2013.sex.EW)-1)), sum) %>%
  mutate(age="Total") %>%
  bind_rows(data2013.sex.EW)

#By region
data2013.reg.EW <- read_excel(temp, sheet="Weekly Figures 2013", range="A43:BA52", col_names=FALSE)
colnames(data2013.reg.EW) <- c("reg", format(seq.Date(from=as.Date("2013-01-04"), by="7 days", 
                                                      length.out=ncol(data2013.reg.EW)-1), "%d/%m/%y"))

#2012 data
temp <- tempfile()
source <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fbirthsdeathsandmarriages%2fdeaths%2fdatasets%2fweeklyprovisionalfiguresondeathsregisteredinenglandandwales%2f2012/publishedweek2012.xls"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

#By age
data2012.age.EW <- read_excel(temp, sheet="Weekly Figures 2012", range="A16:BA22", col_names=FALSE)
colnames(data2012.age.EW) <- c("age", format(seq.Date(from=as.Date("2012-01-06"), by="7 days", 
                                                      length.out=ncol(data2012.age.EW)-1), "%d/%m/%y"))
data2012.age.EW$age <- factor(data2012.age.EW$age, levels=c("Under 1 year", "01-14", "15-44", "45-64", "65-74", 
                                                            "75-84", "85+"))
data2012.age.EW <- arrange(data2012.age.EW, age)

#By sex
data2012.male.EW <- read_excel(temp, sheet="Weekly Figures 2012", range="A25:BA31", col_names=FALSE)
colnames(data2012.male.EW) <- c("age", format(seq.Date(from=as.Date("2012-01-06"), by="7 days", 
                                                       length.out=ncol(data2012.male.EW)-1), "%d/%m/%y"))
data2012.male.EW$sex <- "Male"

data2012.female.EW <- read_excel(temp, sheet="Weekly Figures 2012", range="A34:BA40", col_names=FALSE)
colnames(data2012.female.EW) <- c("age", format(seq.Date(from=as.Date("2012-01-06"), by="7 days", 
                                                         length.out=ncol(data2012.female.EW)-1), "%d/%m/%y"))
data2012.female.EW$sex <- "Female"

data2012.sex.EW <- bind_rows(data2012.male.EW, data2012.female.EW)
data2012.sex.EW$age <- factor(data2012.sex.EW$age, levels=c("Under 1 year", "01-14", "15-44", "45-64", 
                                                            "65-74", "75-84", "85+"))
data2012.sex.EW$sex <- factor(data2012.sex.EW$sex, levels=c("Male", "Female"))
data2012.sex.EW <- arrange(data2012.sex.EW, age, sex)

#Add total rows
data2012.sex.EW <- data2012.sex.EW %>%
  group_by(sex) %>%
  summarise_at(c(2:(ncol(data2012.sex.EW)-1)), sum) %>%
  mutate(age="Total") %>%
  bind_rows(data2012.sex.EW)

#By region
data2012.reg.EW <- read_excel(temp, sheet="Weekly Figures 2012", range="A43:BA52", col_names=FALSE)
colnames(data2012.reg.EW) <- c("reg", format(seq.Date(from=as.Date("2012-01-06"), by="7 days", 
                                                      length.out=ncol(data2012.reg.EW)-1), "%d/%m/%y"))

#2011 data
temp <- tempfile()
source <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fbirthsdeathsandmarriages%2fdeaths%2fdatasets%2fweeklyprovisionalfiguresondeathsregisteredinenglandandwales%2f2011/publishedweek2011.xls"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

#By age
data2011.age.EW <- read_excel(temp, sheet="Weekly Figures 2011", range="A17:BA23", col_names=FALSE)
colnames(data2011.age.EW) <- c("age", format(seq.Date(from=as.Date("2011-01-07"), by="7 days", 
                                                      length.out=ncol(data2011.age.EW)-1), "%d/%m/%y"))
data2011.age.EW$age <- factor(data2011.age.EW$age, levels=c("Under 1 year", "01-14", "15-44", "45-64", "65-74", 
                                                            "75-84", "85+"))
data2011.age.EW <- arrange(data2011.age.EW, age)

#By sex
data2011.male.EW <- read_excel(temp, sheet="Weekly Figures 2011", range="A26:BA32", col_names=FALSE)
colnames(data2011.male.EW) <- c("age", format(seq.Date(from=as.Date("2011-01-07"), by="7 days", 
                                                       length.out=ncol(data2011.male.EW)-1), "%d/%m/%y"))
data2011.male.EW$sex <- "Male"

data2011.female.EW <- read_excel(temp, sheet="Weekly Figures 2011", range="A35:BA41", col_names=FALSE)
colnames(data2011.female.EW) <- c("age", format(seq.Date(from=as.Date("2011-01-07"), by="7 days", 
                                                         length.out=ncol(data2011.female.EW)-1), "%d/%m/%y"))
data2011.female.EW$sex <- "Female"

data2011.sex.EW <- bind_rows(data2011.male.EW, data2011.female.EW)
data2011.sex.EW$age <- factor(data2011.sex.EW$age, levels=c("Under 1 year", "01-14", "15-44", "45-64", 
                                                            "65-74", "75-84", "85+"))
data2011.sex.EW$sex <- factor(data2011.sex.EW$sex, levels=c("Male", "Female"))
data2011.sex.EW <- arrange(data2011.sex.EW, age, sex)

#Add total rows
data2011.sex.EW <- data2011.sex.EW %>%
  group_by(sex) %>%
  summarise_at(c(2:(ncol(data2011.sex.EW)-1)), sum) %>%
  mutate(age="Total") %>%
  bind_rows(data2011.sex.EW)

#By region
data2011.reg.EW <- read_excel(temp, sheet="Weekly Figures 2011", range="A44:BA53", col_names=FALSE)
colnames(data2011.reg.EW) <- c("reg", format(seq.Date(from=as.Date("2011-01-07"), by="7 days", 
                                                      length.out=ncol(data2011.reg.EW)-1), "%d/%m/%y"))

#2010 data
temp <- tempfile()
source <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fbirthsdeathsandmarriages%2fdeaths%2fdatasets%2fweeklyprovisionalfiguresondeathsregisteredinenglandandwales%2f2010/publishedweek2010.xls"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

#By age
data2010.age.EW <- read_excel(temp, sheet="Weekly Figures 2010", range="A16:BA22", col_names=FALSE)
colnames(data2010.age.EW) <- c("age", format(seq.Date(from=as.Date("2010-01-08"), by="7 days", 
                                                      length.out=ncol(data2010.age.EW)-1), "%d/%m/%y"))
data2010.age.EW$age <- factor(data2010.age.EW$age, levels=c("Under 1 year", "01-14", "15-44", "45-64", "65-74", 
                                                            "75-84", "85+"))
data2010.age.EW <- arrange(data2010.age.EW, age)

#By sex
data2010.male.EW <- read_excel(temp, sheet="Weekly Figures 2010", range="A25:BA31", col_names=FALSE)
colnames(data2010.male.EW) <- c("age", format(seq.Date(from=as.Date("2010-01-08"), by="7 days", 
                                                       length.out=ncol(data2010.male.EW)-1), "%d/%m/%y"))
data2010.male.EW$sex <- "Male"

data2010.female.EW <- read_excel(temp, sheet="Weekly Figures 2010", range="A34:BA40", col_names=FALSE)
colnames(data2010.female.EW) <- c("age", format(seq.Date(from=as.Date("2010-01-08"), by="7 days", 
                                                         length.out=ncol(data2010.female.EW)-1), "%d/%m/%y"))
data2010.female.EW$sex <- "Female"

data2010.sex.EW <- bind_rows(data2010.male.EW, data2010.female.EW)
data2010.sex.EW$age <- factor(data2010.sex.EW$age, levels=c("Under 1 year", "01-14", "15-44", "45-64", 
                                                            "65-74", "75-84", "85+"))
data2010.sex.EW$sex <- factor(data2010.sex.EW$sex, levels=c("Male", "Female"))
data2010.sex.EW <- arrange(data2010.sex.EW, age, age)

#Add total rows
data2010.sex.EW <- data2010.sex.EW %>%
  group_by(sex) %>%
  summarise_at(c(2:(ncol(data2010.sex.EW)-1)), sum) %>%
  mutate(age="Total") %>%
  bind_rows(data2010.sex.EW)

#By region
data2010.reg.EW <- read_excel(temp, sheet="Weekly Figures 2010", range="A43:BA52", col_names=FALSE)
colnames(data2010.reg.EW) <- c("reg", format(seq.Date(from=as.Date("2010-01-08"), by="7 days", 
                                                      length.out=ncol(data2010.reg.EW)-1), "%d/%m/%y"))

#Combine into overall 2010-20 datasets
#For age
data_wide.age.EW <- bind_cols(data2010.age.EW, data2011.age.EW, data2012.age.EW, data2013.age.EW, data2014.age.EW, 
                              data2015.age.EW, data2016.age.EW, data2017.age.EW, data2018.age.EW, data2019.age.EW, 
                              data2020.age.EW)

data.age.EW <- gather(data_wide.age.EW, week, deaths, c(2:ncol(data_wide.age.EW)))
data.age.EW <- subset(data.age.EW, substr(data.age.EW$week,1,3)!="age")
data.age.EW$deaths <- as.numeric(data.age.EW$deaths)

#Add total row
data.age.EW <- data.age.EW %>%
  group_by(week) %>%
  summarise(deaths=sum(deaths)) %>%
  mutate(age="Total") %>%
  bind_rows(data.age.EW)

data.age.EW$week <- as.Date(data.age.EW$week, "%d/%m/%y")
data.age.EW$year <- as.numeric(format(data.age.EW$week, "%Y"))
data.age.EW$weekno <- week(data.age.EW$week)
data.age.EW$reg <- "England & Wales"

#For sex
data2010.sex.EW <- data2010.sex.EW %>%
  select(age, everything())
data_wide.sex.EW <- bind_cols(data2010.sex.EW, data2011.sex.EW, data2012.sex.EW, data2013.sex.EW, data2014.sex.EW, 
                              data2015.sex.EW, data2016.sex.EW, data2017.sex.EW, data2018.sex.EW, data2019.sex.EW, 
                              data2020.sex.EW)
data.sex.EW <- gather(data_wide.sex.EW, week, deaths, c(3:ncol(data_wide.sex.EW)))
data.sex.EW <- subset(data.sex.EW, !substr(data.sex.EW$week,1,3) %in% c("age", "sex"))
data.sex.EW$deaths <- as.numeric(data.sex.EW$deaths)
data.sex.EW$week <- as.Date(data.sex.EW$week, "%d/%m/%y")
data.sex.EW$year <- as.numeric(format(data.sex.EW$week, "%Y"))
data.sex.EW$weekno <- week(data.sex.EW$week)
data.sex.EW$reg <- "England & Wales"
data.sex.EW$age <- factor(data.sex.EW$age, levels=c("Under 1 year", "01-14", "15-44", "45-64", "65-74", "75-84", "85+", "Total"))

#For region
data_wide.reg.EW <- bind_cols(data2010.reg.EW, data2011.reg.EW, data2012.reg.EW, data2013.reg.EW, data2014.reg.EW, 
                              data2015.reg.EW, data2016.reg.EW, data2017.reg.EW, data2018.reg.EW, data2019.reg.EW, 
                              data2020.reg.EW)

data.reg.EW <- gather(data_wide.reg.EW, week, deaths, c(2:ncol(data_wide.reg.EW)))
data.reg.EW <- subset(data.reg.EW, substr(data.reg.EW$week,1,3)!="reg")
data.reg.EW$deaths <- as.numeric(data.reg.EW$deaths)
data.reg.EW$date <- as.Date(data.reg.EW$week, "%d/%m/%y")
data.reg.EW$year <- as.numeric(format(data.reg.EW$date, "%Y"))
data.reg.EW$weekno <- week(data.reg.EW$date)
data.reg.EW <- data.reg.EW[,-c(2)]

#Tidy up
rm(data_wide.age.EW, data_wide.reg.EW, data_wide.sex.EW, data2010.age.EW, data2010.female.EW, data2010.male.EW, data2010.reg.EW, data2010.sex.EW,
   data2011.age.EW, data2011.female.EW, data2011.male.EW, data2011.reg.EW, data2011.sex.EW,
   data2012.age.EW, data2012.female.EW, data2012.male.EW, data2012.reg.EW, data2012.sex.EW,
   data2013.age.EW, data2013.female.EW, data2013.male.EW, data2013.reg.EW, data2013.sex.EW,
   data2014.age.EW, data2014.female.EW, data2014.male.EW, data2014.reg.EW, data2014.sex.EW,
   data2015.age.EW, data2015.female.EW, data2015.male.EW, data2015.reg.EW, data2015.sex.EW,
   data2016.age.EW, data2016.female.EW, data2016.male.EW, data2016.reg.EW, data2016.sex.EW,
   data2017.age.EW, data2017.female.EW, data2017.male.EW, data2017.reg.EW, data2017.sex.EW,
   data2018.age.EW, data2018.female.EW, data2018.male.EW, data2018.reg.EW, data2018.sex.EW,
   data2019.age.EW, data2019.female.EW, data2019.male.EW, data2019.reg.EW, data2019.sex.EW,
   data2020.age.EW, data2020.female.EW, data2020.male.EW, data2020.reg.EW, data2020.sex.EW)

#######################
#Read in Scottish data#
#######################

#Historic weekly deaths data for Scotland is published by National Records of Scotland

temp <- tempfile()
source <- "https://www.nrscotland.gov.uk/files//statistics/weekly-monthly-births-deaths-data/2020/mar/weekly-march-20.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
data2010.S <- read_excel(temp, sheet="2010", range="A6:D57", col_names=FALSE)
data2011.S <- read_excel(temp, sheet="2011", range="A6:D58", col_names=FALSE)
data2012.S <- read_excel(temp, sheet="2012", range="A6:D57", col_names=FALSE)
data2013.S <- read_excel(temp, sheet="2013", range="A6:D57", col_names=FALSE)
data2014.S <- read_excel(temp, sheet="2014", range="A6:D57", col_names=FALSE)
data2015.S <- read_excel(temp, sheet="2015 ", range="A6:D58", col_names=FALSE)
data2016.S <- read_excel(temp, sheet="2016", range="E6:G57", col_names=FALSE)
data2017.S <- read_excel(temp, sheet="2017", range="E6:G57", col_names=FALSE)
data2018.S <- read_excel(temp, sheet="2018", range="E6:G57", col_names=FALSE)
data2019.S <- read_excel(temp, sheet="2019", range="E6:G57", col_names=FALSE)

#Weekly data for 2020 is published in a different Excel file each week on a Wednesday,
#so need to update the link each time from this page https://www.nrscotland.gov.uk/covid19stats
#Need to manually update the cell range when reading data in.

#Take 2020 data from dedicated COVID-19 page, which is updated more regularly
temp <- tempfile()
source <- "https://www.nrscotland.gov.uk/files//statistics/covid19/covid-deaths-data-week-17.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
data2020.S <- data.frame(t(read_excel(temp, sheet="Table 2 - All deaths", range="C6:S7", col_names=FALSE))[,c(2)])
date <- data.frame(date=format(seq.Date(from=as.Date("2019-12-30"), by="7 days", length.out=nrow(data2020.S)), "%d/%m/%y"))
data2020.S <- cbind(date, data2020.S)
colnames(data2020.S) <- c("date", "deaths")
data2020.S$date <- as.Date(data2020.S$date, "%d/%m/%y")
data2020.S$weekno <- week(data2020.S$date)

#Stick together 2004-15 which share the same structure
data1015.S <- bind_rows(data2010.S, data2011.S, data2012.S, data2013.S, data2014.S, data2015.S)
colnames(data1015.S) <- c("weekno", "date", "births", "deaths")
data1015.S$date <- as.Date(data1015.S$date)

#Then 2016-19 data
data1619.S <- bind_rows(data2016.S, data2017.S, data2018.S, data2019.S)
colnames(data1619.S) <- c("weekno", "date", "deaths")
data1619.S$date <- as.Date(data1619.S$date)

data.S <- bind_rows(data1015.S, data1619.S, data2020.S)

#Recalculate dates to align with ONS data (which uses week to, not w/c)
data.S$date <- data.S$date+days(6)

data.S$weekno <- week(data.S$date)
data.S$year <- year(data.S$date)
data.S$reg <- "Scotland"
data.S <- data.S[,-c(3)]

#Tidy up
rm(date, data2010.S, data2011.S, data2012.S, data2013.S, data2014.S, data2015.S, data2016.S, data2017.S,
   data2018.S, data2019.S, data2020.S, data1015.S, data1619.S)

#############################
#Read in Northern Irish data#
#############################

#NI data from NISRA is published to the same Excel file (so no need to update link) each Friday

#Need to manually update the range by one each week
temp <- tempfile()
source <- "https://www.nisra.gov.uk/sites/nisra.gov.uk/files/publications/Weekly_Deaths.xls"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
data2020.NI <- read_excel(temp, sheet="Weekly Deaths_2020", range="B5:C19", col_names=FALSE)
colnames(data2020.NI) <- c("date", "deaths")

temp <- tempfile()
source <- "https://www.nisra.gov.uk/sites/nisra.gov.uk/files/publications/Weekly_Deaths%20-%20Historical.xls"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
data2019.NI <- read_excel(temp, sheet="Weekly Deaths_2019", range="C5:D56", col_names=FALSE)
colnames(data2019.NI) <- c("date", "deaths")

data2018.NI <- read_excel(temp, sheet="Weekly Deaths_2018", range="C5:D56", col_names=FALSE)
colnames(data2018.NI) <- c("date", "deaths")

data2017.NI <- read_excel(temp, sheet="Weekly Deaths_2017", range="C5:D57", col_names=FALSE)
colnames(data2017.NI) <- c("date", "deaths")

data2016.NI <- read_excel(temp, sheet="Weekly Deaths_2016", range="C5:D56", col_names=FALSE)
colnames(data2016.NI) <- c("date", "deaths")

data2015.NI <- read_excel(temp, sheet="Weekly Deaths_2015", range="C5:D57", col_names=FALSE)
colnames(data2015.NI) <- c("date", "deaths")

data2014.NI <- read_excel(temp, sheet="Weekly Deaths_2014", range="C5:D56", col_names=FALSE)
colnames(data2014.NI) <- c("date", "deaths")

data2013.NI <- read_excel(temp, sheet="Weekly Deaths_2013", range="C5:D56", col_names=FALSE)
colnames(data2013.NI) <- c("date", "deaths")

data2012.NI <- read_excel(temp, sheet="Weekly Deaths_2012", range="C5:D56", col_names=FALSE)
colnames(data2012.NI) <- c("date", "deaths")

data2011.NI <- read_excel(temp, sheet="Weekly Deaths_2011", range="C5:D56", col_names=FALSE)
colnames(data2011.NI) <- c("date", "deaths")

data2010.NI <- read_excel(temp, sheet="Weekly Deaths_2010", range="C5:D57", col_names=FALSE)
colnames(data2010.NI) <- c("date", "deaths")

data.NI <- bind_rows(data2010.NI, data2011.NI, data2012.NI, data2013.NI, data2014.NI, data2015.NI, data2016.NI, 
                     data2017.NI, data2018.NI, data2019.NI, data2020.NI)
data.NI$date <- as.Date(data.NI$date)

data.NI$weekno <- week(data.NI$date)
data.NI$year <- year(data.NI$date)
data.NI$reg <- "Northern Ireland"

#Tidy up
rm(data2010.NI, data2011.NI, data2012.NI, data2013.NI, data2014.NI, data2015.NI, data2016.NI, data2017.NI,
   data2018.NI, data2019.NI, data2020.NI)

#Stick regional data together for whole of UK
data.reg.UK <- bind_rows(data.reg.EW, data.S, data.NI)

###########
#Save data#
###########

#E&W data by age
write.csv(data.age.EW, "Data/deaths_age_EW.csv")
#E&W data by sex
write.csv(data.sex.EW, "Data/deaths_sex_EW.csv")
#UK data by region
write.csv(data.reg.UK, "Data/deaths_reg_UK.csv")

################
#E&W only plots#
################

#Overall plot

#Extract max/min values
#split off 2020 data
data.age.EW.new <- subset(data.age.EW, year==2020 & age=="Total")
data.age.EW.old <- subset(data.age.EW, year<2020 & age=="Total")

data.age.EW.old <- data.age.EW.old %>%
  group_by(weekno, reg) %>%
  summarise(max=max(deaths), min=min(deaths), mean=mean(deaths))

#Generate filled area for total excess deaths vs. previous 10-year maximum
data.age.EW.new <- merge(data.age.EW.new, data.age.EW.old, by=c("weekno"))
data.age.EW.new <- data.age.EW.new %>%
  mutate(ymin=pmin(deaths, max))

tiff("Outputs/ONSWeeklyDeaths.tiff", units="in", width=10, height=8, res=300)
ggplot()+
  geom_ribbon(data=data.age.EW.new, aes(x=weekno, ymin=ymin, ymax=deaths), fill="Red", alpha=0.2)+
  geom_ribbon(data=data.age.EW.old, aes(x=weekno, ymin=min, ymax=max), fill="Skyblue2")+
  geom_line(data=data.age.EW.old, aes(x=weekno, y=mean), colour="Grey50", linetype=2)+
  geom_line(data=data.age.EW.new, aes(x=weekno, y=deaths), colour="Red")+
  theme_classic()+
  scale_x_continuous(name="Week number", breaks=c(0,10,20,30,40,50))+
  scale_y_continuous(name="Deaths registered")+
  expand_limits(y=0)+
  labs(title="Deaths from all causes have risen sharply across England & Wales",
       subtitle="Weekly deaths in 2020 compared to the range in 2010-19. Data up to 17th April",
       caption="Data from ONS | Plot by @VictimOfMaths")+
  annotate(geom="text", x=16.5, y=15000, label="Unprecedented excess deaths", colour="Red", hjust=0)+
  annotate(geom="text", x=30, y=9700, label="Historic maximum", colour="Skyblue4")+
  annotate(geom="text", x=30, y=8000, label="Historic minimum", colour="Skyblue4")+
  annotate(geom="text", x=48, y=8800, label="Historic mean", colour="grey30")+
  geom_curve(aes(x=48, y=9000, xend=47, yend=9800), colour="grey30", curvature=0.15,
             arrow=arrow(length=unit(0.1, "cm"), type="closed"), lineend="round")

dev.off()  

#Plot by sex

#Extract max/min values
#split off 2020 data
data.sex.EW.new <- subset(data.sex.EW, year==2020 & age=="Total")
data.sex.EW.old <- subset(data.sex.EW, year<2020 & age=="Total")

data.sex.EW.old <- data.sex.EW.old %>%
  group_by(weekno, sex) %>%
  summarise(max=max(deaths), min=min(deaths), mean=mean(deaths))

#Generate filled area for total excess deaths vs. previous 10-year maximum
data.sex.EW.new <- merge(data.sex.EW.new, data.sex.EW.old, by=c("weekno", "sex"))
data.sex.EW.new <- data.sex.EW.new %>%
  group_by(sex) %>%
  mutate(ymin=pmin(deaths, max))

#Calculate excess deaths vs. mean so far this year
data.sex.EW.new$excess <- data.sex.EW.new$deaths-data.sex.EW.new$mean
sex.EW.excess <- data.sex.EW.new %>%
  group_by(sex) %>%
  summarise(excess=sum(excess))

ann_text1 <- data.frame(weekno=c(17,17), deaths=c(10900,11400), sex=c("Female", "Male"))

tiff("Outputs/ONSWeeklyDeathsxSex.tiff", units="in", width=12, height=8, res=300)
ggplot()+
  geom_ribbon(data=data.sex.EW.old, aes(x=weekno, ymin=min, ymax=max), fill="Skyblue2")+
  geom_line(data=data.sex.EW.old, aes(x=weekno, y=mean), colour="Grey50", linetype=2)+
  geom_line(data=data.sex.EW.new, aes(x=weekno, y=deaths), colour="Red")+
  theme_classic()+
  facet_wrap(~sex)+
  scale_x_continuous(name="Week number", breaks=c(0,10,20,30,40,50))+
  scale_y_continuous(name="Deaths registered")+
  expand_limits(y=0)+
  labs(title="The burden of COVID-19 deaths have fallen disproportionately on men",
       subtitle="Weekly deaths in 2020 compared to the range in 2010-19. Data up to 17th April",
       caption="Data from ONS | Plot by @VictimOfMaths")+
  theme(strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)))+
  geom_text(data=ann_text1, aes(x=weekno, y=deaths), label=c(paste(round(sex.EW.excess[2,2],0),"excess deaths vs. 2010-19 average"), 
                                                             paste(round(sex.EW.excess[1,2],0),"excess deaths vs. 2010-19 average")), 
            size=3, colour=c("Red", "Red"), hjust=0)
dev.off()  

#By sex and age

#Extract max/min values
#split off 2020 data
data.sex.age.EW.new <- subset(data.sex.EW, year==2020 & !age %in% c("Total", "Under 1 year", "01-14", "15-44"))
data.sex.age.EW.old <- subset(data.sex.EW, year<2020 & !age %in% c("Total", "Under 1 year", "01-14", "15-44"))

data.sex.age.EW.old <- data.sex.age.EW.old %>%
  group_by(weekno, sex, age) %>%
  summarise(max=max(deaths), min=min(deaths), mean=mean(deaths))

#Generate filled area for total excess deaths vs. previous 10-year maximum
data.sex.age.EW.new <- merge(data.sex.age.EW.new, data.sex.age.EW.old, by=c("weekno", "sex", "age"))
data.sex.age.EW.new <- data.sex.age.EW.new %>%
  group_by(sex, age) %>%
  mutate(ymin=pmin(deaths, max))

#Calculate excess deaths vs. mean so far this year
data.sex.age.EW.new$excess <- data.sex.age.EW.new$deaths-data.sex.age.EW.new$mean
sex.age.EW.excess <- data.sex.age.EW.new %>%
  group_by(sex, age) %>%
  summarise(excess=sum(excess))

ann_text2 <- data.frame(weekno=rep(16.5, times=8), deaths=c(920,1300,2900,5400,1350,2000,3700,4000), 
                        sex=rep(c("Female", "Male"), each=4), 
                        age=rep(c("45-64", "65-74", "75-84", "85+"), times=2))

tiff("Outputs/ONSWeeklyDeathsxSexxAge.tiff", units="in", width=12, height=8, res=300)
ggplot()+
  geom_ribbon(data=data.sex.age.EW.old, aes(x=weekno, ymin=min, ymax=max), fill="Skyblue2")+
  geom_line(data=data.sex.age.EW.old, aes(x=weekno, y=mean), colour="Grey50", linetype=2)+
  geom_line(data=data.sex.age.EW.new, aes(x=weekno, y=deaths), colour="Red")+
  theme_classic()+
  facet_grid(age~sex)+
  scale_x_continuous(name="Week number", breaks=c(0,10,20,30,40,50))+
  scale_y_continuous(name="Deaths registered")+
  expand_limits(y=0)+
  labs(title="The burden of COVID-19 deaths have fallen disproportionately on men across all ages",
       subtitle="Weekly deaths in 2020 compared to the range in 2010-19. Data up to 17th April",
       caption="Data from ONS | Plot by @VictimOfMaths")+
  theme(strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)))+
  geom_text(data=ann_text2, aes(x=weekno, y=deaths), label=c(paste(round(sex.age.EW.excess[5,3],0),"excess deaths"), 
                                                             paste(round(sex.age.EW.excess[6,3],0),"excess deaths"),
                                                             paste(round(sex.age.EW.excess[7,3],0),"excess deaths"),
                                                             paste(round(sex.age.EW.excess[8,3],0),"excess deaths"),
                                                             paste(round(sex.age.EW.excess[1,3],0),"excess deaths vs. 2010-19 average"),
                                                             paste(round(sex.age.EW.excess[2,3],0),"excess deaths"),
                                                             paste(round(sex.age.EW.excess[3,3],0),"excess deaths"),
                                                             paste(round(sex.age.EW.excess[4,3],0),"excess deaths")),
            size=3, colour=rep("red", times=8), hjust=0)
dev.off()  

#Plot by age

#Compress age groups under 45
data.age.EW$age2 <- case_when(
  data.age.EW$age %in% c("Under 1 year", "01-14", "15-44") ~ "Under 45",
  data.age.EW$age!="Total" ~ data.age.EW$age
)

data.age.EW$age2 <- factor(data.age.EW$age2, levels=c("Under 45", "45-64", "65-74", "75-84", "85+"))

data.age.EW <- data.age.EW %>%
  group_by(weekno, age2, year) %>%
  mutate(deaths=sum(deaths))

#Extract max/min values
#split off 2020 data
data.age.EW.new <- subset(data.age.EW, year==2020 & !is.na(age2))
data.age.EW.old <- subset(data.age.EW, year<2020 & !is.na(age2))

data.age.EW.old <- data.age.EW.old %>%
  group_by(weekno, age2) %>%
  summarise(max=max(deaths), min=min(deaths), mean=mean(deaths))

#Generate filled area for total excess deaths vs. previous 10-year maximum
data.age.EW.new <- merge(data.age.EW.new, data.age.EW.old, by=c("weekno", "age2"))
data.age.EW.new <- data.age.EW.new %>%
  group_by(age2) %>%
  mutate(ymin=pmin(deaths, max))

#Calculate excess deaths vs. mean so far this year
data.age.EW.new$excess <- data.age.EW.new$deaths-data.age.EW.new$mean
age.EW.excess <- data.age.EW.new %>%
  group_by(age2) %>%
  summarise(excess=sum(excess))

ann_text3 <- data.frame(weekno=rep(17, times=5), deaths=c(1000, 1800, 2500, 4500, 6000), age2=c("Under 45", "45-64", "65-74", "75-84", "85+"))

tiff("Outputs/ONSWeeklyDeathsxAge.tiff", units="in", width=12, height=8, res=300)
ggplot()+
  geom_ribbon(data=data.age.EW.new, aes(x=weekno, ymin=ymin, ymax=deaths), fill="Red", alpha=0.2)+
  geom_ribbon(data=data.age.EW.old, aes(x=weekno, ymin=min, ymax=max), fill="Skyblue2")+
  geom_line(data=data.age.EW.old, aes(x=weekno, y=mean), colour="Grey50", linetype=2)+
  geom_line(data=data.age.EW.new, aes(x=weekno, y=deaths), colour="Red")+
  theme_classic()+
  facet_wrap(~age2)+
  scale_x_continuous(name="Week number", breaks=c(0,10,20,30,40,50))+
  scale_y_continuous(name="Deaths registered")+
  expand_limits(y=0)+
  labs(title="All-cause deaths in England & Wales are at record levels across all age groups over 45",
       subtitle="Weekly deaths in 2020 compared to the range in 2010-19. Data up to 17th April",
       caption="Data from ONS | Plot by @VictimOfMaths")+
  theme(strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)))+
  geom_text(data=ann_text3, aes(x=weekno, y=deaths), label=c(paste(round(age.EW.excess[1,2],0),"deaths compared to\n2010-19 average"), 
                                                             paste(round(age.EW.excess[2,2],0),"excess deaths"),
                                                             paste(round(age.EW.excess[3,2],0),"excess deaths"),
                                                             paste(round(age.EW.excess[4,2],0),"excess deaths"),
                                                             paste(round(age.EW.excess[5,2],0),"excess deaths")), 
            size=3, colour=rep("red", times=5), hjust=0)
dev.off()  

###################
#Plot for Scotland#
###################
#Overall plot

#Extract max/min values
#split off 2020 data
data.S.new <- subset(data.S, year==2020)
data.S.old <- subset(data.S, year<2020)

data.S.old <- data.S.old %>%
  group_by(weekno) %>%
  summarise(max=max(deaths), min=min(deaths), mean=mean(deaths))

#Generate filled area for total excess deaths vs. previous 10-year maximum
data.S.new <- merge(data.S.new, data.S.old, by=c("weekno"))
data.S.new <- data.S.new %>%
  mutate(ymin=pmin(deaths, max))

tiff("Outputs/NRSWeeklyDeaths.tiff", units="in", width=10, height=8, res=300)
ggplot()+
  geom_ribbon(data=data.S.new, aes(x=weekno, ymin=ymin, ymax=deaths), fill="Red", alpha=0.2)+
  geom_ribbon(data=data.S.old, aes(x=weekno, ymin=min, ymax=max), fill="Skyblue2")+
  geom_line(data=data.S.old, aes(x=weekno, y=mean), colour="Grey50", linetype=2)+
  geom_line(data=data.S.new, aes(x=weekno, y=deaths), colour="Red")+
  theme_classic()+
  scale_x_continuous(name="Week number", breaks=c(0,10,20,30,40,50))+
  scale_y_continuous(name="Deaths registered")+
  expand_limits(y=0)+
  labs(title="Scotland looks to have passed the peak of all-cause deaths",
       subtitle="Weekly deaths in 2020 compared to the range in 2010-19. Data up to 26th April",
       caption="Data from NRS | Plot by @VictimOfMaths")+
  annotate(geom="text", x=17.5, y=1500, label="Unprecedented excess deaths", colour="Red", hjust=0)+
  annotate(geom="text", x=30, y=1150, label="Historic maximum", colour="Skyblue4")+
  annotate(geom="text", x=30, y=800, label="Historic minimum", colour="Skyblue4")+
  annotate(geom="text", x=48, y=850, label="Historic mean", colour="grey30")+
  geom_curve(aes(x=48, y=900, xend=47, yend=1050), colour="grey30", curvature=0.15,
             arrow=arrow(length=unit(0.1, "cm"), type="closed"), lineend="round")

dev.off()  

###########################
#Plot for Northern Ireland#
###########################
#Overall plot

#Extract max/min values
#split off 2020 data
data.NI.new <- subset(data.NI, year==2020)
data.NI.old <- subset(data.NI, year<2020)

data.NI.old <- data.NI.old %>%
  group_by(weekno) %>%
  summarise(max=max(deaths), min=min(deaths), mean=mean(deaths))

#Generate filled area for total excess deaths vs. previous 10-year maximum
data.NI.new <- merge(data.NI.new, data.NI.old, by=c("weekno"))
data.NI.new <- data.NI.new %>%
  mutate(ymin=pmin(deaths, max))

tiff("Outputs/NISRAWeeklyDeaths.tiff", units="in", width=10, height=8, res=300)
ggplot()+
  geom_ribbon(data=data.NI.new, aes(x=weekno, ymin=ymin, ymax=deaths), fill="Red", alpha=0.2)+
  geom_ribbon(data=data.NI.old, aes(x=weekno, ymin=min, ymax=max), fill="Skyblue2")+
  geom_line(data=data.NI.old, aes(x=weekno, y=mean), colour="Grey50", linetype=2)+
  geom_line(data=data.NI.new, aes(x=weekno, y=deaths), colour="Red")+
  theme_classic()+
  scale_x_continuous(name="Week number", breaks=c(0,10,20,30,40,50))+
  scale_y_continuous(name="Deaths registered")+
  expand_limits(y=0)+
  labs(title="Deaths in Northern Ireland look to have plateaued",
       subtitle="Weekly deaths in 2020 compared to the range in 2010-19. Data up to 19th April",
       caption="Data from NISRA | Plot by @VictimOfMaths")+
  annotate(geom="text", x=16.5, y=400, label="Unprecedented excess deaths", colour="Red", hjust=0)+
  annotate(geom="text", x=30, y=320, label="Historic maximum", colour="Skyblue4")+
  annotate(geom="text", x=30, y=160, label="Historic minimum", colour="Skyblue4")+
  annotate(geom="text", x=47, y=220, label="Historic mean", colour="grey30")+
  geom_curve(aes(x=48, y=230, xend=47, yend=285), colour="grey30", curvature=0.15,
             arrow=arrow(length=unit(0.1, "cm"), type="closed"), lineend="round")

dev.off()  

###############################
#Plots for the whole of the UK#
###############################

#Extract max/min values
#split off 2020 data
data.reg.UK.new <- subset(data.reg.UK, year==2020)
data.reg.UK.old <- subset(data.reg.UK, year<2020)

data.reg.UK.old <- data.reg.UK.old %>%
  group_by(weekno, reg) %>%
  summarise(max=max(deaths), min=min(deaths), mean=mean(deaths))

#Generate filled area for total excess deaths vs. previous 10-year maximum
data.reg.UK.new <- merge(data.reg.UK.new, data.reg.UK.old, by=c("weekno", "reg"))
data.reg.UK.new <- data.reg.UK.new %>%
  group_by(reg) %>%
  mutate(ymin=pmin(deaths, max))

#Calculate excess deaths vs. mean so far this year
data.reg.UK.new$excess <- data.reg.UK.new$deaths-data.reg.UK.new$mean
reg.UK.excess <- data.reg.UK.new %>%
  group_by(reg) %>%
  summarise(excess=sum(excess))

data.reg.UK.new <- data.reg.UK.new %>%
  group_by(reg) %>%
  mutate(maxexcess=max(excess))

data.reg.UK.new$reg <- fct_reorder(data.reg.UK.new$reg, -data.reg.UK.new$maxexcess)
data.reg.UK.old$reg <- factor(data.reg.UK.old$reg, levels=levels(data.reg.UK.new$reg))

ann_text4 <- data.frame(weekno=c(16.5, 28, 28), deaths=c(2000, 1100,600), reg=rep("London", times=3))

tiff("Outputs/ONSNRSNISRAWeeklyDeathsxReg.tiff", units="in", width=12, height=8, res=300)
ggplot()+
  geom_ribbon(data=data.reg.UK.new, aes(x=weekno, ymin=ymin, ymax=deaths), fill="Red", alpha=0.2)+
  geom_ribbon(data=data.reg.UK.old, aes(x=weekno, ymin=min, ymax=max), fill="Skyblue2")+
  geom_line(data=data.reg.UK.old, aes(x=weekno, y=mean), colour="Grey50", linetype=2)+
  geom_line(data=data.reg.UK.new, aes(x=weekno, y=deaths), colour="Red")+
  theme_classic()+
  facet_wrap(~reg)+
  scale_x_continuous(name="Week number", breaks=c(0,10,20,30,40,50))+
  scale_y_continuous(name="Deaths registered")+
  expand_limits(y=0)+
  labs(title="Deaths from all causes have risen sharply, but not equally, across the UK",
       subtitle="Weekly deaths in 2020 compared to the range in 2010-19\nEngland, Wales & Northern Ireland data to April 17th\nScotland data to April 26th",
       caption="Data from ONS, NRS & NISRA | Plot by @VictimOfMaths")+
  theme(strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)))+
  geom_text(data=ann_text4, aes(x=weekno, y=deaths), label=c("Unprecedented excess deaths\nin 2020","Max", "Min"), size=3, 
            colour=c("Red", "deepskyblue4", "deepskyblue4"), hjust=0)
dev.off() 

#Generate cumulative death counts by year
data.reg.UK <- data.reg.UK %>%
  group_by(reg, year) %>%
  mutate(cumul_deaths=cumsum(deaths))

ann_text5 <- data.frame(weekno=16, cumul_deaths=27000, reg="London")

tiff("Outputs/ONSNRSNISRAWeeklyCumulDeaths_reg.tiff", units="in", width=12, height=8, res=300)
ggplot()+
  geom_line(data=subset(data.reg.UK, year!=2020), aes(x=weekno, y=cumul_deaths, group=as.factor(year)), colour="Grey80")+
  geom_line(data=subset(data.reg.UK, year==2020), aes(x=weekno, y=cumul_deaths), colour="Red")+
  theme_classic()+
  facet_wrap(~reg)+
  facet_wrap(~reg)+
  scale_x_continuous(name="Week number")+
  scale_y_continuous(name="Deaths registered")+
  theme(strip.background=element_blank(), strip.text=element_text(face="bold"))+
  labs(title="Cumulative deaths in the UK from all causes in 2020 vs. 2010-2019",
       caption="Data from ONS, NRS & NISRA | Plot by @VictimOfMaths")+
  geom_text(data=ann_text5, aes(x=weekno, y=cumul_deaths), label=c("2020"), size=3, colour="Red")
dev.off()
