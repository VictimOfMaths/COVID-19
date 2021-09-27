rm(list=ls())

library(tidyverse)
library(curl)
library(readxl)
library(extrafont)
library(ggtext)
library(ragg)
library(lubridate)
library(gganimate)

#Download data from PHE surveillance report
#https://www.gov.uk/government/statistics/national-flu-and-covid-19-surveillance-reports-2021-to-2022-season


url <- "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/1020037/Weekly_Influenza_and_COVID19_report_data_W38_v2.xlsx"

temp <- tempfile()
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

data <- read_excel(temp, sheet="Figure 62&63. COVID Vac Age Sex", range="B13:N30", col_names=FALSE) %>% 
  select(`...1`, `...2`, `...3`, `...5`, `...6`, `...9`, `...12`)

colnames(data) <- c("Age", "Male_Pop", "Male_Vax1", "Female_Pop", "Female_Vax1", "Male_Vax2",
                     "Female_Vax2")

data <- data %>% 
  filter(Age!="18 to under 25") %>% 
  rowwise() %>% 
  mutate(Male_Unvax=Male_Pop-Male_Vax1,
         Female_Unvax=Female_Pop-Female_Vax1,
         Male_Vax1Only=Male_Vax1-Male_Vax2,
         Female_Vax1Only=Female_Vax1-Female_Vax2) %>% 
  ungroup() %>% 
  select(Age, Male_Unvax, Female_Unvax, Male_Vax1Only, Female_Vax1Only, Male_Vax2, Female_Vax2)

data_long <- pivot_longer(data, cols=c(2:7), names_to=c("Sex", "Measure"), names_sep="_", 
                          values_to="Value") %>% 
  mutate(Value=if_else(Sex=="Male", -Value, Value),
         Age=case_when(
           Age=="Under 12" ~ "<12",
           Age=="12 to under 16" ~ "12-15",
           Age=="16 to under 18" ~ "16-17",
           Age=="18 to under 20" ~ "18-19",
           Age=="20 to under 25" ~ "20-24",
           Age=="25 to under 30" ~ "25-29",
           Age=="30 to under 35" ~ "30-34",
           Age=="35 to under 40" ~ "35-39",
           Age=="40 to under 45" ~ "40-44",
           Age=="45 to under 50" ~ "45-49",
           Age=="50 to under 55" ~ "50-54",
           Age=="55 to under 60" ~ "55-59",
           Age=="60 to under 65" ~ "60-64",
           Age=="65 to under 70" ~ "65-69",
           Age=="70 to under 75" ~ "70-74",
           Age=="75 to under 80" ~ "75-79",
           TRUE ~ "80+"),
         Age=factor(Age, levels=c("<12", "12-15", "16-17", "18-19", "20-24", "25-29", "30-34", "35-39", 
                                  "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", 
                                  "80+")),
         SexMeasure=paste0(Sex, Measure))

agg_tiff("Outputs/COVIDVaxxAgexSex.tiff", units="in", width=8, height=7, res=800)
ggplot(data_long, aes(x=Value, y=Age, fill=SexMeasure))+
  geom_col(position="stack", show.legend=FALSE)+
  scale_x_continuous(breaks=c(-5000000, -2500000, 0, 2500000, 5000000),
                     labels=c("5m", "2.5m", "0", "2.5m", "5m"),
                     limits=c(-5000000, 5000000), name="")+
  scale_y_discrete(name="Age group")+
  scale_fill_manual(values=c("Grey80", "#7dffdd", "#00cc99", "Grey80", "#be7dff", "#6600cc"))+
  theme_classic()+
  theme(axis.line.y=element_blank(), text=element_text(family="Lato"), 
        plot.title=element_text(face="bold", size=rel(1.6)), plot.subtitle=element_markdown())+
  labs(title="Vaccine delivery in England by age and sex",
       subtitle="The number of people who are <span style='color:Grey60;'>unvaccinated</span>, men with <span style='color:#be7dff;'>one</span> or <span style='color:#6600cc;'>two</span> doses and women with <span style='color:#7dffdd;'>one</span> or <span style='color:#00cc99;'>two</span> doses",
       caption="Data from PHE, population figures from NIMS\nPlot by @VictimOfMaths")+
  annotate("text", x=-4000000, y=9, label="Men", colour="#6600cc", size=rel(6), 
           fontface="bold", family="Lato")+
  annotate("text", x=4000000, y=9, label="Women", colour="#00cc99", size=rel(6), 
           fontface="bold", family="Lato")
dev.off()  

agg_tiff("Outputs/COVIDVaxxAgexSexv2.tiff", units="in", width=8, height=7, res=800)
ggplot(data_long %>% 
         mutate(SexMeasure=factor(SexMeasure, levels=c("FemaleVax2", "FemaleVax1Only", "FemaleUnvax",
                                                       "MaleVax2", "MaleVax1Only", "MaleUnvax"))), 
       aes(x=Value, y=Age, fill=SexMeasure))+
  geom_col(position="stack", show.legend=FALSE)+
  geom_segment(aes(x=0, xend=0, y=0.5, yend=17.5))+
  scale_x_continuous(breaks=c(-5000000, -2500000, 0, 2500000, 5000000),
                     labels=c("5m", "2.5m", "0", "2.5m", "5m"),
                     limits=c(-5000000, 5000000), name="")+
  scale_y_discrete(name="Age group")+
  scale_fill_manual(values=c("#00cc99", "#7dffdd", "Grey80",  "#6600cc", "#be7dff","Grey80"))+
  theme_classic()+
  theme(axis.line.y=element_blank(), text=element_text(family="Lato"), 
        plot.title=element_text(face="bold", size=rel(1.6)), plot.subtitle=element_markdown())+
  labs(title="Vaccine delivery in England by age and sex",
       subtitle="The number of people who are <span style='color:Grey60;'>unvaccinated</span>, men with <span style='color:#be7dff;'>one</span> or <span style='color:#6600cc;'>two</span> doses and women with <span style='color:#7dffdd;'>one</span> or <span style='color:#00cc99;'>two</span> doses",
       caption="Data from PHE, population figures from NIMS\nPlot by @VictimOfMaths")+
  annotate("text", x=-4000000, y=9, label="Men", colour="#6600cc", size=rel(6), 
           fontface="bold", family="Lato")+
  annotate("text", x=4000000, y=9, label="Women", colour="#00cc99", size=rel(6), 
           fontface="bold", family="Lato")
dev.off()

#Bring in ONS 2020 population figures for comparison
url <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fpopulationandmigration%2fpopulationestimates%2fdatasets%2fpopulationestimatesforukenglandandwalesscotlandandnorthernireland%2fmid2020/ukpopestimatesmid2020on2021geography.xls"

temp <- tempfile()
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

ONSpop_m <- read_excel(temp, sheet="MYE2 - Males", range="E8:CQ12") %>% 
  slice_tail() %>% 
  gather(Age, pop_m, c(1:ncol(.)))

ONSpop_f <- read_excel(temp, sheet="MYE2 - Females", range="E8:CQ12") %>% 
  slice_tail() %>% 
  gather(Age, pop_f, c(1:ncol(.)))

ONSpop <- merge(ONSpop_m, ONSpop_f) %>% 
  mutate(Age=as.numeric(substr(Age, 1, 2)),
         Age=case_when(
           Age<12 ~ "Under 12",
           Age<16 ~ "12 to under 16",
           Age<18 ~ "16 to under 18",
           Age<20 ~ "18 to under 20",
           Age<25 ~ "20 to under 25",
           Age<30 ~ "25 to under 30",
           Age<35 ~ "30 to under 35",
           Age<40 ~ "35 to under 40",
           Age<45 ~ "40 to under 45",
           Age<50 ~ "45 to under 50",
           Age<55 ~ "50 to under 55",
           Age<60 ~ "55 to under 60",
           Age<65 ~ "60 to under 65",
           Age<70 ~ "65 to under 70",
           Age<75 ~ "70 to under 75",
           Age<80 ~ "75 to under 80",
           TRUE ~ "Over 80")) %>% 
  group_by(Age) %>% 
  summarise(pop_m=sum(pop_m), pop_f=sum(pop_f)) %>% 
  ungroup()

ONSdata <- merge(data, ONSpop) %>% 
  mutate(Male_Unvax=pop_m-Male_Vax1Only-Male_Vax2,
         Female_Unvax=pop_f-Female_Vax1Only-Female_Vax2)

ONSdata_long <- pivot_longer(ONSdata, cols=c(2:7), names_to=c("Sex", "Measure"), names_sep="_", 
                          values_to="Value") %>% 
  mutate(Value=if_else(Sex=="Male", -Value, Value),
         Age=case_when(
           Age=="Under 12" ~ "<12",
           Age=="12 to under 16" ~ "12-15",
           Age=="16 to under 18" ~ "16-17",
           Age=="18 to under 20" ~ "18-19",
           Age=="20 to under 25" ~ "20-24",
           Age=="25 to under 30" ~ "25-29",
           Age=="30 to under 35" ~ "30-34",
           Age=="35 to under 40" ~ "35-39",
           Age=="40 to under 45" ~ "40-44",
           Age=="45 to under 50" ~ "45-49",
           Age=="50 to under 55" ~ "50-54",
           Age=="55 to under 60" ~ "55-59",
           Age=="60 to under 65" ~ "60-64",
           Age=="65 to under 70" ~ "65-69",
           Age=="70 to under 75" ~ "70-74",
           Age=="75 to under 80" ~ "75-79",
           TRUE ~ "80+"),
         Age=factor(Age, levels=c("<12", "12-15", "16-17", "18-19", "20-24", "25-29", "30-34", "35-39", 
                                  "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", 
                                  "80+")),
         SexMeasure=paste0(Sex, Measure))

agg_tiff("Outputs/COVIDVaxxAgexSexONS.tiff", units="in", width=8, height=7, res=800)
ggplot(ONSdata_long, aes(x=Value, y=Age, fill=SexMeasure))+
  geom_col(position="stack", show.legend=FALSE)+
  scale_x_continuous(breaks=c(-5000000, -2500000, 0, 2500000, 5000000),
                     labels=c("5m", "2.5m", "0", "2.5m", "5m"),
                     limits=c(-5000000, 5000000), name="")+
  scale_y_discrete(name="Age group")+
  scale_fill_manual(values=c("Grey80", "#7dffdd", "#00cc99", "Grey80", "#be7dff", "#6600cc"))+
  theme_classic()+
  theme(axis.line.y=element_blank(), text=element_text(family="Lato"), 
        plot.title=element_text(face="bold", size=rel(1.6)), plot.subtitle=element_markdown())+
  labs(title="Vaccine delivery in England by age and sex",
       subtitle="The number of people who are <span style='color:Grey60;'>unvaccinated</span>, men with <span style='color:#be7dff;'>one</span> or <span style='color:#6600cc;'>two</span> doses and women with <span style='color:#7dffdd;'>one</span> or <span style='color:#00cc99;'>two</span> doses",
       caption="Data from PHE, population figures from ONS\nPlot by @VictimOfMaths")+
  annotate("text", x=-4000000, y=9, label="Men", colour="#6600cc", size=rel(6), 
           fontface="bold", family="Lato")+
  annotate("text", x=4000000, y=9, label="Women", colour="#00cc99", size=rel(6), 
           fontface="bold", family="Lato")
dev.off()  

agg_tiff("Outputs/COVIDVaxxAgexSexONSv2.tiff", units="in", width=8, height=7, res=800)
ggplot(ONSdata_long %>% 
         mutate(SexMeasure=factor(SexMeasure, levels=c("FemaleVax2", "FemaleVax1Only", "FemaleUnvax",
                                                       "MaleVax2", "MaleVax1Only", "MaleUnvax"))), 
       aes(x=Value, y=Age, fill=SexMeasure))+
  geom_col(position="stack", show.legend=FALSE)+
  geom_segment(aes(x=0, xend=0, y=0.5, yend=17.5))+
  scale_x_continuous(breaks=c(-5000000, -2500000, 0, 2500000, 5000000),
                     labels=c("5m", "2.5m", "0", "2.5m", "5m"),
                     limits=c(-5000000, 5000000), name="")+
  scale_y_discrete(name="Age group")+
  scale_fill_manual(values=c("#00cc99", "#7dffdd", "Grey80",  "#6600cc", "#be7dff","Grey80"))+
  theme_classic()+
  theme(axis.line.y=element_blank(), text=element_text(family="Lato"), 
        plot.title=element_text(face="bold", size=rel(1.6)), plot.subtitle=element_markdown())+
  labs(title="Vaccine delivery in England by age and sex",
       subtitle="The number of people who are <span style='color:Grey60;'>unvaccinated</span>, men with <span style='color:#be7dff;'>one</span> or <span style='color:#6600cc;'>two</span> doses and women with <span style='color:#7dffdd;'>one</span> or <span style='color:#00cc99;'>two</span> doses",
       caption="Data from PHE, population figures from ONS\nPlot by @VictimOfMaths")+
  annotate("text", x=-4000000, y=9, label="Men", colour="#6600cc", size=rel(6), 
           fontface="bold", family="Lato")+
  annotate("text", x=4000000, y=9, label="Women", colour="#00cc99", size=rel(6), 
           fontface="bold", family="Lato")
dev.off()  

#####################################

#Produce version with compressed age bands
#V1 10 year bands for >20
agg_tiff("Outputs/COVIDVaxxAgexSexv2.tiff", units="in", width=8, height=6, res=800)
data_long %>% 
  mutate(Age=case_when(
    Age %in% c("18-24", "25-29") ~ "18-29",
    Age %in% c("30-34", "35-39") ~ "30-39",
    Age %in% c("40-44", "45-49") ~ "40-49",
    Age %in% c("50-54", "55-59") ~ "50-59",
    Age %in% c("60-64", "65-69") ~ "60-69",
    Age %in% c("70-74", "75-79") ~ "70-79",
    TRUE ~ as.character(Age))) %>% 
  group_by(Age, SexMeasure) %>% 
  summarise(Value=sum(Value)) %>% 
  ungroup() %>% 
  ggplot(aes(x=Value, y=Age, fill=SexMeasure))+
  geom_col(position="stack", show.legend=FALSE)+
  scale_x_continuous(breaks=c(-7500000, -5000000, -2500000, 0, 2500000, 5000000, 7500000),
                     labels=c("7.5m", "5m", "2.5m", "0", "2.5m", "5m", "7.5m"),
                     limits=c(-7500000, 7500000), name="")+
  scale_y_discrete(name="Age group")+
  scale_fill_manual(values=c("Grey80", "#7dffdd", "#00cc99", "Grey80", "#be7dff", "#6600cc"))+
  theme_classic()+
  theme(axis.line.y=element_blank(), text=element_text(family="Lato"), 
        plot.title=element_text(face="bold", size=rel(1.4)), plot.subtitle=element_markdown())+
  labs(title="Vaccine delivery in England by age and sex",
       subtitle="The number of people who are <span style='color:Grey60;'>unvaccinated</span>, men with <span style='color:#be7dff;'>one</span> or <span style='color:#6600cc;'>two</span> doses and women with <span style='color:#7dffdd;'>one</span> or <span style='color:#00cc99;'>two</span> doses",
       caption="Data from PHE, population figures from NIMS\nPlot by @VictimOfMaths")+
  annotate("text", x=-5000000, y=6.5, label="Men", colour="#6600cc", size=rel(6), fontface="bold",
           family="Lato")+
  annotate("text", x=5000000, y=6.5, label="Women", colour="#00cc99", size=rel(6), fontface="bold",
           family="Lato")
dev.off()  

#V2 20 year bands
agg_tiff("Outputs/COVIDVaxxAgexSexv3.tiff", units="in", width=8, height=5, res=800)
data_long %>% 
  mutate(Age=case_when(
    Age %in% c("18-24", "25-29", "30-34", "35-39") ~ "18-39",
    Age %in% c("40-44", "45-49", "50-54", "55-59") ~ "40-59",
    Age %in% c("60-64", "65-69", "70-74", "75-79") ~ "60-79",
    TRUE ~ as.character(Age))) %>% 
  group_by(Age, SexMeasure) %>% 
  summarise(Value=sum(Value)) %>% 
  ungroup() %>% 
  ggplot(aes(x=Value, y=Age, fill=SexMeasure))+
  geom_col(position="stack", show.legend=FALSE)+
  scale_x_continuous(breaks=c(-7500000, -5000000, -2500000, 0, 2500000, 5000000, 7500000),
                     labels=c("7.5m", "5m", "2.5m", "0", "2.5m", "5m", "7.5m"),
                     limits=c(NA,NA), name="")+
  scale_y_discrete(name="Age group")+
  scale_fill_manual(values=c("Grey80", "#7dffdd", "#00cc99", "Grey80", "#be7dff", "#6600cc"))+
  theme_classic()+
  theme(axis.line.y=element_blank(), text=element_text(family="Lato"), 
        plot.title=element_text(face="bold", size=rel(1.4)), plot.subtitle=element_markdown())+
  labs(title="Vaccine delivery in England by age and sex",
       subtitle="The number of people who are <span style='color:Grey60;'>unvaccinated</span>, men with <span style='color:#be7dff;'>one</span> or <span style='color:#6600cc;'>two</span> doses and women with <span style='color:#7dffdd;'>one</span> or <span style='color:#00cc99;'>two</span> doses",
       caption="Data from PHE, population figures from NIMS\nPlot by @VictimOfMaths")+
  annotate("text", x=-5000000, y=5, label="Men", colour="#6600cc", size=rel(6), fontface="bold",
           family="Lato")+
  annotate("text", x=5000000, y=5, label="Women", colour="#00cc99", size=rel(6), fontface="bold",
           family="Lato")
dev.off()  

#Animated versions
#Read in historic data
#Week 29
url <- "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/1007253/Weekly_Influenza_and_COVID19_report_data_W30.xlsx"

temp <- tempfile()
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

dataw29 <- read_excel(temp, sheet="Figure 62&63. COVID Vac Age Sex", range="B13:N26", col_names=FALSE) %>% 
  select(`...1`, `...2`, `...3`, `...5`, `...6`, `...9`, `...12`)

colnames(dataw29) <- c("Age", "Male_Pop", "Male_Vax1", "Female_Pop", "Female_Vax1", "Male_Vax2",
                    "Female_Vax2")

#Week 28
url <- "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/1005058/Weekly_Influenza_and_COVID19_report_data_W29.xlsx"

temp <- tempfile()
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

dataw28 <- read_excel(temp, sheet="Figure 62&63. COVID Vac Age Sex", range="B13:N26", col_names=FALSE) %>% 
  select(`...1`, `...2`, `...3`, `...5`, `...6`, `...9`, `...12`)

colnames(dataw28) <- c("Age", "Male_Pop", "Male_Vax1", "Female_Pop", "Female_Vax1", "Male_Vax2",
                    "Female_Vax2")

#Week 27
url <- "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/1002563/Weekly_Influenza_and_COVID19_report_data_w28.xlsx"

temp <- tempfile()
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

dataw27 <- read_excel(temp, sheet="Figure 61&62. COVID Vac Age Sex", range="B13:N26", col_names=FALSE) %>% 
  select(`...1`, `...2`, `...3`, `...5`, `...6`, `...9`, `...12`)

colnames(dataw27) <- c("Age", "Male_Pop", "Male_Vax1", "Female_Pop", "Female_Vax1", "Male_Vax2",
                    "Female_Vax2")


#Week 26
url <- "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/1000375/Weekly_Influenza_and_COVID19_report_data_w27.xlsx"

temp <- tempfile()
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

dataw26 <- read_excel(temp, sheet="Figure 57&58. COVID Vac Age Sex", range="B13:N26", col_names=FALSE) %>% 
  select(`...1`, `...2`, `...3`, `...5`, `...6`, `...9`, `...12`)

colnames(dataw26) <- c("Age", "Male_Pop", "Male_Vax1", "Female_Pop", "Female_Vax1", "Male_Vax2",
                    "Female_Vax2")

#Week 25

url <- "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/998394/Weekly_Influenza_and_COVID19_report_data_W26.xlsx"

temp <- tempfile()
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

dataw25 <- read_excel(temp, sheet="Figure 57&58. COVID Vac Age Sex", range="B13:N26", col_names=FALSE) %>% 
  select(`...1`, `...2`, `...3`, `...5`, `...6`, `...9`, `...12`)

colnames(dataw25) <- c("Age", "Male_Pop", "Male_Vax1", "Female_Pop", "Female_Vax1", "Male_Vax2",
                    "Female_Vax2")


#Week 24
url <- "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/996366/Weekly_Influenza_and_COVID19_report_data_w25.xlsx"

temp <- tempfile()
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

dataw24 <- read_excel(temp, sheet="Figure 57&58. COVID Vac Age Sex", range="B13:N26", col_names=FALSE) %>% 
  select(`...1`, `...2`, `...3`, `...5`, `...6`, `...9`, `...12`)

colnames(dataw24) <- c("Age", "Male_Pop", "Male_Vax1", "Female_Pop", "Female_Vax1", "Male_Vax2",
                    "Female_Vax2")

#Week 23
url <- "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/994574/Weekly_Influenza_and_COVID19_report_data_w24.xlsx"

temp <- tempfile()
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

dataw23 <- read_excel(temp, sheet="Figure 57&58. COVID Vac Age Sex", range="B13:N26", col_names=FALSE) %>% 
  select(`...1`, `...2`, `...3`, `...5`, `...6`, `...9`, `...12`)

colnames(dataw23) <- c("Age", "Male_Pop", "Male_Vax1", "Female_Pop", "Female_Vax1", "Male_Vax2",
                    "Female_Vax2")

#Week 22
url <- "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/992616/Weekly_Influenza_and_COVID19_report_data_w23.xlsx"

temp <- tempfile()
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

dataw22 <- read_excel(temp, sheet="Figure 55&56 COVID Vac Age Sex", range="B13:N26", col_names=FALSE) %>% 
  select(`...1`, `...2`, `...3`, `...5`, `...6`, `...9`, `...12`)

colnames(dataw22) <- c("Age", "Male_Pop", "Male_Vax1", "Female_Pop", "Female_Vax1", "Male_Vax2",
                    "Female_Vax2")


#Week 21
url <- "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/991080/Weekly_Influenza_and_COVID19_report_data_w22.xlsx"

temp <- tempfile()
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

dataw21 <- read_excel(temp, sheet="Figure 55&56 COVID Vac Age Sex", range="B13:N26", col_names=FALSE) %>% 
  select(`...1`, `...2`, `...3`, `...5`, `...6`, `...9`, `...12`)

colnames(dataw21) <- c("Age", "Male_Pop", "Male_Vax1", "Female_Pop", "Female_Vax1", "Male_Vax2",
                    "Female_Vax2")

#Week 20
url <- "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/989842/Weekly_Influenza_and_COVID19_report_data_W21.xlsx"

temp <- tempfile()
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

dataw20 <- read_excel(temp, sheet="Figure 58&59 COVID Vac Age Sex", range="B13:N26", col_names=FALSE) %>% 
  select(`...1`, `...2`, `...3`, `...5`, `...6`, `...9`, `...12`)

colnames(dataw20) <- c("Age", "Male_Pop", "Male_Vax1", "Female_Pop", "Female_Vax1", "Male_Vax2",
                    "Female_Vax2")

#Week 19
url <- "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/988026/Weekly_Influenza_and_COVID19_report_data_w20.xlsx"

temp <- tempfile()
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

dataw19 <- read_excel(temp, sheet="Figure 58&59 COVID Vac Age Sex", range="B13:N23", col_names=FALSE) %>% 
  select(`...1`, `...2`, `...3`, `...5`, `...6`, `...9`, `...12`)

colnames(dataw19) <- c("Age", "Male_Pop", "Male_Vax1", "Female_Pop", "Female_Vax1", "Male_Vax2",
                    "Female_Vax2")


#Week 18
url <- "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/986164/Weekly_Influenza_and_COVID19_report_data_w19.xlsx"

temp <- tempfile()
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

dataw18 <- read_excel(temp, sheet="Figure 58&59 COVID Vac Age Sex", range="B13:N23", col_names=FALSE) %>% 
  select(`...1`, `...2`, `...3`, `...5`, `...6`, `...9`, `...12`)

colnames(dataw18) <- c("Age", "Male_Pop", "Male_Vax1", "Female_Pop", "Female_Vax1", "Male_Vax2",
                    "Female_Vax2")

#Week 17
url <- "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/983708/Weekly_Influenza_and_COVID19_report_data_w18.xlsx"

temp <- tempfile()
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

dataw17 <- read_excel(temp, sheet="Figure 58&59 COVID Vac Age Sex", range="B13:N23", col_names=FALSE) %>% 
  select(`...1`, `...2`, `...3`, `...5`, `...6`, `...9`, `...12`)

colnames(dataw17) <- c("Age", "Male_Pop", "Male_Vax1", "Female_Pop", "Female_Vax1", "Male_Vax2",
                    "Female_Vax2")

#Week 16
url <- "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/982284/Weekly_Influenza_and_COVID19_report_data_w17.xlsx"

temp <- tempfile()
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

dataw16 <- read_excel(temp, sheet="Figure 58&59 COVID Vac Age Sex", range="B13:N23", col_names=FALSE) %>% 
  select(`...1`, `...2`, `...3`, `...5`, `...6`, `...9`, `...12`)

colnames(dataw16) <- c("Age", "Male_Pop", "Male_Vax1", "Female_Pop", "Female_Vax1", "Male_Vax2",
                       "Female_Vax2")

#Week 15
url <- "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/979626/Weekly_Influenza_and_COVID19_report_data_w16.xlsx"

temp <- tempfile()
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

dataw15 <- read_excel(temp, sheet="Figure 58&59 COVID Vac Age Sex", range="B13:N23", col_names=FALSE) %>% 
  select(`...1`, `...2`, `...3`, `...5`, `...6`, `...9`, `...12`)

colnames(dataw15) <- c("Age", "Male_Pop", "Male_Vax1", "Female_Pop", "Female_Vax1", "Male_Vax2",
                    "Female_Vax2")


#Week 14
dataw14 <- read_excel(temp, sheet="Figure 58&59 COVID Vac Age Sex", range="B31:N41", col_names=FALSE) %>% 
  select(`...1`, `...2`, `...3`, `...5`, `...6`, `...9`, `...12`)

colnames(dataw14) <- c("Age", "Male_Pop", "Male_Vax1", "Female_Pop", "Female_Vax1", "Male_Vax2",
                       "Female_Vax2")

#Data for weeks prior to 21 uses different age bands. Aligning them is a project for the interested reader...

mergeddata <- dataw29 %>% mutate(Week=29) %>% 
  bind_rows(dataw28 %>% mutate(Week=28)) %>% 
  bind_rows(dataw27 %>% mutate(Week=27)) %>% 
  bind_rows(dataw26 %>% mutate(Week=26)) %>% 
  bind_rows(dataw25 %>% mutate(Week=25)) %>% 
  bind_rows(dataw24 %>% mutate(Week=24)) %>% 
  bind_rows(dataw23 %>% mutate(Week=23)) %>% 
  bind_rows(dataw22 %>% mutate(Week=22)) %>% 
  bind_rows(dataw21 %>% mutate(Week=21)) %>% 
  rowwise() %>% 
  mutate(Male_Unvax=Male_Pop-Male_Vax1,
         Female_Unvax=Female_Pop-Female_Vax1,
         Male_Vax1Only=Male_Vax1-Male_Vax2,
         Female_Vax1Only=Female_Vax1-Female_Vax2) %>% 
  ungroup() %>% 
  select(Age, Week, Male_Unvax, Female_Unvax, Male_Vax1Only, Female_Vax1Only, Male_Vax2, Female_Vax2) %>% 
  bind_rows(data %>% mutate(Week=30))

mergeddata_long <- pivot_longer(mergeddata, cols=c(3:8), names_to=c("Sex", "Measure"), names_sep="_", 
                          values_to="Value") %>% 
  mutate(Value=if_else(Sex=="Male", -Value, Value),
         Age=case_when(
           Age=="Under 18 years" ~ "<18",
           Age=="18 to 24 years" ~ "18-24",
           Age=="25 to 29 years" ~ "25-29",
           Age=="30 to 34 years" ~ "30-34",
           Age=="35 to 39 years" ~ "35-39",
           Age=="40 to 44 years" ~ "40-44",
           Age=="45 to 49 years" ~ "45-49",
           Age=="50 to 54 years" ~ "50-54",
           Age=="55 to 59 years" ~ "55-59",
           Age=="60 to 64 years" ~ "60-64",
           Age=="65 to 69 years" ~ "65-69",
           Age=="70 to 74 years" ~ "70-74",
           Age=="75 to 79 years" ~ "75-79",
           TRUE ~ "80+"),
         Age=factor(Age, levels=c("<18", "18-24", "25-29", "30-34", "35-39", "40-44", "45-49",
                                  "50-54", "55-59", "60-64",
                                  "65-69", "70-74", "75-79", "80+")),
         SexMeasure=paste0(Sex, Measure),
         date=as.Date("2021-04-11")+weeks(Week-14)) %>% 
  arrange(Week)

anim <- ggplot(mergeddata_long, aes(x=Value, y=Age, fill=SexMeasure))+
  geom_col(position="stack", show.legend=FALSE)+
  scale_x_continuous(breaks=c(-7500000, -5000000, -2500000, 0, 2500000, 5000000, 7500000),
                     labels=c("7.5m", "5m", "2.5m", "0", "2.5m", "5m", "7.5m"),
                     limits=c(-7500000, 7500000), name="")+
  scale_y_discrete(name="Age group")+
  scale_fill_manual(values=c("Grey80", "#7dffdd", "#00cc99", "Grey80", "#be7dff", "#6600cc"))+
  theme_classic()+
  theme(axis.line.y=element_blank(), text=element_text(family="Lato"), 
        plot.title=element_text(face="bold", size=rel(1.4)), plot.subtitle=element_markdown())+
  labs(caption="Data from PHE, population figures from NIMS\nPlot by @VictimOfMaths")+
  annotate("text", x=-4000000, y=9, label="Men", colour="#6600cc", size=rel(6), fontface="bold",
           family="Lato")+
  annotate("text", x=4000000, y=9, label="Women", colour="#00cc99", size=rel(6), fontface="bold",
           family="Lato")+
  transition_states(date, transition_length=2, state_length=1, wrap=FALSE)+
  ggtitle("Vaccine delivery in England by age and sex up to {closest_state}",
          subtitle="The number of people who are <span style='color:Grey60;'>unvaccinated</span>, men with <span style='color:#be7dff;'>one</span> or <span style='color:#6600cc;'>two</span> doses<br>and women with <span style='color:#7dffdd;'>one</span> or <span style='color:#00cc99;'>two</span> doses")

animate(anim, units="in", width=8, height=8*4/5, res=250, 
        renderer=gifski_renderer("Outputs/COVIDVaxPyramidAnim.gif"), 
        device="ragg_png", end_pause=5, duration=10, fps=8)
