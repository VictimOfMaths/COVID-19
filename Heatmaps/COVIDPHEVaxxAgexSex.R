rm(list=ls())

library(tidyverse)
library(curl)
library(readxl)
library(extrafont)
library(ggtext)
library(ragg)

#Download data from PHE surveillance report
#https://www.gov.uk/government/statistics/national-flu-and-covid-19-surveillance-reports

url <- "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/979626/Weekly_Influenza_and_COVID19_report_data_w16.xlsx"

temp <- tempfile()
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

data <- read_excel(temp, sheet="Figure 58&59 COVID Vac Age Sex", range="B13:N23", col_names=FALSE) %>% 
  select(`...1`, `...2`, `...3`, `...5`, `...6`, `...9`, `...12`)

colnames(data) <- c("Age", "Male_Pop", "Male_Vax1", "Female_Pop", "Female_Vax1", "Male_Vax2",
                     "Female_Vax2")

data <- data %>% 
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
           Age=="Under 20 years" ~ "<20",
           Age=="20 to 29 years" ~ "20-29",
           Age=="30 to 39 years" ~ "30-39",
           Age=="40 to 49 years" ~ "40-49",
           Age=="50 to 54 years" ~ "50-54",
           Age=="55 to 59 years" ~ "55-59",
           Age=="60 to 64 years" ~ "60-64",
           Age=="65 to 69 years" ~ "65-69",
           Age=="70 to 74 years" ~ "70-74",
           Age=="75 to 79 years" ~ "75-79",
           TRUE ~ "80+"),
         Age=factor(Age, levels=c("<20", "20-29", "30-39", "40-49", "50-54", "55-59", "60-64",
                                  "65-69", "70-74", "75-79", "80+")),
         SexMeasure=paste0(Sex, Measure))

agg_tiff("Outputs/COVIDVaxxAgexSex.tiff", units="in", width=8, height=7, res=800)
ggplot(data_long, aes(x=Value, y=Age, fill=SexMeasure))+
  geom_col(position="stack", show.legend=FALSE)+
  scale_x_continuous(breaks=c(-7500000, -5000000, -2500000, 0, 2500000, 5000000, 7500000),
                     labels=c("7.5m", "5m", "2.5m", "0", "2.5m", "5m", "7.5m"),
                     limits=c(-7500000, 7500000), name="")+
  scale_y_discrete(name="Age group")+
  scale_fill_manual(values=c("Grey80", "#7dffdd", "#00cc99", "Grey80", "#be7dff", "#6600cc"))+
  theme_classic()+
  theme(axis.line.y=element_blank(), text=element_text(family="Roboto"), 
        plot.title=element_text(face="bold", size=rel(1.4)), plot.subtitle=element_markdown())+
  labs(title="Vaccine delivery in England by age and sex",
       subtitle="The number of people who are <span style='color:Grey60;'>unvaccinated</span>, men with <span style='color:#be7dff;'>one</span> or <span style='color:#6600cc;'>two</span> doses and women with <span style='color:#7dffdd;'>one</span> or <span style='color:#00cc99;'>two</span> doses",
       caption="Data from PHE, population figures from NIMS\nPlot by @VictimOfMaths")+
  annotate("text", x=-4000000, y=9, label="Men", colour="#6600cc", size=rel(6), fontface="bold")+
  annotate("text", x=4000000, y=9, label="Women", colour="#00cc99", size=rel(6), fontface="bold")
dev.off()  
