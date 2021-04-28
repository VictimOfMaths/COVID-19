rm(list=ls())

library(tidyverse)
library(curl)
library(readxl)
library(showtext)

options(scipen=9999)

#Download Vaccination data
url <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/04/COVID-19-weekly-announced-vaccinations-22-April-2021.xlsx"

temp <- tempfile()
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

rawdata <- as.data.frame(t(read_excel(temp, sheet="Gender, Age & Region", range="C17:AN23", col_names=F)))

colnames(rawdata) <- c("East of England", "London", "Midlands", "North East & Yorkshire", "North West",
                    "South East", "South West")

data <- rawdata %>% 
  filter(!is.na(London)) %>% 
  mutate(Dose=rep(c("1st", "2nd"), each=18), Sex=rep(c("Male", "Female"), times=18),
         Age=rep(c("16-45", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80+"),
         each=2, times=2)) %>% 
  gather(Region, Vax, c(1:7)) %>% 
  spread(Dose, Vax)

#Bring in population data (based on ONS as NIMS is linked to lower level geographies only)
url <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/populationestimatesforukenglandandwalesscotlandandnorthernireland/mid2019april2020localauthoritydistrictcodes/ukmidyearestimates20192020ladcodes.xls"

temp <- tempfile()
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

malepop <- read_excel(temp, sheet="MYE2 - Males", range="B5:CQ431") %>% 
  filter(Geography1=="Region") %>% 
  gather(Age, Pop, c(4:94)) %>% 
  mutate(Region=case_when(
    Name=="NORTH WEST" ~ "North West",
    Name %in% c("EAST MIDLANDS", "WEST MIDLANDS") ~ "Midlands",
    Name=="EAST" ~ "East of England",
    Name=="LONDON" ~ "London",
    Name=="SOUTH EAST" ~ "South East",
    Name=="SOUTH WEST" ~ "South West",
    TRUE ~ "North East & Yorkshire"),
    Age=as.numeric(if_else(Age=="90+", "90", Age)),
    Age=case_when(
      Age<16~ "<16", Age<45 ~ "16-45", Age<50 ~ "45-49", Age<55 ~ "50-54", Age<60 ~ "55-59", Age<65 ~ "60-64",
      Age<70 ~ "65-69", Age<75 ~ "70-74", Age<80 ~ "75-79", TRUE ~ "80+")) %>% 
  group_by(Region, Age) %>% 
  summarise(Pop=sum(Pop)) %>% 
  ungroup() %>% 
  mutate(Sex="Male")

femalepop <- read_excel(temp, sheet="MYE2 - Females", range="B5:CQ431") %>% 
  filter(Geography1=="Region") %>% 
  gather(Age, Pop, c(4:94)) %>% 
  mutate(Region=case_when(
    Name=="NORTH WEST" ~ "North West",
    Name %in% c("EAST MIDLANDS", "WEST MIDLANDS") ~ "Midlands",
    Name=="EAST" ~ "East of England",
    Name=="LONDON" ~ "London",
    Name=="SOUTH EAST" ~ "South East",
    Name=="SOUTH WEST" ~ "South West",
    TRUE ~ "North East & Yorkshire"),
    Age=as.numeric(if_else(Age=="90+", "90", Age)),
    Age=case_when(
      Age<16~ "<16", Age<45 ~ "16-45", Age<50 ~ "45-49", Age<55 ~ "50-54", Age<60 ~ "55-59", Age<65 ~ "60-64",
      Age<70 ~ "65-69", Age<75 ~ "70-74", Age<80 ~ "75-79", TRUE ~ "80+")) %>% 
  group_by(Region, Age) %>% 
  summarise(Pop=sum(Pop)) %>% 
  ungroup() %>% 
  mutate(Sex="Female")

plotdata <- data %>% 
  merge(bind_rows(malepop, femalepop), by=c("Region", "Age", "Sex")) %>% 
  rowwise() %>% 
  mutate(Unvax=Pop-`1st`, Vax1Only=`1st`-`2nd`) %>% 
  ungroup() %>% 
  select(-c("1st", "Pop")) %>% 
  gather(Measure, Value, c("Unvax", "Vax1Only", "2nd")) %>% 
  mutate(SexMeasure=paste0(Sex, Measure),
         Value=if_else(Value<0,0,Value),
         Value=if_else(Sex=="Male", -Value, Value),
         Age=factor(Age, levels=c("<16", "16-45", "45-49", "50-54", "55-59", "60-64",
                                "65-69", "70-74", "75-79", "80+")),
         SexMeasure=factor(SexMeasure, levels=c("MaleUnvax", "MaleVax1Only", "Male2nd",
                                                "FemaleUnvax", "FemaleVax1Only", "Female2nd")))
         
agg_tiff("Outputs/COVIDVaxxAgexSexxRegion.tiff", units="in", width=9, height=7, res=800)
   ggplot(plotdata, aes(x=Value, y=Age, fill=SexMeasure))+
      geom_col(position="stack", show.legend=FALSE)+
      scale_x_continuous(name="", limits=c(-2100000, 2100000),
                         breaks=c(-2e06, -1e06, 0, 1e06, 2e06),
                         labels=c("2m", "1m", "0", "1m", "2m"))+
      scale_y_discrete(name="Age group")+
      scale_fill_manual(values=c("Grey80", "#be7dff", "#6600cc", "Grey80", "#7dffdd", "#00cc99"))+
      facet_wrap(~Region)+ 
      theme_classic()+
      theme(axis.line.y=element_blank(), text=element_text(family="Roboto"), 
            plot.title=element_text(face="bold", size=rel(1.4)), plot.subtitle=element_markdown(),
            strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)))+
      labs(title="Regional COVID-19 vaccine delivery in England by age and sex",
           subtitle="The number of people who are <span style='color:Grey60;'>unvaccinated</span>, men with <span style='color:#be7dff;'>one</span> or <span style='color:#6600cc;'>two</span> doses and women with <span style='color:#7dffdd;'>one</span> or <span style='color:#00cc99;'>two</span> doses",
          caption="Data from PHE, population figures from ONS\nPlot by @VictimOfMaths")
dev.off()           
         

