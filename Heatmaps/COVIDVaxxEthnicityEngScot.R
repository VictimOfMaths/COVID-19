rm(list=ls())

library(tidyverse)
library(curl)
library(readxl)
library(ggtext)
library(extrafont)
library(ragg)
library(paletteer)

theme_custom <- function() {
  theme_classic() %+replace%
    theme(plot.title.position="plot", plot.caption.position="plot",
          strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)),
          plot.title=element_text(face="bold", size=rel(1.5), hjust=0,
                                  margin=margin(0,0,5.5,0)),
          text=element_text(family="Lato"))
}

#Scottish data doesn't appear to be machine readable, sadface.
#https://www.scotlandscensus.gov.uk/webapi/jsf/tableView/tableView.xhtml
Scotdata <- data.frame(age=c("0-15", "16-24", "25-34", "35-49", "50-64", "65+"),
                       White=c(836005, 593489, 617472, 1101772, 1025729, 882940),
                       `Asian/Asian Scottish/Asian British`=c(31449, 28356, 33048, 29106, 12925, 5794),
                       `Black/African/Caribbean/Black British`=
                         c(7774+1499, 3915+924, 9081+1236, 7126+1847,1374+740, 368+294),
                       `Mixed/Other`=c(9097+3507, 3597+2207, 2945+3545, 2602+3230, 1080+1392, 
                                       494+444),
                       Country="Scotland") %>% 
  set_names(c("age", "White", "Asian/Asian Scottish/Asian British",
              "Black/African/Caribbean/Black British", "Mixed/Other", "Country")) %>% 
  gather(Ethnicity, pop, c(2:5))

Engurl <- "https://www.ons.gov.uk/file?uri=/aboutus/transparencyandgovernance/freedomofinformationfoi/ethnicgroupsbysexandagefromthe2011census/dc2101ewethnicgroupbysexbyage.xlsx"
temp <- tempfile()
temp <- curl_download(url=Engurl, destfile=temp, quiet=FALSE, mode="wb")

Engdata <- read_excel(temp, sheet="All persons", range="A11:Y33") %>% 
  select(Age, `White: Total`, `Mixed/multiple ethnic group: Total`,
         `Asian/Asian British: Total`, `Black/African/Caribbean/Black British: Total`,
         `Other ethnic group: Total`) %>% 
  set_names(c("age", "White", "Mixed", "Asian/Asian Scottish/Asian British",
              "Black/African/Caribbean/Black British", "Other")) %>% 
  rowwise() %>% 
  mutate(`Mixed/Other`=Mixed+Other) %>% 
  select(-c("Mixed", "Other")) %>% 
  gather(Ethnicity, pop, c(2:5)) %>% 
  mutate(age=case_when(
           age %in% c("Age 0 to 4", "Age 5 to 7", "Age 8 to 9", "Age 10 to 14", "Age 15") ~ "0-15",
           age %in% c("Age 16 to 17", "Age 18 to 19", "Age 20 to 24") ~ "16-24",
           age %in% c("Age 25 to 29", "Age 30 to 34") ~ "25-34",
           age %in% c("Age 35 to 39", "Age 40 to 44", "Age 45 to 49") ~ "35-49",
           age %in% c("Age 50 to 54", "Age 55 to 59", "Age 60 to 64") ~ "50-64",
           TRUE ~ "65+")) %>% 
  group_by(Ethnicity, age) %>% 
  summarise(pop=sum(pop)) %>% 
  ungroup() %>% 
  mutate(Country="England")

data <- bind_rows(Engdata, Scotdata)

agg_tiff("Outputs/EthnicityxAgeEngScot.tiff", units="in", width=8, height=6, res=800)
ggplot(data, aes(x=age, y=pop, fill=Ethnicity))+
  geom_col(position="fill")+
  scale_x_discrete(name="Age")+
  scale_y_continuous(name="Proportion of population", labels=label_percent(accuracy=1))+
  scale_fill_paletteer_d("rcartocolor::Safe")+
  facet_wrap(~Country)+
  theme_custom()+
  labs(title="England has a much larger non-white population than Scotland",
       subtitle="Population by age and self-reported ethnicity in the 2011 census",
       caption="Data from ONS and NRS | Plot by @VictimOfMaths")
dev.off()

#Estimate uptake if all that mattered was ethnicity, using uptake by ethnicity
#approximated from PHE surveillance reports
calcs <- data %>% 
  group_by(Country, age) %>% 
  mutate(total=sum(pop)) %>% 
  ungroup() %>% 
  mutate(popprop=pop/total,
         uptake=case_when(
           Ethnicity=="White" ~ 0.929,
           Ethnicity=="Asian/Asian Scottish/Asian British" ~ 0.83,
           Ethnicity=="Black/African/Caribbean/Black British" ~ 0.67,
           TRUE ~ 0.76),
         vaxxed=popprop*uptake) %>% 
  group_by(Country, age) %>% 
  summarise(vaxxed=sum(vaxxed))

agg_tiff("Outputs/COVIDVaxxEthnicityModelledEngScot.tiff", units="in", width=8, height=6, res=800)
ggplot(calcs, aes(x=age, y=vaxxed, fill=Country))+
  geom_col(position="dodge")+
  scale_y_continuous(name="Proportion of population", labels=label_percent(accuracy=1))+
  scale_fill_manual(values=c("#F44B4B", "#0076BB"), name="Country")+
  theme_custom()+
  labs(title="Ethnic differences mean you'd expect higher vaccine uptake in Scotland",
       subtitle="Expected 1st dose vaccine uptake by age based on ethnic composition of each age group by country from the 2011 census and vaccine uptake rates from UKHSA",
       caption="Data from UKHSA, ONS and NRS | Plot by @VictimOfMaths")
dev.off()
