rm(list=ls())

library(tidyverse)
library(curl)
library(readxl)
library(lubridate)
library(paletteer)
library(extrafont)
library(ragg)
library(ggtext)

theme_custom <- function() {
  theme_classic() %+replace%
    theme(plot.title.position="plot", plot.caption.position="plot",
          strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)),
          plot.title=element_text(face="bold", size=rel(1.5), hjust=0,
                                  margin=margin(0,0,5.5,0)),
          text=element_text(family="Lato"),
          plot.subtitle=element_text(colour="Grey40", hjust=0, vjust=1),
          plot.caption=element_text(colour="Grey40", hjust=1, vjust=1, size=rel(0.8)),
          axis.text=element_text(colour="Grey40"),
          axis.title=element_text(colour="Grey20"),
          legend.text=element_text(colour="Grey40"),
          legend.title=element_text(colour="Grey20"))
}

vaxpal <- c("#FFBC42", "#D81159") 

temp <- tempfile()
url1 <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/datasets/deathsbyvaccinationstatusengland/deathsoccurringbetween1april2021and31december2022/referencetable.xlsx"
temp <- curl_download(url=url1, destfile=temp, quiet=FALSE, mode="wb")

vaxstatus <- read_excel(temp, sheet="Table 1", range="A4:J571") %>%  
  mutate(Date=as.Date(paste(Year, Month, "01", sep="-"), "%Y-%B-%d")) %>% 
  select(c(1,4:7, 9:11)) %>% 
  set_names("Cause", "Vaccination Status", "Deaths", "PersonYears", "Rate", 
            "LowerCI", "UpperCI", "Date") %>% 
  mutate(across(c(Rate, LowerCI, UpperCI), ~as.numeric(.x)))

vaxxage <- read_excel(temp, sheet="Table 2", range="A4:K3091") %>%  
  mutate(Date=as.Date(paste(Year, Month, "01", sep="-"), "%Y-%B-%d")) %>% 
  select(c(1, 4:8, 10:12)) %>% 
  set_names("Cause", "Age", "Vaccination Status", "Deaths", "PersonYears", "Rate", 
            "LowerCI", "UpperCI", "Date") %>% 
  mutate(across(c(Rate, LowerCI, UpperCI), ~as.numeric(.x)))

vaxxsex <- read_excel(temp, sheet="Table 3", range="A4:K1138") %>%  
  mutate(Date=as.Date(paste(Year, Month, "01", sep="-"), "%Y-%B-%d")) %>% 
  select(c(1,2, 5:8, 10:12)) %>% 
  set_names("Sex", "Cause", "Vaccination Status", "Deaths", "PersonYears", "Rate", 
            "LowerCI", "UpperCI", "Date") %>% 
  mutate(across(c(Rate, LowerCI, UpperCI), ~as.numeric(.x)))
                      
vaxxagexsex <- read_excel(temp, sheet="Table 4", range="A4:L4120") %>% 
  mutate(Date=as.Date(paste(Year, Month, "01", sep="-"), "%Y-%B-%d"))                     

agg_tiff("Outputs/DeathsxVaxxCause.tiff", units="in", width=10, height=6, res=600)
vaxstatus %>% filter(`Vaccination Status` %in% c("Ever vaccinated", "Unvaccinated")) %>% 
ggplot(aes(x=Date))+
  geom_ribbon(aes(ymin=LowerCI, ymax=UpperCI, fill=`Vaccination Status`), 
              alpha=0.3, show.legend=FALSE)+
  geom_line(aes(y=Rate, colour=`Vaccination Status`))+
  scale_x_date(labels=date_format("%b %y"))+ 
  scale_y_continuous(name="Age-standardised deaths per 100,000 person years")+
  scale_fill_manual(values=vaxpal)+
  scale_colour_manual(values=vaxpal, name="")+
  facet_wrap(~Cause)+
  theme_custom()+
  theme(legend.position = "bottom", plot.title=element_markdown())+
  labs(title="Death rates are consistently lower among <span style='color:#FFBC42;'> vaccinated </span> vs. <span style='color:#D81159;'> unvaccinated </span> people",
       subtitle="Age-standardised mortality rates by cause and vaccination status in England. Shaded areas represent 95% Confidence Intervals\n ",
       caption="Data from ONS | Plot by @VictimOfMaths")

dev.off()

vaxxageredux <- vaxxage %>% 
  mutate(VaxStatus=if_else(`Vaccination Status`=="Unvaccinated", "Unvaccinated",
                           "Ever Vaccinated")) %>% 
  group_by(Cause, Age, Date, VaxStatus) %>% 
  summarise(Rate=weighted.mean(Rate, PersonYears, na.rm=TRUE),
            LowerCI=weighted.mean(LowerCI, PersonYears, na.rm=TRUE),
            UpperCI=weighted.mean(UpperCI, PersonYears, na.rm=TRUE), .groups="drop")

agg_tiff("Outputs/DeathsxVaxxCausexAge.tiff", units="in", width=10, height=6, res=600)
vaxxageredux %>% filter(Cause=="All causes" & Age!="90+") %>% 
  ggplot(aes(x=Date))+
  geom_ribbon(aes(ymin=LowerCI, ymax=UpperCI, fill=VaxStatus), 
              alpha=0.3, show.legend=FALSE)+
  geom_line(aes(y=Rate, colour=VaxStatus))+
  scale_x_date(labels=date_format("%b %y"))+ 
  scale_y_continuous(name="Age-standardised deaths per 100,000 person years",
                     limits=c(0,NA))+
  scale_fill_manual(values=vaxpal)+
  scale_colour_manual(values=vaxpal, name="")+
  facet_wrap(~Age, scales="free_y")+
  theme_custom()+
  theme(legend.position = "bottom", plot.title=element_markdown())+
  labs(title="Death rates are consistently lower among <span style='color:#FFBC42;'> vaccinated </span> vs. <span style='color:#D81159;'> unvaccinated </span> people",
       subtitle="Age-standardised all cause mortality rates by age, cause and vaccination status in England. Shaded areas represent 95% Confidence Intervals\n ",
       caption="Data from ONS | Plot by @VictimOfMaths")

dev.off()

#Currently appears to be a labelling error in the ONS data here with the male
#and female dates not aligning
agg_tiff("Outputs/DeathsxVaxxCausexSex.tiff", units="in", width=12, height=6, res=600)
vaxxsex %>% filter(`Vaccination Status` %in% c("Ever vaccinated", "Unvaccinated")) %>% 
  ggplot(aes(x=Date))+
  geom_ribbon(aes(ymin=LowerCI, ymax=UpperCI, fill=`Vaccination Status`), 
              alpha=0.3, show.legend=FALSE)+
  geom_line(aes(y=Rate, colour=`Vaccination Status`))+
  scale_x_date(labels=date_format("%b %y"))+ 
  scale_y_continuous(name="Age-standardised deaths per 100,000 person years",
                     limits=c(0,NA))+
  scale_fill_manual(values=vaxpal)+
  scale_colour_manual(values=vaxpal, name="")+
  facet_grid(Sex~Cause)+
  theme_custom()+
  theme(legend.position = "bottom", plot.title=element_markdown())+
  labs(title="Death rates are consistently lower among <span style='color:#FFBC42;'> vaccinated </span> vs. <span style='color:#D81159;'> unvaccinated </span> people",
       subtitle="Age-standardised all cause mortality rates by age, cause and vaccination status in England. Shaded areas represent 95% Confidence Intervals\n ",
       caption="Data from ONS | Plot by @VictimOfMaths")

dev.off()
