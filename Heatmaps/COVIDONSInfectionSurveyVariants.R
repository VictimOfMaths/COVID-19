rm(list=ls())

library(tidyverse)
library(curl)
library(lubridate)
library(ggtext)
library(extrafont)
library(ragg)
library(paletteer)
library(readxl)
library(scales)
library(geofacet)

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

#Set up facets
mygrid <- data.frame(name=c("Scotland", "North East", "Northern Ireland", "North West",
                            "Yorkshire & Humber", "West Midlands", "East Midlands",
                            "East of England", "South West", "London", "South East"),
                     row=c(1,1,2,2,2,3,3,3,4,4,4), col=c(2,3,1,2,3,1,2,3,1,2,3), 
                     code=c(1:11))

#Download technical data from ONS infection survey including variant estimates 
temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fhealthandsocialcare%2fconditionsanddiseases%2fdatasets%2fcovid19infectionsurveytechnicaldata%2f2022/20220311covid19infectionsurveydatasetstechnical2.xlsx"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

natdata <- read_excel(temp, sheet="1c", range="A6:AK49") %>% 
  set_names("Date", paste(rep(c("England", "Wales", "Northern Ireland", "Scotland"), each=9), 
                          rep(c("BA.1", "BA.2", "Unknown"), each=3, times=4), 
                          rep(c("perc", "lower", "upper"), times=12), sep="_")) %>% 
  mutate(Date=as.Date(Date)) %>% 
  pivot_longer(c(2:37), names_to=c("Country", "Variant", "Metric"), names_sep="_", values_to="Percentage") %>% 
  mutate(Percentage=as.numeric(Percentage)) %>% 
  spread(Metric, Percentage)

#Bring in English regions
regdata <- read_excel(temp, sheet="1d", range="A6:CD48") %>% 
  set_names("Date", paste(rep(c("North East", "North West", "Yorkshire & Humber", 
                                "East Midlands", "West Midlands", "East of England",
                                "London", "South East", "South West"), each=9), 
                          rep(c("BA.1", "BA.2", "Unknown"), each=3, times=9), 
                          rep(c("perc", "lower", "upper"), times=27), sep="_")) %>% 
  mutate(Date=as.Date(Date)) %>% 
  pivot_longer(c(2:82), names_to=c("Region", "Variant", "Metric"), names_sep="_", values_to="Percentage") %>% 
  mutate(Percentage=as.numeric(Percentage)) %>% 
  spread(Metric, Percentage)

#Combine regions with Scotland, Wales and NI
combined <- natdata %>% 
  filter(Country!="England") %>% 
  rename(Region=Country) %>% 
  bind_rows(regdata)

#Plot prevalence by variant
ggplot(natdata, aes(x=Date, y=perc/100, fill=Variant))+
  geom_area()+
  scale_x_date(name="")+
  scale_y_continuous(name="Proportion of population", labels=label_percent(accuracy=0.1))+
  scale_fill_manual(values=c("#11B2E8", "#CF2154", "#DBF4F8"))+
  facet_wrap(~Country)+
  theme_custom()

ggplot(regdata, aes(x=Date, y=perc/100, fill=Variant))+
  geom_area()+
  scale_x_date(name="")+
  scale_y_continuous(name="Proportion of population", labels=label_percent(accuracy=1))+
  scale_fill_manual(values=c("#11B2E8", "#CF2154", "#DBF4F8"))+
  facet_wrap(~Region)+
  theme_custom()

agg_tiff("Outputs/COVIDONSInfectionStudyVariants.tiff", units="in", width=10, height=7, res=500)
ggplot(combined, aes(x=Date, y=perc/100, fill=Variant))+
  geom_area()+
  scale_x_date(name="")+
  scale_y_continuous(name="Proportion of population testing positive", 
                     labels=label_percent(accuracy=1))+
  scale_fill_manual(values=c("#11B2E8", "#CF2154", "#DBF4F8"))+
  facet_geo(~Region, grid=mygrid)+
  theme_custom()+
  theme(plot.subtitle=element_markdown())+
  labs(title="Rising COVID prevalence across the UK is being driven by the BA.2 variant",
       subtitle="Estimated proportion of the population testing positive for the<span style='color:#11B2E8;'> BA.1</span> or <span style='color:#CF2154;'>BA.2</span> variants according to the ONS Infection Survey.<br>A small number of samples did not contain enough genetic material to determine the variant.",
       caption="Data from ONS | Plot by @VictimOfMaths")
dev.off()

#Plot prevalence by variant
ggplot(natdata, aes(x=Date, y=perc/100, fill=Variant))+
  geom_area(position="fill")+
  scale_x_date(name="")+
  scale_y_continuous(name="Proportion of cases", labels=label_percent(accuracy=1))+
  scale_fill_manual(values=c("#11B2E8", "#CF2154", "#DBF4F8"))+
  facet_wrap(~Country)+
  theme_custom()
   
ggplot(regdata, aes(x=Date, y=perc/100, fill=Variant))+
  geom_area(position="fill")+
  scale_x_date(name="")+
  scale_y_continuous(name="Proportion of cases", labels=label_percent(accuracy=1))+
  scale_fill_manual(values=c("#11B2E8", "#CF2154", "#DBF4F8"))+
  facet_wrap(~Region)+
  theme_custom()

agg_tiff("Outputs/COVIDONSInfectionStudyVariantsProp.tiff", units="in", width=10, height=7, res=500)
ggplot(combined, aes(x=Date, y=perc/100, fill=Variant))+
  geom_area(position="fill")+
  scale_x_date(name="")+
  scale_y_continuous(name="Proportion of cases", labels=label_percent(accuracy=1))+
  scale_fill_manual(values=c("#11B2E8", "#CF2154", "#DBF4F8"))+
  facet_geo(~Region, grid=mygrid)+
  theme_custom()+
  theme(plot.subtitle=element_markdown())+
  labs(title="But BA.1 is still hanging in there in some parts of the country",
       subtitle="Estimated proportion people testing positive for COVID who have either the<span style='color:#11B2E8;'> BA.1</span> or <span style='color:#CF2154;'>BA.2</span> variants according to the ONS Infection Survey.<br>A small number of samples did not contain enough genetic material to determine the variant.",
       caption="Data from ONS | Plot by @VictimOfMaths")
dev.off()
