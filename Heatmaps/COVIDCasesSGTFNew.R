rm(list=ls())

library(tidyverse)
library(curl)
library(lubridate)
library(RcppRoll)
library(readODS)
library(paletteer)
library(extrafont)
library(ragg)
library(scales)

options(scipen=99999)

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

#Read in case data by variant from UKHSA https://www.gov.uk/government/publications/covid-19-variants-genomically-confirmed-case-numbers
temp <- tempfile()
url <- ("https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/1063376/variants-of-concern-technical-briefing-39-data-england-25-March-2022.ods")
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

seqdata <- read_ods(temp, sheet="Fig4", range="A3:H2418") %>% 
  mutate(specimen_date=as.Date(specimen_date, format="%d/%m/%Y"),
         Variant=case_when(
           specimen_date<as.Date("2022-01-10") & sgtf=="SGTP" ~ "Delta",
           specimen_date>=as.Date("2022-01-10") & sgtf=="SGTP" ~ "BA.2",
           sgtf=="SGTF" ~ "BA.1"),
         Variant=factor(Variant, levels=c("Delta", "BA.1", "BA.2")))

ggplot(seqdata, aes(x=specimen_date, y=percent/100, fill=Variant))+
  geom_area()+
  scale_x_date(name="")+
  scale_y_continuous(name="Proportion of cases sequenced", labels=label_percent(accuracy=1))+
  scale_fill_paletteer_d("unikn::pal_unikn_pref")+
  facet_wrap(~UKHSA_region)+
  theme_custom()

#Bring in case data
temp2 <- tempfile()
url2 <- ("https://api.coronavirus.data.gov.uk/v2/data?areaType=region&metric=newCasesBySpecimenDate&format=csv")
temp2 <- curl_download(url=url2, destfile=temp2, quiet=FALSE, mode="wb")

casedata <- read.csv(temp2) %>% 
  select(2,4,5) %>% 
  set_names("UKHSA_region", "specimen_date", "Cases") %>% 
  mutate(specimen_date=as.Date(specimen_date),
         cases_roll=roll_mean(Cases, n=7, align="center", fill=NA))

data <- merge(seqdata, casedata) %>% 
  mutate(seqCases=percent*cases_roll/100)

natdata <- data %>% 
  group_by(specimen_date, Variant) %>% 
  summarise(seqCases=sum(seqCases)) %>% 
  ungroup()

ggplot(data, aes(x=specimen_date, y=seqCases, fill=Variant))+
  geom_area(position="identity", alpha=0.6)+
  scale_y_continuous(name="Estimated total cases per day")+
  scale_fill_manual(values=c("#4AB2B8", "#F6871F", "#ED1C24"), name="")+
  scale_colour_manual(values=c("#4AB2B8", "#F6871F", "#ED1C24"), name="")+
  facet_wrap(~UKHSA_region)+
  theme_custom()+
  labs()

agg_png("Outputs/COVIDSGTFCases.png", units="in", width=8, height=6, res=800)
ggplot(natdata, aes(x=specimen_date, y=seqCases, fill=Variant, colour=Variant))+
  geom_area(position="identity", alpha=0.6)+
  scale_x_date(name="")+
  scale_y_continuous(name="Estimated total cases per day")+
  scale_fill_manual(values=c("#4AB2B8", "#F6871F", "#ED1C24"), name="")+
  scale_colour_manual(values=c("#4AB2B8", "#F6871F", "#ED1C24"), name="")+
  theme_custom()+
  labs(title="BA.2's rise to dominance has been slower than BA.1's",
       subtitle="Estimated number of daily cases by variant, based on confirmed case data apportioned between variants\nusing SGTF data up to 22nd March",
       caption="Data from TaqPath/UKHSA | Plot by @VictimOfMaths")
dev.off()

agg_png("Outputs/COVIDSGTFCasesProp.png", units="in", width=8, height=6, res=800)
ggplot(natdata, aes(x=specimen_date, y=seqCases, fill=Variant, colour=Variant))+
  geom_area(position="fill", alpha=0.6)+
  scale_x_date(name="")+
  scale_y_continuous(name="Proportion of sequenced cases", labels=label_percent(accuracy=1))+
  scale_fill_manual(values=c("#4AB2B8", "#F6871F", "#ED1C24"), name="")+
  scale_colour_manual(values=c("#4AB2B8", "#F6871F", "#ED1C24"), name="")+
  theme_custom()+
  labs(title="BA.2's rise to dominance has been slower than BA.1's",
       subtitle="Proportion of daily cases sequenced in England by variant, based on SGTF data up to 22nd March",
       caption="Data from TaqPath/UKHSA | Plot by @VictimOfMaths")
dev.off()
