rm(list=ls())

library(tidyverse)
library(curl)
library(paletteer)
library(extrafont)
library(geofacet)
library(scales)
library(ragg)

url <- "https://covid-surveillance-data.cog.sanger.ac.uk/download/lineages_by_ltla_and_week.tsv"

temp <- tempfile()
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

rawdata <- read_tsv(temp)

#Read in LTLA to region lookup

temp <- tempfile()
source <- "https://opendata.arcgis.com/datasets/0c3a9643cc7c4015bb80751aad1d2594_0.csv"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
LADtoRegion <- read.csv(temp)[,c(1,4)]
colnames(LADtoRegion) <- c("LTLA", "Region")

data <- merge(rawdata, LADtoRegion,all.x=TRUE) %>% 
  mutate(Region=case_when(
    LTLA %in% c("E07000246", "E06000058", "E06000059") ~ "South West",
    LTLA %in% c("E07000245", "E07000244") ~ "East of England",
    TRUE ~ Region),
    WeekEndDate=as.Date(WeekEndDate),
    strain=case_when(
      Lineage=="B.1.177" ~ "B.1.177",
      Lineage=="AY.4.2" ~ "Delta (AY4.2 variant)",
      Lineage=="B.1.617.2" | substr(Lineage,1,3)=="AY." ~ "Delta (OG)",
      Lineage=="B.1.1.7" ~ "Alpha",
      Lineage %in% c("B.1.1.529", "BA.1") ~ "Omicron",
      Lineage=="BA.2" ~ "Stealth Omicron",
      TRUE ~ "Other variants")) %>% 
  group_by(WeekEndDate, strain, Region) %>% 
  summarise(Count=sum(Count)) %>% 
  ungroup() %>% 
  group_by(WeekEndDate, Region) %>% 
  mutate(Total=sum(Count)) %>% 
  ungroup() %>% 
  mutate(prop=Count/Total,
         strain=factor(strain, levels=c("B.1.177", "Alpha", "Delta (OG)", "Delta (AY4.2 variant)",
                                        "Omicron", "Stealth Omicron", "Other variants")))

#Compare regions
mygrid <- data.frame(name=c("North East", "North West", "Yorkshire and The Humber",
                            "West Midlands", "East Midlands", "East of England",
                            "South West", "London", "South East"),
                     row=c(1,2,2,3,3,3,4,4,4), col=c(2,1,2,1,2,3,1,2,3),
                     code=c(1:9))

agg_tiff("Outputs/COVIDGenomesCountxReg.tiff", units="in", width=10, height=8, res=500)
ggplot(data, aes(x=WeekEndDate, y=Count, fill=strain))+
  geom_col(position="stack")+
  scale_x_date(name="")+
  scale_y_continuous(name="Genomes sequenced")+
  scale_fill_paletteer_d("khroma::bright", name="Lineage")+
  facet_geo(~Region, grid=mygrid)+
  theme_classic()+
  theme(strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)),
        text=element_text(family="Lato"), plot.title=element_text(face="bold", size=rel(1.4)))+
  labs(title="We are now sequencing more COVID genomes than ever before",
       subtitle=paste0("Number of total COVID-19 genomes sequenced by the Wellcome Sanger Institute identified as belonging to selected major lineages.\nData up to ", max(rawdata$WeekEndDate)),
       caption="Data from Wellcome Sanger Institute | Plot by @VictimOfMaths")
dev.off()

agg_tiff("Outputs/COVIDGenomesStackedxReg.tiff", units="in", width=10, height=8, res=500)
ggplot(data, aes(x=WeekEndDate, y=prop, fill=strain))+
  geom_col(position="stack")+
  scale_x_date(name="")+
  scale_y_continuous(name="Genomes sequenced", labels=label_percent(accuracy=1))+
  scale_fill_paletteer_d("khroma::bright", name="Lineage")+
  facet_geo(~Region, grid=mygrid)+
  theme_classic()+
  theme(strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)),
        text=element_text(family="Lato"), plot.title=element_text(face="bold", size=rel(1.4)),
        axis.text.x=element_text(angle=45, hjust=1, vjust=1))+
  labs(title="The Delta variant is still dominant across England. For now",
       subtitle=paste0("Proportion of total COVID-19 genomes sequenced by the Wellcome Sanger Institute identified as belonging to selected major lineages.\nData up to ", max(rawdata$WeekEndDate)),
       caption="Data from Wellcome Sanger Institute | Plot by @VictimOfMaths")
dev.off()

#National picture
natdata <- data %>% 
  group_by(WeekEndDate, strain) %>% 
  summarise(Count=sum(Count), Total=sum(Total)) %>% 
  ungroup() %>% 
  mutate(prop=Count/Total)

agg_tiff("Outputs/COVIDGenomesCount.tiff", units="in", width=10, height=8, res=500)
ggplot(natdata, aes(x=WeekEndDate, y=Count, fill=strain))+
  geom_col(position="stack")+
  scale_x_date(name="")+
  scale_y_continuous(name="Genomes sequenced")+
  scale_fill_paletteer_d("khroma::bright", name="Lineage")+
  theme_classic()+
  theme(strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)),
        text=element_text(family="Lato"), plot.title=element_text(face="bold", size=rel(1.4)))+
  labs(title="We are now sequencing more COVID genomes than ever before",
       subtitle=paste0("Number of total COVID-19 genomes sequenced by the Wellcome Sanger Institute identified as belonging to selected major lineages.\nData up to ", max(rawdata$WeekEndDate)),
       caption="Data from Wellcome Sanger Institute | Plot by @VictimOfMaths")
dev.off()

agg_tiff("Outputs/COVIDGenomesStacked.tiff", units="in", width=10, height=8, res=500)
ggplot(natdata, aes(x=WeekEndDate, y=Count, fill=strain))+
  geom_col(position="fill")+
  scale_x_date(name="")+
  scale_y_continuous(name="Proprtion of genomes sequenced", labels=label_percent(accuracy=1))+
  scale_fill_paletteer_d("khroma::bright", name="Lineage")+
  theme_classic()+
  theme(strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)),
        text=element_text(family="Lato"), plot.title=element_text(face="bold", size=rel(1.4)),
        axis.text.x=element_text(angle=45, hjust=1, vjust=1))+
  labs(title="The Delta variant is still dominant across England. For now",
       subtitle=paste0("Proportion of total COVID-19 genomes sequenced by the Wellcome Sanger Institute identified as belonging to selected major lineages.\nData up to ", max(rawdata$WeekEndDate)),
       caption="Data from Wellcome Sanger Institute | Plot by @VictimOfMaths")
dev.off()

#Using more current COG data (no subnational split)
#Download Covid genome data
temp <- tempfile()
source <- "https://cog-uk.s3.climb.ac.uk/phylogenetics/latest/cog_metadata.csv"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

data <- read.csv(temp) %>% 
  mutate(sample_date=as.Date(sample_date)) %>% 
  select(sample_date, scorpio_call, epi_week, lineage) %>% 
  mutate(lineagegroup=case_when(
    substr(scorpio_call, 1, 5)=="Alpha" ~ "Alpha",
    scorpio_call=="Delta (AY.4.2-like)" ~ "Delta (AY4.2 variant)",
    substr(scorpio_call, 1, 5)=="Delta" ~ "Delta (OG)",
    grepl("Omicron", scorpio_call) ~ "Omicron",
    lineage=="B.1.177" ~ "B.1.177",
    TRUE ~ "Other variants"),
    lineagegroup=factor(lineagegroup, levels=c("B.1.177", "Alpha", "Delta (OG)", "Delta (AY4.2 variant)",
                                               "Omicron", "Other variants")))

dailydata <- data %>% 
  group_by(lineagegroup, sample_date) %>% 
  summarise(count=n())

agg_tiff("Outputs/COVIDGenomesStackedCOG.tiff", units="in", width=10, height=8, res=500)
ggplot(dailydata %>% filter(sample_date>as.Date("2020-08-01")), 
       aes(x=sample_date, y=count, fill=lineagegroup))+
  geom_col(position="fill")+
  scale_x_date(name="")+
  scale_y_continuous(name="Proportion of genomes sequenced", labels=label_percent(accuracy=1))+
  scale_fill_paletteer_d("khroma::bright", name="Lineage")+
  theme_classic()+
  theme(text=element_text(family="Lato"), 
        plot.title=element_text(face="bold", size=rel(1.6)))+
  labs(title="Omicron is coming...",
       subtitle=paste0("Proportion of total COVID-19 genomes reported by COG-UK as belonging to selected major lineages.\nData up to ", max(dailydata$sample_date)),
       caption="Data from COG-UK | Plot by @VictimOfMaths")
dev.off()
