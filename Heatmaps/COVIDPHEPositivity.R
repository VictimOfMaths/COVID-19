rm(list=ls())

library(tidyverse)
library(curl)
library(RcppRoll)
library(lubridate)
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

source <- "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/1020037/Weekly_Influenza_and_COVID19_report_data_W38_v2.xlsx"

temp <- tempfile()
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

data_m <- read_excel(temp, sheet="Figure 7. Positivity by age", range="C121:L174") %>% 
  mutate(date=seq.Date(from=as.Date("2020-09-14"), by="weeks", length.out=nrow(.))) %>% 
  gather(age, positivity, c(1:ncol(.)-1)) %>% 
  mutate(age=gsub("_", " to ", age), sex="Male")

data_f <- read_excel(temp, sheet="Figure 7. Positivity by age", range="C177:L230") %>% 
  mutate(date=seq.Date(from=as.Date("2020-09-14"), by="weeks", length.out=nrow(.))) %>% 
  gather(age, positivity, c(1:ncol(.)-1)) %>% 
  mutate(age=gsub("_", " to ", age), sex="Female")

data <- bind_rows(data_m, data_f) %>% 
  mutate(age=factor(age, levels=c("0 to 4", "5 to 9", "10 to 19", "20 to 29", 
                                  "30 to 39", "40 to 49", "50 to 59",
                                  "60 to 69", "70 to 79", "80+")),
         positivity=positivity/100)

agg_tiff("Outputs/COVIDPositivityxAgexSex.tiff", units="in", width=9, height=6, res=800)
ggplot(data %>% filter(date>as.Date("2021-05-25")), aes(x=date, y=positivity, colour=sex))+
  geom_line(show.legend=FALSE)+
  scale_x_date(name="")+
  scale_y_continuous(name="Positivity", labels=label_percent(accuracy=1))+
  scale_colour_manual(values=c("#00cc99", "#6600cc"))+
  facet_wrap(~age)+
  theme_custom()+
  theme(plot.subtitle=element_markdown())+
  labs(title="Positivity rates are consistently higher among men",
       subtitle="Rolling 7-day average of COVID test positivity rates in <span style='color:#6600cc;'>men</span> and <span style='color:#00cc99;'>women</span> for pillar 2 (community) testing in England, by age.",
       caption="Data from PHE | Plot by @VictimOfMaths")
dev.off()

