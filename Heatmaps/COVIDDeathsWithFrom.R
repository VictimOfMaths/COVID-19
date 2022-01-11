rm(list=ls())

library(tidyverse)
library(curl)
library(readxl)
library(paletteer)
library(extrafont)
library(ragg)
library(lubridate)
library(scales)
library(ggtext)

theme_custom <- function() {
  theme_classic() %+replace%
    theme(plot.title.position="plot", plot.caption.position="plot",
          strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)),
          plot.title=element_text(face="bold", size=rel(1.5), hjust=0,
                                  margin=margin(0,0,5.5,0)),
          text=element_text(family="Lato"))
}

#Download latest primary diagnosis data
source <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fbirthsdeathsandmarriages%2fdeaths%2fdatasets%2fweeklyprovisionalfiguresondeathsregisteredinenglandandwales%2f2021/publishedweek522021.xlsx"
temp <- tempfile()
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

data <- as.data.frame(t(read_excel(temp, sheet="Weekly figures by cause", range="B19:BA20", 
                                   col_names=FALSE))) %>% 
  mutate(date=seq.Date(from=as.Date("2021-01-02"), length.out=nrow(.), by="weeks")) %>% 
  set_names("total", "from", "date") %>% 
  mutate(with=total-from) %>% 
  gather(cause, deaths, c("with", "from")) %>% 
  mutate(cause=factor(cause, levels=c("with", "from")))

agg_tiff("Outputs/COVIDDeathsWithFrom.tiff", units="in", width=9, height=7, res=500)
ggplot(data, aes(x=date, y=deaths, fill=cause))+
  geom_col(position="fill", show.legend=FALSE)+
  scale_x_date(name="")+
  scale_y_continuous(name="Proportion of all deaths reported", label=label_percent(accuracy=1))+
  scale_fill_manual(values=c("#F89088", "#40A0D8"))+
  theme_custom()+
  theme(plot.title=element_markdown(), plot.subtitle=element_markdown())+
  labs(title="Most COVID deaths are <span style='color:#40A0D8;'>from</span> not <span style='color:#F89088;'>with</span> COVID",
       subtitle="Proportion of weekly deaths reported in England & Wales that mention COVID on the death certificate<br><span style='color:#40A0D8;'>where COVID was the underlying cause of death</span> or <span style='color:#F89088;'>only mentioned as a contributory cause",
       caption="Data from ONS | Plot by @VictimOfMaths")

dev.off()
