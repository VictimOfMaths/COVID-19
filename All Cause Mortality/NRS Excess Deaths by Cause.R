rm(list=ls())

library(tidyverse)
library(curl)
library(readxl)
library(paletteer)

temp <- tempfile()
source <- "https://www.nrscotland.gov.uk/files//statistics/covid19/covid-deaths-data-week-48.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
endcol <- "AW"

#Historic data for all locations
all.hist <- read_excel(temp, sheet="Table 3 ", range=paste0("B7:",endcol,"12"), col_names=FALSE)
colnames(all.hist) <- c("cause", seq(1:(ncol(all.hist)-1)))
all.hist$time <- "hist"
all.hist$loc <- "All"

#2020 data for all locations
all.2020 <- read_excel(temp, sheet="Table 3 ", range=paste0("B15:",endcol,"20"), col_names=FALSE)
colnames(all.2020) <- c("cause", seq(1:(ncol(all.2020)-1)))
all.2020$time <- "now"
all.2020$loc <- "All"

#Historic data for care homes
ch.hist <- read_excel(temp, sheet="Table 3 ", range=paste0("B31:",endcol,"36"), col_names=FALSE)
colnames(ch.hist) <- c("cause", seq(1:(ncol(ch.hist)-1)))
ch.hist$time <- "hist"
ch.hist$loc <- "Care Home"

#2020 data for care homes
ch.2020 <- read_excel(temp, sheet="Table 3 ", range=paste0("B39:",endcol,"44"), col_names=FALSE)
colnames(ch.2020) <- c("cause", seq(1:(ncol(ch.2020)-1)))
ch.2020$time <- "now"
ch.2020$loc <- "Care Home"

#Historic data for homes
hosp.hist <- read_excel(temp, sheet="Table 3 ", range=paste0("B55:",endcol,"60"), col_names=FALSE)
colnames(hosp.hist) <- c("cause", seq(1:(ncol(hosp.hist)-1)))
hosp.hist$time <- "hist"
hosp.hist$loc <- "Home"

#2020 data for homes
hosp.2020 <- read_excel(temp, sheet="Table 3 ", range=paste0("B63:",endcol,"68"), col_names=FALSE)
colnames(hosp.2020) <- c("cause", seq(1:(ncol(hosp.2020)-1)))
hosp.2020$time <- "now"
hosp.2020$loc <- "Home"

#Historic data for hospitals
home.hist <- read_excel(temp, sheet="Table 3 ", range=paste0("B79:",endcol,"84"), col_names=FALSE)
colnames(home.hist) <- c("cause", seq(1:(ncol(home.hist)-1)))
home.hist$time <- "hist"
home.hist$loc <- "Hospital"

#2020 data for hospitals
home.2020 <- read_excel(temp, sheet="Table 3 ", range=paste0("B87:",endcol,"92"), col_names=FALSE)
colnames(home.2020) <- c("cause", seq(1:(ncol(home.2020)-1)))
home.2020$time <- "now"
home.2020$loc <- "Hospital"

#Historic data for other locations
other.hist <- read_excel(temp, sheet="Table 3 ", range=paste0("B103:",endcol,"108"), col_names=FALSE)
colnames(other.hist) <- c("cause", seq(1:(ncol(other.hist)-1)))
other.hist$time <- "hist"
other.hist$loc <- "Other"

#2020 data for other locations
other.2020 <- read_excel(temp, sheet="Table 3 ", range=paste0("B111:",endcol,"116"), col_names=FALSE)
colnames(other.2020) <- c("cause", seq(1:(ncol(other.2020)-1)))
other.2020$time <- "now"
other.2020$loc <- "Other"

data <- bind_rows(all.hist, all.2020, ch.hist, ch.2020, hosp.hist, hosp.2020, home.hist, home.2020,
                  other.hist, other.2020)

data <- gather(data, week, deaths, c(2:(ncol(data)-2)))

data <- spread(data, time, deaths)

data$week <- as.numeric(data$week)

data <- data %>% 
  mutate(abs=now-hist, rel=abs/hist)

data$cause <- if_else(data$cause=="Circulatory (heart disease and stroke)", "Circulatory",
                      data$cause)

data$cause <- factor(data$cause, levels=c("COVID-19", "Cancer", "Circulatory",
                                          "Dementia / Alzheimers", "Respiratory", "Other"))

data$loc <- factor(data$loc, levels=c("Hospital", "Care Home", "Home", "All"))

#get net deaths difference by location
net.deaths.loc <- data %>% 
  group_by(loc, week) %>% 
  summarise(deaths=sum(abs))

net.deaths.cause <- data %>% 
  group_by(cause, week) %>% 
  summarise(deaths=sum(abs))

#Plot of all locations
tiff("Outputs/NRSExcessxcause.tiff", units="in", width=8, height=6, res=500)
ggplot(data=subset(data, loc=="All"))+
  geom_segment(aes(x=0, xend=ncol(all.hist)-3, y=0, yend=0))+
  geom_bar(aes(x=week, y=abs, fill=cause), stat="identity", position="stack")+
  scale_x_continuous(name="Week")+
  scale_y_continuous(name="Deaths in 2020 vs. 2015-19 average")+
  scale_fill_paletteer_d("LaCroixColoR::paired", name="Cause of death")+
  theme_classic()+
  theme(plot.title=element_text(face="bold", size=rel(1.2)))+
  labs(title="COVID-19 deaths in Scotland have fallen",
       subtitle="Registered deaths in 2020 compared to the previous 5-year average",
       caption="Data from National Records of Scotland | Plot by @VictimOfMaths")
dev.off()

tiff("Outputs/NRSExcessxcausexloc.tiff", units="in", width=12, height=8, res=500)
ggplot(data=subset(data, loc!="All" & loc!="Other"))+
  geom_segment(aes(x=0, xend=ncol(all.hist)-3, y=0, yend=0))+
  geom_bar(aes(x=week, y=abs, fill=cause), stat="identity", position="stack")+
  scale_x_continuous(name="Week")+
  scale_y_continuous(name="Deaths in 2020 vs. 2015-19 average")+
  scale_fill_paletteer_d("LaCroixColoR::paired", name="Cause of death")+
  facet_wrap(~loc)+
  theme_classic()+
  theme(plot.title=element_text(face="bold", size=rel(1.2)))+
  theme(strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)))+
  labs(title="Excess mortality in Scotland by cause and location",
       subtitle="Registered deaths in 2020 compared to the previous 5-year average",
       caption="Data from National Records of Scotland | Plot by @VictimOfMaths")
dev.off()

tiff("Outputs/NRSExcessxlocxcause.tiff", units="in", width=8, height=6, res=500)
ggplot(subset(data, loc!="All" & loc!="Other"))+
  geom_segment(aes(x=0, xend=ncol(all.hist)-3, y=0, yend=0))+
  geom_line(aes(x=week, y=abs, colour=loc))+
  scale_colour_paletteer_d("fishualize::Scarus_tricolor", name="Place of death")+
  scale_x_continuous(name="Week")+
  scale_y_continuous(name="Deaths in 2020 vs. 2015-19 average")+
  facet_wrap(~cause)+
  theme_classic()+
  theme(plot.title=element_text(face="bold", size=rel(1.2)))+
  theme(strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)))+
  labs(title="Elevated levels of home mortality may, to some extent, be displaced cancer deaths",
       subtitle="Excess mortality in Scotland in 2020 by cause and location",
       caption="Data from National Records of Scotland | Plot by @VictimOfMaths")
dev.off()
