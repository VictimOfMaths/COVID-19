rm(list=ls())

library(tidyverse)
library(curl)
library(ggtext)
library(extrafont)
library(RcppRoll)
library(ragg)
library(paletteer)
library(lubridate)

theme_custom <- function() {
  theme_classic() %+replace%
    theme(plot.title.position="plot", plot.caption.position="plot",
          strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)),
          plot.title=element_text(face="bold", size=rel(1.5), hjust=0,
                                  margin=margin(0,0,5.5,0)),
          text=element_text(family="Lato"))
}

#Function borrowed from stackoverflow answer from truenbrand to draw annotations on a polar plot
#https://stackoverflow.com/questions/66196451/draw-straight-line-between-any-two-point-when-using-coord-polar-in-ggplot2-r/66196752#66196752
geom_segment_straight <- function(...) {
  layer <- geom_segment(...)
  new_layer <- ggproto(NULL, layer)
  old_geom <- new_layer$geom
  geom <- ggproto(
    NULL, old_geom,
    draw_panel = function(data, panel_params, coord, 
                          arrow = NULL, arrow.fill = NULL,
                          lineend = "butt", linejoin = "round",
                          na.rm = FALSE) {
      data <- ggplot2:::remove_missing(
        data, na.rm = na.rm, c("x", "y", "xend", "yend", 
                               "linetype", "size", "shape")
      )
      if (ggplot2:::empty(data)) {
        return(zeroGrob())
      }
      coords <- coord$transform(data, panel_params)
      # xend and yend need to be transformed separately, as coord doesn't understand
      ends <- transform(data, x = xend, y = yend)
      ends <- coord$transform(ends, panel_params)
      
      arrow.fill <- if (!is.null(arrow.fill)) arrow.fill else coords$colour
      return(grid::segmentsGrob(
        coords$x, coords$y, ends$x, ends$y,
        default.units = "native", gp = grid::gpar(
          col = alpha(coords$colour, coords$alpha),
          fill = alpha(arrow.fill, coords$alpha),
          lwd = coords$size * .pt,
          lty = coords$linetype,
          lineend = lineend,
          linejoin = linejoin
        ),
        arrow = arrow
      ))
      
    }
  )
  new_layer$geom <- geom
  return(new_layer)
}

#Read in case data from dashboard
source <- "https://api.coronavirus.data.gov.uk/v2/data?areaType=nation&metric=newCasesBySpecimenDate&format=csv"

temp <- tempfile()
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

data <- read.csv(temp) %>% 
  mutate(date=as.Date(date)) %>% 
  rename("cases"="newCasesBySpecimenDate")

#Set value for increment size (basically controls the tightness of the spiral, bigger numbers =>
#a looser spiral)
spiralincrement <- 220

data <- data %>% 
  group_by(date) %>% 
  summarise(cases=sum(cases)) %>% 
  mutate(areaName="UK") %>% 
  ungroup() %>% 
  #Comment out this next row if you wanted data for UK nations, not whole country
  #bind_rows(data) %>% 
  #2020 being a leap year messes things up, so remove 31st December 2020 (arbitrarily) 
  #to make all the years the same length
  #Also cut off the last few days of data as they are incomplete
  filter(date!=as.Date("2020-12-31") & date<max(date)-days(3)) %>% 
  group_by(areaName) %>% 
  mutate(cases_roll=roll_mean(cases, 7, align="center", fill=NA),
         #Create variable to represent the base of each bar - change the number to tighten/relax the spiral
         increment=spiralincrement*c(1:n()),
         #Add cases to the base to get the top of each bar
         incrementcases=increment+cases_roll,
         year=year(date)) %>% 
  ungroup() %>% 
  #Calculate the number of days since the start of the year
  group_by(year) %>% 
  mutate(yeardays=as.numeric(difftime(date ,as.Date(paste0(year, "-01-01")) , units = c("days")))) %>% 
  ungroup()

#Pull out a couple of parameters to control the positioning of the segments
seg2021 <- data$increment[data$year==2020 & data$yeardays==364]-spiralincrement*0.5
seg2122 <- data$increment[data$year==2021 & data$yeardays==364]-spiralincrement*0.5
arrowmin <- max(data$increment[data$year==2022 & !is.na(data$cases_roll)])+spiralincrement*4
arrowxpos <- max(data$yeardays[data$year==2022 & !is.na(data$cases_roll)])+4

agg_tiff("Outputs/COVIDCasesSpiral.tiff", units="in", width=8, height=8, res=800)
ggplot()+
  #Need to plot each year separately, to 'trick' coord_polar to make a spiral, not a single
  #loop
  geom_rect(data=data %>% filter(year==2020 & ! is.na(cases_roll)),
            aes(xmin=yeardays, xmax=yeardays+1, ymin=increment, ymax=incrementcases, 
                fill=cases_roll), show.legend=FALSE)+
  geom_rect(data=data %>% filter(year==2021 & ! is.na(cases_roll)),
            aes(xmin=yeardays, xmax=yeardays+1, ymin=increment, ymax=incrementcases, 
                fill=cases_roll), show.legend=FALSE)+
  geom_rect(data=data %>% filter(year==2022 & ! is.na(cases_roll)),
            aes(xmin=yeardays, xmax=yeardays+1, ymin=increment, ymax=incrementcases, 
                fill=cases_roll), show.legend=FALSE)+
  geom_line(data=data %>% filter(year==2020 & ! is.na(cases_roll)),
            aes(x=yeardays, y=increment), colour="black")+
  geom_line(data=data %>% filter(year==2021),
            aes(x=yeardays, y=increment), colour="black")+
  geom_line(data=data %>% filter(year==2022 & yeardays<arrowxpos-3),
            aes(x=yeardays, y=increment), colour="black")+
  #Add a couple of tiny segments to patch the holes in the baseline at the end of each year
  geom_segment_straight(aes(x=363.5, xend=0.5, y=seg2021, yend=seg2021+2*spiralincrement), 
                        colour="black")+
  geom_segment_straight(aes(x=363.5, xend=0.5, y=seg2122, yend=seg2122+2*spiralincrement), 
                        colour="black")+
  scale_x_continuous(breaks=c(0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334),
                     labels=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep",
                              "Oct", "Nov", "Dec"))+
  scale_colour_paletteer_c("viridis::rocket", direction=-1)+
  scale_fill_paletteer_c("viridis::rocket", direction=-1)+
  coord_polar()+
  theme_void()+
  theme(panel.grid.major.x=element_line(colour="Grey90"),
        axis.text.x=element_text(colour="Grey60"),
        text=element_text(family="Lato"), plot.title=element_text(face="bold", size=rel(1.8)),
        plot.title.position = "plot", plot.caption.position = "plot")+
  #Add low key legend for a bit of context
  geom_segment(aes(y=arrowmin, yend=arrowmin+200000, x=arrowxpos, xend=arrowxpos), colour="Grey30",
               arrow = arrow(length=unit(0.20,"cm"), ends="both", type = "closed"))+
  #Will need to manually tweak the placement of this annotation
  annotate("text", x=arrowxpos+3, y=250000, label="200,000\ncases\nper\nday", hjust=0, colour="Grey30",
           size=rel(2.5), family="Lato")+
  labs(title="The eternal spiral of COVID",
       subtitle="COVID case numbers in the UK since the start of the pandemic",
       caption="Data from coronavirus.data.gov.uk | Inspiration from the NYT | Plot by @VictimOfMaths")

  
dev.off()

#Repeat with hospital admissions. Because why not

#Read in data from dashboard
source2 <- "https://api.coronavirus.data.gov.uk/v2/data?areaType=overview&metric=newAdmissions&format=csv"

temp2 <- tempfile()
temp2 <- curl_download(url=source2, destfile=temp2, quiet=FALSE, mode="wb")

spiralincrement2 <- 20

data2 <- read.csv(temp2) %>% 
  mutate(date=as.Date(date)) %>% 
  arrange(date) %>% 
  filter(date!=as.Date("2020-12-31")) %>% 
  mutate(adm_roll=roll_mean(newAdmissions, 5, align="center", fill=NA),
         #Create variable to represent the base of each bar - change the number to tighten/relax the spiral
         increment=spiralincrement2*c(1:n()),
         #Add cases to the base to get the top of each bar
         incrementadm=increment+adm_roll,
         year=year(date)) %>% 
  #Calculate the number of days since the start of the year
  group_by(year) %>% 
  mutate(yeardays=as.numeric(difftime(date ,as.Date(paste0(year, "-01-01")) , units = c("days")))) %>% 
  ungroup()

seg20212 <- data2$increment[data2$year==2020 & data2$yeardays==364]-spiralincrement2*0.5
seg21222 <- data2$increment[data2$year==2021 & data2$yeardays==364]-spiralincrement2*0.5
arrowmin2 <- max(data2$increment[data2$year==2022 & !is.na(data2$adm_roll)])+spiralincrement2*4
arrowxpos2 <- max(data2$yeardays[data2$year==2022 & !is.na(data2$adm_roll)])+4

agg_tiff("Outputs/COVIDAdmissionsSpiral.tiff", units="in", width=8, height=8, res=800)
ggplot()+
  #Need to plot each year separately
  geom_rect(data=data2 %>% filter(year==2020 & ! is.na(adm_roll)),
            aes(xmin=yeardays, xmax=yeardays+1, ymin=increment, ymax=incrementadm, 
                fill=adm_roll), show.legend=FALSE)+
  geom_rect(data=data2 %>% filter(year==2021 & ! is.na(adm_roll)),
            aes(xmin=yeardays, xmax=yeardays+1, ymin=increment, ymax=incrementadm, 
                fill=adm_roll), show.legend=FALSE)+
  geom_rect(data=data2 %>% filter(year==2022 & ! is.na(adm_roll)),
            aes(xmin=yeardays, xmax=yeardays+1, ymin=increment, ymax=incrementadm, 
                fill=adm_roll), show.legend=FALSE)+
  #negative offset is to cover up the base of the bars
  geom_line(data=data2 %>% filter(year=="2020" & ! is.na(adm_roll)),
            aes(x=yeardays, y=increment), colour="black")+
  geom_line(data=data2 %>% filter(year=="2021"),
            aes(x=yeardays, y=increment), colour="black")+
  geom_line(data=data2 %>% filter(year=="2022" & yeardays<arrowxpos2-2),
            aes(x=yeardays, y=increment), colour="black")+
  #Add a couple of tiny segments to patch the holes in the baseline at the end of each year
  geom_segment_straight(aes(x=363.5, xend=0.5, y=seg20212, yend=seg20212+2*spiralincrement2), 
                        colour="black")+
  geom_segment_straight(aes(x=363.5, xend=0.5, y=seg21222, yend=seg21222+2*spiralincrement2), 
                        colour="black")+
  scale_x_continuous(breaks=c(0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334),
                     labels=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep",
                              "Oct", "Nov", "Dec"))+
  scale_colour_paletteer_c("viridis::mako", direction=-1)+
  scale_fill_paletteer_c("viridis::mako", direction=-1)+
  coord_polar()+
  theme_void()+
  theme(panel.grid.major.x=element_line(colour="Grey90"),
        axis.text.x=element_text(colour="Grey60"),
        text=element_text(family="Lato"), plot.title=element_text(face="bold", size=rel(1.8)),
        plot.title.position = "plot", plot.caption.position = "plot")+
  #Add low key legend for a bit of context
  #Add low key legend for a bit of context
  geom_segment(aes(y=arrowmin2, yend=arrowmin2+2500, x=arrowxpos2, xend=arrowxpos2), colour="Grey30",
               arrow = arrow(length=unit(0.15,"cm"), ends="both", type = "closed"))+
  #Will need to manually tweak the placement of this annotation
  annotate("text", x=arrowxpos2+3, y=14500, label="2,500\nadmissions\nper day", hjust=0, colour="Grey30",
           size=rel(2.5), family="Lato")+
  labs(title="The eternal spiral of COVID",
       subtitle="Daily new COVID admissions in the UK since the start of the pandemic",
       caption="Data from coronavirus.data.gov.uk | Inspiration from the NYT | Plot by @VictimOfMaths")

dev.off()

#And a sensible version
ggplot(data2, aes(x=yeardays, y=adm_roll, colour=as.factor(year)))+
  geom_line()+
  scale_x_continuous(breaks=c(0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334),
                     labels=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep",
                              "Oct", "Nov", "Dec"))+
  coord_polar()+
  theme_void()+
  theme(panel.grid.major.x=element_line(colour="Grey90"),
        axis.text.x=element_text(colour="Grey60"),
        text=element_text(family="Lato"), plot.title=element_text(face="bold", size=rel(1.8)),
        plot.title.position = "plot", plot.caption.position = "plot")

