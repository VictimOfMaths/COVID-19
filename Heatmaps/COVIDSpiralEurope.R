rm(list=ls())

library(tidyverse)
library(curl)
library(ggtext)
library(extrafont)
library(RcppRoll)
library(ragg)
library(paletteer)
library(lubridate)
library(geofacet)

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

#Read in hospital data from Our World In Data
source <- "https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/hospitalizations/covid-hospitalizations.csv"

temp <- tempfile()
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

data <- read.csv(temp) %>% 
  mutate(date=as.Date(date)) %>%
  filter(indicator %in% c("Daily ICU occupancy per million", 
                          "Daily hospital occupancy per million", 
                          "Weekly new hospital admissions per million",
                          "Weekly new ICU admissions per million")) %>% 
  filter(iso_code %in% c("AUT", "BEL", "BGR", "HRV", "CYP", "CZE", "DNK", "EST", "FIN", "FRA",
                         "DEU", "GRC", "HUN", "ISL", "IRL", "ITA", "LIE", "LTU", "LUX", "MLT",
                         "NLD", "NOR", "POL", "PRT", "ROU", "SRB", "SVK", "SVN", "ESP", "SWE",
                         "CHE", "GBR"))

#Set value for increment size (basically controls the tightness of the spiral, bigger numbers =>
#a looser spiral)
spiralincrement <- 5

dailydata <- data %>% 
  filter(indicator %in% c("Daily ICU occupancy per million", 
                          "Daily hospital occupancy per million")) %>% 
  #2020 being a leap year messes things up, so remove 31st December 2020 (arbitrarily) 
  #to make all the years the same length
  filter(date!=as.Date("2020-12-31")) %>% 
  group_by(iso_code, entity, indicator) %>%
  arrange(date) %>% 
  mutate(value_roll=roll_mean(value, 7, align="center", fill=NA),
         #Create variable to represent the base of each bar - change the number to tighten/relax the spiral
         increment=spiralincrement*c(1:n()),
         #Add cases to the base to get the top of each bar
         incrementcases=increment+value_roll,
         year=year(date)) %>% 
  ungroup() %>% 
  #Calculate the number of days since the start of the year
  group_by(year) %>% 
  mutate(yeardays=as.numeric(difftime(date ,as.Date(paste0(year, "-01-01")) , units = c("days")))) %>% 
  ungroup()

#Occupancy plot
occdata <- dailydata %>% filter(indicator=="Daily hospital occupancy per million")

#Pull out a couple of parameters to control the positioning of the segments
params <- data.frame(entity=unique(occdata$entity)) %>% 
  merge(occdata)
seg2021 <- occdata$increment[occdata$year==2020 & occdata$yeardays==364]-spiralincrement*0.5
seg2122 <- occdata$increment[occdata$year==2021 & occdata$yeardays==364]-spiralincrement*0.5
arrowmin <- max(occdata$increment[occdata$year==2022 & !is.na(occdata$value_roll)])+spiralincrement*4
arrowxpos <- max(occdata$yeardays[occdata$year==2022 & !is.na(occdata$value_roll)])+4

occparams <- occdata %>% group_by(entity, iso_code) %>% 
  summarise(seg2021=increment[year==2020 & yeardays==max(yeardays[year==2020])]-spiralincrement*0.5,
            seg2122=increment[year==2021 & yeardays==max(yeardays[year==2021])]-spiralincrement*0.5,
            arrowmin=max(increment[year==2022 & !is.na(value_roll)])+spiralincrement*4,
            arrowxpos=max(yeardays[year==2022 & !is.na(value_roll)]))

agg_tiff("Outputs/COVIDSpiralEuropeICU.tiff", units="in", width=14, height=14, res=500)
ggplot(occdata)+
  #Need to plot each year separately, to 'trick' coord_polar to make a spiral, not a single
  #loop
  geom_rect(data=occdata %>% filter(year==2020 & ! is.na(value_roll)),
            aes(xmin=yeardays, xmax=yeardays+1, ymin=increment, ymax=incrementcases, 
                fill=value_roll), show.legend=FALSE)+
  geom_rect(data=occdata %>% filter(year==2021 & ! is.na(value_roll)),
            aes(xmin=yeardays, xmax=yeardays+1, ymin=increment, ymax=incrementcases, 
                fill=value_roll), show.legend=FALSE)+
  geom_rect(data=occdata %>% filter(year==2022 & ! is.na(value_roll)),
            aes(xmin=yeardays, xmax=yeardays+1, ymin=increment, ymax=incrementcases, 
                fill=value_roll), show.legend=FALSE)+
  geom_line(data=occdata %>% filter(year==2020 & ! is.na(value_roll)),
            aes(x=yeardays, y=increment), colour="black")+
  geom_line(data=occdata %>% filter(year==2021),
            aes(x=yeardays, y=increment), colour="black")+
  geom_line(data=occdata %>% filter(year==2022),
            aes(x=yeardays, y=increment), colour="black")+
  #Add a couple of tiny segments to patch the holes in the baseline at the end of each year
  geom_segment_straight(data=occparams, aes(x=363.5, xend=0.5, y=seg2021, yend=seg2021+2*spiralincrement), 
                        colour="black")+
  geom_segment_straight(data=occparams, aes(x=363.5, xend=0.5, y=seg2122, yend=seg2122+2*spiralincrement), 
                        colour="black")+
  scale_x_continuous(breaks=c(0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334),
                     labels=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep",
                              "Oct", "Nov", "Dec"))+
  scale_y_continuous(limits=c(0,5000))+
  scale_colour_paletteer_c("viridis::rocket", direction=-1)+
  scale_fill_paletteer_c("viridis::rocket", direction=-1)+
  #facet_wrap(~entity)+
  facet_geo(~iso_code, grid="europe_countries_grid2", label="name")+
  coord_polar()+
  theme_void()+
  theme(panel.grid.major.x=element_line(colour="Grey90"),
        axis.text.x=element_text(colour="Grey60"),
        text=element_text(family="Lato"), plot.title=element_text(face="bold", size=rel(1.8)),
        plot.title.position = "plot", plot.caption.position = "plot")+
  #Add low key legend for a bit of context
  #geom_segment(aes(y=arrowmin, yend=arrowmin+200000, x=arrowxpos, xend=arrowxpos), colour="Grey30",
  #             arrow = arrow(length=unit(0.20,"cm"), ends="both", type = "closed"))+
  #Will need to manually tweak the placement of this annotation
  #annotate("text", x=arrowxpos+3, y=250000, label="200,000\ncases\nper\nday", hjust=0, colour="Grey30",
  #         size=rel(2.5), family="Lato")+
  labs(title="The eternal spiral of COVID",
       subtitle="COVID hospital occupancy across Europe",
       caption="Data from coronavirus.data.gov.uk | Inspiration from the NYT | Plot by @VictimOfMaths")

dev.off()
