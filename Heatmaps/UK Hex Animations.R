rm(list=ls())

library(tidyverse)
library(geojsonio)
library(broom)
library(zoo)
library(gganimate)

#Get English UTLA-level case trajectories
#Prepared by COVID_LA_Plots/Underlying_Code.R
data <- read.csv("COVID_LA_Plots/LACases.csv")[,-c(1)]

#Sort out Buckinghamshire
temp <- subset(data, code=="E06000060")

data$code <- if_else(data$code=="E06000060", "E07000004", as.character(data$code))
data$name <- if_else(data$name=="Buckinghamshire", "Aylesbury Vale", as.character(data$name))

temp1 <- temp
temp1$code <- "E07000005"
temp1$name <- "Chiltern"

temp2 <- temp
temp2$code <- "E07000006"
temp2$name <- "South Bucks"

temp$code <- "E07000007"
temp$name <- "Wycombe"

data <- bind_rows(data, temp, temp1, temp2)

#Bring in hexmap
#Read in hex boundaries (adapted from from https://olihawkins.com/2018/02/1 and ODI Leeds)
hex <- geojson_read("Data/UKLA.geojson", what="sp")

# Fortify into a data frame format to be shown with ggplot2
hexes <- tidy(hex, region="id")

hexes$id <- if_else(hexes$id=="E09000001", "E09000012", hexes$id)

data <- left_join(hexes, data, by=c("id"="code"), all.y=TRUE)
data$date <- as.Date(data$date)

#Remove Isles of Scilly which are too small to have their own data
data <- subset(data, id!="E06000053")

#extract latest date with full UK data
data <- data %>%
  group_by(country) %>%
  mutate(min=min(date), max=max(date))

completefrom <- max(data$min, na.rm=TRUE)
completeto <- min(data$max, na.rm=TRUE)

HexAnimUK <- ggplot()+
  geom_polygon(data=subset(data, date>as.Date("2020-03-06") & date<=completeto & country!="Republic of Ireland"), 
               aes(x=long, y=lat, group=id, fill=casesroll_avg))+
  coord_fixed()+
  scale_fill_distiller(palette="Spectral", name="Daily confirmed\ncases (7-day\nrolling avg.)", na.value="white")+
  theme_classic()+
  theme(axis.line=element_blank(), axis.ticks=element_blank(), axis.text=element_blank(),
        axis.title=element_blank(),  plot.title=element_text(face="bold"))+
  transition_time(date)+
  labs(title="Visualising the spread of COVID-19 across the UK",
       subtitle="Rolling 7-day average number of new confirmed cases.\nDate: {frame_time}",
       caption="Data from PHE, PHW, PHS & DoHNI\nVisualisation by @VictimOfMaths")

animate(HexAnimUK, duration=18, fps=10, width=2000, height=3000, res=300, renderer=gifski_renderer("Outputs/HexAnimUK.gif"), 
        end_pause=60)
