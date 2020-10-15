rm(list=ls())

library(tidyverse)
library(curl)
library(paletteer)
library(lubridate)
library(gt)
library(sf)
library(rmapshaper)

daydata <- read.csv("COVID_LA_Plots/LACases.csv")[,-c(1)]

daydata$date <- as.Date(daydata$date)

#New tiered system

tiers <- daydata %>% filter(country=="England" & name!="England")

tiers$tier <- case_when(
  tiers$name %in% c("Barking and Dagenham", "Barnet", "Bexley", "Brent", "Bromley", "Camden",
                      "Croydon", "Ealing", "Enfield", "Greenwich", "Hackney and City of London",
                      "Haringey", "Havering", "Hillingdon", "Hounslow", "Islington", 
                      "Kensington and Chelsea", "Kingston upon Thames", "Lambeth", "Lewisham",
                      "Merton", "Newham", "Redbridge", "Richmond upon Thames", "Southwark",
                      "Sutton", "Tower Hamlets", "Waltham Forest", "Wandsworth", "Westminster",
                      "Barrow-in-Furness", "Chesterfield", "Erewash", "North East Derbyshire",
                      "Basildon", "Braintree", "Brentwood", "Castle Point", "Chelmsford", 
                      "Colchester", "Epping Forest", "Harlow", "Maldon", "Rochford", "Tendring",
                      "Uttlesford", "Elmbridge", "York") ~ "New Tier 2",
  tiers$name %in% c("Cheshire West and Chester", "Cheshire East", "Warrington", "Manchester",
                      "Bolton", "Bury", "Stockport", "Tameside", "Trafford", "Wigan", "Salford",
                      "Rochdale", "Oldham", "Blackpool", "Blackburn with Darwen",
                      "Burnley", "Chorley", "Fylde", "Hyndburn", "Lancaster", "Pendle", "Preston",
                      "Ribble Valley", "Rossendale", "South Ribble", "West Lancashire", "Wyre",
                      "Leeds", "Bradford", "Kirklees", "Calderdale", "Wakefield", "Barnsley", 
                      "Rotherham", "Doncaster", "Sheffield", "County Durham", "Northumberland", "Newcastle",
                      "South Tyneside", "North Tyneside", "Gateshead", "Sunderland", "Middlesbrough",
                      "Redcar and Cleveland", "Stockton-on-Tees", "Darlington", "Hartlepool",
                      "Birmingham", "Sandwell", "Solihull", "Wolverhampton", "Walsall", "Leicester",
                      "Oadby and Wigston", "Ashfield", "Bassetlaw", "Broxtowe", "Gedling", 
                      "Mansfield", "Newark and Sherwood", "Nottingham", "Rushcliffe") ~ "Tier 2",
  tiers$name=="High Peak" ~ "Partial Tier 2",
  tiers$name %in% c("Liverpool", "Knowsley", "Wirral", "St Helens", "Sefton", "Halton") ~ "Tier 3",
  TRUE ~ "Tier 1"
)

ggplot(tiers)+
  geom_line(aes(x=date, y=caserate_avg, group=name, colour=tier))+
  scale_x_date(name="", limits=c(as.Date("2020-09-01"), as.Date(max(tiers$date)-days(4))))+
  theme_classic()

#Sort out Buckinghamshire to match shapefile
temp <- subset(tiers, code=="E06000060")

tiers$code <- if_else(tiers$code=="E06000060", "E07000004", as.character(tiers$code))
tiers$name <- if_else(tiers$name=="Buckinghamshire", "Aylesbury Vale", as.character(tiers$name))

temp1 <- temp
temp1$code <- "E07000005"
temp1$name <- "Chiltern"

temp2 <- temp
temp2$code <- "E07000006"
temp2$name <- "South Bucks"

temp$code <- "E07000007"
temp$name <- "Wycombe"

tiers <- bind_rows(tiers, temp, temp1, temp2)

#Bring in map
temp <- tempfile()
temp2 <- tempfile()
source <- "https://opendata.arcgis.com/datasets/1d78d47c87df4212b79fe2323aae8e08_0.zip?outSR=%7B%22latestWkid%22%3A27700%2C%22wkid%22%3A27700%7D"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)

#The actual shapefile has a different name each time you download it, so need to fish the name out of the unzipped file
name <- list.files(temp2, pattern=".shp")
shapefile <- st_read(file.path(temp2, name))

names(shapefile)[names(shapefile) == "lad19cd"] <- "code"

simplemap <- ms_simplify(shapefile, keep=0.2, keep_shapes = TRUE)

map.tiers <- full_join(simplemap, tiers, by="code", all.y=TRUE)
map.tiers$date <- as.Date(map.tiers$date)

map.tiers$tier <- factor(map.tiers$tier, levels=c("Tier 1", "Partial Tier 2", "Tier 2", 
                                                  "New Tier 2", "Tier 3"))

#Map of current cases
tiff("Outputs/COVIDTiersMap.tiff", units="in", width=8, height=9, res=500)
map.tiers %>% 
  filter(date==as.Date(max(tiers$date)-days(4))) %>% 
  ggplot()+
  geom_sf(aes(geometry=geometry, fill=caserate_avg, colour=tier))+
  scale_fill_distiller(palette="Spectral", name="Daily cases\n(rolling 7-day avg.)")+
  scale_colour_paletteer_d("LaCroixColoR::Apricot", direction=-1, name="Restriction level")+
  theme_classic()+
  theme(axis.line=element_blank(), axis.ticks=element_blank(), axis.text=element_blank(),
        axis.title=element_blank(), plot.title=element_text(face="bold"))+
  labs(title="COVID restriction levels and rates of new cases in England",
       subtitle="Government COVID alert levels and rolling 7-day average of confirmed new cases in Local Authorities\nData up to 11th October",
       caption="Data from DHSC & PHE | Plot by @VictimOfMaths")
dev.off()


#Additional analysis of local lockdown restrictions
daydata$flag <- case_when(
  daydata$name %in% c("Bolton", "Bury", "Manchester", "Oldham", "Rochdale", "Salford", 
                      "Stockport", "Tameside", "Trafford", "Wigan")~ 1,
  TRUE ~ 0)

tiff("Outputs/COVIDManchesterLAs.tiff", units="in", width=8, height=6, res=500)
ggplot()+
  #geom_line(data=subset(daydata, date>as.Date("2020-07-01")), 
  #           aes(x=date, y=caserate_avg, group=name), colour="Grey80")+
  geom_line(data=subset(daydata, flag==1 & date>as.Date("2020-07-01")), 
            aes(x=date, y=caserate_avg, colour=name))+
  geom_segment(aes(x=as.Date("2020-08-01"), xend=as.Date("2020-08-01"), y=0, yend=20), 
               colour="Red", linetype=2)+
  scale_x_date(name="Date")+
  scale_y_continuous(name="New cases per 100,000 population per day")+
  scale_colour_paletteer_d("LaCroixColoR::paired", name="")+
  theme_classic()+
  labs(title="The case for wider relaxation of local restrictions in Manchester looks weak",
       subtitle="Rolling 7-day average rates of new confirmed COVID-19 cases",
       caption="Data from PHE | Plot by @VictimOfMaths")
dev.off()

up_arrow <- "<span style=\"color:red\">&#9650;</span>"
down_arrow <- "<span style=\"color:green\">&#9660;</span>"

daydata %>% 
  filter(date %in% c(as.Date("2020-08-01"), as.Date("2020-09-01")) & flag==1) %>% 
  select(name, date, casesroll_avg) %>% 
  spread(date, casesroll_avg) %>% 
  mutate(change=`2020-09-01`-`2020-08-01`, changeperc=change*100/`2020-08-01`) %>% 
  gt() %>%
  tab_spanner(label="7-day average new cases",columns=vars(`2020-08-01`, `2020-09-01`)) %>% 
  tab_spanner(label="Change from start of restrictions", columns=vars(change, changeperc)) %>% 
  cols_label(name="", `2020-08-01`="1st August", `2020-09-01`="1st September",
             change="Absolute", changeperc="Relative") %>% 
  fmt_number(columns=vars(`2020-08-01`, `2020-09-01`, change), decimals=1) %>% 
  fmt_number(columns=vars(changeperc), decimals=0) %>% 
  text_transform(locations=cells_body(columns="change", rows=change<0),
                 fn = function(x) paste(x, down_arrow)) %>% 
  text_transform(locations=cells_body(columns="change", rows=change>0),
                 fn = function(x) paste(x, up_arrow)) %>% 
  text_transform(locations=cells_body(columns="changeperc"),
                 fn = function(x) paste0(x, "%"))

daydata$flag2 <- case_when(
  daydata$name %in% c("Manchester", "Bury", "Tameside", "Rochdale",
                      "Salford", "Oldham", "Preston", "Blackburn with Darwen",
                      "Pendle", "Bradford", "Calderdale", "Kirklees",
                      "Glasgow City", "West Dunbartonshire",
                      "East Renfrewshire", "Leicester",
                      "Bolton", "Trafford") ~ "Restrictions in place",
  daydata$name %in% c("Stockport", "Burnley", "Hyndburn", "Wigan",
                      "Rossendale") ~ "Restrictions relaxed",
  TRUE ~ "No Local Restrictions")

tiff("Outputs/COVIDRestrictions.tiff", units="in", width=10, height=10, res=500)
daydata %>% 
  group_by(name) %>% 
  mutate(max=max(date)) %>% 
  filter(date==max & caserate_avg>1.97 & !name %in% c("England", "Scotland",
                                                      "Wales", "Northern Ireland")) %>% 
  ggplot()+
  geom_col(aes(x=caserate_avg*7, y=fct_reorder(name, caserate_avg), fill=flag2),
           show.legend = FALSE)+
  scale_x_continuous(name="New COVID-19 cases per 100,000 in the last 7 days")+
  scale_y_discrete(name="")+
  scale_fill_manual(values=c("Grey70", "#E41A1C", "#4DAF4A"))+
  theme_classic()+
  theme(plot.subtitle=element_markdown())+
  labs(title="Local COVID-related restrictions are being applied inconsistently",
       subtitle="Local Authorities with restrictions <span style='color:#E41A1C;'>in place </span>or that have been <span style='color:#4DAF4A;'>relaxed in the past week",
       caption="Data from ONS, NRS, DoHNI, PHE, PHW & PHS | Plot by @VictimOfMaths")
dev.off()

daydata$restrictions <- case_when(
  daydata$name=="Leicester" & daydata$date>=as.Date("2020-06-29") ~ "Leicester",
  daydata$name %in% c("Manchester", "Trafford", "Bury", "Tameside", "Rochdale", "Salford", "Bolton",
                      "Oldham", "Preston", "Blackburn with Darwen", "Pendle", "Bradford",
                      "Calderdale", "Kirklees") & daydata$date>=as.Date("2020-07-31") ~ "Northern England",
  daydata$name %in% c("Wigan", "Rossendale") & daydata$date>=as.Date("2020-07-31") & 
    daydata$date<as.Date("2020-08-26") ~ "Wigan & Rossendale",
  daydata$name %in% c("Stockport", "Burnley", "Hyndburn") & daydata$date>=as.Date("2020-07-31") & 
    daydata$date<as.Date("2020-09-02") ~ "Stockport, Burnley & Hyndburn",
  daydata$name=="Aberdeen City" & daydata$date>=as.Date("2020-08-05") & 
    daydata$date<as.Date("2020-08-24") ~ "Aberdeen",
  daydata$name %in% c("Glasgow City", "East Renfrewshire", "West Dunbartonshire") & 
    daydata$date>=as.Date("2020-09-01") ~ "Glasgow")

daydata$restrictions <- factor(daydata$restrictions, levels=c("Leicester", 
                                                              "Wigan & Rossendale",
                                                              "Stockport, Burnley & Hyndburn", 
                                                              "Northern England",
                                                              "Aberdeen",
                                                              "Glasgow"))

temp <- daydata %>% 
  filter(!is.na(restrictions))

restrictedareas <- unique(temp$name)

tiff("Outputs/COVIDRestrictionsLeeds.tiff", units="in", width=8, height=6, res=500)
ggplot()+
  geom_line(data=daydata,
            aes(x=date, y=caserate_avg, group=name), colour="grey75")+
  geom_line(data=subset(daydata, !is.na(restrictions)), 
            aes(x=date, y=caserate_avg, group=name), colour="Tomato")+
  geom_line(data=subset(daydata, name=="Leeds"),
            aes(x=date, y=caserate_avg), colour="RoyalBlue")+
  scale_x_date(limits=c(as.Date("2020-06-20"), as.Date("2020-09-04")), name="")+
  scale_y_continuous(name="Daily confirmed new cases per 100,000")+
  theme_classic()+
  theme(plot.subtitle=element_markdown())+
  labs(title="Leeds is under threat of new lockdown restrictions",
       subtitle="Daily rates of new confirmed COVID-19 cases in <span style='color:royalblue;'>Leeds </span>compared to <span style='color:tomato;'>areas with local restrictions</span>",
       caption="Date from PHE, PHW, PHS & DoHNI | Plot by @VictimOfMaths")
dev.off()

tiff("Outputs/COVIDRestrictionsLeeds2.tiff", units="in", width=8, height=6, res=500)
ggplot()+
  geom_line(data=daydata,
            aes(x=date, y=caserate_avg, group=name), colour="grey75")+
  geom_line(data=subset(daydata, name=="Leeds"),
            aes(x=date, y=caserate_avg), colour="RoyalBlue")+
  geom_line(data=subset(daydata, name=="Rossendale"),
            aes(x=date, y=caserate_avg), colour="ForestGreen")+
  geom_line(data=subset(daydata, name=="South Tyneside"),
            aes(x=date, y=caserate_avg), colour="Purple")+
  scale_x_date(limits=c(as.Date("2020-08-01"), as.Date("2020-09-04")), name="")+
  scale_y_continuous(name="Daily confirmed new cases per 100,000")+
  theme_classic()+
  theme(plot.subtitle=element_markdown())+
  labs(title="Is Leeds the only area under threat of new restrictions?",
       subtitle="Daily rates of new confirmed COVID-19 cases in <span style='color:royalblue;'>Leeds </span>compared to <span style='color:ForestGreen;'>Rossendale</span> and <span style='color:Purple;'>South Tyneside</span>",
       caption="Date from PHE, PHW, PHS & DoHNI | Plot by @VictimOfMaths")
dev.off()

#National-level case figures
tiff("Outputs/COVIDCaserateUK.tiff", units="in", width=8, height=6, res=500)
daydata %>% 
  filter(name %in% c("England", "Wales", "Scotland", "Northern Ireland")) %>% 
  filter(date>=as.Date("2020-07-14") & date<=as.Date("2020-09-10")) %>% 
  ggplot()+
  geom_line(aes(x=date, y=caserate_avg, colour=name))+
  scale_colour_paletteer_d("fishualize::Scarus_quoyi", name="")+
  scale_x_date(name="", date_breaks="1 week", date_labels="%d %b")+
  scale_y_continuous(name="Daily new confirmed COVID-19 cases per 100,000")+
  theme_classic()+
  labs(title="The rise in new confirmed COVID-19 cases has slowed across the UK",
       subtitle="It's not clear if this is due to a lack of available testing, slow processing of the tests that have been conducted,\na genuine slowdown in the increase in prevalence, or a combination of all three.\nHowever, other indicators suggest prevalence is still rising",
       caption="Data from PHE, PHW, PHS, DoHNI | Plot by @VictimOfMaths")
dev.off()  

#Comparisons of local restrictions vs. everywhere else
daydata$flag3 <- case_when(
  daydata$name %in% c("East Renfrewshire", "Glasgow City", "West Dunbartonshire") ~ "Greater Glasgow phase 1",
  daydata$name %in% c("East Dunbartonshire", "Renfrewshire") ~ "Greater Glasgow phase 2",
  daydata$name %in% c("North Lanarkshire","South Lanarkshire") ~ "Lanarkshire",
  daydata$name=="Caerphilly" ~ "Caerphilly",
  daydata$name=="Rhondda Cynon Taf" ~ "Rhondda Cynon Taf",
  daydata$name=="Leicester" ~ "Leicester",
  daydata$name %in% c("Manchester", "Trafford", "Bury", "Tameside", "Rochdale", "Salford", "Bolton",
                      "Oldham", "Preston", "Blackburn with Darwen", "Pendle", "Bradford",
                      "Calderdale", "Kirklees") ~ "Northern England",
  daydata$name %in% c("Wigan", "Rossendale") ~ "Wigan & Rossendale",
  daydata$name %in% c("Stockport", "Burnley", "Hyndburn")  ~ "Stockport, Burnley & Hyndburn",
  daydata$name %in% c()
)

daydata$flag4 <- case_when(
  daydata$name=="Leicester" ~ "Leicester",
  daydata$name %in% c("Manchester", "Trafford", "Bury", "Tameside", "Rochdale", "Salford", "Oldham") ~ "Greater Manchester",
  daydata$name =="Bolton" ~ "Bolton",
  TRUE ~ as.character(daydata$country)
)

tiff("Outputs/COVIDLocalRestrictions1.tiff", units="in", width=10, height=7, res=500)
daydata %>% 
  group_by(flag4, date) %>% 
  filter(country=="England" & !flag4 %in% c("Bolton", "Greater Manchester")) %>% 
  summarise(casesroll_avg=sum(casesroll_avg), pop=sum(pop)) %>% 
  mutate(caserate_avg=casesroll_avg*100000/pop) %>% 
  ggplot()+
  geom_line(aes(x=date, y=caserate_avg, colour=flag4), show.legend = FALSE)+
  geom_segment(aes(x=as.Date("2020-06-29"), xend=as.Date("2020-06-29"), y=0, yend=25),
               colour="#F89088FF", linetype=2)+
  annotate("text", x=as.Date("2020-07-01"), y=23, label="Local restrictions introduced", hjust=0)+
  scale_x_date(name="")+
  scale_y_continuous(name="Daily new cases per 100,000")+
  scale_colour_paletteer_d("palettetown::porygon")+
  theme_classic()+
  theme(plot.subtitle=element_markdown())+
  labs(title="Local Restrictions in Leicester *might* have had a short-term effect",
       subtitle="Rolling 7-day average of new COVID-19 case rates in <span style='color:#F89088FF;'>Leicester </span>compared to <span style='color:#40A0D8FF;'>the rest of England </span>(excluding Greater Manchester)",
       caption="Data from PHE | Plot by @VictimOfMaths")
dev.off()

tiff("Outputs/COVIDLocalRestrictions2.tiff", units="in", width=10, height=7, res=500)
daydata %>% 
  group_by(flag4, date) %>% 
  filter(country=="England" & flag4!="Leicester") %>% 
  summarise(casesroll_avg=sum(casesroll_avg), pop=sum(pop)) %>% 
  mutate(caserate_avg=casesroll_avg*100000/pop) %>% 
  ggplot()+
  geom_line(aes(x=date, y=caserate_avg, colour=flag4), show.legend = FALSE)+
  geom_segment(aes(x=as.Date("2020-07-31"), xend=as.Date("2020-07-31"), y=0, yend=35),
               colour="#017A4AFF", linetype=2)+
  annotate("text", x=as.Date("2020-06-30"), y=32, label="Local restrictions introduced")+
  scale_x_date(name="")+
  scale_y_continuous(name="Daily new cases per 100,000")+
  scale_colour_paletteer_d("awtools::mpalette")+
  theme_classic()+
  theme(plot.subtitle=element_markdown())+
  labs(title="Little evidence that local restrictions in Greater Manchester had much impact",
       subtitle="Rolling 7-day average of new COVID-19 case rates in <span style='color:#017A4AFF;'>Bolton</span> and the rest of <span style='color:#3D98D3FF;'>Greater Manchester </span><br>compared to <span style='color:#FFCE4EFF;'>the rest of England </span>(excluding Leicester)",
       caption="Data from PHE | Plot by @VictimOfMaths")
dev.off()
