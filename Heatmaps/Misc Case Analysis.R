rm(list=ls())

library(tidyverse)
library(curl)
library(paletteer)
library(lubridate)
library(gt)

daydata <- read.csv("COVID_LA_Plots/LACases.csv")[,-c(1)]

daydata$date <- as.Date(daydata$date)

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
  
