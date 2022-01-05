rm(list=ls())

library(tidyverse)
library(curl)
library(RcppRoll)
library(lubridate)
library(ggtext)
library(extrafont)
library(ragg)
library(paletteer)
library(readxl)

theme_custom <- function() {
  theme_classic() %+replace%
    theme(plot.title.position="plot", plot.caption.position="plot",
          strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)),
          plot.title=element_text(face="bold", size=rel(1.5), hjust=0,
                                  margin=margin(0,0,5.5,0)),
          text=element_text(family="Lato"))
}

#Download data (have to do this separately for male and females because the dashboard is weird)
source.f <- "https://api.coronavirus.data.gov.uk/v2/data?areaType=nation&areaCode=E92000001&metric=femaleCases&format=csv"

temp <- tempfile()
temp <- curl_download(url=source.f, destfile=temp, quiet=FALSE, mode="wb")

data.f <- read.csv(temp) %>% 
  mutate(sex="Female")

source.m <- "https://api.coronavirus.data.gov.uk/v2/data?areaType=nation&areaCode=E92000001&metric=maleCases&format=csv"

temp <- tempfile()
temp <- curl_download(url=source.m, destfile=temp, quiet=FALSE, mode="wb")

data.m <- read.csv(temp) %>% 
  mutate(sex="Male")

#Combine and extract daily figures from cumulative ones
data <- bind_rows(data.f, data.m) %>% 
  mutate(date=as.Date(date)) %>% 
  group_by(age, sex) %>% 
  arrange(date) %>% 
  mutate(newcases=value-lag(value, 1),
         ratechange=rate-lag(rate, 1),
         cases_roll=roll_mean(newcases, 7, align="center", fill=NA),
         rates_roll=roll_mean(ratechange, 7, align="center", fill=NA)) %>% 
  ungroup() %>% 
  mutate(age=gsub("_", " ", age),
         age=factor(age, levels=c("0 to 4", "5 to 9", "10 to 14", "15 to 19", "20 to 24",
                                  "25 to 29", "30 to 34", "35 to 39", "40 to 44", 
                                  "45 to 49", "50 to 54", "55 to 59", "60 to 64",
                                  "65 to 69", "70 to 74", "75 to 79", "80 to 84",
                                  "85 to 89", "90+")))

agg_tiff("Outputs/COVIDCasesxSex.tiff", units="in", width=12, height=7, res=500)
ggplot(data %>% filter(date>as.Date("2021-05-25") & date<max(date)-days(3)), 
       aes(x=date, y=rates_roll, colour=sex))+
  geom_line(show.legend=FALSE)+
  scale_x_date(name="")+
  scale_y_continuous(name="Daily new cases per 100,000")+
  scale_colour_manual(values=c("#00cc99", "#6600cc"))+
  facet_wrap(~age)+
  theme_custom()+
  theme(plot.subtitle=element_markdown(), strip.text=element_blank())+
  geom_text(data=data %>% filter(date==as.Date("2021-07-16") & sex=="Male"),
            aes(x=date, y=240, label=age), colour="Black", family="Lato", fontface="bold")+
  labs(title="COVID case rates have diverged for men and women",
       subtitle="Rolling 7-day average of new COVID case rates in <span style='color:#6600cc;'>men</span> and <span style='color:#00cc99;'>women</span> in England, by age.",
       caption="Data from coronavirus.data.gov.uk | Plot by @VictimOfMaths")
dev.off()

agg_tiff("Outputs/COVIDCasesxSexFull.tiff", units="in", width=10, height=7, res=500)
ggplot(data %>% filter(date<max(date)-days(3)), 
       aes(x=date, y=rates_roll, colour=sex))+
  geom_line(show.legend=FALSE)+
  scale_x_date(name="")+
  scale_y_continuous(name="Daily new cases per 100,000")+
  scale_colour_manual(values=c("#00cc99", "#6600cc"))+
  facet_wrap(~age)+
  theme_custom()+
  theme(plot.subtitle=element_markdown())+
  labs(title="The gender gap in COVID cases in younger adults in England is growing",
       subtitle="Rolling 7-day average of new COVID case rates in <span style='color:#6600cc;'>men</span> and <span style='color:#00cc99;'>women</span> in England, by age.",
       caption="Data from coronavirus.data.gov.uk | Plot by @VictimOfMaths")
dev.off()

data %>% select(date, age, rates_roll, sex) %>% 
  spread(sex, rates_roll) %>% 
  mutate(ratio=Male/(Male+Female), test=Female-Male) %>% 
  filter(date>as.Date("2021-05-01")) %>% 
  ggplot(aes(x=date, y=ratio, colour=age, group=age))+
  geom_line()+
  theme_custom()

heatmapdata <- data %>% 
  select(sex, rates_roll, age, date) %>% 
  spread(sex, rates_roll) %>% 
  mutate(maleprop=Male/(Male+Female))

agg_tiff("Outputs/COVIDCasesxSexHeatmap.tiff", units="in", width=10, height=7, res=500)
ggplot(heatmapdata %>% filter(date>as.Date("2021-05-25") & date<max(date)-days(3)))+
  geom_tile(aes(x=date, y=age, fill=maleprop))+
  theme_custom()+
  scale_fill_distiller(palette="PRGn", limits=c(0.33,0.67), name="", breaks=c(0.33,0.5,0.67),
                       labels=c("2 Female cases\nfor each\nmale case", "Equal male\nand female\ncases", 
                                "2 Male cases\nfor each\nfemale case"))+
  scale_x_date(name="")+
  scale_y_discrete(name="Age")+
  theme(legend.position = "top", plot.subtitle=element_markdown())+
  guides(fill = guide_colorbar(title.position = 'top', title.hjust = .5,
                               barwidth = unit(20, 'lines'), barheight = unit(.5, 'lines')))+
  labs(title="COVID cases in 15-24 year olds are very female-dominated",
       subtitle="Ratio of <span style='color:#1b7837;'>female</span> to <span style='color:#762a83;'>male</span> cases in England, based on a 7-day rolling average",
       caption="Data from coronavirus.data.gov.uk | Plot by @VictimOfMaths")
dev.off()

agg_tiff("Outputs/COVIDCasesxSexHeatmapFull.tiff", units="in", width=10, height=7, res=500)
ggplot(heatmapdata %>% filter(date>as.Date("2020-03-10") &date<max(date)-days(3)))+
  geom_tile(aes(x=date, y=age, fill=maleprop))+
  theme_custom()+
  scale_fill_distiller(palette="PRGn", limits=c(0.2,0.8), name="", breaks=c(0.2, 0.33,0.5,0.67, 0.8),
                       labels=c("4 Female cases\nfor each\nmale case", 
                                "2:1", 
                                "Equal male\nand female\ncases", 
                                "2:1", 
                                "4 Male cases\nfor each\nmale case"))+
  scale_x_date(name="")+
  scale_y_discrete(name="Age")+
  theme(legend.position = "top", plot.subtitle=element_markdown())+
  guides(fill = guide_colorbar(title.position = 'top', title.hjust = .5,
                               barwidth = unit(20, 'lines'), barheight = unit(.5, 'lines')))+
  labs(title="COVID cases in 20 year-olds in the 1st wave were heavily female-dominated",
       subtitle="Ratio of <span style='color:#1b7837;'>female</span> to <span style='color:#762a83;'>male</span> cases in England, based on a 7-day rolling average",
       caption="Data from coronavirus.data.gov.uk | Plot by @VictimOfMaths")
dev.off()

agg_tiff("Outputs/COVIDCasesxSexHeatmapU60s.tiff", units="in", width=10, height=6, res=800)
ggplot(temp<-heatmapdata %>% filter(date>as.Date("2021-05-25") & date<max(date)-days(3) &
         age %in% c("0 to 4", "5 to 9", "10 to 14", "15 to 19", "20 to 24",
                    "25 to 29", "30 to 34", "35 to 39", "40 to 44", 
                    "45 to 49", "50 to 54", "55 to 59")))+
  geom_tile(aes(x=date, y=age, fill=maleprop))+
  theme_custom()+
  scale_fill_distiller(palette="PRGn", limits=c(0.37,0.63), name="", breaks=c(0.37,0.5,0.63),
                       labels=c("17 Female cases\nfor every\n10 male cases", "Equal male\nand female\ncases", 
                                "17 Male cases\nfor every\n10 female cases"))+
  scale_x_date(name="")+
  scale_y_discrete(name="Age")+
  theme(legend.position = "top", plot.subtitle=element_markdown())+
  guides(fill = guide_colorbar(title.position = 'top', title.hjust = .5,
                               barwidth = unit(20, 'lines'), barheight = unit(.5, 'lines')))+
  labs(title="COVID cases in working age adults are becomine increasingly female-dominated",
       subtitle="Ratio of <span style='color:#1b7837;'>female</span> to <span style='color:#762a83;'>male</span> cases in England, based on a 7-day rolling average",
       caption="Date from coronavirus.data.gov.uk | Plot by @VictimOfMaths")
dev.off()

#Calculate case rate ratios
caseratios <- data %>% 
  group_by(age, date) %>% 
  summarise(cases_roll=sum(cases_roll)) %>% 
  mutate(sex="Total") %>%
  ungroup() %>% 
  bind_rows(data) %>% 
  filter(!is.na(cases_roll)) %>% 
  select(age, sex, date, cases_roll) %>% 
  group_by(age, sex) %>% 
  mutate(caseratio=cases_roll/lag(cases_roll, 7)) %>% 
  ungroup()

#Whole population
popheatmap <- caseratios %>% 
  filter(sex=="Total" & date>as.Date("2020-04-01")) 

agg_tiff("Outputs/COVIDCaseRatioHeatmap.tiff", units="in", width=10, height=6, res=800)
ggplot(popheatmap)+
  geom_tile(aes(x=date, y=age, fill=caseratio))+
  scale_x_date(name="")+
  scale_y_discrete(name="Age")+
  scale_fill_paletteer_c("pals::warmcool", limit=c(0.249,4), direction=-1 ,
                         trans="log", breaks=c(0.25, 0.5, 1, 2, 4), 
                         labels=c("-75%", "-50%", "No change", "+100%", "+300%"),
                         name="Change in cases in the past week")+
  theme_custom()+
  theme(legend.position="top")+
  guides(fill = guide_colorbar(title.position = 'top', title.hjust = .5,
                               barwidth = unit(20, 'lines'), barheight = unit(.5, 'lines')))+
  labs(title="Generally COVID case numbers have risen or fallen across all age groups at once",
       subtitle="Weekly change in the rolling 7-day average number of new COVID cases in England, by age group",
       caption="Data from coronavirus.data.gov.uk | Plot inspired by @danc00ks0n & @russss | Plot by @VictimOfMaths")

dev.off()

agg_tiff("Outputs/COVIDCaseRatioHeatmapRecent.tiff", units="in", width=10, height=6, res=800)
ggplot(popheatmap %>% filter(date>as.Date("2021-10-01") & date<max(date)))+
  geom_tile(aes(x=date, y=age, fill=caseratio))+
  scale_x_date(name="")+
  scale_y_discrete(name="Age")+
  scale_fill_paletteer_c("pals::warmcool", limit=c(1/3.1,3.1), direction=-1 ,
                         trans="log", breaks=c(0.5, 1, 1.9999), 
                         labels=c("-50%", "No change", "+100%"),
                         name="Change in cases in the past week")+
  theme_custom()+
  theme(legend.position="top")+
  guides(fill = guide_colorbar(title.position = 'top', title.hjust = .5,
                               barwidth = unit(20, 'lines'), barheight = unit(.5, 'lines')))+
  labs(title="The Omicron wave has shifted into older, more vulnerable ages",
       subtitle="Weekly change in the rolling 7-day average number of new COVID cases in England, by age group",
       caption="Data from coronavirus.data.gov.uk | Plot inspired by @danc00ks0n & @russss | Plot by @VictimOfMaths")

dev.off()
  
popheatmapxsex <- caseratios %>% 
  filter(sex!="Total" & date>as.Date("2020-04-01")) 

agg_tiff("Outputs/COVIDCaseRatioHeatmapxSex.tiff", units="in", width=10, height=6, res=800)
ggplot(popheatmapxsex)+
  geom_tile(aes(x=date, y=age, fill=caseratio))+
  scale_x_date(name="")+
  scale_y_discrete(name="Age")+
  scale_fill_paletteer_c("pals::warmcool", limit=c(0.249,4), direction=-1 ,
                         trans="log", breaks=c(0.25, 0.5, 1, 2, 4), 
                         labels=c("-75%", "-50%", "No change", "+100%", "+300%"),
                         name="Change in cases in the past week")+
  facet_wrap(~sex)+
  theme_custom()+
  theme(legend.position="top")+
  guides(fill = guide_colorbar(title.position = 'top', title.hjust = .5,
                               barwidth = unit(20, 'lines'), barheight = unit(.5, 'lines')))+
  labs(title="Generally COVID case numbers have risen or fallen across all age groups at once",
       subtitle="Weekly change in the rolling 7-day average number of new COVID cases in England, by age group",
       caption="Data from coronavirus.data.gov.uk | Plot inspired by @danc00ks0n & @russss | Plot by @VictimOfMaths")

dev.off()

agg_tiff("Outputs/COVIDCaseRatioHeatmapxSexRecent.tiff", units="in", width=10, height=6, res=800)
ggplot(popheatmapxsex %>% filter(date>as.Date("2021-05-01")))+
  geom_tile(aes(x=date, y=age, fill=caseratio))+
  scale_x_date(name="")+
  scale_y_discrete(name="Age")+
  scale_fill_paletteer_c("pals::warmcool", limit=c(0.249,4), direction=-1 ,
                         trans="log", breaks=c(0.25, 0.5, 1, 2, 4), 
                         labels=c("-75%", "-50%", "No change", "+100%", "+300%"),
                         name="Change in cases in the past week")+
  facet_wrap(~sex)+
  theme_custom()+
  theme(legend.position="top")+
  guides(fill = guide_colorbar(title.position = 'top', title.hjust = .5,
                               barwidth = unit(20, 'lines'), barheight = unit(.5, 'lines')))+
  labs(title="Generally COVID case numbers have risen or fallen across all age groups at once",
       subtitle="Weekly change in the rolling 7-day average number of new COVID cases in England, by age group",
       caption="Data from coronavirus.data.gov.uk | Plot inspired by @danc00ks0n & @russss | Plot by @VictimOfMaths")

dev.off()

#Get tidy case rates by bringing in ONS 2020 populations
#Bring in ONS 2020 population figures for comparison
url <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fpopulationandmigration%2fpopulationestimates%2fdatasets%2fpopulationestimatesforukenglandandwalesscotlandandnorthernireland%2fmid2020/ukpopestimatesmid2020on2021geography.xls"

temp <- tempfile()
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

ONSpop_m <- read_excel(temp, sheet="MYE2 - Males", range="E8:CQ12") %>% 
  slice_tail() %>% 
  gather(age, Male, c(1:ncol(.)))

ONSpop_f <- read_excel(temp, sheet="MYE2 - Females", range="E8:CQ12") %>% 
  slice_tail() %>% 
  gather(age, Female, c(1:ncol(.)))

ONSpop <- merge(ONSpop_m, ONSpop_f) %>% 
  mutate(age=as.numeric(substr(age, 1, 2)),
         age=case_when(
           age<5 ~ "0 to 4",
           age<10 ~ "5 to 9",
           age<15 ~ "10 to 14",
           age<20 ~ "15 to 19",
           age<25 ~ "20 to 24",
           age<30 ~ "25 to 29",
           age<35 ~ "30 to 34",
           age<40 ~ "35 to 39",
           age<45 ~ "40 to 44",
           age<50 ~ "45 to 49",
           age<55 ~ "50 to 54",
           age<60 ~ "55 to 59",
           age<65 ~ "60 to 64",
           age<70 ~ "65 to 69",
           age<75 ~ "70 to 74",
           age<80 ~ "75 to 79",
           age<85 ~ "80 to 84",
           age<90 ~ "85 to 89",
           TRUE ~ "90+")) %>% 
   group_by(age) %>% 
  summarise(Male=sum(Male), Female=sum(Female)) %>% 
  ungroup()

ratesdata <- caseratios %>% 
  merge(ONSpop %>% 
  rowwise() %>% 
  mutate(Total=Male+Female) %>% 
  gather(sex, pop, c(2:4)) %>%
  ungroup()) %>% 
  mutate(rates_roll=cases_roll*100000/pop)

agg_png("Outputs/COVIDCasesHeatmapxAge.png", units="in", width=10, height=6, res=500)
ggplot(ratesdata %>% filter(sex=="Total" & date>as.Date("2021-10-01") & date<max(date)-days(1)),
       aes(x=date, y=age, fill=rates_roll))+
  geom_tile()+
  scale_x_date(name="")+
  scale_y_discrete(name="Age")+
  scale_fill_paletteer_c("viridis::inferno", name="Cases per 100,000")+
  theme_custom()+
  theme(legend.position="top")+
  guides(fill = guide_colorbar(title.position = 'top', title.hjust = .5, 
                               barwidth = unit(20, 'lines'), barheight = unit(.5, 'lines')))+
  labs(title="COVID cases are working their way up the age groups",
       subtitle="Rolling 7-day average rates of new COVID-19 cases by age in England",
       caption="Data from coronavirus.data.gov.uk | Plot by @VictimOfMaths")

dev.off()

agg_png("Outputs/COVIDCaseRatesLineRecent.png", units="in", width=10, height=6, res=500)
ggplot(ratesdata %>% filter(sex=="Total" & date>as.Date("2021-10-01") & date<max(date)-days(1)), 
       aes(x=date, y=rates_roll, colour=age))+

  geom_line()+
  scale_x_date(name="")+
  scale_y_continuous(name="New COVID cases per 100,000")+
  scale_colour_paletteer_d("pals::stepped", name="Age")+
  theme_custom()+
  labs(title="Current case rates have risen in all adult age groups",
       subtitle="Rolling 7-day average number of new COVID cases in England, by age group",
       caption="Data from coronavirus.data.gov.uk | Plot by @VictimOfMaths")

dev.off()

agg_png("Outputs/COVIDCaseRatesLineRecent60Plus.png", units="in", width=10, height=6, res=800)
ggplot(ratesdata %>% filter(sex=="Total" & date>as.Date("2021-10-01") & 
                              date<max(date)-days(1)), 
       aes(x=date, y=rates_roll, group=age))+
  geom_line(colour="Grey70")+
  geom_line(data=ratesdata %>% filter(sex=="Total" & date>as.Date("2021-10-01") & 
                                        date<max(date)-days(1) &
                                        age %in% c("60 to 64", "65 to 69", "70 to 74",
                                                   "75 to 79", "80 to 84", "85 to 89",
                                                   "90+")),
            aes(colour=age))+
  scale_x_date(name="")+
  scale_y_continuous(name="New COVID cases per 100,000")+
  scale_colour_manual(values=c("#0F8299FF", "#3E9FB3FF", "#7ABECCFF", "#B8DEE6FF", 
                               "#3D0F99FF", "#653EB3FF", "#967ACCFF"), name="Age")+
  theme_custom()+
  labs(title="COVID cases have fallen in the ages that received boosters earliest",
       subtitle="Rolling 7-day average number of new COVID cases in England, by age group",
       caption="Data from coronavirus.data.gov.uk | Plot by @VictimOfMaths")

dev.off()

agg_png("Outputs/COVIDCaseRatesLineRecentu60.png", units="in", width=10, height=6, res=800)
ggplot(ratesdata %>% filter(sex=="Total" & date>as.Date("2021-10-01") & 
                              date<max(date)-days(1)), 
       aes(x=date, y=rates_roll, group=age))+
  geom_line(colour="Grey70")+
  geom_line(data=ratesdata %>% filter(sex=="Total" & date>as.Date("2021-10-01") & 
                                        date<max(date)-days(1) &
                                        !age %in% c("60 to 64", "65 to 69", "70 to 74",
                                                   "75 to 79", "80 to 84", "85 to 89",
                                                   "90+")),
            aes(colour=age))+
  scale_x_date(name="")+
  scale_y_continuous(name="New COVID cases per 100,000")+
  scale_colour_paletteer_d("pals::stepped", name="Age")+
  theme_custom()+
  labs(title="COVID cases have risen in younger age groups",
       subtitle="Rolling 7-day average number of new COVID cases in England, by age group",
       caption="Data from coronavirus.data.gov.uk | Plot by @VictimOfMaths")

dev.off()

#Line chart of CRRs
agg_tiff("Outputs/COVIDCaseRatioLineRecent.tiff", units="in", width=10, height=6, res=800)
ggplot(popheatmap %>% filter(date>as.Date("2021-10-01") & date<max(date)))+
  geom_hline(yintercept=1, colour="Grey50")+
  geom_hline(yintercept=0.5, colour="Grey70", linetype=2)+
  geom_hline(yintercept=2, colour="Grey70", linetype=2)+
  geom_text(aes(x=as.Date("2021-10-10"), y=0.52, label="Cases halving each week"),
            colour="Grey70")+
  geom_text(aes(x=as.Date("2021-10-10"), y=2.1, label="Cases doubling each week"),
            colour="Grey70")+
  geom_line(aes(x=date, y=caseratio, colour=age))+
  scale_x_date(name="")+
  scale_y_continuous(trans="log", name="7-day Case Rate Ratio",
                     breaks=c(0.5, 1, 2), 
                     labels=c("-50%", "No change", "+100%"))+
  scale_colour_paletteer_d("pals::stepped", name="Age")+
  theme_custom()+
  labs(title="COVID cases are growing at different rates in different age groups",
       subtitle="Weekly change in the rolling 7-day average number of new COVID cases in England, by age group",
       caption="Data from coronavirus.data.gov.uk | Plot by @VictimOfMaths")

dev.off()

agg_tiff("Outputs/COVIDCaseRatioLineRecent019.tiff", units="in", width=10, height=6, res=800)
ggplot(popheatmap %>% filter(date>as.Date("2021-10-01") & date<max(date)))+
  geom_hline(yintercept=1, colour="Grey50")+
  geom_hline(yintercept=0.5, colour="Grey70", linetype=2)+
  geom_hline(yintercept=2, colour="Grey70", linetype=2)+
  geom_text(aes(x=as.Date("2021-10-10"), y=0.52, label="Cases halving each week"),
            colour="Grey70")+
  geom_text(aes(x=as.Date("2021-10-10"), y=2.1, label="Cases doubling each week"),
            colour="Grey70")+
  #geom_rect(aes(xmin=as.Date("2021-10-25"), xmax=as.Date("2021-10-29"),
  #              ymin=0.5, ymax=2), fill="Grey90", colour="Grey90")+
  #geom_text(aes(x=as.Date("2021-10-23"), y=1.5, label="Half term"),
  #          colour="Grey70", angle=90)+
  geom_line(aes(x=date, y=caseratio, group=age), colour="Grey80")+
  geom_line(data=popheatmap %>% filter(date>as.Date("2021-10-01") & date<max(date) &
                                         age %in% c("0 to 4", "5 to 9", "10 to 14",
                                                    "15 to 19")),
            aes(x=date, y=caseratio, colour=age))+
  scale_x_date(name="")+
  scale_y_continuous(trans="log", name="7-day Case Rate Ratio",
                     breaks=c(0.5, 1, 2), 
                     labels=c("-50%", "No change", "+100%"))+
  scale_colour_manual(values=c("#fa9fb5", "#f768a1", "#c51b8a", "#7a0177"), name="Age")+
  theme_custom()+
  theme(plot.title=element_markdown())+
  labs(title="COVID case rate ratios are low but rising in <span style='color:#c51b8a;'>the under 20s</span>",
       subtitle="Weekly change in the rolling 7-day average number of new COVID cases in England, by age group",
       caption="Data from coronavirus.data.gov.uk | Plot by @VictimOfMaths")

dev.off()

agg_tiff("Outputs/COVIDCaseRatioLineRecent2039.tiff", units="in", width=10, height=6, res=800)
ggplot(popheatmap %>% filter(date>as.Date("2021-10-01") & date<max(date)))+
  geom_hline(yintercept=1, colour="Grey50")+
  geom_hline(yintercept=0.5, colour="Grey70", linetype=2)+
  geom_hline(yintercept=2, colour="Grey70", linetype=2)+
  geom_text(aes(x=as.Date("2021-10-10"), y=0.52, label="Cases halving each week"),
            colour="Grey70")+
  geom_text(aes(x=as.Date("2021-10-10"), y=2.1, label="Cases doubling each week"),
            colour="Grey70")+
  geom_line(aes(x=date, y=caseratio, group=age), colour="Grey80")+
  geom_line(data=popheatmap %>% filter(date>as.Date("2021-10-01") & date<max(date) &
                                         age %in% c("20 to 24", "25 to 29", "30 to 34",
                                                    "35 to 39")),
            aes(x=date, y=caseratio, colour=age))+
  scale_x_date(name="")+
  scale_y_continuous(trans="log", name="7-day Case Rate Ratio",
                     breaks=c(0.5, 1, 2), 
                     labels=c("-50%", "No change", "+100%"))+
  scale_colour_manual(values=c("#fa9fb5", "#f768a1", "#c51b8a", "#7a0177"), name="Age")+
  theme_custom()+
  theme(plot.title=element_markdown())+
  labs(title="Case growth in <span style='color:#c51b8a;'>20-39 year olds</span> has slowed hugely since Omicron first hit",
       subtitle="Weekly change in the rolling 7-day average number of new COVID cases in England, by age group",
       caption="Data from coronavirus.data.gov.uk | Plot by @VictimOfMaths")

dev.off()

agg_tiff("Outputs/COVIDCaseRatioLineRecent4059.tiff", units="in", width=10, height=6, res=800)
ggplot(popheatmap %>% filter(date>as.Date("2021-10-01") & date<max(date)))+
  geom_hline(yintercept=1, colour="Grey50")+
  geom_hline(yintercept=0.5, colour="Grey70", linetype=2)+
  geom_hline(yintercept=2, colour="Grey70", linetype=2)+
  geom_text(aes(x=as.Date("2021-10-10"), y=0.52, label="Cases halving each week"),
            colour="Grey70")+
  geom_text(aes(x=as.Date("2021-10-10"), y=2.1, label="Cases doubling each week"),
            colour="Grey70")+
  geom_line(aes(x=date, y=caseratio, group=age), colour="Grey80")+
  geom_line(data=popheatmap %>% filter(date>as.Date("2021-10-01") & date<max(date) &
                                         age %in% c("40 to 44", "45 to 49", "50 to 54",
                                                    "55 to 59")),
            aes(x=date, y=caseratio, colour=age))+
  scale_x_date(name="")+
  scale_y_continuous(trans="log", name="7-day Case Rate Ratio",
                     breaks=c(0.5, 1, 2), 
                     labels=c("-50%", "No change", "+100%"))+
  scale_colour_manual(values=c("#fa9fb5", "#f768a1", "#c51b8a", "#7a0177"), name="Age")+
  theme_custom()+
  theme(plot.title=element_markdown())+
  labs(title="COVID cases are growing rapidly in <span style='color:#c51b8a;'>40-59 year olds",
       subtitle="Weekly change in the rolling 7-day average number of new COVID cases in England, by age group",
       caption="Data from coronavirus.data.gov.uk | Plot by @VictimOfMaths")

dev.off()

agg_tiff("Outputs/COVIDCaseRatioLineRecent6579.tiff", units="in", width=10, height=6, res=800)
ggplot(popheatmap %>% filter(date>as.Date("2021-10-01") & date<max(date)))+
  geom_hline(yintercept=1, colour="Grey50")+
  geom_hline(yintercept=0.5, colour="Grey70", linetype=2)+
  geom_hline(yintercept=2, colour="Grey70", linetype=2)+
  geom_text(aes(x=as.Date("2021-10-10"), y=0.52, label="Cases halving each week"),
            colour="Grey70")+
  geom_text(aes(x=as.Date("2021-10-10"), y=2.1, label="Cases doubling each week"),
            colour="Grey70")+
  geom_line(aes(x=date, y=caseratio, group=age), colour="Grey80")+
  geom_line(data=popheatmap %>% filter(date>as.Date("2021-10-01") & date<max(date) &
                                         age %in% c("60 to 64", "65 to 69", "70 to 74",
                                                    "75 to 79")),
            aes(x=date, y=caseratio, colour=age))+
  scale_x_date(name="")+
  scale_y_continuous(trans="log", name="7-day Case Rate Ratio",
                     breaks=c(0.5, 1, 2), 
                     labels=c("-50%", "No change", "+100%"))+
  scale_colour_manual(values=c("#fa9fb5", "#f768a1", "#c51b8a", "#7a0177"), name="Age")+
  theme_custom()+
  theme(plot.title=element_markdown())+
  labs(title="COVID case rates in <span style='color:#c51b8a;'>60-79 year olds</span> are doubling every week",
       subtitle="Weekly change in the rolling 7-day average number of new COVID cases in England, by age group",
       caption="Data from coronavirus.data.gov.uk | Plot by @VictimOfMaths")

dev.off()

agg_tiff("Outputs/COVIDCaseRatioLineRecent80Plus.tiff", units="in", width=10, height=6, res=800)
ggplot(popheatmap %>% filter(date>as.Date("2021-10-01") & date<max(date)))+
  geom_hline(yintercept=1, colour="Grey50")+
  geom_hline(yintercept=0.5, colour="Grey70", linetype=2)+
  geom_hline(yintercept=2, colour="Grey70", linetype=2)+
  geom_text(aes(x=as.Date("2021-10-10"), y=0.52, label="Cases halving each week"),
            colour="Grey70")+
  geom_text(aes(x=as.Date("2021-10-10"), y=2.1, label="Cases doubling each week"),
            colour="Grey70")+
  geom_line(aes(x=date, y=caseratio, group=age), colour="Grey80")+
  geom_line(data=popheatmap %>% filter(date>as.Date("2021-10-01") & date<max(date) &
                                         age %in% c("80 to 84", "85 to 89", "90+")),
            aes(x=date, y=caseratio, colour=age))+
  scale_x_date(name="")+
  scale_y_continuous(trans="log", name="7-day Case Rate Ratio",
                     breaks=c(0.5, 1, 2), 
                     labels=c("-50%", "No change", "+100%"))+
  scale_colour_manual(values=c("#f768a1", "#c51b8a", "#7a0177"), name="Age")+
  theme_custom()+
  theme(plot.title=element_markdown())+
  labs(title="COVID case rates are more than doubling each week in <span style='color:#c51b8a;'>the over 80s</span>",
       subtitle="Weekly change in the rolling 7-day average number of new COVID cases in England, by age group",
       caption="Data from coronavirus.data.gov.uk | Plot by @VictimOfMaths")

dev.off()

#Positivity rates by sex (pillar 2 only)
#Data from weekly surveillance reports #https://www.gov.uk/government/statistics/national-flu-and-covid-19-surveillance-reports-2021-to-2022-season
url <- "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/1043581/Weekly_Influenza_and_COVID19_report_data_W51.xlsx"
temp <- tempfile()
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

posdata.m <- read_excel(temp, sheet="Figure 7. Positivity by age", range="C121:L174") %>% 
  mutate(index=0:(nrow(.)-1), date=as.Date("2020-12-14")+weeks(index)) %>% 
  gather(ageband, posrate, c(1:(ncol(.)-2))) %>% 
  mutate(ageband=gsub("_", "-", ageband), sex="Male")

posdata.f <- read_excel(temp, sheet="Figure 7. Positivity by age", range="C177:L230") %>% 
  mutate(index=0:(nrow(.)-1), date=as.Date("2020-12-14")+weeks(index)) %>% 
  gather(ageband, posrate, c(1:(ncol(.)-2))) %>% 
  mutate(ageband=gsub("_", "-", ageband), sex="Female")

posdata <- bind_rows(posdata.m, posdata.f) %>% 
  select(-index) %>% 
  mutate(posrate=posrate/100,
         ageband=factor(ageband, levels=c("0-4", "5-9", "10-19", "20-29", "30-39", "40-49",
                                          "50-59", "60-69", "70-79", "80+")))

agg_tiff("Outputs/COVIDPosratexSex.tiff", units="in", width=10, height=7, res=500)
ggplot(posdata, aes(x=date, y=posrate, colour=sex))+
  geom_line(show.legend=FALSE)+
  scale_x_date(name="")+
  scale_y_continuous(name="Positivity rate", labels=label_percent(accuracy=1))+
  scale_colour_manual(values=c("#00cc99", "#6600cc"))+
  facet_wrap(~ageband)+
  theme_custom()+
  theme(plot.subtitle=element_markdown())+
  labs(title="Women test more than men",
       subtitle="COVID test posivity rate for <span style='color:#00cc99;'>women</span> and <span style='color:#6600cc;'>men</span> in England, for pillar 2 (community) tests, by age",
       caption="Date from UKHSA | Plot by @VictimOfMaths")

dev.off()

sexratio <- posdata %>% spread(sex, posrate) %>% 
  mutate(sexratio=Female/Male) 

agg_tiff("Outputs/COVIDPosrateSexRatio.tiff", units="in", width=8, height=6, res=500)
ggplot(sexratio, aes(x=date, y=sexratio, colour=ageband))+
  geom_hline(yintercept=1, colour="Grey50")+
  geom_line(show.legend=FALSE)+
  geom_text_repel(data=sexratio %>% filter(date==max(date)),
                  aes(x=max(date), y=sexratio, label = ageband, 
                      colour=ageband),
                  family = "Calibri", direction = "y", xlim = c(as.Date("2022-01-01"), NA),
                  hjust = 0, segment.size = .7,
                  segment.alpha = .5,
                  segment.linetype = "dotted",
                  segment.curvature = -0.1,
                  segment.ncp = 3,
                  segment.angle = 20, box.padding = .3, show.legend = FALSE)+
  scale_x_date(name="", limits=c(NA_Date_, as.Date("2022-01-10")))+
  scale_y_continuous(trans="log10", name="Female:Male positivity rate ratio\n(log scale)",
                     breaks=c(0.25, 0.5, 1, 2), limits=c(NA, 2))+
  scale_colour_paletteer_d("rcartocolor::Prism")+
  theme_custom()+
  theme(panel.grid.major.y=element_line(colour="Grey90"))+
  annotate("text", x=as.Date("2021-10-10"), y=0.26, label="Female positivity 4x higher",
           colour="Grey70", family="Lato")+
  annotate("text", x=as.Date("2021-10-10"), y=0.52, label="Female positivity 2x higher",
           colour="Grey70", family="Lato")+
  annotate("text", x=as.Date("2021-10-10"), y=1.92, label="Male positivity 2x higher",
           colour="Grey70", family="Lato")+
  labs(title="Adult women consistently test more than men",
       subtitle="Ratio of COVID test positivity rates in England, for pillar 2 (community) tests, by age",
       caption="Data from UKHSA | Plot by @VictimOfMaths")

dev.off()



#Scotland
temp <- tempfile()
source <- "https://www.opendata.nhs.scot/dataset/b318bddf-a4dc-4262-971f-0ba329e09b87/resource/9393bd66-5012-4f01-9bc5-e7a10accacf4/download/trend_agesex_20211231.csv"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

scotdata <- read.csv(temp) %>% 
  filter(!AgeGroup %in% c("Total", "0 to 59", "60+")) %>% 
  mutate(date=as.Date(as.character(Date), format="%Y%m%d")) %>% 
  select(date, Sex, AgeGroup, DailyPositive) %>% 
  group_by(Sex, AgeGroup) %>% 
  mutate(cases_roll=roll_mean(DailyPositive, 7, align="center", fill=NA),
         #Populations hard coded because the data is unhelpfully structured and
         #I am a lazy, terrible, person. Source:
         #https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/population/population-estimates/mid-year-population-estimates/mid-2020
         pop=case_when(
           AgeGroup=="0 to 14" & Sex=="Male" ~ 135959+152847+151875,
           AgeGroup=="15 to 19" & Sex=="Male" ~ 144207,
           AgeGroup=="20 to 24" & Sex=="Male" ~ 173302,
           AgeGroup=="25 to 44" & Sex=="Male" ~ 189139+185637+174079+159586,
           AgeGroup=="45 to 64" & Sex=="Male" ~ 169376+189355+193348+170701,
           AgeGroup=="65 to 74" & Sex=="Male" ~ 144529+135910,
           AgeGroup=="75 to 84" & Sex=="Male" ~ 89206+60270,
           AgeGroup=="85plus" & Sex=="Male" ~ 32227+13659,
           AgeGroup=="0 to 14" & Sex=="Female" ~ 127847+145056+146206,
           AgeGroup=="15 to 19" & Sex=="Female" ~ 137913,
           AgeGroup=="20 to 24" & Sex=="Female" ~ 168453,
           AgeGroup=="25 to 44" & Sex=="Female" ~ 188065+188432+181587+164780,
           AgeGroup=="45 to 64" & Sex=="Female" ~ 180548+203758+205996+181868,
           AgeGroup=="65 to 74" & Sex=="Female" ~ 155904+149920,
           AgeGroup=="75 to 84" & Sex=="Female" ~ 109004+83026,
           AgeGroup=="85plus" & Sex=="Female" ~ 52335+30090),
         caserate_roll=cases_roll*100000/pop)

agg_tiff("Outputs/COVIDCasesxSexScot.tiff", units="in", width=10, height=7, res=800)
ggplot(scotdata %>% filter(date>as.Date("2021-05-10") & date<max(date)-days(3) &
                             Sex!="Total"), 
       aes(x=date, y=caserate_roll, colour=Sex))+
  geom_line(show.legend=FALSE)+
  scale_x_date(name="")+
  scale_y_continuous(name="Daily new cases per 100,000")+
  scale_colour_manual(values=c("#00cc99", "#6600cc"))+
  facet_wrap(~AgeGroup)+
  theme_custom()+
  theme(plot.subtitle=element_markdown())+
  labs(title="The male/female gap in Scottish COVID cases seems fairly stable",
       subtitle="Rolling 7-day average of new COVID case rates in <span style='color:#6600cc;'>men</span> and <span style='color:#00cc99;'>women</span> in Scotland, by age.",
       caption="Data from Public Health Scotland | Plot by @VictimOfMaths")
dev.off()

#Calculate case rate ratios
caseratios.scot <- scotdata %>% 
  group_by(AgeGroup, date) %>% 
  summarise(cases_roll=sum(cases_roll)) %>% 
  mutate(Sex="Total") %>%
  ungroup() %>% 
  bind_rows(data) %>% 
  filter(!is.na(cases_roll)) %>% 
  select(AgeGroup, Sex, date, cases_roll) %>% 
  group_by(AgeGroup, Sex) %>% 
  mutate(caseratio=cases_roll/lag(cases_roll, 7)) %>% 
  ungroup()

#Whole population
popheatmap.scot <- caseratios.scot %>% 
  filter(Sex=="Total" & date>as.Date("2020-04-01")) 

agg_tiff("Outputs/COVIDCaseRatioHeatmapScot.tiff", units="in", width=10, height=6, res=800)
ggplot(popheatmap.scot)+
  geom_tile(aes(x=date, y=AgeGroup, fill=caseratio))+
  scale_x_date(name="")+
  scale_y_discrete(name="Age")+
  scale_fill_paletteer_c("pals::warmcool", limit=c(0.249,4), direction=-1 ,
                         trans="log", breaks=c(0.25, 0.5, 1, 2, 4), 
                         labels=c("-75%", "-50%", "No change", "+100%", "+300%"),
                         name="Change in cases in the past week")+
  theme_custom()+
  theme(legend.position="top")+
  guides(fill = guide_colorbar(title.position = 'top', title.hjust = .5,
                               barwidth = unit(20, 'lines'), barheight = unit(.5, 'lines')))+
  labs(title="Generally COVID case numbers have risen or fallen across all age groups at once",
       subtitle="Weekly change in the rolling 7-day average number of new COVID cases in England, by age group",
       caption="Data from coronavirus.data.gov.uk | Plot inspired by @danc00ks0n & @russss | Plot by @VictimOfMaths")

dev.off()

scotheatmapdata <- scotdata %>% 
  select(Sex, caserate_roll, AgeGroup, date) %>% 
  spread(Sex, caserate_roll) %>% 
  mutate(maleprop=Male/(Male+Female))

agg_tiff("Outputs/COVIDCasesxSexHeatmapScot.tiff", units="in", width=10, height=7, res=500)
ggplot(scotheatmapdata %>% filter(date>as.Date("2021-05-25") & date<max(date)-days(3)))+
  geom_tile(aes(x=date, y=AgeGroup, fill=maleprop))+
  theme_custom()+
  scale_fill_distiller(palette="PRGn", limits=c(0.3,0.7), name="", breaks=c(0.33,0.5,0.67),
                       labels=c("2 Female cases\nfor each\nmale case", "Equal male\nand female\ncases", 
                                "2 Male cases\nfor each\nfemale case"))+
  scale_x_date(name="")+
  scale_y_discrete(name="Age")+
  theme(legend.position = "top", plot.subtitle=element_markdown())+
  guides(fill = guide_colorbar(title.position = 'top', title.hjust = .5,
                               barwidth = unit(20, 'lines'), barheight = unit(.5, 'lines')))+
  labs(title="Scotland has a smaller gender imbalance in 20-44 year old case rates than England",
       subtitle="Ratio of <span style='color:#1b7837;'>female</span> to <span style='color:#762a83;'>male</span> cases in Scotland, based on a 7-day rolling average",
       caption="Date from Public Health Scotland | Plot by @VictimOfMaths")
dev.off()

agg_tiff("Outputs/COVIDCaseRatioHeatmapRecentScot.tiff", units="in", width=10, height=6, res=500)
ggplot(popheatmap.scot %>% filter(date>as.Date("2021-05-01")))+
  geom_tile(aes(x=date, y=AgeGroup, fill=caseratio))+
  scale_x_date(name="")+
  scale_y_discrete(name="Age")+
  scale_fill_paletteer_c("pals::warmcool", limit=c(0.249,4), direction=-1 ,
                         trans="log", breaks=c(0.25, 0.5, 1, 2, 4), 
                         labels=c("-75%", "-50%", "No change", "+100%", "+300%"),
                         name="Change in cases in the past week")+
  theme_custom()+
  theme(legend.position="top")+
  guides(fill = guide_colorbar(title.position = 'top', title.hjust = .5,
                               barwidth = unit(20, 'lines'), barheight = unit(.5, 'lines')))+
  labs(title="COVID case rates in Scotland are falling across all age groups",
       subtitle="Weekly change in the rolling 7-day average number of new COVID cases in Scotland, by age group",
       caption="Data from NHS Scotland | Plot inspired by @danc00ks0n & @russss | Plot by @VictimOfMaths")

dev.off()

agg_tiff("Outputs/COVIDCaseRatesLineRecentScot.tiff", units="in", width=10, height=6, res=800)
ggplot(scotdata %>% filter(date>as.Date("2021-05-10") & date<max(date)-days(3) &
                             Sex!="Total"), 
       aes(x=date, y=caserate_roll, colour=AgeGroup, group=AgeGroup))+
  #geom_rect(aes(xmin=as.Date("2021-06-11"), xmax=as.Date("2021-07-11"), ymin=0, ymax=220),
  #          fill="Grey90", colour="Grey90")+
  #geom_rect(aes(xmin=as.Date("2021-09-01"), xmax=as.Date("2021-09-27"), ymin=0, ymax=220),
  #          fill="Grey90", colour="Grey90")+
  #geom_segment(aes(x=as.Date("2021-07-19"), xend=as.Date("2021-07-19"), y=0, yend=220),
  #             colour="Grey30", linetype=2)+
  geom_line()+
  facet_wrap(~Sex)+
  scale_x_date(name="")+
  scale_y_continuous(name="New COVID cases per 100,000")+
  scale_colour_paletteer_d("awtools::a_palette", name="Age")+
  theme_custom()+
  labs(title="Current case rates are still highest (by far) in schoolchildren",
       subtitle="Rolling 7-day average number of new COVID cases in Scotland, by age group",
       caption="Data from coronavirus.data.gov.uk | Plot by @VictimOfMaths")

dev.off()
