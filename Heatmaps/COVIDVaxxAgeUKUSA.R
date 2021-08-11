rm(list=ls())

library(tidyverse)
library(curl)
library(extrafont)
library(ggtext)
library(scales)
library(readxl)


theme_custom <- function() {
  theme_classic() %+replace%
    theme(plot.title.position="plot", plot.caption.position="plot",
          strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)),
          plot.title=element_text(face="bold", size=rel(1.5), hjust=0,
                                  margin=margin(0,0,5.5,0)),
          text=element_text(family="Lato"))
}

#US data from https://covid.cdc.gov/covid-data-tracker/#vaccination-demographic
#Hard coded because I can't work out how to extract the download url from the PowerBI dashboard
USpop <- 331449281

USdata <- data.frame(age=c("<12", "12-15", "16-17", "18-24", "25-39", "40-49", "50-64",
                              "65-74", "75+"), 
                        dose1=c(198609, 6388261, 3931936, 15268822, 36501123, 25352681, 45542907, 
                                27423936, 18823020),
                        dose2=c(122769, 4607456, 3122352, 12411413, 30610887, 21683574, 39865850,
                                24394904, 16744576),
                        popperc=c(0.144, 0.05, 0.025, 0.092, 0.205, 0.122, 0.194, 0.098, 0.07)) %>% 
  mutate(pop=USpop*popperc,
         unvax=pop-dose1,
         dose1only=dose1-dose2,
         agemin=case_when(
           age=="<12" ~ 0,
           TRUE ~ as.numeric(substr(age, 1, 2))),
         agemax=case_when(
           age=="<12" ~ 12,
           age=="75+" ~ 100,
           TRUE ~ as.numeric(substr(age, 4,5))+1),
         agewidth=agemax-agemin,
         dose2plot=dose2/agewidth,
         dose1plot=dose1/agewidth,
         unvaxplot=pop/agewidth)

#UK data from NHS vaccination spreadsheet
temp <- tempfile()
source <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/08/COVID-19-weekly-announced-vaccinations-05-August-2021.xlsx"
UKvax <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

UKpop <- read_excel(UKvax, sheet="Population estimates (ONS 2020)", range="B16:D29", col_names=FALSE) %>% 
  select(-`...2`) %>% 
  set_names(c("age", "pop"))

UKdata <- as.data.frame(t(read_excel(UKvax, sheet="NHS Region", range="D12:AH13", col_names=FALSE))) %>% 
  filter(!is.na(V1)) %>% 
  mutate(dose=rep(c("dose1", "dose2"), each=14),
         V2=as.numeric(V2)) %>% 
  spread(dose, V2) %>% 
  rename(age=V1) %>% 
  merge(UKpop) %>% 
  mutate(unvax=pop-dose1,
         dose1only=dose1-dose2,
         agemin=case_when(
           age=="Under 18" ~ 0,
           TRUE ~ as.numeric(substr(age, 1, 2))),
         agemax=case_when(
           age=="Under 18" ~ 18,
           age=="80+" ~ 100,
           TRUE ~ as.numeric(substr(age, 4,5))+1),
         agewidth=agemax-agemin,
         dose2plot=dose2/agewidth,
         dose1plot=dose1/agewidth,
         unvaxplot=pop/agewidth)

tiff("Outputs/COVIDVaxxAgeUKUSA.tiff", units="in", width=9, height=6.6, res=500)
ggplot()+
  geom_rect(data=UKdata, aes(xmin=0, xmax=dose2plot, ymin=agemin, ymax=agemax), fill="darkred")+
  geom_rect(data=UKdata, aes(xmin=dose2plot, xmax=dose1plot, ymin=agemin, ymax=agemax), fill="tomato")+
  geom_rect(data=UKdata, aes(xmin=dose1plot, xmax=unvaxplot, ymin=agemin, ymax=agemax), fill="Grey70")+
  geom_rect(data=USdata, aes(xmax=0, xmin=-dose2plot, ymin=agemin, ymax=agemax), fill="navyblue")+
  geom_rect(data=USdata, aes(xmax=-dose2plot, xmin=-dose1plot, ymin=agemin, ymax=agemax), fill="royalblue")+
  geom_rect(data=USdata, aes(xmax=-dose1plot, xmin=-unvaxplot, ymin=agemin, ymax=agemax), fill="Grey70")+
  geom_segment(aes(x=0, xend=0, y=0, yend=100), colour="White")+
  scale_x_continuous(name="Number of people at each single year of age", labels=abs)+
  scale_y_continuous(name="Age", limits=c(0,100))+
  theme_custom()+
  theme(plot.title=element_markdown(), plot.subtitle=element_markdown())+
  labs(title="<span style='color:navyblue;'>The US</span> has vaccinated fewer of its older population against COVID than <span style='color:darkred;'>England",
       subtitle="The number of people in each country who are <span style='color:Grey70;'>unvaccinated</span>, have received <span style='color:royalblue;'>one </span><span style='color:tomato;'>dose </span>or are <span style='color:navyblue;'>fully </span><span style='color:darkred;'>vaccinated</span>.",
       caption="Data from CDC, NHS England and ONS\nPlot by @VictimOfMaths")
dev.off()

tiff("Outputs/COVIDVaxxAgeUKUSANorm.tiff", units="in", width=9, height=6.6, res=500)
ggplot()+
  geom_rect(data=UKdata, aes(xmin=0, xmax=dose2plot, ymin=agemin, ymax=agemax), fill="darkred")+
  geom_rect(data=UKdata, aes(xmin=dose2plot, xmax=dose1plot, ymin=agemin, ymax=agemax), fill="tomato")+
  geom_rect(data=UKdata, aes(xmin=dose1plot, xmax=unvaxplot, ymin=agemin, ymax=agemax), fill="Grey70")+
  geom_rect(data=USdata, aes(xmax=0, xmin=-dose2plot/5.9, ymin=agemin, ymax=agemax), fill="navyblue")+
  geom_rect(data=USdata, aes(xmax=-dose2plot/5.9, xmin=-dose1plot/5.9, ymin=agemin, ymax=agemax), fill="royalblue")+
  geom_rect(data=USdata, aes(xmax=-dose1plot/5.9, xmin=-unvaxplot/5.9, ymin=agemin, ymax=agemax), fill="Grey70")+
  geom_segment(aes(x=0, xend=0, y=0, yend=100), colour="White")+
  scale_x_continuous(name="Number of people at each single year of age",
                     breaks=c(-508474.6, 0, 500000), labels=c("3,000,000", "0", "500,000"))+
  scale_y_continuous(name="Age", limits=c(0,100))+
  theme_custom()+
  theme(plot.title=element_markdown(), plot.subtitle=element_markdown())+
  labs(title="<span style='color:navyblue;'>The US</span> has vaccinated fewer of its older population against COVID than <span style='color:darkred;'>England",
       subtitle="The number of people in each country who are <span style='color:Grey70;'>unvaccinated</span>, have received <span style='color:royalblue;'>one </span><span style='color:tomato;'>dose </span>or are <span style='color:navyblue;'>fully </span><span style='color:darkred;'>vaccinated</span>.<br>Areas are scaled to imply equal populations, so the x-axis scales on the left- and right- of the centre are different.",
       caption="Data from CDC, NHS England and ONS\nPlot by @VictimOfMaths")
dev.off()

tiff("Outputs/COVIDVaxxAgeUKUSANormUnvax.tiff", units="in", width=9, height=6.6, res=500)
ggplot()+
  geom_rect(data=UKdata, aes(xmin=0, xmax=unvax/agewidth, ymin=agemin, ymax=agemax), fill="Grey70")+
  geom_rect(data=USdata, aes(xmax=0, xmin=-unvax/(agewidth*5.9), ymin=agemin, ymax=agemax), fill="Grey70")+
  geom_segment(aes(x=0, xend=0, y=0, yend=100), colour="White")+
  annotate("text", x=-500000, y=80, label="USA", colour="navyblue", family="Lato", size=rel(6),
           fontface="bold")+
  annotate("text", x=500000, y=80, label="England", colour="darkred", family="Lato", size=rel(6),
           fontface="bold")+
  scale_x_continuous(name="Number of people at each single year of age",
                     breaks=c(-508474.6, 0, 500000), labels=c("3,000,000", "0", "500,000"))+
  scale_y_continuous(name="Age", limits=c(0,100))+
  theme_custom()+
  labs(title="The US has vaccinated fewer of its older population against COVID than England",
       subtitle="The number of people in each country who are unvaccinated.\nAreas are scaled to imply equal populations, so the x-axis scales on the left- and right- of the centre are different.",
       caption="Data from CDC, NHS England and ONS\nPlot by @VictimOfMaths")
dev.off()

tiff("Outputs/COVIDVaxxAgeUKUSAPerc.tiff", units="in", width=9, height=6.6, res=500)
ggplot()+
  geom_rect(data=UKdata, aes(xmin=0, xmax=dose2/pop, ymin=agemin, ymax=agemax), fill="darkred")+
  geom_rect(data=UKdata, aes(xmin=dose2/pop, xmax=dose1/pop, ymin=agemin, ymax=agemax), fill="tomato")+
  geom_rect(data=UKdata, aes(xmin=dose1/pop, xmax=1, ymin=agemin, ymax=agemax), fill="Grey70")+
  geom_rect(data=USdata, aes(xmax=0, xmin=-dose2/pop, ymin=agemin, ymax=agemax), fill="navyblue")+
  geom_rect(data=USdata, aes(xmax=-dose2/pop, xmin=-dose1/pop, ymin=agemin, ymax=agemax), fill="royalblue")+
  geom_rect(data=USdata, aes(xmax=-dose1/pop, xmin=-1, ymin=agemin, ymax=agemax), fill="Grey70")+
  geom_segment(aes(x=0, xend=0, y=0, yend=100), colour="White")+
  scale_x_continuous(name="Proportion of population", breaks=c(-1, -0.5, 0, 0.5, 1), 
                     labels=c("100%", "50%", "0%", "50%", "100%"))+
  scale_y_continuous(name="Age", limits=c(0,100))+
  theme_custom()+
  theme(plot.title=element_markdown(), plot.subtitle=element_markdown())+
  labs(title="<span style='color:navyblue;'>The US</span> has vaccinated fewer of its older population against COVID than <span style='color:darkred;'>England",
       subtitle="The proportion of people at each age in each country who are <span style='color:Grey70;'>unvaccinated</span>, have received <span style='color:royalblue;'>one </span><span style='color:tomato;'>dose </span>or are <span style='color:navyblue;'>fully </span><span style='color:darkred;'>vaccinated</span>.",
       caption="Data from CDC, NHS England and ONS\nPlot by @VictimOfMaths")
dev.off()

#CHRs from Dan Howdon
CHR2534 <- 0.015435
CHR3544 <- 0.022447
CHR4554 <- 0.02882
CHR5564 <- 0.0491
CHR6574 <- 0.115683
CHR7584 <- 0.270174
CHR85plus <- 0.419715

#Merge together
data <- UKdata %>% mutate(country="England") %>% 
  bind_rows(USdata %>% mutate(country="USA")) %>% 
  select(age, dose1only, dose2, unvax, pop, agemin, agemax, agewidth, country) %>% 
  gather(dose, number, c(dose1only, dose2, unvax)) %>% 
  #Calculate hospitalisation risk reduction from vaccinations using effectiveness data from
  #https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/1008919/Vaccine_surveillance_report_-_week_31.pdf
  mutate(risk=case_when(
    dose=="dose1only" ~ number*(1-0.8),
    dose=="dose2" ~ number*(1-0.96),
    TRUE ~ number)) %>% 
  group_by(age, pop, agemin, agemax, agewidth, country) %>% 
  summarise(risk=sum(risk)) %>% 
  ungroup() %>% 
  mutate(riskreduction=risk/pop) %>% 
  arrange(fct_rev(country), age) %>% 
  #Merge in CHRs calculated by Dan Howdon
  #Weighted averages informed by pop data from https://data.census.gov/cedsci/table?q=United%20States&g=0100000US&y=2019&tid=ACSST1Y2019.S0101
  #and ONS
  mutate(CHR=case_when(
    age %in% c("25-29", "30-34") ~ CHR2534,
    age %in% c("35-39", "40-44") ~ CHR3544,
    age %in% c("45-49", "50-54") ~ CHR4554,
    age %in% c("55-59", "60-64") ~ CHR5564,
    age %in% c("65-69", "70-74", "65-74") ~ CHR6574,
    age %in% c("75-79") ~ CHR7584,
    age=="25-39" ~ ((23.2+22.3)*CHR2534+21.7*CHR3544)/(23.2+22.3+21.7),
    age=="40-49" ~ (20.2*CHR3544+20.4*CHR4554)/(20.2+20.4),
    age=="50-64" ~ (20.5*CHR4554+(21.5+21.0)*CHR5564)/(20.5+21.5+21.0),
    age=="75+" ~ ((9.8+6.3)*CHR7584+6.4*CHR85plus)/(9.8+6.3+6.4),
    age=="80+" ~ (1.4*CHR7584+1.4*CHR85plus)/(1.4+1.4),
    TRUE ~ 0),
    hosp=risk*CHR, hosp_avert=pop*CHR-hosp)

#Visualise risk reduction
tiff("Outputs/COVIDVaxxAgeUKUSARiskRed.tiff", units="in", width=10, height=6.6, res=500)
ggplot(data)+
  geom_rect(aes(xmin=agemin, xmax=agemax, ymin=0, ymax=riskreduction, fill=country), alpha=0.6,
            show.legend=FALSE)+
  geom_segment(aes(x=0, xend=100, y=1, yend=1))+
  scale_x_continuous(name="Age")+
  scale_y_continuous(name="Proportion of risk without vaccination", labels=label_percent(accuracy=1))+
  scale_fill_manual(values=c("tomato", "royalblue"))+
  theme_custom()+
  theme(plot.title=element_markdown())+
  labs(title="<span style='color:tomato;'>England's</span> vaccine programme has reduced hospitalisation risk much more than <span style='color:royalblue;'>the USA's",
       subtitle="Shaded areas represent risk of hospitalisation from COVID as a proportion of the risk in an entirely unvaccinated population, by age.",
       caption="Vaccination data from CDC and NHS England | Population data from US Census Bureau and ONS | Vaccine effectiveness data from PHE\nPlot by @VictimOfMaths")
dev.off()

#Visualise hospitalisations averted
tiff("Outputs/COVIDVaxxAgeUKUSARiskAdj.tiff", units="in", width=9, height=6.6, res=500)
ggplot()+
  geom_rect(data=data %>% filter(country=="England"), aes(xmin=0, xmax=hosp/agewidth, ymin=agemin, ymax=agemax),
            fill="tomato")+
  geom_rect(data=data %>% filter(country=="England"), aes(xmin=hosp/agewidth, xmax=(hosp+hosp_avert)/agewidth, ymin=agemin, ymax=agemax),
            fill="Grey70")+
  geom_rect(data=data %>% filter(country=="USA"), aes(xmax=0, xmin=-hosp/agewidth, ymin=agemin, ymax=agemax),
            fill="royalblue")+
  geom_rect(data=data %>% filter(country=="USA"), aes(xmax=-hosp/agewidth, xmin=-(hosp+hosp_avert)/agewidth, ymin=agemin, ymax=agemax),
            fill="Grey70")+
  geom_segment(aes(x=0, xend=0, y=25, yend=100), colour="White")+
  scale_x_continuous(name="Total hospitalisations for each single year of age",
                     labels=abs)+
  scale_y_continuous(name="Age", limits=c(25,100))+
  theme_custom()+
  theme(plot.title=element_markdown(), plot.subtitle=element_markdown())+
  labs(title="Vaccination has reduced COVID hospitalisation risks more <span style='color:tomato;'>in England</span> than <span style='color:royalblue;'>in the US",
       subtitle="<span style='color:royalblue;'>Coloured </span><span style='color:tomato;'>areas</span> represent the total number of hospital admissions we would expect in each country if everyone developed COVID,<br>based on current vaccination levels.<span style='color:Grey60;'>Grey areas</span> represent hospital admissions averted as a result of vaccination.",
       caption="Vaccination data from CDC and NHS England | Population data from US Census Bureau and ONS\nVaccine effectiveness data from PHE | CHRs estimated by Dan Howdon\nPlot by @VictimOfMaths")
dev.off()

tiff("Outputs/COVIDVaxxAgeUKUSARiskAdjNorm.tiff", units="in", width=9, height=6.6, res=500)
ggplot()+
  geom_rect(data=data %>% filter(country=="England"), aes(xmin=0, xmax=hosp/agewidth, ymin=agemin, ymax=agemax),
            fill="tomato")+
  geom_rect(data=data %>% filter(country=="England"), aes(xmin=hosp/agewidth, xmax=(hosp+hosp_avert)/agewidth, ymin=agemin, 
                                                          ymax=agemax),
            fill="Grey70")+
  geom_rect(data=data %>% filter(country=="USA"), aes(xmax=0, xmin=-hosp/(agewidth*5.9), ymin=agemin, ymax=agemax),
            fill="royalblue")+
  geom_rect(data=data %>% filter(country=="USA"), aes(xmax=-hosp/(agewidth*5.9), xmin=-hosp/(agewidth*5.9)-hosp_avert/(agewidth*5.9), 
                                                      ymin=agemin, ymax=agemax),
            fill="Grey70")+
  geom_segment(aes(x=0, xend=0, y=25, yend=100), colour="White")+
  scale_x_continuous(name="Total hospitalisations for each single year of age",
                     breaks=c(-50847.46, 0, 50000), 
                     labels=c("300,000", "0", "50,000"),
                     limits=c(-110000, 110000))+
  scale_y_continuous(name="Age", limits=c(25,100))+
  theme_custom()+
  theme(plot.title=element_markdown(), plot.subtitle=element_markdown())+
  labs(title="Vaccination has reduced COVID hospitalisation risks more <span style='color:tomato;'>in England</span> than <span style='color:royalblue;'>in the US",
       subtitle="<span style='color:royalblue;'>Coloured </span><span style='color:tomato;'>areas</span> represent the total number of hospital admissions we would expect in each country if everyone developed COVID,<br>based on current vaccination levels.<span style='color:Grey60;'>Grey areas</span> represent hospital admissions averted as a result of vaccination.<br>Areas are scaled to imply equal populations, so the x-axis scales on the left- and right- of the centre are different.",
       caption="Vaccination data from CDC and NHS England | Population data from US Census Bureau and ONS\nVaccine effectiveness data from PHE | CHRs estimated by Dan Howdon\nPlot by @VictimOfMaths")+
  annotate("text", x=-60000, y=50, label="USA", colour="royalblue", family="Lato", size=rel(6),
           fontface="bold")+
  annotate("text", x=60000, y=50, label="England", colour="tomato", family="Lato", size=rel(6),
           fontface="bold")+
  annotate("text", x=-85000, y=90, label="Hospital admissions\nbased on current vaccinations", colour="Black", 
           family="Lato", size=rel(4))+
  annotate("text", x=75000, y=90, label="Admissions averted\nby vaccinations", colour="Black", family="Lato", size=rel(4))+
  geom_curve(aes(x=-58000, y=90, xend=-5000, yend=85), 
             colour="Black", curvature=-0.25, arrow=arrow(length=unit(0.1, "cm"), type="closed"), 
             lineend="round")+
  geom_curve(aes(x=58000, y=90, xend=35000, yend=85), 
             colour="Black", curvature=-0.15, arrow=arrow(length=unit(0.1, "cm"), type="closed"), 
             lineend="round")
dev.off()

#Calculate actual numbers
data %>% group_by(country) %>% 
  summarise(hosp=sum(hosp), hosp_avert=sum(hosp_avert)) %>% 
  mutate(percchange=hosp_avert/(hosp+hosp_avert))

