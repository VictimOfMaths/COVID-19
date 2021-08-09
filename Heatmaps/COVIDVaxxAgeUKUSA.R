rm(list=ls())

library(tidyverse)
library(curl)
library(extrafont)
library(ggtext)

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
  labs(title="<span style='color:navyblue;'>The US</span> has vaccinated fewer of its older population against COVID than <span style='color:darkred;'>the UK",
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
  labs(title="<span style='color:navyblue;'>The US</span> has vaccinated fewer of its older population against COVID than <span style='color:darkred;'>the UK",
       subtitle="The number of people in each country who are <span style='color:Grey70;'>unvaccinated</span>, have received <span style='color:royalblue;'>one </span><span style='color:tomato;'>dose </span>or are <span style='color:navyblue;'>fully </span><span style='color:darkred;'>vaccinated</span>.<br>Areas are scaled to imply equal populations, so the x-axis scales on the left- and right- of the centre are different.",
       caption="Data from CDC, NHS England and ONS\nPlot by @VictimOfMaths")
dev.off()


