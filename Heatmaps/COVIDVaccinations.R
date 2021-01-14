rm(list=ls())

library(tidyverse)
library(curl)
library(ukcovid19)
library(paletteer)
library(ggtext)

#Currently the R interface seems broken and doesn't return data for the 1st dose
#Call vaccination data from coronavirus.gov.uk API
#APIdata <- get_data(filters="areaType=nation", 
#                    structure=list(date="date",
#                                   name="areaName",
#                                   dose1="weeklyPeopleVaccinatedFirstDoseByVaccinationDate",
#                                   dose2="weeklyPeopleVaccinatedSecondDoseByVaccinationDate"))

#So read in data directly
temp <- tempfile()
source="https://api.coronavirus.data.gov.uk/v2/data?areaType=nation&metric=weeklyPeopleVaccinatedFirstDoseByVaccinationDate&metric=weeklyPeopleVaccinatedSecondDoseByVaccinationDate&format=csv"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
APIdata <- read.csv(temp)
colnames(APIdata) <- c("date", "type", "code", "name", "dose1", "dose2")

#English data for w/e 13/12/20 and 20/12/20 is reported together, so model it out,
#assuming ratio between the weeks is equal to the total for the rest of the UK
#(i.e. 31.05% of vaccinations from these weeks combined were in the 1st week)
APIdata <- bind_rows(APIdata, 
                     data.frame(date="2020-12-13", type="nation", code="E92000001", name="England",
                                dose1=APIdata$dose1[APIdata$date=="2020-12-20" & APIdata$name=="England"]*0.3105,
                                dose2=APIdata$dose2[APIdata$date=="2020-12-20" & APIdata$name=="England"]*0.3105))

APIdata$dose1[APIdata$date=="2020-12-20" & APIdata$name=="England"] <- APIdata$dose1[APIdata$date=="2020-12-20" & APIdata$name=="England"]*(1-0.3105)
APIdata$dose2[APIdata$date=="2020-12-20" & APIdata$name=="England"] <- APIdata$dose2[APIdata$date=="2020-12-20" & APIdata$name=="England"]*(1-0.3105)

#Bring in populations (hard coded because I AM A MONSTER)
APIdata <- APIdata %>% mutate(
  pop=case_when(name=="England" ~ 56286961,
                name=="Wales" ~ 3152879,
                name=="Scotland" ~ 5463300,
                name=="Northern Ireland" ~ 1893667))
 
#Calculate UK total
natdata <- APIdata %>% 
  group_by(date) %>% 
  summarise(dose1=sum(dose1), dose2=sum(dose2), pop=sum(pop)) %>% 
  mutate(name="UK")

APIdata <- APIdata %>% 
  bind_rows(natdata) %>% 
  group_by(name) %>%
  arrange(date) %>% 
  mutate(dose1cumul=cumsum(dose1), dose2cumul=cumsum(dose2)) %>% 
  ungroup() %>% 
  mutate(dose1rate=dose1*1000000/pop,
         dose2rate=dose2*1000000/pop,
         dose1cumulrate=dose1cumul*1000000/pop,
         dose2cumulrate=dose2cumul*1000000/pop)

tiff("Outputs/COVIDVaccinationRate.tiff", units="in", width=8, height=6, res=500)
ggplot()+
  geom_col(data=subset(APIdata, name!="UK"), aes(x=date, y=dose1rate, fill=name), position="dodge")+
  scale_x_discrete(name="Week ending")+
  scale_y_continuous(name="First doses delivered per 1m population")+
  scale_fill_paletteer_d("fishualize::Scarus_quoyi", name="")+
  theme_classic()+
  theme(plot.title=element_text(face="bold", size=rel(1.2)))+
  labs(title="Vaccine delivery across the UK has started accelerating",
       subtitle="Rates of delivery of 1st COVID-19 vaccine doses by country",
       caption="Data from coronavirus.gov.uk | Plot by @VictimOfMaths\nEnglish data for the weeks ending 13th & 20th Dec is reported together, so figures for these weeks\nare estimated assuming the distribution between these weeks matched the rest of the UK")
dev.off()

tiff("Outputs/COVIDVaccinationTotals.tiff", units="in", width=8, height=6, res=500)
APIdata %>% 
  filter(name!="UK") %>% 
  select(date, name, dose1cumulrate, dose2cumulrate) %>% 
  gather(dose, rate, c(3,4)) %>% 
  mutate(date=as.Date(date)) %>% 
  ggplot()+
  geom_line(aes(x=date, y=rate, colour=name, linetype=dose))+
  scale_x_date(name="")+
  scale_y_continuous(name="Doses delivered per 1m population")+
  scale_colour_paletteer_d("fishualize::Scarus_quoyi", name="Nation")+
  scale_linetype_discrete(name="Dose", labels=c("1st", "2nd"))+
  theme_classic()+
  theme(plot.title=element_text(face="bold", size=rel(1.2)))+
  labs(title="Wales is lagging behind the rest of the UK in delivering COVID-19 vaccines",
       subtitle="Rates of delivery of COVID-19 vaccines by country",
       caption="Data from coronavirus.gov.uk | Plot by @VictimOfMaths\nEnglish data for the weeks ending 13th & 20th Dec is reported together, so figures for these weeks\nare estimated assuming the distribution between these weeks matched the rest of the UK")
dev.off()

tiff("Outputs/COVIDVaccinationTarget.tiff", units="in", width=8, height=6, res=500)
APIdata %>% 
  filter(name=="UK") %>% 
  select(date, name, dose1cumul, dose2cumul) %>% 
  gather(dose, count, c(3,4)) %>% 
  mutate(date=as.Date(date)) %>% 
  ggplot()+
  geom_line(aes(x=date, y=count/1000000, linetype=dose), colour="Darkred")+
  geom_hline(yintercept=13.4, colour="SkyBlue")+
  scale_x_date(name="", limits=c(as.Date("2020-12-10"), as.Date("2021-02-14")))+
  scale_y_continuous(name="Total vaccine doses delivered (millions)")+
  scale_linetype_discrete(name="Dose", labels=c("1st", "2nd"))+
  theme_classic()+
  theme(plot.title=element_text(face="bold", size=rel(1.2)), plot.subtitle=element_markdown())+
  labs(title="Vaccination delivery in the UK needs to speed up further",
       subtitle="<span style='color:DarkRed;'>Total COVID-19 vaccinations delivered</span>, by dose, in the UK compared to <span style='color:SkyBlue2;'>Boris Johnson's mid-February target",
       caption="Data from coronavirus.gov.uk | Plot by @VictimOfMaths\nEnglish data for the weeks ending 13th & 20th Dec is reported together, so figures for these weeks\nare estimated assuming the distribution between these weeks matched the rest of the UK")
dev.off()
