rm(list=ls())

library(tidyverse)
library(readxl)
library(extrafont)
library(ragg)

theme_custom <- function() {
  theme_classic() %+replace%
    theme(plot.title.position="plot", plot.caption.position="plot",
          strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)),
          plot.title=element_text(face="bold", size=rel(1.5), hjust=0,
                                  margin=margin(0,0,5.5,0)),
          text=element_text(family="Lato"))
}
#All data from the latest PHE vaccine surveillance report
#https://www.gov.uk/government/publications/covid-19-vaccine-surveillance-report

#Cases
cases <- data.frame(
  age=c("Under 18", "18-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+"),
  Unlinked=c(15901, 19529, 12452, 8930, 6868, 3657, 2034, 1124),
  Unvaccinated=c(141676, 53187, 33986, 15106, 7552, 2650, 910, 545),
  Fully_vaccinated=c(757, 32533, 43004, 67349, 67652, 38119, 22270, 10087)
) %>% 
  mutate(metric="cases")

deaths28 <- data.frame(
  age=c("Under 18", "18-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+"),
  Unlinked=c(0,1,2,3,3,7,2,7),
  Unvaccinated=c(3,13,31,54,100,115,129,155),
  Fully_vaccinated=c(0,3,8,27,71,194,428,928)
) %>% 
  mutate(metric="deaths")

admissions <-  data.frame(
  age=c("Under 18", "18-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+"),
  Unlinked=c(25,14,16,14,10,7,3,1),
  Unvaccinated=c(404,387,516,497,421,328,194,144),
  Fully_vaccinated=c(0,80,118,220,406,571,873,965)
) %>% 
  mutate(metric="admissions")

rawdata <- bind_rows(cases, admissions, deaths28)

#Bring in vaccinated and NIMS/ONS population data - use figures from week 32 (to allow for 2 weeks post 2nd jab by
#~mid-point of 4 week analysis window)
url <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/08/COVID-19-weekly-announced-vaccinations-12-August-2021.xlsx"
temp <- tempfile()
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

vaxed <- read_excel(temp, sheet="NHS Region", range="U12:AH13") %>% 
  mutate(`18-29`=`18-24`+`25-29`, `30-39`=`30-34`+`35-39`, `40-49`=`40-44`+`45-49`,
         `50-59`=`50-54`+`55-59`, `60-69`=`60-64`+`65-69`, `70-79`=`70-74`+`75-79`) %>% 
  select(`Under 18`, `18-29`, `30-39`, `40-49`, `50-59`, `60-69`, `70-79`, `80+`) %>% 
  gather(age, vaxpop)

NIMSpop <- read_excel(temp, sheet="Population estimates (NIMS)", range="F13:S14") %>% 
  mutate(`18-29`=`18-24`+`25-29`, `30-39`=`30-34`+`35-39`, `40-49`=`40-44`+`45-49`,
         `50-59`=`50-54`+`55-59`, `60-69`=`60-64`+`65-69`, `70-79`=`70-74`+`75-79`) %>% 
  select(`Under 18`, `18-29`, `30-39`, `40-49`, `50-59`, `60-69`, `70-79`, `80+`) %>% 
  gather(age, pop_NIMS)

ONSpop <- read_excel(temp, sheet="Population estimates (ONS 2020)", range="B16:D29", col_names=FALSE) %>% 
  select(-2) %>% 
  set_names(c("age", "pop_ONS")) %>% 
  spread(age, pop_ONS) %>% 
  mutate(`18-29`=`18-24`+`25-29`, `30-39`=`30-34`+`35-39`, `40-49`=`40-44`+`45-49`,
         `50-59`=`50-54`+`55-59`, `60-69`=`60-64`+`65-69`, `70-79`=`70-74`+`75-79`) %>% 
  select(`Under 18`, `18-29`, `30-39`, `40-49`, `50-59`, `60-69`, `70-79`, `80+`) %>% 
  gather(age, pop_ONS)

data <- merge(rawdata, vaxed) %>% 
  merge(NIMSpop) %>% 
  merge(ONSpop) %>% 
  mutate(unvaxpop_ONS=pop_ONS-vaxpop, unvaxpop_NIMS=pop_NIMS-vaxpop,
         age=factor(age, levels=c("Under 18", "18-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")))

#Graphic of difference in estimates of unvaxed pop
agg_tiff("Outputs/EngPopUnvaxEstimates.tiff", units="in", width=9, height=6, res=800)
data %>% filter(metric=="cases") %>%
  mutate(popdiff_abs=unvaxpop_ONS-unvaxpop_NIMS,
         popdiff_rel=popdiff_abs/unvaxpop_NIMS, 
         labpos=if_else(popdiff_abs<0, 1.1, -0.1)) %>% 
  ggplot()+
  geom_col(aes(x=popdiff_abs, y=age), fill="aquamarine4")+
  geom_vline(aes(xintercept=0), colour="Grey60")+
  geom_text(aes(x=popdiff_abs, y=age, label=paste0(if_else(popdiff_abs>0, "+", ""),
                                                   round(popdiff_rel*100, 0), "%"),
            hjust=labpos), size=rel(3.6))+
  scale_x_continuous(limits=c(-1800000, 1800000), breaks=c(-1500000, -1000000, -500000, 0, 500000,
                                                           1000000, 1500000),
                     labels=c("-1.5m", "-1m", "-0.5m", "0", "+0.5m", "+1m", "+1.5m"),
                     name="Difference between using ONS and NIMS denominators")+
  scale_y_discrete(name="Age group")+
  annotate("text", x=-1000000, y=8, label="ONS estimates\nlower than NIMS", 
           colour="Grey60", size=rel(5), family="Lato")+  
  annotate("text", x=1000000, y=8, label="ONS estimates\nhigher than NIMS", 
           colour="Grey60", size=rel(5), family="Lato")+  
  theme_custom()+
  labs(title="Estimating the number of unvaccinated people is hard",
       subtitle="Difference between estimates of the number of people in England who have not yet received two COVID vaccine doses\nbased on NIMS and ONS population estimates. Bars represent the absolute differences, labels the relative difference.",
       caption="Data from NHS England | Plot by @VictimOfMaths")
dev.off()  
  


