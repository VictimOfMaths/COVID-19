rm(list=ls())

library(tidyverse)
library(paletteer)
library(curl)
library(readxl)

temp <- tempfile()
source <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fbirthsdeathsandmarriages%2fdeaths%2fdatasets%2fdeathsinvolvingcovid19bylocalareaanddeprivation%2f1march2020to17april2020/referencetablesdraft.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

data <- read_excel(temp, sheet="Table 3", range="B6:P25", col_names=FALSE)[,-c(4:6,9:11, 14,15)]

colnames(data) <- c("IMD", "deaths.pers", "rate.pers", "deaths.m", "rate.m", "deaths.f", "rate.f")

data$cause <- rep(c("All cause", "COVID-19"), each=10, times=1)

#Calculate 'other cause' outcomes
data_wide <- pivot_wider(data, names_from=cause, values_from=c(2:7))

data_wide$deaths.pers_other <- data_wide$`deaths.pers_All cause`-data_wide$`deaths.pers_COVID-19`
data_wide$rate.pers_other <- data_wide$`rate.pers_All cause`-data_wide$`rate.pers_COVID-19`
data_wide$deaths.m_other <- data_wide$`deaths.m_All cause`-data_wide$`deaths.m_COVID-19`
data_wide$rate.m_other <- data_wide$`rate.m_All cause`-data_wide$`rate.m_COVID-19`
data_wide$deaths.f_other <- data_wide$`deaths.f_All cause`-data_wide$`deaths.f_COVID-19`
data_wide$rate.f_other <- data_wide$`rate.f_All cause`-data_wide$`rate.f_COVID-19`

data <- pivot_longer(data_wide, cols=c(2:19), names_to=c("measure", "cause"), 
                     values_to="values", names_sep="_")

data$metric <- case_when(
  substr(data$measure, 1, 6)=="deaths" ~ "deaths",
  substr(data$measure, 1, 4)=="rate" ~ "rate",
  TRUE ~ "ERROR"
)

data$sex <- case_when(
  endsWith(data$measure, "m")==TRUE ~ "Male",
  endsWith(data$measure, "f")==TRUE ~ "Female",
  TRUE~"all"
)

data$sex <- factor(data$sex, levels=c("Male", "Female", "all"))
data$cause <- factor(data$cause, levels=c("other", "COVID-19", "All cause"))
data$IMD <- factor(data$IMD, levels=c("1 (most deprived)", "2", "3", "4", "5", "6", "7", "8", "9", "10 (least deprived)"))

########
#Deaths#
########

#Generate labels
tempA <- (data$values[data$sex=="Male" & data$cause=="other" & data$IMD=="1 (most deprived)" & data$metric=="deaths"]-
  data$values[data$sex=="Male" & data$cause=="other" & data$IMD=="10 (least deprived)" & data$metric=="deaths"])/
  data$values[data$sex=="Male" & data$cause=="other" & data$IMD=="10 (least deprived)" & data$metric=="deaths"]

labA <- paste0(round(tempA*100,0), "% more non-COVID-19 deaths\nin the most vs. least\ndeprived areas")

tempB <- (data$values[data$sex=="Male" & data$cause=="COVID-19" & data$IMD=="1 (most deprived)" & data$metric=="deaths"]-
           data$values[data$sex=="Male" & data$cause=="COVID-19" & data$IMD=="10 (least deprived)" & data$metric=="deaths"])/
  data$values[data$sex=="Male" & data$cause=="COVID-19" & data$IMD=="10 (least deprived)" & data$metric=="deaths"]

labB <- paste0(round(tempB*100,0), "% more COVID-19 deaths\nin the most vs. least\ndeprived areas")

tempC <- (data$values[data$sex=="Female" & data$cause=="other" & data$IMD=="1 (most deprived)" & data$metric=="deaths"]-
           data$values[data$sex=="Female" & data$cause=="other" & data$IMD=="10 (least deprived)" & data$metric=="deaths"])/
  data$values[data$sex=="Female" & data$cause=="other" & data$IMD=="10 (least deprived)" & data$metric=="deaths"]

labC <- paste0(round(tempC*100,0), "% more non-COVID-19 deaths\nin the most vs. least\ndeprived areas")

tempD <- (data$values[data$sex=="Female" & data$cause=="COVID-19" & data$IMD=="1 (most deprived)" & data$metric=="deaths"]-
           data$values[data$sex=="Female" & data$cause=="COVID-19" & data$IMD=="10 (least deprived)" & data$metric=="deaths"])/
  data$values[data$sex=="Female" & data$cause=="COVID-19" & data$IMD=="10 (least deprived)" & data$metric=="deaths"]

labD <- paste0(round(tempD*100,0), "% more COVID-19 deaths\nin the most vs. least\ndeprived areas")

#Set up annotations
ann_text1 <- data.frame(IMD=rep(5, times=4), values=c(4800, -900, 4700, -900), sex=c("Male", "Male", "Female", "Female"),
                        cause=rep("other", times=4))

ann_arrows1a <- data.frame(x=rep(c(3.4,6.7), times=4), xend=rep(c(1,10), times=4), 
                         y=rep(c(4700, -1000), times=2), yend=c(4000, 200, 3800, 200),
                         sex=rep(c("Male", "Female"), each=2), cause=rep("other", times=4))

ann_arrows1b <- data.frame(x=rep(c(3.4,6.7), times=4), xend=rep(c(1,10), times=4), 
                           y=rep(c(-1000, 4700), times=2), yend=c(300, 3500, 200, 3300),
                           sex=rep(c("Male", "Female"), each=2), cause=rep("other", times=4))

tiff("Outputs/COVIDIneqDeath.tiff", units="in", width=12, height=8, res=300)
ggplot(subset(data, cause!="All cause" & sex!="all" & metric=="deaths"), aes(x=IMD, y=values, fill=cause))+
  geom_col()+
  scale_fill_paletteer_d("palettetown::porygon", name="Cause", labels=c("Other", "COVID-19"))+
  scale_x_discrete(name="Deprivation decile")+
  scale_y_continuous(name="Total deaths 1st Mar-17th Apr")+
  coord_cartesian(ylim=c(0,5000), clip="off")+
  facet_wrap(~sex)+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), strip.background=element_blank(),
        strip.text=element_text(face="bold", size=rel(1)), plot.title.position="plot",
        axis.text=element_text(colour="black"))+
  labs(title="COVID-19 has hit the most deprived areas hardest",
      subtitle="Higher deprivation is associated with more deaths from both COVID-19 and non-COVID-19 causes during the pandemic",
       caption="Data from ONS | Plot by @VictimOfMaths")+
  geom_text(data=ann_text1, aes(x=IMD, y=values), 
            label=c(labA, labB, labC, labD), colour="Grey40")+
  geom_curve(data=ann_arrows1a, aes(x=x, xend=xend, y=y, yend=yend), colour="Grey40", curvature=0.21, 
             arrow=arrow(length=unit(0.2, "cm"), type="closed"))+
  geom_curve(data=ann_arrows1b, aes(x=x, xend=xend, y=y, yend=yend), colour="Grey40", curvature=-0.21, 
             arrow=arrow(length=unit(0.2, "cm"), type="closed"))
dev.off()

########
#Rates#
#######

#Generate labels
tempE <- (data$values[data$sex=="Male" & data$cause=="other" & data$IMD=="1 (most deprived)" & data$metric=="rate"]-
           data$values[data$sex=="Male" & data$cause=="other" & data$IMD=="10 (least deprived)" & data$metric=="rate"])/
  data$values[data$sex=="Male" & data$cause=="other" & data$IMD=="10 (least deprived)" & data$metric=="rate"]

labE <- paste0(round(tempE*100,0), "% higher non-COVID-19 death\nrate in the most vs.\nleast deprived areas")

tempF <- (data$values[data$sex=="Male" & data$cause=="COVID-19" & data$IMD=="1 (most deprived)" & data$metric=="rate"]-
           data$values[data$sex=="Male" & data$cause=="COVID-19" & data$IMD=="10 (least deprived)" & data$metric=="rate"])/
  data$values[data$sex=="Male" & data$cause=="COVID-19" & data$IMD=="10 (least deprived)" & data$metric=="rate"]

labF <- paste0(round(tempF*100,0), "% higher COVID-19 death\nrate in the most vs.\nleast deprived areas")

tempG <- (data$values[data$sex=="Female" & data$cause=="other" & data$IMD=="1 (most deprived)" & data$metric=="rate"]-
           data$values[data$sex=="Female" & data$cause=="other" & data$IMD=="10 (least deprived)" & data$metric=="rate"])/
  data$values[data$sex=="Female" & data$cause=="other" & data$IMD=="10 (least deprived)" & data$metric=="rate"]

labG <- paste0(round(tempG*100,0), "% higher non-COVID-19 death\nrate in the most vs.\nleast deprived areas")

tempH <- (data$values[data$sex=="Female" & data$cause=="COVID-19" & data$IMD=="1 (most deprived)" & data$metric=="rate"]-
           data$values[data$sex=="Female" & data$cause=="COVID-19" & data$IMD=="10 (least deprived)" & data$metric=="rate"])/
  data$values[data$sex=="Female" & data$cause=="COVID-19" & data$IMD=="10 (least deprived)" & data$metric=="rate"]

labH <- paste0(round(tempH*100,0), "% higher COVID-19 death\nrate in the most vs.\nleast deprived areas")

#Set up annotations
ann_text2 <- data.frame(IMD=rep(5, times=4), values=c(270, -50, 220, -50), sex=c("Male", "Male", "Female", "Female"),
                        cause=rep("other", times=4))

ann_arrows2a <- data.frame(x=rep(c(3.2,6.8), times=4), xend=rep(c(1,10), times=4), 
                           y=c(260,-50, 210,-50), yend=c(220, 20, 160, 10),
                           sex=rep(c("Male", "Female"), each=2), cause=rep("other", times=4))

ann_arrows2b <- data.frame(x=rep(c(3.2,6.8), times=4), xend=rep(c(1,10), times=4), 
                           y=c(-50,260,-50,210), yend=c(30, 130, 25, 90),
                           sex=rep(c("Male", "Female"), each=2), cause=rep("other", times=4))

tiff("Outputs/COVIDIneqRate.tiff", units="in", width=12, height=8, res=300)
ggplot(subset(data, cause!="All cause" & sex!="all" & metric=="rate"), aes(x=IMD, y=values, fill=cause))+
  geom_col()+
  scale_fill_paletteer_d("palettetown::porygon", name="Cause", labels=c("Other", "COVID-19"))+
  scale_x_discrete(name="Deprivation decile")+
  scale_y_continuous(name="Age-standardised death rate\nper 100,000 1st Mar-17th Apr")+
  coord_cartesian(ylim=c(0,300), clip="off")+
  facet_wrap(~sex)+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), strip.background=element_blank(),
        strip.text=element_text(face="bold", size=rel(1)), plot.title.position="plot",
        axis.text=element_text(colour="black"))+
  labs(title="Inequalities in COVID-19 impact are even steeper after adjusting for age",
       subtitle="Age-standardised rates of both confirmed COVID-19 deaths and all other causes are roughly twice as high in the most vs. least deprived areas",
       caption="Data from ONS | Plot by @VictimOfMaths")+
  geom_text(data=ann_text2, aes(x=IMD, y=values), 
            label=c(labE, labF, labG, labH), colour="Grey40")+
  geom_curve(data=ann_arrows2a, aes(x=x, xend=xend, y=y, yend=yend), colour="Grey40", curvature=0.16, 
             arrow=arrow(length=unit(0.2, "cm"), type="closed"))+
  geom_curve(data=ann_arrows2b, aes(x=x, xend=xend, y=y, yend=yend), colour="Grey40", curvature=-0.29, 
             arrow=arrow(length=unit(0.2, "cm"), type="closed"))
dev.off()
