rm(list=ls())

library(tidyverse)
library(curl)
library(readxl)
library(ggtext)
library(paletteer)

###################################################################################
#Weekly data

#Read in 2020 data for Scotland
temp <- tempfile()
source <- "https://www.nrscotland.gov.uk/files//statistics/covid19/covid-deaths-data-week-37.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
data <- read_excel(temp, sheet="Table 6", range="B6:N15", col_names=FALSE)
colnames(data) <- c("SIMD", "Total_Deaths", "Total_Rate", "Total_LowerCI", "Total_UpperCI",
                    "Male_Deaths", "Male_Rate", "Male_LowerCI", "Male_UpperCI", 
                    "Female_Deaths", "Female_Rate", "Female_LowerCI", "Female_UpperCI")
data$cause <- rep(c("All Cause", "COVID-19"), each=5)

data_long <- data %>% 
  pivot_longer(cols=c(2:13), names_to=c("sex", "metric"), names_sep="_", values_to="deaths") %>% 
  spread(cause, deaths) %>% 
  mutate(`Other`=`All Cause`-`COVID-19`) %>% 
  gather(cause, deaths, c(4:6))

data_long$sex <- factor(data_long$sex, levels=c("Male", "Female", "Total"))
data_long$cause <- factor(data_long$cause, levels=c("Other", "COVID-19", "All Cause"))

########
#Deaths#
########

#Generate labels
tempA <- (data_long$deaths[data_long$sex=="Male" & data_long$cause=="Other" & data_long$SIMD=="1 (most deprived)" & data_long$metric=="Deaths"]-
            data_long$deaths[data_long$sex=="Male" & data_long$cause=="Other" & data_long$SIMD=="5 (least deprived)" & data_long$metric=="Deaths"])/
  data_long$deaths[data_long$sex=="Male" & data_long$cause=="Other" & data_long$SIMD=="5 (least deprived)" & data_long$metric=="Deaths"]

labA <- paste0(round(tempA*100,0), "% more non-COVID-19 deaths\nin the most vs. least\ndeprived areas")

tempB <- (data_long$deaths[data_long$sex=="Male" & data_long$cause=="COVID-19" & data_long$SIMD=="1 (most deprived)" & data_long$metric=="Deaths"]-
            data_long$deaths[data_long$sex=="Male" & data_long$cause=="COVID-19" & data_long$SIMD=="5 (least deprived)" & data_long$metric=="Deaths"])/
  data_long$deaths[data_long$sex=="Male" & data_long$cause=="COVID-19" & data_long$SIMD=="5 (least deprived)" & data_long$metric=="Deaths"]

labB <- paste0(round(tempB*100,0), "% more COVID-19 deaths\nin the most vs. least\ndeprived areas")

tempC <- (data_long$deaths[data_long$sex=="Female" & data_long$cause=="Other" & data_long$SIMD=="1 (most deprived)" & data_long$metric=="Deaths"]-
            data_long$deaths[data_long$sex=="Female" & data_long$cause=="Other" & data_long$SIMD=="5 (least deprived)" & data_long$metric=="Deaths"])/
  data_long$deaths[data_long$sex=="Female" & data_long$cause=="Other" & data_long$SIMD=="5 (least deprived)" & data_long$metric=="Deaths"]

labC <- paste0(round(tempC*100,0), "% more non-COVID-19 deaths\nin the most vs. least\ndeprived areas")

tempD <- (data_long$deaths[data_long$sex=="Female" & data_long$cause=="COVID-19" & data_long$SIMD=="1 (most deprived)" & data_long$metric=="Deaths"]-
            data_long$deaths[data_long$sex=="Female" & data_long$cause=="COVID-19" & data_long$SIMD=="5 (least deprived)" & data_long$metric=="Deaths"])/
  data_long$deaths[data_long$sex=="Female" & data_long$cause=="COVID-19" & data_long$SIMD=="5 (least deprived)" & data_long$metric=="Deaths"]

labD <- paste0(round(tempD*100,0), "% more COVID-19 deaths\nin the most vs. least\ndeprived areas")

#Set up annotations
ann_text1 <- data.frame(SIMD=rep(3, times=4), deaths=c(4200, -700, 4000, -700), sex=c("Male", "Male", "Female", "Female"),
                        cause=rep("Other", times=4))

ann_arrows1a <- data.frame(x=rep(c(2,4), times=4), xend=rep(c(1,5), times=4), 
                           y=c(4200, -700,4000,-700), yend=c(3600, 200, 3400, 200),
                           sex=rep(c("Male", "Female"), each=2), cause=rep("Other", times=4))

ann_arrows1b <- data.frame(x=rep(c(2,4), times=4), xend=rep(c(1,5), times=4), 
                           y=c(-700, 4200, -700, 4000), yend=c(300, 2000, 200, 2100),
                           sex=rep(c("Male", "Female"), each=2), cause=rep("Other", times=4))

tiff("Outputs/COVIDScotIneqDeath.tiff", units="in", width=12, height=8, res=500)
data_long %>% 
  filter(sex!="Total" & cause!="All Cause" & metric=="Deaths") %>% 
ggplot(aes(x=SIMD, y=deaths, fill=cause))+
  geom_col()+
  scale_fill_paletteer_d("palettetown::porygon", name="Cause")+
  scale_x_discrete(name="Deprivation quintile")+
  scale_y_continuous(name="Deaths between 1st March & 31st August 2020")+
  coord_cartesian(ylim=c(0,4500), clip="off")+
  facet_wrap(~sex)+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), strip.background=element_blank(),
        strip.text=element_text(face="bold", size=rel(1)), plot.title.position="plot",
        axis.text=element_text(colour="black"), plot.subtitle=element_markdown())+
  geom_text(data=ann_text1, aes(x=SIMD, y=deaths), 
            label=c(labA, labB, labC, labD), colour="Grey40")+
  geom_curve(data=ann_arrows1a, aes(x=x, xend=xend, y=y, yend=yend), colour="Grey40", curvature=0.21, 
             arrow=arrow(length=unit(0.2, "cm"), type="closed"))+
  geom_curve(data=ann_arrows1b, aes(x=x, xend=xend, y=y, yend=yend), colour="Grey40", curvature=-0.21, 
             arrow=arrow(length=unit(0.2, "cm"), type="closed"))+
  labs(title="Deprived areas in Scotland have borne the brunt of the impact of COVID-19",
       subtitle="<span style='color:grey40;'>Higher deprivation is associated with substantially more deaths in 2020 from both <span style='color:#f89088;'>COVID-19 <span style='color:grey40;'>and <span style='color:#40a0d8;'>all other causes",
       caption="Data from National Records of Scotland | Plot by @VictimOfMaths")
dev.off()

#######
#Rates#
#######

#Generate labels
tempE <- (data_long$deaths[data_long$sex=="Male" & data_long$cause=="Other" & data_long$SIMD=="1 (most deprived)" & data_long$metric=="Rate"]-
            data_long$deaths[data_long$sex=="Male" & data_long$cause=="Other" & data_long$SIMD=="5 (least deprived)" & data_long$metric=="Rate"])/
  data_long$deaths[data_long$sex=="Male" & data_long$cause=="Other" & data_long$SIMD=="5 (least deprived)" & data_long$metric=="Rate"]

labE <- paste0(round(tempE*100,0), "% higher non-COVID-19 death\nrate in the most vs.\nleast deprived areas")

tempF <- (data_long$deaths[data_long$sex=="Male" & data_long$cause=="COVID-19" & data_long$SIMD=="1 (most deprived)" & data_long$metric=="Rate"]-
            data_long$deaths[data_long$sex=="Male" & data_long$cause=="COVID-19" & data_long$SIMD=="5 (least deprived)" & data_long$metric=="Rate"])/
  data_long$deaths[data_long$sex=="Male" & data_long$cause=="COVID-19" & data_long$SIMD=="5 (least deprived)" & data_long$metric=="Rate"]

labF <- paste0(round(tempF*100,0), "% higher non-COVID-19 death\nrate in the most vs.\nleast deprived areas")

tempG <- (data_long$deaths[data_long$sex=="Female" & data_long$cause=="Other" & data_long$SIMD=="1 (most deprived)" & data_long$metric=="Rate"]-
            data_long$deaths[data_long$sex=="Female" & data_long$cause=="Other" & data_long$SIMD=="5 (least deprived)" & data_long$metric=="Rate"])/
  data_long$deaths[data_long$sex=="Female" & data_long$cause=="Other" & data_long$SIMD=="5 (least deprived)" & data_long$metric=="Rate"]

labG <- paste0(round(tempG*100,0), "% higher non-COVID-19 death\nrate in the most vs.\nleast deprived areas")

tempH <- (data_long$deaths[data_long$sex=="Female" & data_long$cause=="COVID-19" & data_long$SIMD=="1 (most deprived)" & data_long$metric=="Rate"]-
            data_long$deaths[data_long$sex=="Female" & data_long$cause=="COVID-19" & data_long$SIMD=="5 (least deprived)" & data_long$metric=="Rate"])/
  data_long$deaths[data_long$sex=="Female" & data_long$cause=="COVID-19" & data_long$SIMD=="5 (least deprived)" & data_long$metric=="Rate"]

labH <- paste0(round(tempH*100,0), "% higher non-COVID-19 death\nrate in the most vs.\nleast deprived areas")

#Set up annotations
ann_text2 <- data.frame(SIMD=rep(3, times=4), deaths=c(1100, -190, 900, -190), sex=c("Male", "Male", "Female", "Female"),
                        cause=rep("Other", times=4))

ann_arrows2a <- data.frame(x=rep(c(2,4), times=4), xend=rep(c(1,5), times=4), 
                           y=c(1100, -190,900,-190), yend=c(900, 30, 650, 25),
                           sex=rep(c("Male", "Female"), each=2), cause=rep("Other", times=4))

ann_arrows2b <- data.frame(x=rep(c(2,4), times=4), xend=rep(c(1,5), times=4), 
                           y=c(-190, 1100, -190, 900), yend=c(50, 450, 50, 300),
                           sex=rep(c("Male", "Female"), each=2), cause=rep("Other", times=4))

tiff("Outputs/COVIDScotIneqRate.tiff", units="in", width=12, height=8, res=500)
data_long %>% 
  filter(sex!="Total" & cause!="All Cause" & metric=="Rate") %>% 
  ggplot(aes(x=SIMD, y=deaths, fill=cause))+
  geom_col()+
  scale_fill_paletteer_d("palettetown::porygon", name="Cause")+
  scale_x_discrete(name="Deprivation quintile")+
  scale_y_continuous(name="Age-standardised deaths per 100,000\nbetween 1st March & 31st August 2020")+
  coord_cartesian(ylim=c(0,1200), clip="off")+
  facet_wrap(~sex)+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), strip.background=element_blank(),
        strip.text=element_text(face="bold", size=rel(1)), plot.title.position="plot",
        axis.text=element_text(colour="black"), plot.subtitle=element_markdown())+
  geom_text(data=ann_text2, aes(x=SIMD, y=deaths), 
            label=c(labE, labF, labG, labH), colour="Grey40")+
  geom_curve(data=ann_arrows2a, aes(x=x, xend=xend, y=y, yend=yend), colour="Grey40", curvature=0.21, 
             arrow=arrow(length=unit(0.2, "cm"), type="closed"))+
  geom_curve(data=ann_arrows2b, aes(x=x, xend=xend, y=y, yend=yend), colour="Grey40", curvature=-0.21, 
             arrow=arrow(length=unit(0.2, "cm"), type="closed"))+
  labs(title="Inequalities increase once you adjust for age differences",
       subtitle="<span style='color:grey40;'>Higher deprivation is associated with substantially higher age-standardised death rates in 2020 from both <span style='color:#f89088;'>COVID-19 <span style='color:grey40;'>and <span style='color:#40a0d8;'>all other causes",
       caption="Data from National Records of Scotland | Plot by @VictimOfMaths")
dev.off()

#Bring in historic inequalities
temp <- tempfile()
source <- "https://www.nrscotland.gov.uk/files//statistics/age-standardised-death-rates-esp/2018/age-standard-death-rates-18-tab7.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
hist.all <- read_excel(temp, sheet="Table 7", range="A24:Q28", col_names=FALSE)[,c(1,5,8,11,14,17)]
colnames(hist.all) <- c("Year", "1 (most deprived)", "2", "3", "4", "5 (least deprived)")
hist.all$sex <- "Total"

hist.m <- read_excel(temp, sheet="Table 7", range="A56:Q60", col_names=FALSE)[,c(1,5,8,11,14,17)]
colnames(hist.m) <- c("Year", "1 (most deprived)", "2", "3", "4", "5 (least deprived)")
hist.m$sex <- "Male"

hist.f <- read_excel(temp, sheet="Table 7", range="A88:Q92", col_names=FALSE)[,c(1,5,8,11,14,17)]
colnames(hist.f) <- c("Year", "1 (most deprived)", "2", "3", "4", "5 (least deprived)")
hist.f$sex <- "Female"

hist <- bind_rows(hist.all, hist.m, hist.f)
hist$cause <- "All Cause"
hist$metric <- "Rate"

hist_long <- gather(hist, SIMD, deaths, c(2:6))

#Adjust deaths to estimate historic deaths between 1st March & 30th August using within-year % of deaths
#In these months (not available split by IMD)
temp <- tempfile()
source <- "https://www.nrscotland.gov.uk/files//statistics/weekly-monthly-births-deaths-data/2020/aug/monthly-august-20-tab3.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
month.adj <- read_excel(temp, sheet="Scotland", range="A5:N35")
month.adj$adjprop <- (month.adj$Mar+month.adj$Apr+month.adj$May+month.adj$Jun+
  month.adj$Jul+month.adj$Aug)/month.adj$...2
month.adj <- month.adj[c(1,15)]
colnames(month.adj) <- c("Year", "adjprop") 

hist_long <- merge(hist_long, month.adj, by="Year")
hist_long$deaths <- hist_long$deaths*hist_long$adjprop

data_long$Year <- 2020

data_full <- bind_rows(data_long, hist_long)

data_full$sex <- factor(data_full$sex, levels=c("Male", "Female", "Total"))
data_full$cause <- factor(data_full$cause, levels=c("Other", "COVID-19", "All Cause"))

tiff("Outputs/COVIDScotIneqRateHist.tiff", units="in", width=12, height=8, res=500)
ggplot()+
  geom_col(data=subset(data_full, Year==2020 & metric=="Rate" & sex!="Total" & cause!="All Cause"),
           aes(x=SIMD, y=deaths, fill=cause), show.legend=FALSE)+
  geom_jitter(data=subset(data_full, Year!=2020 & sex!="Total"), 
              aes(x=SIMD, y=deaths), colour="midnightblue")+
  scale_fill_paletteer_d("palettetown::porygon", name="Cause")+
  scale_x_discrete(name="Deprivation quintile")+
  scale_y_continuous(name="Age-standardised deaths per 100,000\nbetween 1st March & 31st August")+
  facet_wrap(~sex)+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), strip.background=element_blank(),
        strip.text=element_text(face="bold", size=rel(1)), plot.title.position="plot",
        axis.text=element_text(colour="black"), plot.subtitle=element_markdown())+
  labs(title="All-cause deaths in Scotland have been slightly more unequal than usual in 2020",
       subtitle="<span style='color:grey40;'>Age-standardised mortality rates in Scotland between March 1st and August 31st by deprivation quintile from <span style='color:midnightblue;'>all causes in 2014-18<span style='color:grey40;'> and from <span style='color:#f89088;'>COVID-19<span style='color:grey40;'> and <span style='color:#40a0d8;'>other causes<span style='color:grey40;'> in 2020",
       caption="Data from National Records of Scotland | Plot by @VictimOfMaths")
dev.off()

#Calculate SII & RII
data_full$cumproppop <- case_when(
  data_full$SIMD=="5 (least deprived)" ~ 0.1,
  data_full$SIMD=="4" ~ 0.3,
  data_full$SIMD=="3" ~ 0.5,
  data_full$SIMD=="2" ~ 0.7,
  data_full$SIMD=="1 (most deprived)" ~ 0.9
)

SII <- data_full %>%
  filter(metric=="Rate") %>% 
  group_by(Year, sex, cause) %>% 
  do(tidy(lm(deaths ~ cumproppop, data=.))) %>% 
  pivot_wider(id_cols=c("Year", "sex", "cause"), names_from=term, 
              values_from=c("estimate", "std.error"))

colnames(SII) <- c("year", "Sex", "cause", "intercept", "SII", "int.SE", "SII.SE")
SII$RII <- (SII$intercept+SII$SII)/SII$intercept

tiff("Outputs/COVIDSIIScot.tiff", units="in", width=10, height=8, res=500)
ggplot()+
  geom_line(data=subset(SII, cause=="All Cause" & Sex!="Total"), aes(x=year, y=SII), colour="midnightblue")+
  geom_point(data=subset(SII, cause %in% c("COVID-19", "Other") & year==2020 & Sex!="Total"), 
             aes(x=year, y=SII, colour=cause), show.legend=FALSE)+
  scale_x_continuous(name="Year")+
  scale_y_continuous(name="Slope Index of Inequality (SII)")+
  scale_colour_paletteer_d("palettetown::porygon")+
  coord_cartesian(ylim=c(0,NA))+
  facet_wrap(~Sex)+
  theme_classic()+
  theme(strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)),
        plot.subtitle=element_markdown())+
  labs(title="Absolute inequality in deaths has risen, particularly for men, in 2020",
       subtitle="<span style='color:grey40;'>Slope Index of Inequality in age-standardised deaths in Scotland from <span style='color:midnightblue;'>all causes<span style='color:grey40;'>, <span style='color:#f89088;'>COVID-19 <span style='color:grey40;'>and <span style='color:#40a0d8;'>all other causes",
       caption="Data from ONS | Plot by @VictimOfMaths")
dev.off()

tiff("Outputs/COVIDRIIScot.tiff", units="in", width=10, height=8, res=500)
ggplot()+
  geom_line(data=subset(SII, cause=="All Cause" & Sex!="Total"), aes(x=year, y=RII), colour="midnightblue")+
  geom_point(data=subset(SII, cause %in% c("COVID-19", "Other") & year==2020 & Sex!="Total"), 
             aes(x=year, y=RII, colour=cause), show.legend=FALSE)+
  scale_x_continuous(name="Year")+
  scale_y_continuous(name="Relative Index of Inequality (RII)")+
  scale_colour_paletteer_d("palettetown::porygon")+
  coord_cartesian(ylim=c(0,NA))+
  facet_wrap(~Sex)+
  theme_classic()+
  theme(strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)),
        plot.subtitle=element_markdown())+
  labs(title="Relative inequality in deaths is in line with recent years",
       subtitle="<span style='color:grey40;'>Relative Index of Inequality in age-standardised deaths in Scotland from <span style='color:midnightblue;'>all causes<span style='color:grey40;'>, <span style='color:#f89088;'>COVID-19 <span style='color:grey40;'>and <span style='color:#40a0d8;'>all other causes",
       caption="Data from ONS | Plot by @VictimOfMaths")
dev.off()

