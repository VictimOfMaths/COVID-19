# COVID-19
Plots and analysis relating to the pandemic

1) LAHeatmaps.R generates heatmaps showing Upper Tier Local Authority trajectories in both confirmed COVID-19 cases and estimated COVID-19 deaths (in hospitals only) inspired by similar plots for US states from @Marco_Piani. The approach to modelling deaths, which are only published at NHS trust level, was developed by @Benj_Barr

![alt text](https://github.com/VictimOfMaths/COVID-19/blob/master/COVIDLACasesHeatmap.png)

2) AllCauseMortality.R harmonises weekly all-cause mortality data from England & Wales (from ONS), Scotland (from NRS) and Northern Ireland (from NISRA) and draws plots comparing deaths in 2020 so far to the previous decade, split by age, sex and region inspired by a plot from @EdConwaySky

![alt text](https://github.com/VictimOfMaths/COVID-19/blob/master/ONSWeeklyDeathsxAge.png)

3) COVIDExposures.R brings together data on health deprivation and estimates of the potential COVID-19 mortality risk based on the age-sex structure of the population (following the approach developed by @ikashnitsky and @jm_aburto) at Lower Super Output Area level and plots bivariate maps highlighting areas with the greatest potential COVID-19 risk. I also made a Shiny app which creates slightly lower resolution versions of the same plots online https://victimofmaths.shinyapps.io/covidmapper/

![alt text](https://github.com/VictimOfMaths/COVID-19/blob/master/COVIDBivariateLondon.png)
