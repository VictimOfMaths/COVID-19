# COVID-19
Plots and analysis relating to the pandemic

1) Heatmaps:<br>[English LA Heatmaps.R]https://github.com/VictimOfMaths/COVID-19/blob/master/Heatmaps/English%20LA%20Heatmaps.R) generates heatmaps showing English Upper Tier Local Authority trajectories in both confirmed COVID-19 cases and estimated COVID-19 deaths (in hospitals only) inspired by similar plots for US states from [@Marco_Piani](https://twitter.com/Marco_Piani). The approach to modelling deaths, which are only published at NHS trust level, was developed by [@Benj_Barr](https://twitter.com/Benj_Barr).<br>
[Scottish HB Heatmaps.R](https://github.com/VictimOfMaths/COVID-19/blob/master/Heatmaps/Scottish%20HB%20Heatmaps.R), [Welsh LA Heatmaps.R](https://github.com/VictimOfMaths/COVID-19/blob/master/Heatmaps/Welsh%20LA%20Heatmaps.R), [Irish County Heatmaps.R](https://github.com/VictimOfMaths/COVID-19/blob/master/Heatmaps/Irish%20County%20Heatmaps.R) and [German State Heatmaps.R](https://github.com/VictimOfMaths/COVID-19/blob/master/Heatmaps/German%20State%20Heatmaps.R) produce equivalent case trajectory plots for Scottish Health Boards, Welsh Local Authorities, Irish Counties and German Bundesländer respectively.<br>
[UK Hex Animation.R](https://github.com/VictimOfMaths/COVID-19/blob/master/Heatmaps/UK%20Hex%20Animations.R) uses this data to generate an animated hex map of COVID-19 cases across the UK & Ireland, built on various excellent hex map resources from [@ODILeeds](https://twitter.com/ODILeeds) and [@olihawkins](https://twitter.com/olihawkins).

![Cases heatmap](https://github.com/VictimOfMaths/COVID-19/blob/master/Heatmaps/COVIDLACasesHeatmap.png)

2) [AllCauseMortality.R](https://github.com/VictimOfMaths/COVID-19/blob/master/AllCauseMortality.R) harmonises weekly all-cause mortality data from England & Wales (from ONS), Scotland (from NRS) and Northern Ireland (from NISRA) and draws plots comparing deaths in 2020 so far to the previous decade, split by age, sex and region inspired by a plot from @EdConwaySky.
<br>[NRSDeathsDetail.R](https://github.com/VictimOfMaths/COVID-19/blob/master/NRSDeathsDetail) uses richer data published by NRS to look at patterns in excess mortality in Scotland by place of death, Health Board area and age.<br>
[AllCauseDeathsxAge.R](https://github.com/VictimOfMaths/COVID-19/blob/master/AllCauseDeathsxAge.R) Compares age-specific excess mortality rates between UK countries and brings in international data from the [Human Mortality Database](https://www.mortality.org/) for comparison.

![Excess deaths](https://github.com/VictimOfMaths/COVID-19/blob/master/ONSNRSNISRAWeeklyDeathsxReg.png)

3) [COVIDExposures.R](https://github.com/VictimOfMaths/COVID-19/blob/master/COVIDExposures.R) brings together data on health deprivation and estimates of the potential COVID-19 mortality risk based on the age-sex structure of the population (following the approach developed by @ikashnitsky and @jm_aburto) at Lower Super Output Area level and plots bivariate maps highlighting areas with the greatest potential COVID-19 risk. I also made a Shiny app which creates slightly lower resolution versions of the same plots online, which you can find here: https://victimofmaths.shinyapps.io/covidmapper/

![Bivariate map](https://github.com/VictimOfMaths/COVID-19/blob/master/COVIDBivariateLondon.png)

4) [COVIDCasesByIMD.R](https://github.com/VictimOfMaths/COVID-19/blob/master/COVIDCasesByIMD.R) takes published figures on confirmed COVID-19 cases by Local Authority in England and maps that onto quintiles of the Index of Multiple Deprivation, then plots a variety of case trajectories by deprivation quintile as well as a map of confirmed case rates.

![Quintile plot](https://github.com/VictimOfMaths/COVID-19/blob/master/COVIDQuintilesLonRate.png)

5) [ONSDeathIneq.R](https://github.com/VictimOfMaths/COVID-19/blob/master/ONSDeathIneq.R) takes data the ONS have published for England on deaths from COVID-19 and other causes between 1st March-17th April and illustrates socioeconomic inequalities in the impact of the pandemic.
<br>[ONSDeathIneq2.R](https://github.com/VictimOfMaths/COVID-19/blob/master/ONSDeathIneq2.R) brings in historical data on socioeconomic inequalities in all-cause deaths to compare the inequality impacts of the pandemic on mortality to historical levels of inequality.

![Inequality plot](https://github.com/VictimOfMaths/COVID-19/blob/master/COVIDIneqRate.png)
