### EDA and Visualizarion     ###
### COM SCI X 450.2 - 393986  ###
### Group Project              ###
#######################################################
## Members: Chi-Lin Hung, Brandon Wang, Dian Wang    ##
#######################################################

library(readxl)
library(tidyverse)
library(cluster)
library(spDataLarge)
library(tmap)
library(scatterplot3d)
library(ggplot2)
library(GGally)
library(dplyr)

################## load data ##################
jobGrowth2023 <- read.csv("data/job-growth-by-state-2023.csv")
unemployment <- read.csv("data/UnemploymentInAmericaPerUSState.csv")
cli2023 <- read.csv("data/cost-of-living-index-by-state-2023.csv")





########################################################
###   Unemployment  in American Per State            ###
########################################################

################## preprocessing ######################
unemployment <- transform(unemployment, Total.Employment.in.State.Area = as.numeric(gsub(",","",Total.Employment.in.State.Area)))
unemployment <- transform(unemployment, Total.Unemployment.in.State.Area = as.numeric(gsub(",","",Total.Unemployment.in.State.Area)))
unemployment <- transform(unemployment, Total.Civilian.Non.Institutional.Population.in.State.Area = as.numeric(gsub(",","",Total.Civilian.Non.Institutional.Population.in.State.Area)))
unemployment <- transform(unemployment, Total.Civilian.Labor.Force.in.State.Area = as.numeric(gsub(",","",Total.Civilian.Labor.Force.in.State.Area)))
unemployment

unemployment %>%
  select(-FIPS.Code) %>%
  group_by(State.Area, Year) %>%
  summarise(sTotalEmployment = sum(Total.Employment.in.State.Area),
            sTotalUnemployment = sum(Total.Unemployment.in.State.Area),
            sTotalCivNonIns = sum(Total.Civilian.Non.Institutional.Population.in.State.Area),
            sTotalCivLabor = sum(Total.Civilian.Labor.Force.in.State.Area),
            aUnemploy = mean(Percent.....of.Labor.Force.Unemployed.in.State.Area)) -> unemploy_areaYearSum

unemploy_areaYearSum


unemploy_areaYearSum %>%
  select(-Year) %>%
  group_by(State.Area) %>%
  summarise(ssTotalEmployment = sum(sTotalEmployment),
            ssTotalUnemployment = sum(sTotalUnemployment),
            ssTotalCivNonIns = sum(sTotalCivNonIns),
            ssTotalCivLabor = sum(sTotalCivLabor),
            aaUnemploy = mean(aUnemploy)) -> unemploy_areasum
unemploy_areasum

################## 1976-2022 ######################
### Plot barplot ###
options(scipen = 999)
png("np_employ76-22.png", width = 400, height = 650)
unemploy_areasum$ssTotalEmployment <- unemploy_areasum$ssTotalEmployment / 1000000000
barplot(unemploy_areasum$ssTotalEmployment, main = "1976-2022 Total Employed Population", xlab = "Population in Billion",
        names.arg = unemploy_areasum$State.Area, las = 2, horiz = T, cex.names = 0.5, col = c("skyblue", "maroon","lightgreen", "pink"))
dev.off()
png("np_unemploy76-22.png", width = 400, height = 650)
unemploy_areasum$ssTotalUnemployment <- unemploy_areasum$ssTotalUnemployment / 1000000
barplot(unemploy_areasum$ssTotalUnemployment, main = "1976-2022 Total Unemployed Population", xlab = "Population in Million",
        names.arg = unemploy_areasum$State.Area, las = 2, horiz = T, cex.names = 0.5, col = c("skyblue", "maroon","lightgreen", "pink"))
dev.off()
png("np_civnonins76-22.png", width = 400, height = 650)
unemploy_areasum$ssTotalCivNonIns <- unemploy_areasum$ssTotalCivNonIns / 1000000000
barplot(unemploy_areasum$ssTotalCivNonIns, main = "1976-2022 Total Civilian Non-Institutional Population", xlab = "Population in Billion",
        names.arg = unemploy_areasum$State.Area, las = 2, horiz = T, cex.names = 0.5, col = c("skyblue", "maroon","lightgreen", "pink"))
dev.off()
png("np_civlabfor76-22.png", width = 400, height = 650)
unemploy_areasum$ssTotalCivLabor <- unemploy_areasum$ssTotalCivLabor / 1000000000
barplot(unemploy_areasum$ssTotalCivLabor, main = "1976-2022 Total Civilian Labor Force", xlab = "Population in Billion",
        names.arg = unemploy_areasum$State.Area, las = 2, horiz = T, cex.names = 0.5, col = c("skyblue", "maroon","lightgreen", "pink"))
dev.off()

### output cleaned file
write.csv(unemploy_areaYearSum,file='unemployed76-22_cleaned.csv', row.names=FALSE)


################## 2022 ######################
### Plot barplot ###
unemploy_areaYearSum %>%
  filter(Year == "2022") -> unemploy_2022Sum

unemploy_2022Sum %>%
  summary
png("np_employ22.png", width = 400, height = 650)
unemploy_2022Sum
unemploy_2022Sum$sTotalEmployment <- unemploy_2022Sum$sTotalEmployment / 1000000
barplot(unemploy_2022Sum$sTotalEmployment, main = "2022 Employed Population", xlab = "Population in Million",
        names.arg = unemploy_2022Sum$State.Area, las = 2, horiz = T, cex.names = 0.5, col = c("skyblue", "maroon","lightgreen", "pink"))
dev.off()
png("np_unemploy22.png", width = 400, height = 650)
unemploy_2022Sum$sTotalUnemployment <- unemploy_2022Sum$sTotalUnemployment / 1000000
barplot(unemploy_2022Sum$sTotalUnemployment, main = "2022 Unemployed Population", xlab = "Population in Million",
        names.arg = unemploy_2022Sum$State.Area, las = 2, horiz = T, cex.names = 0.5, col = c("skyblue", "maroon","lightgreen", "pink"))
dev.off()
png("np_civnonins22.png", width = 400, height = 650)
unemploy_2022Sum$sTotalCivNonIns <- unemploy_2022Sum$sTotalCivNonIns / 1000000
barplot(unemploy_2022Sum$sTotalCivNonIns, main = "2022 Civilian Non-Institutional Population", xlab = "Population in Million",
        names.arg = unemploy_2022Sum$State.Area, las = 2, horiz = T, cex.names = 0.5, col = c("skyblue", "maroon","lightgreen", "pink"))
dev.off()
png("np_civlabfor22.png", width = 400, height = 650)
unemploy_2022Sum$sTotalCivLabor <- unemploy_2022Sum$sTotalCivLabor / 1000000
barplot(unemploy_2022Sum$sTotalCivLabor, main = "2022 Civilian Labor Force", xlab = "Population in Million",
        names.arg = unemploy_2022Sum$State.Area, las = 2, horiz = T, cex.names = 0.5, col = c("skyblue", "maroon","lightgreen", "pink"))
dev.off()


### Plot map ###
us_states
unemploy_2022Sum 
barplot(unemploy_2022Sum$aUnemploy,  main = "2022 Unemployed Percentage", xlab = "Percentage",
        names.arg = unemploy_2022Sum$State.Area, las = 2, horiz = T, cex.names = 0.5, col = c("skyblue", "maroon","lightgreen", "pink"))
unemploy_2022Sum %>%
  rename(NAME = State.Area) -> unemploy_2022Sum0
(unemploy_2022p <- merge(us_states, unemploy_2022Sum0, by="NAME"))


tmap_mode("view")
tm_shape(unemploy_2022p) + tm_polygons(c("sTotalUnemployment", "sTotalEmployment")) +
  tm_facets(sync = TRUE, ncol = 2)

tm_shape(unemploy_2022p) + tm_polygons(c("sTotalUnemployment", "sTotalCivNonIns")) +
  tm_facets(sync = TRUE, ncol = 2)

tm_shape(unemploy_2022p) + tm_polygons(c("sTotalUnemployment", "sTotalCivLabor")) +
  tm_facets(sync = TRUE, ncol = 2)

tm_shape(unemploy_2022p) + tm_polygons(c("sTotalUnemployment", "sTotalCivLabor")) +
  tm_facets(sync = TRUE, ncol = 2)


### output cleaned file
write.csv(unemploy_2022Sum,file='unemployed2022_cleaned.csv', row.names=FALSE)


###################################
### Job Growth 2023             ###
###################################

################## preprocessing ######################
str(jobGrowth2023)

jobGrowth2023 %>%
  summary

(jobGrowth2023 %>% 
    select(state, densityMi, pop2022, pop2023, TotalJobsFeb2023, YoYNetGrowthIn1000s, YoYNetGrowthPerc) -> jobGrowth2023)

### Plot barplot ###
png("jg_density23.png", width = 400, height = 650)
barplot(jobGrowth2023$densityMi, main = "Population Density per State", xlab = "Number of People per Square Mile",
        names.arg = jobGrowth2023$state, las = 2, horiz = T, cex.names = 0.5, col = c("skyblue", "maroon","lightgreen", "pink"))
dev.off()
png("jg_totaljobs23.png", width = 400, height = 650)
jobGrowth2023$JobGrowthTotalJobsFeb2023 <- jobGrowth2023$JobGrowthTotalJobsFeb2023 / 1000000
barplot(jobGrowth2023$JobGrowthTotalJobsFeb2023, main = "Total Number of Jobs", xlab = "Number of Jobs in Million",
        xlim = c(0, 20), names.arg = jobGrowth2023$state, las = 2, horiz = T, cex.names = 0.5, col = c("skyblue", "maroon","lightgreen", "pink"))
dev.off()
png("jg_yoynetgr23.png", width = 400, height = 650)
barplot(jobGrowth2023$JobGrowthYoYNetGrowthIn1000s, main = "YoY Jobs Net Growth In 1000s", xlab = "Number of Jobs Net Growth in Thousands",
        names.arg = jobGrowth2023$state, las = 2, horiz = T, cex.names = 0.5, col = c("skyblue", "maroon","lightgreen", "pink"))
dev.off()

png("jg_yoynetgrperc23.png", width = 400, height = 650)
barplot(jobGrowth2023$JobGrowthYoYNetGrowthPerc, main = "YoY Jobs Net Growth Percentage", xlab = "Jobs Net Growth in Percentage",
        names.arg = jobGrowth2023$state, las = 2, horiz = T, cex.names = 0.5, col = c("skyblue", "maroon","lightgreen", "pink"))
dev.off()


### Plot map ###
jobGrowth2023 %>%
  rename(NAME = state) -> jobGrowth2023_

(jobGrowth2023p <- merge(us_states, jobGrowth2023_, by="NAME"))

tmap_mode("plot")

tm_shape(jobGrowth2023p) + tm_polygons("densityMi") +
  tm_symbols(col = "darkred", border.col = "red", size = "densityMi")

tm_shape(jobGrowth2023p) + tm_polygons(legend.title = "Population 2022 vs 2023") +
  tm_symbols(col = "pop2022", border.col = "blue", size = "pop2023", scale = 2) +
  tm_layout(legend.title.size = 0.5,
            legend.text.size = 0.4,
            legend.hist.size = 0.3,
            legend.position = c("right","bottom"))

jobGrowth2023p %>%
  rename(TotalJobFeb2023 = JobGrowthTotalJobsFeb2023,
         YoYNetGrowthIn1000s = JobGrowthYoYNetGrowthIn1000s,
         YoYNetGrowthPerc = JobGrowthYoYNetGrowthPerc) -> jobGrowth2023p

tm_shape(jobGrowth2023p) + tm_polygons("TotalJobFeb2023")

tm_shape(jobGrowth2023p) + tm_polygons("TotalJobFeb2023") +
  tm_symbols(col = "darkgreen", border.col = "green", size = "YoYNetGrowthPerc", scale = 1.5) +
  tm_layout(legend.title.size = 0.5,
            legend.text.size = 0.4,
            legend.hist.size = 0.3,
            legend.position = c("right","bottom"))

tm_shape(jobGrowth2023p) + tm_polygons("TotalJobFeb2023") +
  tm_symbols(col = "deepskyblue3", border.col = "darkblue", size = "YoYNetGrowthIn1000s", scale = 1.5) +
  tm_layout(legend.title.size = 0.5,
            legend.text.size = 0.4,
            legend.hist.size = 0.3,
            legend.position = c("right","bottom"))

#### plot scatmat
ggscatmat(jobGrowth2023)

write.csv(jobGrowth2023,file='jobgrowth2023_cleaned.csv', row.names=FALSE)

str(jobGrowth2023)



###################################
### Cost of Living 2023         ###
###################################

################## preprocessing ######################
str(cli2023)
(cli2023 %>%
    select(state, CostOfLivingIndex2023, CostOfLivingIndexGroceryCostsIndex, CostOfLivingIndexHealthCostsIndex, 
           CostOfLivingIndexHousingCostsIndex, CostOfLivingIndexMiscCostsIndex, CostOfLivingIndexTransportationCostsIndex,
           CostOfLivingIndexUtilityCostsIndex) -> cli2023)

# filter last row which does not contain state data
cli2023 %>%
  filter(row_number() <= n()-1) -> cli2023 


cli2023 %>% 
  rename(CostOfLivingIndex = CostOfLivingIndex2023,
         GroceryCostsIndex = CostOfLivingIndexGroceryCostsIndex,
         HealthCostsIndex = CostOfLivingIndexHealthCostsIndex,
         HousingCostsIndex = CostOfLivingIndexHousingCostsIndex,
         MiscCostsIndex = CostOfLivingIndexMiscCostsIndex,
         TransportationCostsIndex = CostOfLivingIndexTransportationCostsIndex,
         UtilityCostsIndex = CostOfLivingIndexUtilityCostsIndex) -> cli2023
cli2023 %>% summary()


### plot barplot ###
plot(cli2023)
png("cli_cli23.png", width = 400, height = 650)
barplot(cli2023$CostOfLivingIndex , main = "Cost of Living Index", #xlab = "Number of People per Square Mile",
        names.arg = cli2023$state, las = 2, horiz = T, cex.names = 0.5, col = c("skyblue", "maroon","lightgreen", "pink"))
dev.off()
png("cli_grocery23.png", width = 400, height = 650)
barplot(cli2023$GroceryCostsIndex , main = "Grocery Cost Index", #xlab = "Number of People per Square Mile",
        names.arg = cli2023$state, las = 2, horiz = T, cex.names = 0.5, col = c("skyblue", "maroon","lightgreen", "pink"))
dev.off()
png("cli_health23.png", width = 400, height = 650)
barplot(cli2023$HealthCostsIndex , main = "Health Costs Index", #xlab = "Number of People per Square Mile",
        names.arg = cli2023$state, las = 2, horiz = T, cex.names = 0.5, col = c("skyblue", "maroon","lightgreen", "pink"))
dev.off()
png("cli_housing23.png", width = 400, height = 650)
barplot(cli2023$HousingCostsIndex , main = "Housing Costs Index", #xlab = "Number of People per Square Mile",
        names.arg = cli2023$state, las = 2, horiz = T, cex.names = 0.5, col = c("skyblue", "maroon","lightgreen", "pink"))
dev.off()
png("cli_misc23.png", width = 400, height = 650)
barplot(cli2023$MiscCostsIndex , main = "Micellaneous Costs Index", #xlab = "Number of People per Square Mile",
        names.arg = cli2023$state, las = 2, horiz = T, cex.names = 0.5, col = c("skyblue", "maroon","lightgreen", "pink"))
dev.off()
png("cli_transp23.png", width = 400, height = 650)
barplot(cli2023$TransportationCostsIndex , main = "Transportation Costs Index", #xlab = "Number of People per Square Mile",
        names.arg = cli2023$state, las = 2, horiz = T, cex.names = 0.5, col = c("skyblue", "maroon","lightgreen", "pink"))
dev.off()
png("cli_uti23.png", width = 400, height = 650)
barplot(cli2023$UtilityCostsIndex , main = "Utility Costs Index", #xlab = "Number of People per Square Mile",
        names.arg = cli2023$state, las = 2, horiz = T, cex.names = 0.5, col = c("skyblue", "maroon","lightgreen", "pink"))
dev.off()
str(cli2023)


### plot map ###
cli2023 %>%
  rename(NAME = state) -> cli2023_

(cli2023p <- merge(us_states, cli2023_, by="NAME"))


cli2023p %>%
  summary

tm_shape(cli2023p) + tm_polygons("CostOfLivingIndex") +
  tm_layout(legend.title.size = 0.6,
            legend.text.size = 0.5,
            legend.hist.size = 0.4)
tm_shape(cli2023p) + tm_polygons("GroceryCostsIndex") +
  tm_layout(legend.title.size = 0.6,
            legend.text.size = 0.5,
            legend.hist.size = 0.4)
tm_shape(cli2023p) + tm_polygons("HealthCostsIndex") +
  tm_layout(legend.title.size = 0.6,
            legend.text.size = 0.5,
            legend.hist.size = 0.4)
tm_shape(cli2023p) + tm_polygons("HousingCostsIndex") +
  tm_layout(legend.title.size = 0.6,
            legend.text.size = 0.5,
            legend.hist.size = 0.4)
tm_shape(cli2023p) + tm_polygons("MiscCostsIndex") +
  tm_layout(legend.title.size = 0.6,
            legend.text.size = 0.5,
            legend.hist.size = 0.4)
tm_shape(cli2023p) + tm_polygons("TransportationCostsIndex") +
  tm_layout(legend.title.size = 0.6,
            legend.text.size = 0.5,
            legend.hist.size = 0.4)
tm_shape(cli2023p) + tm_polygons("UtilityCostsIndex") +
  tm_layout(legend.title.size = 0.6,
            legend.text.size = 0.5,
            legend.hist.size = 0.4)


#### plot scatter plot & scatmat
plot(cli2023)
ggscatmat(cli2023)


write.csv(cli2023,file='cost_of_living_cleaned.csv', row.names=FALSE)



####################################
### Statistics & Analysis        ###
####################################

################## summary statistics ####################
options(scipen = 999)
(unemployeddata <- read.csv("data/unemployed2022_cleaned.csv"))
unemplynum <- unemployeddata %>%
  select("sTotalEmployment", "sTotalUnemployment","sTotalCivNonIns","sTotalCivLabor")
boxplot(unemplynum, main = "Job Growth Summary Statistics")

options(scipen = 0)
jobgrowthdata <- read.csv("jobgrowth2023_cleaned.csv")
jobgrowthdata
jobgrowthnum <- jobgrowthdata %>%
  select("densityMi", "pop2022", "pop2023",	"JobGrowthTotalJobsFeb2023", "JobGrowthYoYNetGrowthIn1000s",	"JobGrowthYoYNetGrowthPerc")
box <- boxplot(jobgrowthnum, xaxt = "n", main = "Job Growth Summary Statistics")
tick <- seq_along(box$names)
text(tick, par("usr")[1] - 0.1, adj = 1.4, box$names, srt = 35, xpd = TRUE, cex = 0.6)

options(scipen = 999)
costoflivingdata <- read.csv("cost_of_living_cleaned.csv")
costoflivingdata
costoflivingnum <- costoflivingdata %>%
  select("CostOfLivingIndex",	"GroceryCostsIndex", "HealthCostsIndex", "HousingCostsIndex",	"MiscCostsIndex", "TransportationCostsIndex", "UtilityCostsIndex")
box <- boxplot(costoflivingnum, xaxt = "n", main = "Cost of Living Summary Statistics")
tick <- seq_along(box$names)
text(tick, par("usr")[1] - 0.2, adj = 0, box$names, srt = 55, xpd = TRUE, cex = 0.6)



################## Analysis ####################
# Unemployment x Cost of Living Scatterplots
unemployment_data <- read.csv("data/unemployed2022_cleaned.csv")
cost_of_living_data <- read.csv("data/cost_of_living_cleaned.csv")

merged_data <- merge(unemployment_data, cost_of_living_data, by = "state")

data_to_plot <- merged_data %>%
  select("State.Area", "sTotalEmployment", "CostOfLivingIndex")

for(index in c("CostOfLivingIndex", "CostOfLivingIndex", "etc.")) {
  plot_data <- data_to_plot %>%
    select("State.Area", "sTotalEmployment", "CostOfLivingIndex")
  
  ggplot(plot_data, aes_string(x = index, y = "sTotalEmployment")) +
    geom_point() +
    geom_smooth(method = "lm", color = "blue") +
    labs(title = paste("Unemployment Rate vs", index),
         x = index,
         y = "Unemployment Rate") +
    theme_minimal()
}

# Job Growth x Cost of Living Scatterplots
jobgrowth_data <- read.csv("data/jobgrowth2023_cleaned.csv")
cost_of_living_data <- read.csv("data/cost_of_living_cleaned.csv")

merged_data <- merge(jobgrowth_data, cost_of_living_data, by = "state")

data_to_plot <- merged_data %>%
  select("State.Area", "pop2023", "CostOfLivingIndex")

for(index in c("CostOfLivingIndex", "CostOfLivingIndex", "etc.")) {
  plot_data <- data_to_plot %>%
    select("State.Area", "pop2023", "CostOfLivingIndex")
  
  ggplot(plot_data, aes_string(x = index, y = "pop2023")) +
    geom_point() +
    geom_smooth(method = "lm", color = "blue") +
    labs(title = paste("Job Growth Rate vs", index),
         x = index,
         y = "Job Growth Rate") +
    theme_minimal()
}

