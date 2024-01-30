### EDA and Visualizarion     ###
### COM SCI X 450.2 - 393986  ###
### Individual Project        ###
#################################
## Name: Chi-Lin Hung          ##
## SID: X2072273               ##
#################################


# Data Source: 
# 1. Cost of Living Index by Country 2023 Mid-Year
#   https://www.numbeo.com/cost-of-living/rankings_by_country.jsp
# 2. country region match
#   https://github.com/lukes/ISO-3166-Countries-with-Regional-Codes/blob/master/all/all.csv

library(readxl)
library(tidyverse)

####### load data #######
living_index2023f_ <- read_excel("data/Cost_of_Living_Index_Country.xlsx", sheet = 1)



#############################
####### Preprocessing #######
#############################

### rename 
newNames <- c("country","costLivingInd2023" , "rentInd2023", "costRentInd2023", "groceryInd2023", "restaurantInd2023", "locPurchaseInd2023")
(names(living_index2023f_) <- newNames)

### add region columns: left join data
country_region <- subset(countries, select = c("name1","region","sub.region", "intermediate.region", "ss.region"))
living_index2023f <- living_index2023f_ %>% 
  left_join(country_region, by = c("country" = "name1"))

### check NA
living_index2023f$region %>%
  is.na() %>%
  sum()

### tackle NA
living_index2023f[19,8] <- "Asia"
living_index2023f[19,9] <- "Eastern Asia"
living_index2023f[28,8] <- "Asia"
living_index2023f[28,9] <- "Eastern Asia"
living_index2023f[70,8] <- "Africa"
living_index2023f[70,9] <- "Western Africa"
living_index2023f %>% drop_na() -> living_index2023f
living_index2023f %>% head()

### check NA
living_index2023f$region %>%
  is.na() %>%
  sum()



#############################
#######   Plotting    #######
#############################

############ boxplot ##########################################

## check Country Names
living_index2023f %>%
  distinct(region)
## set factor Country Names
(living_index2023f$sub.region_f <- factor(living_index2023f$ss.region, 
                                          levels = c("Australia and New Zealand", "Central Asia", "Eastern Asia", "South-eastern Asia",
                                                     "Southern Asia", "Western Asia", "Northern Africa", "Middle Africa","Eastern Africa", "Southern Africa", "Western Africa", #"Sub-Saharan Africa", 
                                                     "Eastern Europe", "Northern Europe", "Southern Europe", 
                                                     "Western Europe", "Latin America and the Caribbean","Northern America", "Melanesia")) )
## provide color for better visial
mycolor <- c(rep("pink", 1), rep("orange", 5), rep("yellow", 5), rep("lightgreen",4), rep("skyblue", 2), rep("purple2", 1))


## Cost Living and Rent plot
jpeg("box_clri.jpg",width = 600, height = 600)
box_cri <- boxplot(living_index2023f$costRentInd2023~living_index2023f$sub.region_f, xaxt = "n",
                   xlab = "Region", ylab = "Cost Living and Rent Index", col = mycolor,
                   main = "Cost Living and Rent Index across Regions 2023 Mid-Year")
(living_index2023f$costRentInd2023 %>% mean -> cri_mean)
abline(h = cri_mean, lty = 5, col = "brown", lwd = 2)
tick <- seq_along(box_cri$names)
text(tick, par("usr")[1] - 0.5, adj = 0.5, box_cri$names, srt = 55, xpd = TRUE, cex = 0.6)
text(0, 35.4, label="average", adj =0, cex = 1, col = "brown", lwd = 8)
text(3, 65, label="Hong Kong", cex = 0.5)
text(4, 88, label="Singapore", cex = 0.5)
text(4, 38.7, label="Brunei", cex = 0.5)
text(5, 38, label="Maldives", cex = 0.5)
text(15, 83.5, label="Switzarland", cex = 0.5)
text(16, 95, label="Cayman\nIslands", cex = 0.5)
text(16, 67, label="Bahamas", cex = 0.5)
text(16, 52, label="Barbado", cex = 0.5)
dev.off()

## Local Purchase plot
jpeg("box_lpi.jpg",width = 600, height = 600)
box_cri <- boxplot(living_index2023f$locPurchaseInd2023~living_index2023f$sub.region_f, xaxt = "n",
                   xlab = "Region", ylab = "Local Purchase Power Index", col = mycolor,
                   main = "Local Purchase Power Index across Regions 2023 Mid-Year")
(living_index2023f$locPurchaseInd2023 %>% mean -> cri_mean)
abline(h = cri_mean, lty = 5, col = "brown", lwd = 2)
tick <- seq_along(box_cri$names)
text(tick, par("usr")[1] - 0.5, adj = 1.1, box_cri$names, srt = 55, xpd = TRUE, cex = 0.6)
text(0, cri_mean - 2, label="average", adj =0, cex = 1, col = "brown", lwd = 8)
text(3, 91, label="Japan", cex = 0.5)
text(3, 18, label="Mongolia", cex = 0.5)
text(5, 59, label="Pakistan", cex = 0.5)
text(13, 153, label="Isle of Man", cex = 0.5)
text(16, 67, label="Cayman\nIslands", cex = 0.5)
text(16.5, 53, label="Peurto Rico", cex = 0.5)
text(16.5, 12, label="Chile", cex = 0.5)
text(16.5, 2.5, label="Cuba", cex = 0.5)

dev.off()




############ dot chart ##########################################

############################ EU 
### subset EU countries
living_index2023f %>%
  filter(str_detect(region, "Europe")) -> eu_2023
### set group and color
colors <- c("orange", "deepskyblue3","chartreuse4", "maroon")
grp <- as.factor(eu_2023$sub.region)

### ploting
jpeg("dot_eu_cli_.jpg", width = 800, height = 650)
dotchart((eu_2023$costLivingInd2023), labels = eu_2023$country,
         color = colors[grp], main = "Cost of Living Index in Europe 2023 Mid-Year", xlab = "Cost of Living Index", group = grp, gcolor = colors)
abline(v = mean(eu_2023$costLivingInd2023), col = "aquamarine3", lty = 2, lwd = 2.5)
text(mean(eu_2023$costLivingInd2023)+2, 47, label="avg", adj =0, cex = 1.1, col = "aquamarine3", srt = 90)
dev.off()

jpeg("dot_eu_ri_.jpg", width = 800, height = 650)
dotchart((eu_2023$rentInd2023), labels = eu_2023$country,
         color = colors[grp], main = "Rent Index in Europe 2023 Mid-Year", xlab = "Rent Index", group = grp, gcolor = colors)
abline(v = mean(eu_2023$rentInd2023), col = "aquamarine3", lty = 2, lwd = 2.5)
text(mean(eu_2023$rentInd2023)+1, 47, label="avg", adj =0, cex = 1.1, col = "aquamarine3", srt = 90)
dev.off()

jpeg("dot_eu_clri_.jpg", width = 800, height = 650)
dotchart((eu_2023$costRentInd2023), labels = eu_2023$country,
         color = colors[grp], main = "Cost Living and Rent Index in Europe 2023 Mid-Year", xlab = "Cost Living and Rent Index", group = grp, gcolor = colors)
abline(v = mean(eu_2023$costRentInd2023), col = "aquamarine3", lty = 2, lwd = 2.5)
text(mean(eu_2023$costRentInd2023)+1.5, 47, label="avg", adj =0, cex = 1.1, col = "aquamarine3", srt = 90)
dev.off()

jpeg("dot_eu_gi_.jpg", width = 800, height = 650)
dotchart((eu_2023$groceryInd2023), labels = eu_2023$country,
         color = colors[grp], main = "Grocery Index in Europe 2023 Mid-Year", xlab = "Grocery Index", group = grp, gcolor = colors)
abline(v = mean(eu_2023$groceryInd2023), col = "aquamarine3", lty = 2, lwd = 2.5)
text(mean(eu_2023$groceryInd2023)+1.5, 47, label="avg", adj =0, cex = 1.1, col = "aquamarine3", srt = 90)
dev.off()

jpeg("dot_eu_resti_.jpg", width = 800, height = 650)
dotchart((eu_2023$restaurantInd2023), labels = eu_2023$country,
         color = colors[grp], main = "Restaurant Index in Europe 2023 Mid-Year", xlab = "Restaurant Index", group = grp, gcolor = colors)
abline(v = mean(eu_2023$restaurantInd2023), col = "aquamarine3", lty = 2, lwd = 2.5)
text(mean(eu_2023$restaurantInd2023)+1.5, 47, label="avg", adj =0, cex = 1.1, col = "aquamarine3", srt = 90)
dev.off()

jpeg("dot_eu_lpi_.jpg", width = 800, height = 650)
dotchart((eu_2023$locPurchaseInd2023), labels = eu_2023$country,
         color = colors[grp], main = "Local Purchase Index in Europe 2023 Mid-Year", xlab = "Local Purchase Index", group = grp, gcolor = colors)
abline(v = mean(eu_2023$locPurchaseInd2023), col = "aquamarine3", lty = 2, lwd = 2.5)
text(mean(eu_2023$locPurchaseInd2023)+1.5, 47, label="avg", adj =0, cex = 1.1, col = "aquamarine3", srt = 90)
dev.off()

################################ Asia 
## subset Asian countries
living_index2023f %>%
  filter(str_detect(region, "Asia")) -> asia_2023

## set groups and colors
asia_2023 %>%
  distinct(sub.region)
colors <- c("orange", "deepskyblue3","chartreuse4", "brown2", "darkorchid2")
grp <- as.factor(asia_2023$sub.region)

## plotting
mean(asia_2023$costLivingInd2023)
jpeg("dot_asia_cli_.jpg", width = 800, height = 650)
dotchart((asia_2023$costLivingInd2023), labels = asia_2023$country,
         color = colors[grp], main = "Cost of Living Index in Asia 2023 Mid-Year", xlab = "Cost of Living Index", group = grp, gcolor = colors)
abline(v = mean(asia_2023$costLivingInd2023), col = "antiquewhite3", lty = 2, lwd = 2.5)
text(mean(asia_2023$costLivingInd2023)+ 1.8, 50.5, label="avg", adj =0, cex = 1.1, col = "antiquewhite3", srt = 90)
dev.off()

jpeg("dot_asia_ri_.jpg", width = 800, height = 650)
dotchart((asia_2023$rentInd2023), labels = asia_2023$country,
         color = colors[grp], main = "Rent Index in Asia 2023 Mid-Year", xlab = "Rent Index", group = grp, gcolor = colors)
abline(v = mean(asia_2023$rentInd2023), col = "antiquewhite3", lty = 2, lwd = 2.5)
text(mean(asia_2023$rentInd2023)+2, 50.5, label="avg", adj =0, cex = 1.1, col = "antiquewhite3", srt = 90)
dev.off()

jpeg("dot_asia_clri_.jpg", width = 800, height = 650)
dotchart((asia_2023$costRentInd2023), labels = asia_2023$country,
         color = colors[grp], main = "Cost Living and Rent Index in Asia 2023 Mid-Year", xlab = "Cost Living and Rent Index", group = grp, gcolor = colors)
abline(v = mean(asia_2023$costRentInd2023), col = "antiquewhite3", lty = 2, lwd = 2.5)
text(mean(asia_2023$costRentInd2023)+2, 50.5, label="avg", adj =0, cex = 1.1, col = "antiquewhite3", srt = 90)
dev.off()

jpeg("dot_asia_gi_.jpg", width = 800, height = 650)
dotchart((asia_2023$groceryInd2023), labels = asia_2023$country,
         color = colors[grp], main = "Grocery Index in Asia 2023 Mid-Year", xlab = "Grocery Index", group = grp, gcolor = colors)
abline(v = mean(asia_2023$restaurantInd2023), col = "antiquewhite3", lty = 2, lwd = 2.5)
text(mean(asia_2023$restaurantInd2023)+2, 50.5, label="avg", adj =0, cex = 1.1, col = "antiquewhite3", srt = 90)
dev.off()

jpeg("dot_asia_resti_.jpg", width = 800, height = 650)
dotchart((asia_2023$restaurantInd2023), labels = asia_2023$country,
         color = colors[grp], main = "Restaurant Index in Asia 2023 Mid-Year", xlab = "Restaurant Index", group = grp, gcolor = colors)
abline(v = mean(asia_2023$restaurantInd2023), col = "antiquewhite3", lty = 2, lwd = 2.5)
text(mean(asia_2023$restaurantInd2023)+2, 50.5, label="avg", adj =0, cex = 1.1, col = "antiquewhite3", srt = 90)
dev.off()

jpeg("dot_asia_lpi_.jpg", width = 800, height = 650)
dotchart((asia_2023$locPurchaseInd2023), labels = asia_2023$country,
         color = colors[grp], main = "Local Purchase Index in Asia 2023 Mid-Year", xlab = "Local Purchase Index", group = grp, gcolor = colors)
abline(v = mean(asia_2023$locPurchaseInd2023), col = "antiquewhite3", lty = 2, lwd = 2.5)
text(mean(asia_2023$locPurchaseInd2023)+2, 50.5, label="avg", adj =0, cex = 1.1, col = "antiquewhite3", srt = 90)
dev.off()

################################## America 
## subset American countries
living_index2023f %>%
  filter(str_detect(region, "Americas")) -> america_2023

## set groups and colors
america_2023 %>%
  distinct(intermediate.region)
colors <- c("orange", "deepskyblue3","chartreuse4",  "darkorchid2")
grp <- as.factor(america_2023$intermediate.region)

## plotting
mean(america_2023$costLivingInd2023)
jpeg("dot_america_cli_.jpg", width = 800, height = 650)
dotchart((america_2023$costLivingInd2023), labels = america_2023$country,
         color = colors[grp], main = "Cost of Living Index in America 2023 Mid-Year", xlab = "Cost of Living Index", group = grp, gcolor = colors)
abline(v = mean(america_2023$costLivingInd2023), col = "maroon", lty = 2, lwd = 2.5)
text(mean(america_2023$costLivingInd2023)+ 1.8, 35.2, label="avg", adj =0, cex = 1.1, col = "maroon", srt = 90)
dev.off()

jpeg("dot_america_ri_.jpg", width = 800, height = 650)
dotchart((america_2023$rentInd2023), labels = america_2023$country,
         color = colors[grp], main = "Rent Index in America 2023 Mid-Year", xlab = "Rent Index", group = grp, gcolor = colors)
abline(v = mean(america_2023$rentInd2023), col = "maroon", lty = 2, lwd = 2.5)
text(mean(america_2023$rentInd2023)+2, 35.2, label="avg", adj =0, cex = 1.1, col = "maroon", srt = 90)
dev.off()

jpeg("dot_america_clri_.jpg", width = 800, height = 650)
dotchart((america_2023$costRentInd2023), labels = america_2023$country,
         color = colors[grp], main = "Cost Living and Rent Index in America 2023 Mid-Year", xlab = "Cost Living and Rent Index", group = grp, gcolor = colors)
abline(v = mean(america_2023$costRentInd2023), col = "maroon", lty = 2, lwd = 2.5)
text(mean(america_2023$costRentInd2023)+2, 35.2, label="avg", adj =0, cex = 1.1, col = "maroon", srt = 90)
dev.off()

jpeg("dot_america_gi_.jpg", width = 800, height = 650)
dotchart((america_2023$groceryInd2023), labels = america_2023$country,
         color = colors[grp], main = "Grocery Index in America 2023 Mid-Year", xlab = "Grocery Index", group = grp, gcolor = colors)
abline(v = mean(america_2023$restaurantInd2023), col = "maroon", lty = 2, lwd = 2.5)
text(mean(america_2023$restaurantInd2023)+2, 35.2, label="avg", adj =0, cex = 1.1, col = "maroon", srt = 90)
dev.off()

jpeg("dot_america_resti_.jpg", width = 800, height = 650)
dotchart((america_2023$restaurantInd2023), labels = america_2023$country,
         color = colors[grp], main = "Restaurant Index in America 2023 Mid-Year", xlab = "Restaurant Index", group = grp, gcolor = colors)
abline(v = mean(america_2023$restaurantInd2023), col = "maroon", lty = 2, lwd = 2.5)
text(mean(america_2023$restaurantInd2023)+2, 35.2, label="avg", adj =0, cex = 1.1, col = "maroon", srt = 90)
dev.off()

jpeg("dot_america_lpi_.jpg", width = 800, height = 650)
dotchart((america_2023$locPurchaseInd2023), labels = america_2023$country,
         color = colors[grp], main = "Local Purchase Index in America 2023 Mid-Year", xlab = "Local Purchase Index", group = grp, gcolor = colors)
abline(v = mean(america_2023$locPurchaseInd2023), col = "maroon", lty = 2, lwd = 2.5)
text(mean(america_2023$locPurchaseInd2023)+2, 35.2, label="avg", adj =0, cex = 1.1, col = "maroon", srt = 90)
dev.off()



############ bar plot ##########################################
str(living_index2023f)

## Local Purchase top 10
jpeg("ggbar_lpi.jpg", width = 600, height = 600)
ord_lpi <- living_index2023f[order(living_index2023f$locPurchaseInd2023, decreasing = T), ]
ord_lpi %>%
  select(country, locPurchaseInd2023, ss.region) %>%
  head(10) -> ord_lpi
ord_lpi

ggplot(ord_lpi, aes( x = reorder(country, -locPurchaseInd2023), y = locPurchaseInd2023, fill = ss.region)) + 
  geom_col() + theme_classic() + theme(axis.text.x = element_text(angle = 90)) + labs(x = 'Country', y = 'Local Purchase Power Index', title = "Top 10 Country by Local Purchase Power Index") +
  scale_fill_discrete(name = "Sub-region")
dev.off()

## Cost of Living and rent top 10
jpeg("ggbar_clri.jpg", width = 600, height = 600)
ord_lpi <- living_index2023f[order(living_index2023f$costRentInd2023, decreasing = T), ]
ord_lpi %>%
  select(country, costRentInd2023, ss.region) %>%
  head(10) -> ord_lpi
ord_lpi

ggplot(ord_lpi, aes( x = reorder(country, -costRentInd2023), y = costRentInd2023, fill = ss.region)) + 
  geom_col() + theme_classic() + theme(axis.text.x = element_text(angle = 90)) + labs(x = 'Country', y = 'Cost of Living Index', title = "Top 10 Country by Cost of Living and Rent Index") +
  scale_fill_discrete(name = "Sub-region")
dev.off()

## sum of Cost of Living Index
living_index2023f %>%
  group_by(sub.region_f) %>%
  summarize(sum_cli = sum(costLivingInd2023),
            avg_cli = mean(costLivingInd2023)) %>% data.frame -> summary_cli
barplot(summary_cli$sum_cli, names.arg = summary_cli$sub.region, col = mycolor,las = 2, main = "Sum of CLI")



############ histogram ##########################################
jpeg("CLI_histline_rgn.jpg", width = 500, height = 500)
hist(living_index2023f$costLivingInd2023[living_index2023f$region == "Europe"], col = rgb(0,1,0, 0.2), 
     freq = F, breaks = seq(0, 150, 10), ylim = c(0,0.06), xlab = "Cost of Living Index", 
     main = "Cost of Living Index by Regions")
hist(living_index2023f$costLivingInd2023[living_index2023f$region == "Asia"], add = T, col = rgb(1,0,0, 0.4), freq = F, breaks = seq(0, 150, 10))
hist(living_index2023f$costLivingInd2023[living_index2023f$region == "Americas"], add = T, col = rgb(1,0.5,0, 0.2), freq = F, breaks = seq(0, 150, 10))
hist(living_index2023f$costLivingInd2023[living_index2023f$region == "Africa"], add = T, col = rgb(0,0,1, 0.2), freq = F, breaks = seq(0, 150, 10))

lines(density(living_index2023f$costLivingInd2023[living_index2023f$region == "Africa"], bw = 3.5), col = rgb(0,0,1, 0.3), lwd = 2)
lines(density(living_index2023f$costLivingInd2023[living_index2023f$region == "Asia"], bw = 3.5), col = rgb(1,0,0, 0.6), lwd = 2)
lines(density(living_index2023f$costLivingInd2023[living_index2023f$region == "Europe"], bw = 3.5), col = rgb(0,1,0, 0.5), lwd = 2)
lines(density(living_index2023f$costLivingInd2023[living_index2023f$region == "Americas"], bw = 0.5), col = rgb(1, 0.5,0, 0.7), lwd = 0.5, type = "i")
legend("topright",title = "Local Purchase Index", c( "Europe", "Asia", "Americas", "Africa"), col=c(rgb(0,1,0, 0.2), rgb(1,0,0,0.4), rgb(1,0.5,0,0.3), rgb(0,0,1,0.2)), lwd=10)
dev.off()	



########### Scatter Plot ##########################################
jpeg("livingIndex2023_scatter.jpg", width = 600, height = 620)
plot(living_index2023f[2:7], main = "2023 Indexes")
dev.off()

########## Regression ##############
jpeg("scatter_rentLiving.jpg", width = 500, height = 500)
(model_cr <- lm(rentInd2023 ~ costLivingInd2023, data = living_index2023f))
summary(model_cr)
plot(living_index2023f$costLivingInd2023[living_index2023f$region == "Americas"], 
     living_index2023f$rentInd2023[living_index2023f$region == "Americas"], 
     xlim = c(0, 145), ylim = c(0, 100),  pch = 17, col = "maroon", 
     main = "Living Index Scatter Plot", xlab = "Rent Index", ylab = "Cost and Living Index")
points(living_index2023f$costLivingInd2023[living_index2023f$region == "Asia"], 
       living_index2023f$rentInd2023[living_index2023f$region == "Asia"], 
       pch = 20, col = "royalblue")
points(living_index2023f$costLivingInd2023[living_index2023f$region == "Europe"], 
       living_index2023f$rentInd2023[living_index2023f$region == "Europe"], 
       pch = 21, col = "lightseagreen")
points(living_index2023f$costLivingInd2023[living_index2023f$region == "Oceania"], 
       living_index2023f$rentInd2023[living_index2023f$region == "Oceania"], 
       pch = 19, col = "mediumpurple")
points(living_index2023f$costLivingInd2023[living_index2023f$region == "Africa"], 
       living_index2023f$rentInd2023[living_index2023f$region == "Africa"], 
       pch = 18, col = "orange")
abline(model_cr, col = "lightcyan4", lty = 1, lwd = 2)
text(14,29, col = "lightcyan4", labels = "Y = -13.123 + 0.659X", adj = 0)
legend("bottomright", c("America", "Asia", "Europe", "Oceania", "Africa"),
       col = c("maroon", "royalblue", "lightseagreen","mediumpurple","orange"),
       pch = c(17, 20, 21, 19, 18))
dev.off()


jpeg("scatter_lpLiving.jpg", width = 500, height = 500)
(model_cp <- lm(locPurchaseInd2023 ~ costLivingInd2023, data = living_index2023f))
summary(model_cp)
plot(living_index2023f$costLivingInd2023[living_index2023f$region == "Americas"], 
     living_index2023f$locPurchaseInd2023[living_index2023f$region == "Americas"],
     xlim = c(15, 145), ylim = c(0, 155),  pch = 17, col = "maroon", 
     main = "Living Index Scatter Plot", xlab = "Local Purchase Power Index", ylab = "Cost and Living Index")
points(living_index2023f$costLivingInd2023[living_index2023f$region == "Asia"], 
       living_index2023f$locPurchaseInd2023[living_index2023f$region == "Asia"], 
       pch = 20, col = "royalblue")
points(living_index2023f$costLivingInd2023[living_index2023f$region == "Europe"], 
       living_index2023f$locPurchaseInd2023[living_index2023f$region == "Europe"], 
       pch = 21, col = "lightseagreen")
points(living_index2023f$costLivingInd2023[living_index2023f$region == "Oceania"], 
       living_index2023f$locPurchaseInd2023[living_index2023f$region == "Oceania"], 
       pch = 19, col = "mediumpurple")
points(living_index2023f$costLivingInd2023[living_index2023f$region == "Africa"], 
       living_index2023f$locPurchaseInd2023[living_index2023f$region == "Africa"], 
       pch = 18, col = "orange")
abline(model_cp, col = "lavenderblush4", lty = 1, lwd = 2)
text(85, 125, col = "lavenderblush4", labels = "Y = 3.529 + 0.956X", adj = 0, cex = 1.1)
legend("bottomright", c("America", "Asia", "Europe", "Oceania", "Africa"),
       col = c("maroon", "royalblue", "lightseagreen","mediumpurple","orange"),
       pch = c(17, 20, 21, 19, 18))
dev.off()
