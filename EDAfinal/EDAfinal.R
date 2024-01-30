### Final Individual Project  ###
#################################
## Name: Chi-Lin Hung          ##
## SID: X2072273               ##
#################################


library(cluster)
library(ggplot2)
library(readxl)
library(corrplot)
library(GGally)
library(dplyr)
library(scatterplot3d)
library(tidyverse)

######################### load data #########################
housing <- read.csv("data/housing.csv")
school <- read.csv("data/schools.csv")

housing %>% summary()
school %>% summary()


# change character columns to factors for seeing the count of char columns
housingf <- housing
housingf[sapply(housing, is.character)] <- lapply(housing[sapply(housing, is.character)], as.factor)
housingf %>% summary()

# check numbers 
housingf %>%
  group_by(baths) %>%
  count

housingf %>%
  group_by(year) %>%
  count


######################### Preprocessing 1 #########################
# listwise deletion
housing <- housing[!(housing$beds == 999|housing$baths == 25| housing$year == 2111| housing$year == 1495),]
housing$type[housing$type == "condominium"] <- "condo"
housing$type[housing$type == "townhouse"] <- "town house"
housing %>% na.omit() -> housing

# change character columns to factors for seeing the count of char columns
housingf <- housing
housingf[sapply(housing, is.character)] <- lapply(housing[sapply(housing, is.character)], as.factor)
housingf %>% summary()


# merge data
(housing_school <- housingf %>%
    left_join(school, by = c("elementary" = "school")))
(housing_school <- housing_school %>%
    rename("size_ele" = "size", 
           "rating_ele" = "rating"))

(housing_school <- housing_school %>%
    left_join(school, by = c("middle" = "school")))
(housing_school <- housing_school %>%
    rename("size_mid" = "size", 
           "rating_mid" = "rating"))

(housing_school <- housing_school %>%
    left_join(school, by = c("high" = "school")))
(housing_school <- housing_school %>%
    rename("size_high" = "size", 
           "rating_high" = "rating"))



######################### One-variable visuals #########################

par(mfrow = c(1,2))
boxplot(housingf$baths, main = "Bathroom Number", col = "maroon") 
boxplot(housingf$beds, main = "Bedroom Number", col = "yellow2") 

par(mfrow = c(1, 1))
hist(housingf$lotsize, main = "Unit's Lot Size", xlab = "Lotsize", ylab = "Count", col = "palegreen")
hist(housingf$sqft, main = "Unit's Square Footage", xlab = "Square Footage", ylab = "Count", col = "pink")
hist(housingf2$year, main = "Built Year of Units", xlab = "Built Year", ylab = "Count", col = "skyblue2")

housingf %>%
  count(neighborhood) -> housing_nbcount

mcolor <- c("blue", "gold", "green", "orange", "purple", "red","grey88", "yellow")
barplot(housing_nbcount$n, names.arg = housing_nbcount$neighborhood, col = mcolor, main = "Neighborhood")



######################### Two-variable visuals #########################

### a. Neighborhood features

# 1. Neighborhood and price [boxplot]
price_mean <- mean(housing_school$soldprice)/1000
housing_school$soldprice_th <- housing_school$soldprice / 1000
plot(housing_school$neighborhood, housing_school$soldprice_th, col = mcolor, 
     xlab = "Neighborhood", main = "Neighborhood Sold Price in Thousands")
abline(h = price_mean, col = "lightcoral", lty = 3, lwd = 2)
text(0.2,price_mean-2, label = "Average", adj=0, cex = 1, col = "lightcoral", lwd = 1)

# 2. Neighborhood and Years [high density plot]
ggplot(housing_school, aes(x = neighborhood, y = year, color = neighborhood)) + geom_jitter() + 
  scale_color_manual(values = c("Blue" = "blue", "Gold" = "gold", "Green" = "green", "Orange" = "orange", "Purple" = "purple", "Red" = "red","Silver" ="grey47","Yellow"="yellow2")) +
  labs(title = "Units built Years across Neighborhood") +
  theme_bw() + theme(legend.position = "none")


# 3. Neighborhood & levels [bar plot]
#filter na & prepare for plot
housing_school %>%
  filter(levels != "?") %>%
  group_by(neighborhood) %>%
  count(levels) -> hs_levelsCount
#plot
ggplot(hs_levelsCount, aes(x = neighborhood, y = n, fill = levels)) + geom_col(position = "dodge") +
  labs(title = "Level Count across Neighborhood") +theme_classic()

# 4. Neighborhood & cooling [barplot]
housing_school %>%
  filter(cooling != "") %>%
  group_by(neighborhood) %>%
  count(cooling) -> hs_cooling
ggplot(hs_cooling, aes(x = neighborhood, y = n, fill = cooling)) + geom_col(position = "dodge") +
  labs(title = "Cooling Count across Neighborhood") +theme_classic()

# 5. Neighborhood & heating [barplot]
housing_school %>%
  filter(heating != "") %>%
  group_by(neighborhood) %>%
  count(heating) -> hs_heating
ggplot(hs_heating, aes(x = neighborhood, y = n, fill = heating)) + geom_col(position = "dodge") +
  labs(title = "Central Heating Count across Neighborhood") +theme_classic()

# 6. Neighborhood & fireplace [barplot]
housing_school %>%
  filter(fireplace != "") %>%
  group_by(neighborhood) %>%
  count(fireplace) -> hs_fireplace
ggplot(hs_fireplace, aes(x = neighborhood, y = n, fill = fireplace)) + geom_col(position = "dodge") +
  labs(title = "Fireplace Count across Neighborhood") +theme_classic()

# 7. Neighborhood & type & price [heatmap]
housing_school %>%
  group_by(neighborhood,type) %>%
  summarise(price_avg = mean(soldprice),
            price_sd = sd(soldprice)) -> hs_priceMS
hs_priceMS
ggplot(hs_priceMS, aes(x = type, y = neighborhood, fill = price_avg)) + geom_raster() + 
  scale_fill_gradient(low = "pink", high = "pink4") + 
  labs(title = "Units type and price") +
  geom_text(label = hs_neighborhoodType$n, color = "white")+theme_classic()


# 8-16. School rating & sold price [high density plot, special plot]
par(mfrow = c(1, 3))
plot(housing_school$rating_ele, housing_school$soldprice, col = mcolor, main = "Elementary School Rating and Sold Price", xlab = "Rating of Elementary School", ylab = "Unit Sold Price")
plot(housing_school$rating_mid, housing_school$soldprice, col = mcolor, main = "Middle School Rating and Sold Price", xlab = "Rating of Middle School", ylab = "Unit Sold Price")
plot(housing_school$rating_high, housing_school$soldprice, col = mcolor, main = "High School Rating and Sold Price", xlab = "Rating of High School", ylab = "Unit Sold Price")


sp(housing_school$rating_ele, housing_school$soldprice, jitter = list(x=2, y=2), smoother = T, spread = T, regLine = T, xlab = "Rating of Elementary School", ylab = "Unit Sold Price",main = "Elementary School Rating and Sold Price")
sp(housing_school$rating_mid, housing_school$soldprice, jitter = list(x=3, y=2), smoother = T, spread = T, regLine = T, main = "Middle School Rating and Sold Price", xlab = "Rating of Middle School", ylab = "Unit Sold Price")
sp(housing_school$rating_high, housing_school$soldprice, jitter = list(x=3, y=2), smoother = T, spread = T, regLine = T, main = "High School Rating and Sold Price", xlab = "Rating of High School", ylab = "Unit Sold Price")

par(mfrow = c(1, 1))
ggplot(housing_school, aes(x = soldprice, y = rating_ele, color = neighborhood)) + geom_jitter() + #geom_point(position = "jitter") +
  scale_color_manual(values = c("Blue" = "blue", "Gold" = "gold", "Green" = "green", "Orange" = "orange", "Purple" = "purple", "Red" = "red","Silver" ="grey47","Yellow"="yellow2")) +
  theme_light() + labs(y = "Elementary School Rating",  title = "Elementary School Rating and Sold Price" ) 

ggplot(housing_school, aes(x = soldprice, y = rating_mid, color = neighborhood)) + geom_jitter() +
  scale_color_manual(values = c("Blue" = "blue", "Gold" = "gold", "Green" = "green", "Orange" = "orange", "Purple" = "purple", "Red" = "red","Silver" ="grey47","Yellow"="yellow2")) +
  theme_light() + labs(y = "Middle School Rating",  title = "Middle School Rating and Sold Price" ) 

ggplot(housing_school, aes(x = soldprice, y = rating_high, color = neighborhood)) + geom_jitter() +
  scale_color_manual(values = c("Blue" = "blue", "Gold" = "gold", "Green" = "green", "Orange" = "orange", "Purple" = "purple", "Red" = "red","Silver" ="grey47","Yellow"="yellow2")) +
  theme_light() + labs(y = "High School Rating",  title = "High School Rating and Sold Price" ) 


# 17. bathroom & price [jitter]
ggplot(housing_school, aes(x = baths, y = soldprice, color = neighborhood)) + geom_jitter() +
  scale_color_manual(values = c("Blue" = "blue", "Gold" = "gold", "Green" = "green", "Orange" = "orange", "Purple" = "purple", "Red" = "red","Silver" ="grey47","Yellow"="yellow2")) +
  theme_light() + labs(x = "Bathroom",title = "Bathroom and Sold Price", y = "Sold Price")

# 18. bedroom & price  [jitter]
ggplot(housing_school, aes(x = beds, y = soldprice, color = neighborhood)) + geom_jitter() +
  scale_color_manual(values = c("Blue" = "blue", "Gold" = "gold", "Green" = "green", "Orange" = "orange", "Purple" = "purple", "Red" = "red","Silver" ="grey47","Yellow"="yellow2")) +
  theme_light() + labs(title = "Bedroom and Sold Price", x = "Bedroom", y = "Sold Price")


# 19-21. Price & cooling, heating, fireplace
clr <- c("orchid",  "pink2", "aquamarine3")
par(mfrow = c(3,1))
housing_school %>%
  filter(cooling != "" & heating != "" & fireplace != "") -> hs_chfFilter
dotchart(hs_chfFilter$soldprice, labels = hs_chfFilter$cooling,
         groups = hs_chfFilter$cooling, gcolor = clr,color = clr[hs_chfFilter$cooling], main = "Price and Cooling")
dotchart(hs_chfFilter$soldprice, labels = hs_chfFilter$heating,
         groups = hs_chfFilter$heating, gcolor = clr,color = clr[hs_chfFilter$heating], main = "Price and Heating")
dotchart(hs_chfFilter$soldprice, labels = hs_chfFilter$fireplace,
         groups = hs_chfFilter$fireplace, gcolor = clr,color = clr[hs_chfFilter$fireplace], main = "Price and Fireplace")

par(mfrow = c(1,1))

######################### Analysis #########################
### scatter matrix
housing_school %>%
  select(-8:-14)  %>%
  select(-soldprice_th)-> hs_0

ggscatmat(hs_0)


### regression
housing_school %>% head()
housing_school %>%
  select(neighborhood, soldprice, beds, baths, sqft, lotsize, year, type, rating_ele, rating_mid, rating_high) -> hs_m1
hs_m1 %>%
  na.omit() -> hs_m1
m1 <- lm(soldprice~neighborhood+beds+baths+sqft+lotsize+year+type+rating_ele+rating_mid+rating_high, data=hs_m1)
summary(m1)
step(m1, direction = "backward")


m2 <- lm(soldprice~beds+sqft+year+type+rating_ele+rating_mid+rating_high, data=hs_m1) # no neighborhood & other non-significant features
summary(m2)
step(m2, direction = "backward")

m3 <- lm(soldprice~beds+sqft+year+rating_ele+rating_mid+rating_high, data=hs_m1) # no neighborhood & type & other non-significant features
summary(m3)


### Interaction
m5 <- lm(soldprice~type*rating_high, data=hs_m1)
summary(m5)


### plotting
t_col <- function(color, percent = 50, name = NULL) {
  #      color = color name
  #    percent = % transparency
  #       name = an optional name for the color
  
  ## Get RGB values for named color
  rgb.val <- col2rgb(color)
  ## Make new color using input color as base and alpha set by transparency
  t.col <- rgb(rgb.val[1], rgb.val[2], rgb.val[3],
               max = 255,
               alpha = (100 - percent) * 255 / 100,
               names = name)
  ## Save the color
  invisible(t.col)
}

bl1 <- t_col("dodgerblue3", perc = 60, name = "")
gl1 <- t_col("gold", perc = 60, name = "")
grn1 <- t_col("green", perc = 60, name = "")
or1 <- t_col("orange", perc = 60, name = "")
pu1 <- t_col("purple", perc = 60, name = "")
re1 <- t_col("red", perc = 60, name = "")
gr1 <- t_col("grey47", perc = 60, name = "")
yel1 <- t_col("yellow", perc = 60, name = "")

plot(housing_school$soldprice[housing_school$type == "condo"], 
     housing_school$rating_high[housing_school$type == "condo"],
     xlim = c(0, 2500000), ylim = c(0, 10), pch = 17, col = re1, 
     main = "Price & High School Rating by Unit Type", xlab = "Type", ylab = "High School Rating")

points(housing_school$soldprice[housing_school$type == "single-family home"], 
       housing_school$rating_high[housing_school$type == "single-family home"],
       pch = 16, col = grn1)

points(housing_school$soldprice[housing_school$type == "condo"], 
       housing_school$rating_high[housing_school$type == "condo"],
       pch = 21, col = bl1)

points(housing_school$soldprice[housing_school$type == "town house"], 
       housing_school$rating_high[housing_school$type == "town house"],
       pch = 20, col = yel1)


### cluster

# 22. High school rating & sold price
par(mfrow = c(1,1))
(hs_hp <- cbind(matrix(housing_school$rating_high),
                matrix(housing_school$soldprice)))
(colnames(hs_hp) <- c("Rating_High","Price"))
(c1 <- kmeans(hs_hp, 8))
plot(hs_hp, col = c1$cluster, xlab = "High School Rating", ylab = "Price",main = "Cluster of High Shcool Rating and Sold Price")


housing_school %>%
  select(sqft, soldprice) %>%
  na.omit -> hs_sp_
(hs_sp <- cbind(matrix(hs_sp_$sqft),
                matrix(hs_sp_$soldprice)))
(colnames(hs_sp) <- c("SquareFoot","Price"))
(c2 <- kmeans(hs_sp, 8))
plot(hs_sp, col = c2$cluster, xlab = "Square Footage", ylab = "Price",main = "Cluster of Square Footage and Sold Price")


(hs_yp <- cbind(matrix(housing_school$year),
                matrix(housing_school$soldprice)))
(colnames(hs_yp) <- c("Year","Price"))
(c1 <- kmeans(hs_yp, 8))
plot(hs_yp, col = c1$cluster, xlab = "Year", ylab = "Price",main = "Cluster of Year and Sold Price")


(hs_hp <- cbind(matrix(housing_school$rating_high),
                matrix(housing_school$rating_ele)))
(colnames(hs_hp) <- c("Rating_High","Rating_Elementary"))
(c1 <- kmeans(hs_hp, 4))
plot(hs_hp, col = c1$cluster, xlab = "High School Rating", ylab = "Elementary School Rating",main = "Cluster of High Shcool and Elementary School Rating")



####################################### Sensitivity Analysis ########################################

housing <- read.csv("data/housing.csv")
school <- read.csv("data/schools.csv")

housing %>% summary()
school %>% summary()


################## Preprocessing 2 ##################
housing <- housing[!(housing$beds == 999|housing$baths == 25| housing$year == 2111| housing$year == 1495),]
housing$type[housing$type == "condominium"] <- "condo"
housing$type[housing$type == "townhouse"] <- "town house"

#### Categorical -- (Educated) Guess
housing$levels[housing$levels == "?"] <- "1"
housing$cooling[housing$cooling == ""] <- "No"
housing$heating[housing$heating == ""] <- "No"
housing$fireplace[housing$fireplace == ""] <- "No"

#### Numerical -- Single Imputation
housing$sqft[is.na(housing$sqft)] <- 2128
housing$lotsize[is.na(housing$lotsize)] <- 0.2889


# change character columns to factors for seeing the count of char columns
housingf <- housing
housingf[sapply(housing, is.character)] <- lapply(housing[sapply(housing, is.character)], as.factor)
housingf %>% summary()


# merge data
(housing_school <- housingf %>%
    left_join(school, by = c("elementary" = "school")))
(housing_school <- housing_school %>%
    rename("size_ele" = "size", 
           "rating_ele" = "rating"))

(housing_school <- housing_school %>%
    left_join(school, by = c("middle" = "school")))
(housing_school <- housing_school %>%
    rename("size_mid" = "size", 
           "rating_mid" = "rating"))

(housing_school <- housing_school %>%
    left_join(school, by = c("high" = "school")))
(housing_school <- housing_school %>%
    rename("size_high" = "size", 
           "rating_high" = "rating"))



### regression
housing_school %>%
  select(neighborhood, soldprice, beds, baths, sqft, lotsize, year, type, rating_ele, rating_mid, rating_high) -> hs_m1
hs_m1 %>%
  na.omit() -> hs_m1
m1 <- lm(soldprice~neighborhood+beds+baths+sqft+lotsize+year+type+rating_ele+rating_mid+rating_high, data=hs_m1)
summary(m1)
step(m1, direction = "backward")

m2 <- lm(soldprice~beds+sqft+year+type+rating_ele+rating_mid+rating_high, data=hs_m1) # no neighborhood & other non-significant features
summary(m2)
step(m2, direction = "backward")

m3 <- lm(soldprice~beds+sqft+year+rating_ele+rating_mid+rating_high, data=hs_m1) # no neighborhood & type & other non-significant features
summary(m3)

m5 <- lm(soldprice~type*rating_high, data=hs_m1)
summary(m5)


plot(housing_school$soldprice[housing_school$type == "condo"], 
     housing_school$rating_high[housing_school$type == "condo"],
     xlim = c(0, 2500000), ylim = c(0, 10), pch = 17, col = re1, 
     main = "Price & High School Rating by Unit Type", xlab = "Type", ylab = "High School Rating")

points(housing_school$soldprice[housing_school$type == "single-family home"], 
       housing_school$rating_high[housing_school$type == "single-family home"],
       pch = 16, col = grn1)

points(housing_school$soldprice[housing_school$type == "condo"], 
       housing_school$rating_high[housing_school$type == "condo"],
       pch = 21, col = bl1)

points(housing_school$soldprice[housing_school$type == "town house"], 
       housing_school$rating_high[housing_school$type == "town house"],
       pch = 20, col = yel1)
