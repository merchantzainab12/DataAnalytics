# Chapter - 3 Data Exploration and Visualization
install.packages(c("GGally", "ggmap", "mosaic", "treemap"))
setwd("~/Prakash Documents/BUAN 6356/Spring 2020 Course Material/Excel, CSV and JMP FIles")
#### Table 3.2

housing.df <- read.csv("BostonHousing.csv")

#### Figure 3.2

## histogram of MEDV
hist(housing.df$MEDV, xlab = "MEDV")

# alternative plot with ggplot
library(ggplot2)
ggplot(housing.df) + geom_histogram(aes(x = MEDV), binwidth = 5)

## boxplot of MEDV for different values of CHAS
boxplot(housing.df$MEDV ~ housing.df$CHAS, xlab = "CHAS", ylab = "MEDV")

# alternative plot with ggplot
ggplot(housing.df) + geom_boxplot(aes(x = as.factor(CHAS), y = MEDV)) + xlab("CHAS")

## ------------- Validated Above ------------------------------