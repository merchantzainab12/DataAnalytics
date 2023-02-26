# Chapter - 3 Data Exploration and Visualization
install.packages(c("GGally", "ggmap", "mosaic", "treemap"))
setwd("~/Prakash Documents/BUAN 6356/Spring 2020 Course Material/Excel, CSV and JMP FIles")


housing.df <- read.csv("BostonHousing.csv")

#### Figure 3.8

options(scipen=999) # avoid scientific notation

## scatter plot: regular and log scale
plot(housing.df$MEDV ~ housing.df$CRIM, xlab = "CRIM", ylab = "MEDV")


# to use logarithmic scale set argument log = to either 'x', 'y', or 'xy'. 
plot(housing.df$MEDV ~ housing.df$CRIM, 
     xlab = "CRIM", ylab = "MEDV", log = 'xy')

# alternative log-scale plot with ggplot
library(ggplot2)
ggplot(housing.df) + geom_point(aes(x = CRIM, y = MEDV)) + 
  scale_x_log10(breaks = 10^(-2:2),
                labels = format(10^(-2:2), scientific = FALSE, drop0trailing = TRUE)) +
  scale_y_log10(breaks = c(5, 10, 20, 40))

## boxplot: regular and log scale
boxplot(housing.df$CRIM ~ housing.df$CAT..MEDV, 
        xlab = "CAT.MEDV", ylab = "CRIM")
boxplot(housing.df$CRIM ~ housing.df$CAT..MEDV, 
        xlab = "CAT.MEDV", ylab = "CRIM", log = 'y')

# ------------------validated ----------------------

