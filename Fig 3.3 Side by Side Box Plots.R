# Chapter - 3 Data Exploration and Visualization
install.packages(c("GGally", "ggmap", "mosaic", "treemap"))
setwd("~/Prakash Documents/BUAN 6356/Spring 2020 Course Material/Excel, CSV and JMP FIles")
#### Figure 3.3

housing.df <- read.csv("BostonHousing.csv")

#### Figure 3.3

## side-by-side boxplots
# use par() to split the plots into panels.
par(mfcol = c(1, 4))
boxplot(housing.df$NOX ~ housing.df$CAT..MEDV, xlab = "CAT.MEDV", ylab = "NOX")

boxplot(housing.df$LSTAT ~ housing.df$CAT..MEDV, xlab = "CAT.MEDV", ylab = "LSTAT")
boxplot(housing.df$PTRATIO ~ housing.df$CAT..MEDV, xlab = "CAT.MEDV", ylab = "PTRATIO")
boxplot(housing.df$INDUS ~ housing.df$CAT..MEDV, xlab = "CAT.MEDV", ylab = "INDUS")

# -------------------------Validated ------------------------------