# Chapter - 3 Data Exploration and Visualization
install.packages(c("GGally", "ggmap", "mosaic", "treemap"))
setwd("~/Prakash Documents/BUAN 6356/Spring 2020 Course Material/Excel, CSV and JMP FIles")
#### Table 3.2

housing.df <- read.csv("BostonHousing.csv")
head(housing.df, 9)


#### Figure 3.12

library(MASS)
par(mfcol = c(2,1))
parcoord(housing.df[housing.df$CAT..MEDV == 0, -14], main = "CAT.MEDV = 0")
parcoord(housing.df[housing.df$CAT..MEDV == 1, -14], main = "CAT.MEDV = 1")

# ---------- Validated ------------------------------------

