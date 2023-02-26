# Chapter - 3 Data Exploration and Visualization
install.packages(c("GGally", "ggmap", "mosaic", "treemap"))
setwd("~/Prakash Documents/BUAN 6356/Spring 2020 Course Material/Excel, CSV and JMP FIles")



housing.df <- read.csv("BostonHousing.csv")

#### Figure 3.7

## simple plot
# use plot() to generate a matrix of 4X4 panels with variable name on the diagonal, 
# and scatter plots in the remaining panels.
plot(housing.df[, c(1, 3, 12, 13)])

# alternative, nicer plot (displayed)
library(GGally)
ggpairs(housing.df[, c(1, 3, 12, 13)])

# -----------------------Validated -------------------------------------
