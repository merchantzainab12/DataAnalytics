# Chapter - 3 Data Exploration and Visualization
install.packages(c("GGally", "ggmap", "mosaic", "treemap"))
setwd("~/Prakash Documents/BUAN 6356/Spring 2020 Course Material/Excel, CSV and JMP FIles")



#### Figure 3.5

# replace dataFrame with your data.
# is.na() returns a Boolean (TRUE/FALSE) output indicating the location of missing
# values. 
# multiplying the Boolean value by 1 converts the output into binary (0/1).
# heatmap(1 * is.na(dataFrame), Rowv = NA, Colv = NA)
# We will use ToyotaCorolla - Missing Dataset that I created

toyotamissing.df <-read.csv("ToyotaCorolla - missing.csv")
heatmap(1 * is.na(toyotamissing.df), Rowv = NA, Colv = NA)

# ------------------validated ----------------------------------
