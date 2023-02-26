# Chapter - 3 Data Exploration and Visualization
install.packages(c("GGally", "ggmap", "mosaic", "treemap"))
setwd("~/Prakash Documents/BUAN 6356/Spring 2020 Course Material/Excel, CSV and JMP FIles")

#### Figure 3.4

housing.df <- read.csv("BostonHousing.csv")

#### Figure 3.4
## simple heatmap of correlations (without values)
heatmap(cor(housing.df), Rowv = NA, Colv = NA)


## heatmap with values
library(gplots)
heatmap.2(cor(housing.df), Rowv = FALSE, Colv = FALSE, dendrogram = "none", 
          cellnote = round(cor(housing.df),2), 
          notecol = "black", key = FALSE, trace = 'none', margins = c(10,10))

# alternative plot with ggplot

library(ggplot2)
library(reshape) # to generate input for the plot
cor.mat <- round(cor(housing.df),2) # rounded correlation matrix
melted.cor.mat <- melt(cor.mat)
ggplot(melted.cor.mat, aes(x = X1, y = X2, fill = value)) + 
  geom_tile() + 
  geom_text(aes(x = X1, y = X2, label = value))

## - Validated 
