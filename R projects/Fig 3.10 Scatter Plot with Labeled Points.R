# Chapter - 3 Data Exploration and Visualization
install.packages(c("GGally", "ggmap", "mosaic", "treemap"))
setwd("~/Prakash Documents/BUAN 6356/Spring 2020 Course Material/Excel, CSV and JMP FIles")

#### Figure 3.10

utilities.df <- read.csv("Utilities.csv")

plot(utilities.df$Fuel_Cost ~ utilities.df$Sales, 
     xlab = "Sales", ylab = "Fuel Cost", xlim = c(2000, 20000))
text(x = utilities.df$Sales, y = utilities.df$Fuel_Cost, 
     labels = utilities.df$Company, pos = 4, cex = 0.8, srt = 20, offset = 0.2)
# alternative with ggplot
library(ggplot2)
ggplot(utilities.df, aes(y = Fuel_Cost, x = Sales)) + geom_point() +
  geom_text(aes(label = paste(" ", Company)), size = 4, hjust = 0.0, angle = 15) +
  ylim(0.25, 2.25) + xlim(3000, 18000)

# ------------VALIDATED ----------------------------
