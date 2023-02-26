# Chapter - 3 Data Exploration and Visualization
install.packages(c("GGally", "ggmap", "mosaic", "treemap"))
setwd("~/Prakash Documents/BUAN 6356/Spring 2020 Course Material/Excel, CSV and JMP FIles")


housing.df <- read.csv("BostonHousing.csv")

#### Figure 3.6

## color plot
par(mfcol = c(1,1), xpd=TRUE) # allow legend to be displayed outside of plot area
plot(housing.df$NOX ~ housing.df$LSTAT, ylab = "NOX", xlab = "LSTAT",
     col = ifelse(housing.df$CAT..MEDV == 1, "black", "gray"))
# add legend outside of plotting area
# In legend() use argument inset =  to control the location of the legend relative # to the plot.
legend("topleft", inset=c(0, -0.2), 
       legend = c("CAT.MEDV = 1", "CAT.MEDV = 0"), col = c("black", "gray"), 
       pch = 1, cex = 0.5)

# alternative plot with ggplot
library(ggplot2)
ggplot(housing.df, aes(y = NOX, x = LSTAT, colour= CAT..MEDV)) +
  geom_point(alpha = 0.6) 

## panel plots
# compute mean MEDV per RAD and CHAS
# In aggregate() use argument drop = FALSE to include all combinations
# (exiting and missing) of RAD X CHAS.
data.for.plot <- aggregate(housing.df$MEDV, by = list(housing.df$RAD, housing.df$CHAS), 
                           FUN = mean, drop = FALSE)
names(data.for.plot) <- c("RAD", "CHAS", "meanMEDV")

# plot the data
par(mfcol = c(2,1))
barplot(height = data.for.plot$meanMEDV[data.for.plot$CHAS == 0], 
        names.arg = data.for.plot$RAD[data.for.plot$CHAS == 0], 
        xlab = "RAD", ylab = "Avg. MEDV", main = "CHAS = 0")
#  

barplot(height = data.for.plot$meanMEDV[data.for.plot$CHAS == 1], 
        names.arg = data.for.plot$RAD[data.for.plot$CHAS == 1], 
        xlab = "RAD", ylab = "Avg. MEDV", main = "CHAS = 1")
# This plots below the above plot


# alternative plot with ggplot
ggplot(data.for.plot) +
  geom_bar(aes(x = as.factor(RAD), y = `meanMEDV`), stat = "identity") +
  xlab("RAD") + facet_grid(CHAS ~ .)
# 
# ---------------------Validated -------------------------------------------
