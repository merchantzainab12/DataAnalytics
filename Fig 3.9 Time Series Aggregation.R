# Chapter - 3 Data Exploration and Visualization
install.packages(c("GGally", "ggmap", "mosaic", "treemap"))
setwd("~/Prakash Documents/BUAN 6356/Spring 2020 Course Material/Excel, CSV and JMP FIles")
#### Table 3.2


Amtrak.df <- read.csv("Amtrak data.csv")

#### Figure 3.9

library(forecast)
Amtrak.df <- read.csv("Amtrak data.csv")
ridership.ts <- ts(Amtrak.df$Ridership, start = c(1991, 1), end = c(2004, 3), freq = 12)
plot(ridership.ts, xlab = "Year", ylab = "Ridership (in 000s)", ylim = c(1300, 2300))


## fit curve
ridership.lm <- tslm(ridership.ts ~ trend + I(trend^2))
plot(ridership.ts, xlab = "Year", ylab = "Ridership (in 000s)", ylim = c(1300, 2300))
# 
lines(ridership.lm$fitted, lwd = 2)

# alternative plot with ggplot
library(ggplot2)
ggplot(Amtrak.df, aes(y = Ridership, x = Month, group = 12)) +
  geom_line() + geom_smooth(formula = y ~ poly(x, 2), method= "lm",
                            colour = "navy", se = FALSE, na.rm = TRUE)

## zoom in, monthly, and annual plots
ridership.2yrs <- window(ridership.ts, start = c(1991,1), end = c(1992,12))
plot(ridership.2yrs, xlab = "Year", ylab = "Ridership (in 000s)", ylim = c(1300, 2300))
monthly.ridership.ts <- tapply(ridership.ts, cycle(ridership.ts), mean)
plot(monthly.ridership.ts, xlab = "Month", ylab = "Average Ridership",
     ylim = c(1300, 2300), type = "l", xaxt = 'n')
## set x labels
axis(1, at = c(1:12), labels = c("Jan","Feb","Mar", "Apr","May","Jun",
                                 "Jul","Aug","Sep",  "Oct","Nov","Dec"))

annual.ridership.ts <- aggregate(ridership.ts, FUN = mean)
plot(annual.ridership.ts, xlab = "Year", ylab = "Average Ridership",
     ylim = c(1300, 2300))

## ------Validated --------------------------