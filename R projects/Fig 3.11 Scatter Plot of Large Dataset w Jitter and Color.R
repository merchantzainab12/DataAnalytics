# Chapter - 3 Data Exploration and Visualization
install.packages(c("GGally", "ggmap", "mosaic", "treemap"))
setwd("~/Prakash Documents/BUAN 6356/Spring 2020 Course Material/Excel, CSV and JMP FIles")


#### Figure 3.11

# use function alpha() in library scales to add transparent colors
universal.df <- read.csv("UniversalBank.csv")

library(scales)
plot(jitter(universal.df$CCAvg, 1) ~ jitter(universal.df$Income, 1),
     col = alpha(ifelse(universal.df$Securities.Account == 0, "gray", "red"), 0.4),
     pch = 20, log = 'xy', ylim = c(0.1, 10),
     xlab = "Income", ylab = "CCAvg")

plot(jitter(universal.df$CCAvg, 1) ~ jitter(universal.df$Income, 1),
     col = alpha(ifelse(universal.df$Securities.Account == 0, "gray", "black"), 0.4),
     pch = 20, log = 'xy', ylim = c(0.1, 10),
     xlab = "Income", ylab = "CCAvg")

### Question: How to plot without jitter?


# alternative with ggplot
library(ggplot2)
ggplot(universal.df) +
  geom_jitter(aes(x = Income, y = CCAvg, colour = Securities.Account)) + 
  scale_x_log10(breaks = c(10, 20, 40, 60, 100, 200)) +
  scale_y_log10(breaks = c(0.1, 0.2, 0.4, 0.6, 1.0, 2.0, 4.0, 6.0))
# several warning messages 

# -------------------Validated ---- need to plot without jitter ----------------
