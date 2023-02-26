## ------------------------------------------------------------
# Table 6.3 Linear Regression Model of Price Vs Car Attributes|
#                                                             |
## -----------------------------------------------------------|
# Code for fitting a regression model

car.df <- read.csv("ToyotaCorolla.csv")


# use first 1000 rows of data
# select variable for regression

selected.var <- c(3,4,7,8,9,10,12,13,14,17,18)

# partition data

set.seed(1) # set seed for reproducing the partition
train.index <- sample(c(1:1000), 600)
train.df <- car.df[train.index, selected.var]
valid.df <- car.df[-train.index, selected.var]

# use lm() to run a linear regression of Price on all the predictors on the
# training set (it will automatically turn Fuel_Type into dummies).
# use . after ~ to include all the remaining columns in train.df as predictors.

car.lm <- lm(Price ~ ., data = train.df)
# use options () to ensure numbers are not displayed in scientific notation.
options(scipen = 999)
summary(car.lm)

#  -------- Table 6.3 Validated -------------------------
