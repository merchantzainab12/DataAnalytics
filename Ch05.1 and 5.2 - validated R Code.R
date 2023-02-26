#### Table 5.1
# Requires Package pROC
# package forecast is required to evaluate performance
library(forecast)

# load file
toyota.corolla.df <- read.csv("ToyotaCorolla.csv")
names(toyota.corolla.df)
names(toyota.corolla.df[,-c(1,2,8.11)])

# randomly generate training and validation sets
training <- sample(toyota.corolla.df$Id, 600)

validation <- sample(setdiff(toyota.corolla.df$Id, training), 400)

# run linear regression model
reg <- lm(Price~., data=toyota.corolla.df[,-c(1,2,8,11)], subset=training,
          na.action=na.exclude)
pred_t <- predict(reg, na.action=na.pass)
pred_v <- predict(reg, newdata=toyota.corolla.df[validation,-c(1,2,8,11)],
                  na.action=na.pass)
# For pred_v above, a warning message comes up: prediction from a rank-deficient fit may be misleading

## evaluate performance
# training
accuracy(pred_t, toyota.corolla.df[training,]$Price)
# validation
accuracy(pred_v, toyota.corolla.df[validation,]$Price)

# Figure 5.1 --> the following is a simplistic way
trg_error <- (pred_t - toyota.corolla.df[training,]$Price )
hist(trg_error, xlab = "MEDV", ylab = "Training")
valid_error <- (pred_v - toyota.corolla.df[validation,]$Price)
hist(valid_error, xlab = "MEDV", ylab ="Validation")
par(mfrow=c(1,2))
boxplot(trg_error, xlab = "Training", ylab = "MEDV")
boxplot(valid_error, xlab = "Validation", ylab = "MEDV")

# Need to write code for Figure - 1 for boxplots to be side by side



#### Figure 5.2

toyota.corolla.df <- read.csv("ToyotaCorolla.csv")
dim(toyota.corolla.df) # [1436, 39]

# remove missing Price data
toyota.corolla.df <-     
  toyota.corolla.df[!is.na(toyota.corolla.df[validation,]$Price),]

dim(toyota.corolla.df) # [1436, 39]

# Since the dimensions are the same, there were no missing values.



# generate random Training and Validation sets
training <- sample(toyota.corolla.df$Id, 600)
validation <- sample(setdiff(toyota.corolla.df$Id, training), 400)

# regression model based on all numerical predictors
reg <- lm(Price~., data = toyota.corolla.df[,-c(1,2,8,11)], subset = training)

# predictions
pred_v <- predict(reg, newdata = toyota.corolla.df[validation,-c(1,2,8,11)]) # gives warning message -> rank deficient

# load package gains, compute gains (we will use package caret for categorical y later)
library(gains)
gain <- gains(toyota.corolla.df[validation,]$Price[!is.na(pred_v)], pred_v[!is.na(pred_v)])

# cumulative lift chart
options(scipen=999) # avoid scientific notation
# we will compute the gain relative to price
price <- toyota.corolla.df[validation,]$Price[!is.na(toyota.corolla.df[validation,]$Price)]
plot(c(0,gain$cume.pct.of.total*sum(price))~c(0,gain$cume.obs), 
     xlab="# cases", ylab="Cumulative Price", main="Lift Chart", type="l")

# baseline
lines(c(0,sum(price))~c(0,dim(toyota.corolla.df[validation,])[1]), col="gray", lty=2)

# Decile-wise lift chart
barplot(gain$mean.resp/mean(price), names.arg = gain$depth,
        xlab = "Percentile", ylab = "Mean Response", main = "Decile-wise lift chart")

## ---------- Validated --------------------------------------------------

