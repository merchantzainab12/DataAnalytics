#### Classification and Regression Tree
## Chapter - 9.0 (Shmueli)
# Code for building a Regression Tree
# 
# the code for Regression tree (not in the book)
#
# 
# Figure 9.14 
 
install.packages(c("rpart", "rpart.plot"))

library(rpart); library(rpart.plot)

install.packages(c("forecast", "caret", "e1071"))

library(forecast); library(caret); library(e1071)



car.df <- read.csv("ToyotaCorolla.csv")

# use first 1000 rows of data
# select variable for regression

selected.var <- c(3,4,7,8,9,10,12,13,14,17,18)
names(car.df)

# partition data

set.seed(1) # set seed for reproducing the partition
train.index <- sample(c(1:1000), 600)
train.df <- car.df[train.index, selected.var]
valid.df <- car.df[-train.index, selected.var]
#
# build Regression Tree using training data
toyota.rt <- rpart(Price ~ ., data = train.df)
# plot regression tree from training data
prp(toyota.rt)

# now, to compute prediction accuracy

# errors
library(forecast)
library(ggplot2)

accuracy(predict(toyota.rt, train.df), train.df$Price)
accuracy(predict(toyota.rt, valid.df), valid.df$Price)

train.err <- predict(toyota.rt, train.df) - train.df$Price
valid.err <- predict(toyota.rt, valid.df) - valid.df$Price
err <- data.frame(Error = c(train.err, valid.err), 
                  Set = c(rep("Training", length(train.err)),
                          rep("Validation", length(valid.err))))
ggplot(err, aes(x = Set, y = Error)) + geom_boxplot()

## 
# 
####-------------Regression Tree Exercise Completed ------------------

