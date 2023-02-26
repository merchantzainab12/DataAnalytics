#6.1, PART B
housing.df <- read.csv("BostonHousing.csv")
install.packages("reshape2")
library(reshape2)
View(housing.df)
predictors<- housing.df[c(1,4,6,13)]
predictors
set.seed(23)
train.rows <- sample(row.names(predictors), dim(predictors)[1]*0.6) 
train.data <- predictors[train.rows,] 
train.rows.boston <- sample(row.names(housing.df), dim(housing.df)[1]*0.6) 
train.data.boston <- housing.df[train.rows.boston,] 
valid.rows <- setdiff(rownames(predictors), train.rows)
valid.rows.boston <- setdiff(rownames(housing.df), train.rows.boston)
valid.data <- predictors[valid.rows,]
valid.data.boston <- housing.df[valid.rows.boston,] 

reg <- lm(MEDV ~ ., data = predictors, subset = train.rows)
tr.res <- data.frame(train.data$MEDV, reg$fitted.values, reg$residuals)
head(tr.res)

#6.1 PART C
install.packages('forecast', dependencies = TRUE)
library(forecast)

library(readxl)
housing.test <- read_excel("housing_test.xlsx")
pred <- predict(reg, newdata = housing.test)

# 6.1 PART D
#E
boston_reg <- lm(MEDV ~ ., data = train.data.boston)
cor(housing.df)
boston.lm.step.back <-step(boston_reg, direction = "backward")
summary(boston.lm.step.back)
boston_back_pred<-predict(boston.lm.step.back,valid.data.boston )
accuracy(boston_back_pred, valid.data.boston$MEDV)
#car.lm.step.pred <- predict(car.lm.step, valid.df)
#accuracy(car.lm.step.pred, valid.df$Price)

boston.lm.null <- lm(MEDV~1, data = train.df)

# Use step() to run forward selection
boston.lm.step.forw <- step(boston.lm.null, scope = list(lower=boston.lm.null, upper=boston_reg), direction = "forward")

summary(boston.lm.step.forw)
boston_forw_pred<-predict(boston.lm.step.forw,valid.data.boston)
accuracy(boston_forw_pred, valid.data.boston$MEDV)

## Both
boston.lm.step.both <-step(boston_reg, direction = "both")
boston_both_pred<-predict(boston.lm.step.both,train.data.boston )
accuracy(boston_both_pred, train.data.boston$MEDV)
summary(boston.lm.step.both)

#####################################################################################################Q2
set.seed(12356)
creditcard_df <- read.csv("credit_cards.csv")
train.rows <- sample(row.names(creditcard_df), dim(creditcard_df)[1]*0.5) 
train.data <- creditcard_df[train.rows,] 
valid.rows <- sample(setdiff(rownames(creditcard_df), train.rows), dim(creditcard_df)[1]*0.3)
valid.data <- creditcard_df[valid.rows,]
test.rows <- setdiff(rownames(creditcard_df), union(train.rows, valid.rows))
test.data <- creditcard_df[test.rows,]
credit.lm <- lm(Avgexp ~ Age+Acc+Selfempl, data = creditcard_df, subset = train.rows)
#tr.res <- data.frame(train_data$Avgexp, credit.lm$fitted.values, credit.lm$residuals)
head(tr.res)
credit.lm

#######Q1############

credit.card.null <- lm(Avgexp ~1, data = train.data)
credit.card.all <- lm(Avgexp ~ ., data= train.data)
credit.lm.step <- step(credit.card.null, scope = list(lower=credit.card.null, upper= credit.card.all), direction = "forward")

credit.lm.step.back <-step(credit.card.all, direction = "backward")
credit.lm.step.both <-step(credit.card.all, direction = "both")
credit.card.opt <- lm(Avgexp ~ Acc + Income, data= train.data)
summary(credit.lm.step)
credit.lm.step.forw <- predict(credit.lm.step, train.data)
accuracy(credit.lm.step.forw, valid.data$Avgexp)
################### 10 ################

bank.df <- read.csv("banks.csv")

# partition data
set.seed(2)
train.index <- sample(c(1:dim(bank.df)[1]), dim(bank.df)[1]*0.6)  
train.df <- bank.df[train.index, ]
valid.df <- bank.df[-train.index, ]

# run logistic regression
# use glm() (general linear model) with family = "binomial" to fit a logistic 
# regression.
logit.reg <- glm(Financial.Condition ~ .-Obs - TotCap.Assets, data = bank.df, family = "binomial") 
options(scipen=999)
summary(logit.reg)

predicted_valid<-ifelse(temp_df$predicted>0.5,1,0)

######################### 10.3 ####################
install.packages("ggplot2")
library(ggplot2)
install.packages(c("GGally", "ggmap", "mosaic", "treemap"))
RidingMowers.df <- read.csv("RidingMowers.csv")
##Table 2.11 - Fit regression model on training data
logit.reg <- glm(Cat.Ownership ~ ., data = RidingMowers.df[-3], family = "binomial") 
options(scipen=999)
summary(logit.reg)

logit.reg.pred <- predict(logit.reg, RidingMowers.df, type = "response")
temp_df<-data.frame(actual = RidingMowers.df$Cat.Ownership, predicted = logit.reg.pred)
predicted_valid<-ifelse(temp_df$predicted>0.5,1,0)

ggplot(RidingMowers.df, aes(Income,Lot_Size, colour= Ownership)) +
  geom_point(alpha = 0.6, size=3)  + geom_text(aes(label = paste(" ", Ownership)), size = 4, angle=25)
predicted_valid<-as.factor(predicted_valid)
actual_valid<-as.factor(temp_df$actual)
#Build confusion Matrix
library(caret)
cm<-confusionMatrix(actual_valid,predicted_valid)
cm

###############################################################################
#Q3

#A  
Salmons.df <- read.csv("Salmons.csv")
train.data.Salmon <- Salmons.df[Salmons.df$Partition=="t",]
valid.data.Salmon <- Salmons.df[Salmons.df$Partition=="v",]
test.data.Salmon <- Salmons.df[Salmons.df$Partition=="s",]

logit.reg.sal <- glm( Coupon ~ .-Customer, data = train.data.Salmon[-5], family = "binomial")
logit.reg.pred <- predict(logit.reg.sal, valid.data.Salmon, type = "response")
temp_df<-data.frame(actual = valid.data.Salmon$Coupon, predicted = logit.reg.pred)
predicted_valid<-ifelse(temp_df$predicted>0.5,1,0)
predicted_valid<-as.factor(predicted_valid)
actual_valid<-as.factor(temp_df$actual)
cm<-confusionMatrix(actual_valid,predicted_valid)

logit.reg.test <- predict(logit.reg.sal, test.data.Salmon, type = "response")
sorted_df<- logit.reg.test[order(logit.reg.test,decreasing = TRUE)]

#B
library(gains)
gain <- gains(test.data.Salmon$Coupon, logit.reg.test, groups=10)
top_10 <- head(sorted_df,20)
gain$cume.obs
# plot lift chart
plot(c(0,gain$cume.pct.of.total*sum(top_10))~c(0,gain$cume.obs),
     xlab="", ylab="Cumulative", main="", type="l")
lines(c(0,sum(top_10))~c(0, 200),col="blue", lty=3)


df<-data.frame(actual = test.data.Salmon$Coupon, predicted = logit.reg.test)
predicted_test<-ifelse(df$predicted>0.5,1,0)
predicted_test<-as.factor(predicted_test)
actual_test<-as.factor(df$actual)

#C
library(pROC)
r <- roc(actual_test, logit.reg.test)
plot.roc(r)
      