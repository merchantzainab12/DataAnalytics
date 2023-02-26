#### 11.3

library(readr)
library(neuralnet)
library(caret)
library(e1071)
ToyotaCorolla <- read_csv("ToyotaCorolla.csv")
View(ToyotaCorolla)

selected_var <- c("Age_08_04","KM","HP","Doors","Quarterly_Tax",
                  "Guarantee_Period","Price")
binary_var <- c("Automatic","Mfr_Guarantee","Airco","Automatic_airco","CD_Player",
                "Powered_Windows","Sport_Model","Tow_Bar")

#scaling the attributes
max_toyota <- apply(ToyotaCorolla[,selected_var],2,max)
min_toyota <- apply(ToyotaCorolla[,selected_var],2,min)
toyota_scaled.df <- scale(ToyotaCorolla[,selected_var], center = min_toyota, scale = max_toyota-min_toyota)

set.seed(1234)
index = sample(1:nrow(toyota_scaled.df), round(0.6*nrow(toyota_scaled.df)))
train_toyota <- as.data.frame(toyota_scaled.df[index,])
valid_toyota <- as.data.frame(toyota_scaled.df[-index,])

binary_data_train <- ToyotaCorolla[index, binary_var]
binary_data_valid<- ToyotaCorolla[-index, binary_var]

trainData <-cbind(train_toyota,
                  class.ind(ToyotaCorolla[index,]$Fuel_Type),binary_data_train)
validData <-cbind(valid_toyota,
                  class.ind(ToyotaCorolla[-index,]$Fuel_Type),binary_data_valid)

attach(trainData)
nn <- neuralnet(Price ~ ., data = trainData, hidden = 2, stepmax = 1e7,linear.output = T)  
plot(nn, rep = "best")
#Predictions on training dataset
predict_toyota <- compute(nn, trainData)
multiplier <-max(ToyotaCorolla$Price) - min(ToyotaCorolla$Price)

predicted_train_toyota <- predict_toyota$net.result * multiplier + min(ToyotaCorolla$Price)

train_toyota_Price <- as.data.frame((train_toyota$Price)*multiplier + min(ToyotaCorolla$Price))
RMSE_train_toyota_nnw_2 <- sqrt(sum((train_toyota_Price - predicted_train_toyota)^2)/nrow(train_toyota_Price))
RMSE_train_toyota_nnw_2

#Predictions on validation dataset

predict_toyota <- compute(nn, validData)
predict_toyota$net.result 


predicted_valid_toyota <- predict_toyota$net.result * multiplier + min(ToyotaCorolla$Price)
predicted_valid_toyota 

valid_toyota_Price <- as.data.frame((valid_toyota$Price)*multiplier + min(ToyotaCorolla$Price))
valid_toyota_Price

RMSE_valid_toyota_nnw_2 <- sqrt(sum((valid_toyota_Price - predicted_valid_toyota)^2)/nrow(valid_toyota_Price))
RMSE_valid_toyota_nnw_2

# This is the error using Neural Network.
#########Single layer 5 nodes
nn <- neuralnet(Price ~ ., data = trainData, hidden = 5, stepmax = 1e7,linear.output = T)  
plot(nn, rep = "best")
#Predictions on training dataset
predict_toyota <- compute(nn, trainData)
multiplier <-max(ToyotaCorolla$Price) - min(ToyotaCorolla$Price)

predicted_train_toyota <- predict_toyota$net.result * multiplier + min(ToyotaCorolla$Price)

train_toyota_Price <- as.data.frame((train_toyota$Price)*multiplier + min(ToyotaCorolla$Price))
RMSE_train_toyota_nnw_5 <- sqrt(sum((train_toyota_Price - predicted_train_toyota)^2)/nrow(train_toyota_Price))
RMSE_train_toyota_nnw_5

#Predictions on validation dataset

predict_toyota <- compute(nn, validData)
predict_toyota$net.result 


predicted_valid_toyota <- predict_toyota$net.result * multiplier + min(ToyotaCorolla$Price)
predicted_valid_toyota 

valid_toyota_Price <- as.data.frame((valid_toyota$Price)*multiplier + min(ToyotaCorolla$Price))
valid_toyota_Price

RMSE_valid_toyota_nnw_5 <- sqrt(sum((valid_toyota_Price - predicted_valid_toyota)^2)/nrow(valid_toyota_Price))
RMSE_valid_toyota_nnw_5

#########double layer 5,5 nodes
nn <- neuralnet(Price ~ ., data = trainData, hidden =c(5,5) , stepmax = 1e7,linear.output = T) 
plot(nn, rep = "best")

predict_toyota <- compute(nn, trainData)
multiplier <-max(ToyotaCorolla$Price) - min(ToyotaCorolla$Price)

predicted_train_toyota <- predict_toyota$net.result * multiplier + min(ToyotaCorolla$Price)

train_toyota_Price <- as.data.frame((train_toyota$Price)*multiplier + min(ToyotaCorolla$Price))
RMSE_train_toyota_nnw_55 <- sqrt(sum((train_toyota_Price - predicted_train_toyota)^2)/nrow(train_toyota_Price))
RMSE_train_toyota_nnw_55

#Predictions on validation dataset

predict_toyota <- compute(nn, validData)
predict_toyota$net.result 


predicted_valid_toyota <- predict_toyota$net.result * multiplier + min(ToyotaCorolla$Price)
predicted_valid_toyota 

valid_toyota_Price <- as.data.frame((valid_toyota$Price)*multiplier + min(ToyotaCorolla$Price))
valid_toyota_Price

RMSE_valid_toyota_nnw_55 <- sqrt(sum((valid_toyota_Price - predicted_valid_toyota)^2)/nrow(valid_toyota_Price))
RMSE_valid_toyota_nnw_55


###################################################################

# 9.3 A Using CART
library(readr)
ToyotaCorolla <- read_csv("ToyotaCorolla.csv")
View(ToyotaCorolla)
selected.var <- c("Price","Age_08_04","KM", "Fuel_Type", "HP", "Automatic", "Doors", "Quarterly_Tax", "Mfr_Guarantee", "Guarantee_Period", "Airco", "Automatic_airco", "CD_Player", "Powered_Windows", "Sport_Model","Tow_Bar")
names(ToyotaCorolla)
View(ToyotaCorolla)
# partition data

set.seed(1) # set seed for reproducing the partition
train.index <- sample(c(1:1000), 600)
train.df <- ToyotaCorolla[train.index, selected.var]
valid.df <- ToyotaCorolla[-train.index, selected.var]
#
# build Regression Tree using training data
toyota.rt <- rpart(Price ~ ., data = train.df, method = "anova", minbucket = 1, maxdepth = 30, cp = 0.001)
# plot regression tree from training data
prp(toyota.rt)
summary(toyota.rt)


########################## 9.3 B) ii)
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

########################## 9.3 C) Using Linear regression
library(readr)
ToyotaCorolla <- read_csv("ToyotaCorolla.csv")
View(ToyotaCorolla)
selected.var <- c("Price","Age_08_04","KM", "Fuel_Type", "HP", "Automatic", "Doors", "Quarterly_Tax", "Mfr_Guarantee", "Guarantee_Period", "Airco", "Automatic_airco", "CD_Player", "Powered_Windows", "Sport_Model","Tow_Bar")
names(ToyotaCorolla)
View(ToyotaCorolla)
# partition data

set.seed(2) # set seed for reproducing the partition
train.index <- sample(c(1:1000), 600)
train.df <- ToyotaCorolla[train.index, selected.var]
valid.df <- ToyotaCorolla[-train.index, selected.var]
train.df

reg <- lm(Price ~ ., data = train.df) 
tr.res <- data.frame(train.df$Price, reg$fitted.values, reg$residuals)
head(tr.res)

summary(reg)


library(forecast)
library(ggplot2)

accuracy(predict(reg, train.df), train.df$Price)
accuracy(predict(reg, valid.df), valid.df$Price)

############## 9.3 C ii)
train.err <- predict(reg, train.df) - train.df$Price
valid.err <- predict(reg, valid.df) - valid.df$Price
error2 <- data.frame(Error = c(train.err, valid.err), 
                  Set = c(rep("Training", length(train.err)),
                          rep("Validation", length(valid.err))))
ggplot(er2, aes(x = Set, y = Error)) + geom_boxplot()
