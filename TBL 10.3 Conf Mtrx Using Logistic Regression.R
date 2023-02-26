#### Table 10.2

bank.df <- read.csv("UniversalBank.csv")
bank.df <- bank.df[ , -c(1, 5)]  # Drop ID and zip code columns.
# treat Education as categorical (R will create dummy variables)

#Predictor variables can be either 0/1 or numbers not category type
bank.df$Education <- factor(bank.df$Education, levels = c(1, 2, 3), 
                            labels = c("Undergrad", "Graduate", "Advanced/Professional"))

# partition data
set.seed(1234)
train.index <- sample(c(1:dim(bank.df)[1]), dim(bank.df)[1]*0.6)  
train.df <- bank.df[train.index, ]
valid.df <- bank.df[-train.index, ]

# run logistic regression
# use glm() (general linear model) with family = "binomial" to fit a logistic 
# regression.
logit.reg <- glm(Personal.Loan ~ ., data = train.df, family = "binomial") 
options(scipen=999)
summary(logit.reg)

# -------------------Validated ----------------------

#### Table 10.3

# use predict() with type = "response" to compute predicted probabilities. 
logit.reg.pred <- predict(logit.reg, valid.df[, -8], type = "response")      #Removing Personal.Loan from original validation data

# first 5 actual and predicted records
data.frame(actual = valid.df$Personal.Loan[1:5], predicted = logit.reg.pred[1:5])

#to create Confusion Matrix for validation data:
temp_df<-data.frame(actual = valid.df$Personal.Loan, predicted = logit.reg.pred)
temp_df[1:10,]

predicted_valid<-ifelse(temp_df$predicted>0.5,1,0)

#To keep both columns as factor variables
predicted_valid<-as.factor(predicted_valid)
actual_valid<-as.factor(temp_df$actual)
#Build confusion Matrix
library(caret)
cm<-confusionMatrix(actual_valid,predicted_valid)
cm

#to create Confusion Matrix for Train Data:
logit.reg.pred1 <- predict(logit.reg, train.df[, -8], type = "response")
#View
data.frame(actual = train.df$Personal.Loan[1:5], predicted = logit.reg.pred1[1:5])

temp_df1<-data.frame(actual = train.df$Personal.Loan, predicted = logit.reg.pred1)
temp_df1[1:10,]

predicted_train<-ifelse(temp_df1$predicted>0.5,1,0)
predicted_train<-as.factor(predicted_train)
actual_train<-as.factor(temp_df1$actual)

#Build Confusion Matrix
cm1<-confusionMatrix(actual_train,predicted_train)
cm1


#Scoring New dataset
new_data <- read.csv("Unknown.csv")
new_data<- new_data[ , -c(1, 5)]
new_data$Education <- factor(new_data$Education, levels = c(1, 2, 3),  labels = c("Undergrad", "Graduate", "Advanced/Professional"))
pred <- predict(logit.reg,  new_data[-8],type="response")
pred

 final_pred<-ifelse(pred>0.5,1,0)
 new_data$final_pred<-final_pred

View(new_data)

