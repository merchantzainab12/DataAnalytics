#9.1
library(readr)
eBayAuctions <- read_csv("eBayAuctions.csv")
attach(eBayAuctions)

Duration<-as.factor(Duration)
class(Duration)
set.seed(2)

train.index <- sample(c(1:dim(eBayAuctions)[1]), dim(eBayAuctions)[1]*0.6)  
train.df <- eBayAuctions[train.index, ]
valid.df <- eBayAuctions[-train.index, ]

#A
ebay.tree <- rpart(`Competitive?` ~ ., data = eBayAuctions, 
                    control = rpart.control(maxdepth = 7,minbucket = 50), method = "class")
## plot tree
rpart.plot(ebay.tree)
prp(ebay.tree, type = 1, extra = 1, split.font = 1, varlen = -10)  

summary(ebay.tree)

# 9.1 ii)

install.packages(c("forecast", "caret", "e1071"))

library(forecast); library(caret); library(e1071)

ebay.pred.train <- predict(ebay.tree,train.df,type = "class")
confusionMatrix(ebay.pred.train, as.factor(train.df$`Competitive?`))

ebay.pred.valid <- predict(ebay.tree,valid.df,type = "class")
confusionMatrix(ebay.pred.valid, as.factor(valid.df$`Competitive?`))

#################################################################################
library(neuralnet)
library(nnet) 
library(caret)
library(e1071)
trainnn<-train.df[c(6,7,8)]
validnn<-valid.df[c(6,7,8)]

#using min max normaliztion 
process <- preProcess(as.data.frame(trainnn), method=c("range"))
norm_scale <- predict(process, as.data.frame(trainnn))

nn <- neuralnet(`Competitive?` ~ ClosePrice + OpenPrice, data = norm_scale, hidden = 3, stepmax = 1e7, act.fct = "logistic")    
# Plot the Neural Network 
prediction(nn)
plot(nn, rep = "best")

outnn1 <- compute(nn, rep = 1, trainnn)
p1 <- outnn1$net.result # get the probabilities
pred1 <- ifelse(p1>0.5,1,0) # convert probabilities into classification
tabl <- table(pred1, train.df$`Competitive?`)
tabl


confusionMatrix(as.factor(pred1), as.factor(train.df$`Competitive?`))

outnn2 <- compute(nn, rep = 1, validnn)
p2 <- outnn2$net.result # get the probabilities
pred2 <- ifelse(p2>0.5,1,0) # convert probabilities into classification
tabl <- table(pred2, valid.df$`Competitive?`)
tabl


confusionMatrix(as.factor(pred2), as.factor(valid.df$`Competitive?`))
################################################################################

logit.reg <- glm(`Competitive?` ~ ., data = train.df, family = "binomial") 
summary(logit.reg)
logit.reg.pred <- predict(logit.reg, valid.df[,-8], type = "response")  

temp_df<-data.frame(actual = valid.df$`Competitive?`, predicted = logit.reg.pred)

predicted_valid<-ifelse(temp_df$predicted>0.6,1,0)
#To keep both columns as factor variables
predicted_valid<-as.factor(predicted_valid)
actual_valid<-as.factor(temp_df$actual)
#Build confusion Matrix
library(caret)
cm<-confusionMatrix(actual_valid,predicted_valid)
cm

#################################################################
