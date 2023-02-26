################# CART
## CLASSIFICATION TREE

install.packages(c("rpart", "rpart.plot"))
library(rpart); library(rpart.plot)
# use rpart() to run a classification tree.
# define rpart.control() in rpart() to determine the depth of the tree.
class.tree <- rpart(Ownership ~ ., data = mower.df, 
                    control = rpart.control(maxdepth = 2), method = "class")

class.tree <- rpart(Ownership ~ ., data = mower.df, 
                    control = rpart.control(minsplit = 1), method = "class")

# Partitioning
set.seed(1)  
train.index <- sample(c(1:dim(bank.df)[1]), dim(bank.df)[1]*0.6)  
train.df <- bank.df[train.index, ]
valid.df <- bank.df[-train.index, ]

# Predicting values on tree
install.packages(c("forecast", "caret", "e1071"))
library(forecast); library(caret); library(e1071)

default.ct.point.pred.train <- predict(default.ct,train.df,type = "class")

# generate confusion matrix for training data

confusionMatrix(default.ct.point.pred.train, as.factor(train.df$Personal.Loan))

default.ct.point.pred.valid <- predict(default.ct,valid.df,type = "class")
# generate confusion matrix for validation data
confusionMatrix(default.ct.point.pred.valid, as.factor(valid.df$Personal.Loan))

#complexity parameter
cv.ct <- rpart(Personal.Loan ~ ., data = train.df, method = "class", 
               cp = 0.00001, minsplit = 5, xval = 5)

# To find min error tree, best pruned tree
printcp(cv.ct)
rpart.plot(cv.ct)

# Pruning the tree
pruned.ct <- prune(cv.ct, cp = cv.ct$cptable[which.min(cv.ct$cptable[,"xerror"]),"CP"])
length(pruned.ct$frame$var[pruned.ct$frame$var == "<leaf>"])
rpart.plot(pruned.ct)
set.seed(1)
cv.ct <- rpart(Personal.Loan ~ ., data = train.df, method = "class", cp = 0.00001, minsplit = 1, xval = 5)  
# xval is number K of folds in a K-fold cross-validation.
printcp(cv.ct)  

# The R-squared for a regression tree is 1 minus rel error. xerror (or relative cross-validation error where 
# "x" stands for "cross") is a scaled version of overall average of the 5 out-of-sample errors across the 5 folds.

pruned.ct <- prune(cv.ct, cp = 0.0090909)
prp(pruned.ct, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10, 
    box.col=ifelse(pruned.ct$frame$var == "<leaf>", 'gray', 'white')) 

## REGRESSION TREE

toyota.rt <- rpart(Price ~ ., data = train.df)
library(forecast);library(ggplot2)

accuracy(predict(toyota.rt, train.df), train.df$Price)
accuracy(predict(toyota.rt, valid.df), valid.df$Price)

train.err <- predict(toyota.rt, train.df) - train.df$Price
valid.err <- predict(toyota.rt, valid.df) - valid.df$Price
err <- data.frame(Error = c(train.err, valid.err), 
                  Set = c(rep("Training", length(train.err)),
                          rep("Validation", length(valid.err))))
ggplot(err, aes(x = Set, y = Error)) + geom_boxplot()

############################################------NEURAL NETWORK----#########################################################################

library(neuralnet); library(nnet); library(caret); library(e1071)
####### NN FOR CLASSIFICATION

set.seed(1234) # Compare resulting nn$weights with and without seed. 
univ.df <- read.csv("UniversalBank Short.csv")


# Since the target variable is "int", it needs to be converted to a "factor"
Loan <- univ.df$Personal.Loan # Create a column with names
set.seed(1234) # Try with different seeds 


train.index <- sample(c(1:dim(univ_scaled.df)[1]), dim(univ_scaled.df)[1]*0.6)  
train.df <- univ_scaled.df[train.index, ]
valid.df <- univ_scaled.df[-train.index, ]
nnloan <- neuralnet(Loan ~., data=train.df, act.fct = "logistic", linear.output = FALSE)

# predictions and setting a cutoff
outnn1 <- compute(nnloan, rep = 1, train.df)
validnn1 <- compute(nnloan, rep = 1, valid.df)
pv1 <- validnn1$net.result # get the probabilities
predv1 <- ifelse(pv1>0.5,1,0) # convert probabilities into classification
tabvl <- table(predv1, valid.df$Loan)

##NN FOR REGRESSION

# Normalize Data
max_boston <- apply(bostondata.df,2,max)
min_boston <- apply(bostondata.df,2,min)
boston_scaled.df <- scale(bostondata.df, center = min_boston, scale = max_boston - min_boston)

#Partitioning
index = sample(1:nrow(bostondata.df), round(0.7*nrow(bostondata.df)))
train_boston <- as.data.frame(boston_scaled.df[index,])
valid_boston <- as.data.frame(boston_scaled.df[-index,])


boston_nw1 <- neuralnet(MEDV ~ ., data = train_boston, hidden = 10, linear.output = T)
plot(boston_nw1)
# Apply NNW model to the validation data

predict_boston <- compute(boston_nw1, valid_boston)

### convert bck to original units
multiplier <-max(bostondata.df$MEDV) - min(bostondata.df$MEDV)
predicted_valid_boston <- predict_boston$net.result * multiplier + min(bostondata.df$MEDV)

MSE_valid_boston_nnw <- sum((valid_boston_MEDV - predicted_valid_boston)^2)/nrow(valid_boston_MEDV)



