#### Classification and Regression Tree
## Chapter - 9.0 (Shmueli)
# Code for running and plotting classification tree with single split

# --------------------- Fig 9.7: Tree Representation of First Split --------------------
install.packages(c("rpart", "rpart.plot"))

library(rpart); library(rpart.plot)
mower.df <- read.csv("RidingMowers.csv")

# use rpart() to run a classification tree.
# define rpart.control() in rpart() to determine the depth of the tree.
class.tree <- rpart(Ownership ~ ., data = mower.df, 
                    control = rpart.control(maxdepth = 2), method = "class")
## plot tree
# Method - 1: 
rpart.plot(class.tree)
# Method - 2: use prp() to plot the tree. You can control plotting parameters such as color, shape, 
# and information displayed (which and where).
prp(class.tree, type = 1, extra = 1, split.font = 1, varlen = -10)  


# -----------------Fig. 9.7 complete--------------------------------------


#  
# ---------- Figure 9.8: Tree Representation after all splits --------------


class.tree <- rpart(Ownership ~ ., data = mower.df, 
                    control = rpart.control(minsplit = 1), method = "class")

## replaced maxdepth by minsplit
rpart.plot(class.tree)
prp(class.tree, type = 1, extra = 1, split.font = 1, varlen = -10)  


# -------------- Fig 9.8 Complete --------------------------

#
# ---------- Figure 9.9: Default Classification Tree for the Loan Example using the Training Data Set --- 
#
# Code for creating a default classification tree
# Bank Loan Example 

bank.df <- read.csv("UniversalBank.csv")
bank.df <- bank.df[ , -c(1, 5)]  # Drop ID and zip code columns.

# partition
set.seed(1)  
train.index <- sample(c(1:dim(bank.df)[1]), dim(bank.df)[1]*0.6)  
train.df <- bank.df[train.index, ]
valid.df <- bank.df[-train.index, ]

# classification tree
default.ct <- rpart(Personal.Loan ~ ., data = train.df, method = "class")


# plot tree
rpart.plot(default.ct)

prp(default.ct, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10)

# Use rpart.control's maxdepth to Control the depth of tree. Try different values for maxdepth (1,2,3,..)

default.ct <- rpart(Personal.Loan ~ ., data = train.df, control = rpart.control(maxdepth = 3), method = "class")
rpart.plot(default.ct)
prp(default.ct, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10)


# --------------------Fig. 9.9 complete --------------------------------------------- 


#
# ----------- Figure 9.10: A Full Tree using using the Training Data Set

deeper.ct <- rpart(Personal.Loan ~ ., data = train.df, method = "class", cp = 0, minsplit = 1)

# count number of leaves
length(deeper.ct$frame$var[deeper.ct$frame$var == "<leaf>"])
# plot tree
rpart.plot(deeper.ct)
prp(deeper.ct, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10, 
    box.col=ifelse(deeper.ct$frame$var == "<leaf>", 'gray', 'white'))  

## ----------------Fig. 9.10 complete -- Note: plot takes time to make the plot) ------------------


# 
# -------------- Table 9.3: COnfusion Matrices and Accuarcy -------------

# Requires use of libraries forecast, caret and e1071

# For the default (small tree) -------

install.packages(c("forecast", "caret", "e1071"))

library(forecast); library(caret); library(e1071)

# 

# classify records in the "validation data". --> the most important aspect to gage how model would perform!

# set argument type = "class" in predict() to generate predicted class membership.

default.ct.point.pred.train <- predict(default.ct,train.df,type = "class")

# generate confusion matrix for training data

confusionMatrix(default.ct.point.pred.train, as.factor(train.df$Personal.Loan))

### Now, repeat the code for the validation set

default.ct.point.pred.valid <- predict(default.ct,valid.df,type = "class")
# generate confusion matrix for validation data
confusionMatrix(default.ct.point.pred.valid, as.factor(valid.df$Personal.Loan))
## 

## Now, for the Deeper Tree -- Deeper Tree: Training
# 
# Confusion Matrix for the Training Set
deeper.ct.point.pred.train <- predict(deeper.ct,train.df,type = "class")

confusionMatrix(deeper.ct.point.pred.train, as.factor(train.df$Personal.Loan)) # Notice accuracy = 1

# Confusion Matrix for the Validation Set

deeper.ct.point.pred.valid <- predict(deeper.ct,valid.df,type = "class")

confusionMatrix(deeper.ct.point.pred.valid, as.factor(valid.df$Personal.Loan)) # Notice High Accuracy

# Question: Is this the case of overfitting?

# 

#   -------------Table 9.3 complete ------------------------



#

# ---------- Table 9.4: Table of Complexity Parameter (CP) Values and associated Tree Errors

# Code for tabulating tree error as a function of the complexity parameter (CP)

# argument xval refers to the number of folds to use in rpart's built-in
# cross-validation procedure
# argument cp sets the smallest value for the complexity parameter.

cv.ct <- rpart(Personal.Loan ~ ., data = train.df, method = "class", 
               cp = 0.00001, minsplit = 5, xval = 5)

# use printcp() to print the table. 
printcp(cv.ct)
rpart.plot(cv.ct)

#  Table 9.4 completed -----------------------


#
#  -------------  Figure 9.12: Code for pruning the tree

# prune by lower cp

pruned.ct <- prune(cv.ct, 
                   cp = cv.ct$cptable[which.min(cv.ct$cptable[,"xerror"]),"CP"])

length(pruned.ct$frame$var[pruned.ct$frame$var == "<leaf>"])

rpart.plot(pruned.ct)
prp(pruned.ct, type = 1, extra = 1, split.font = 1, varlen = -10)  

# Try pruned tree on Validation data

pruned.ct.pred.valid <- predict(pruned.ct,valid.df,type = "class")

confusionMatrix(pruned.ct.pred.valid, as.factor(valid.df$Personal.Loan)) # Notice  Accuracy

## --------------Fig 9.12 completed ------------------------------


#

# ---------------- Figure 9.13 Best Pruned Tree  -- obtained by full tree to training data, and 
# Pruning it using Cross Validation data and 
#  choosing the smallest tree within onse standard error of the Min XERROR Tree

set.seed(1)
cv.ct <- rpart(Personal.Loan ~ ., data = train.df, method = "class", cp = 0.00001, minsplit = 1, xval = 5)  # minsplit is the minimum number of 
# observations in a node for a split to be attempted. 
# xval is number K of folds in a K-fold cross-validation.

printcp(cv.ct)  # Print out the cp table of cross-validation errors.

# The R-squared for a regression tree is 1 minus rel error. xerror (or relative cross-validation error where 
# "x" stands for "cross") is a scaled version of overall average of the 5 out-of-sample errors across the 5 folds.

# pruned.ct <- prune(cv.ct, cp = 0.0154639)
# pruned.ct <- prune(cv.ct, cp = 0.0169697)
pruned.ct <- prune(cv.ct, cp = 0.0090909)
prp(pruned.ct, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10, 
    box.col=ifelse(pruned.ct$frame$var == "<leaf>", 'gray', 'white')) 

# ------------------------------------------------------------

