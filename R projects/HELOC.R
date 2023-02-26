# AN Example -- Building a Classification Tree ----
# -- R implements a k-fold cross-validation process for pruning decision tree using two-way partitioning 
# (e.g., only training and validation data)

# --------     Install packages and and invoke libraries ------------

install.packages("caret")
install.packages("gains")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("pROC")
library(caret); library(gains);library(rpart); library(rpart.plot); library(pROC)

# ---- Read the data ------
mydata.df <- read.csv("HELOC.csv")
str(mydata.df)

# R requires that the target variable be a factor variable, a categorical data type
# Use the as.factor command to convert the target variable into a categorical type

mydata.df$HELOC <- as.factor(mydata.df$HELOC)
str(mydata.df)

# Use the set.seed command to generate same partitions every time data is sampled.
# We use set.seed(1) here

set.seed(1)

# --- Partition the data 70-30 -----------

train.index <- sample(c(1:dim(mydata.df)[1]), dim(mydata.df)[1]*0.7)  
train.df <- mydata.df[train.index, ]
valid.df <- mydata.df[-train.index, ]

# The "rpart" function is used to generate the default classification tree.
# Within the rpart function, we specify the model structure, data source, and method.
# The method option is set to "class" for developing a classification tree
# To view the details about the default tree, use the "summary" function
# Because R uses the cross-validation method for pruning the tree, to ensure consistency of the
# cross-validation results, we use the set.seed function to set a random seed of 1

set.seed(1)

default_tree <- rpart(HELOC ~ ., data = train.df, method = "class")
summary(default_tree)

# F
# To view the classification tree visually, use the "prp" function.
# The "type" option is set equal to 1 so that all nodes except the leaf nodes are labeled in the tree diagram
# The "extra" option is set equal to 1 so that the number of observations that fall into each node are displayed
# The "under" option is set equal to TRUE to put the number of cases under each decision node in the diagram:

options(scipen=999)# to avoid scientific notation

prp(default_tree, type = 1, extra = 1, under = TRUE)

# Observe carefully look at the default classification tree. Notice that the first decision node is on
# the Sex variable, followed by Age and Income splits --

# How are the number of splits determined in the default classification tree?
# The rpart function uses the complexity parameter "cp" to determine when to stop growing the tree.
# If the cost of adding another split to the tree exceeds the vale of cp, then the tree growth will not continue.
# The default cp value for the rpart function is 0.01.

# However, in most cases, it is very difficult toknow which cp value will produce the best-performance
# beforehand.  Therefore,  a common practice is to grow the full tree and then prune it to a less complex
# tree based on the classification errors produced by a a built-in cross-validation process of the
# rpart function.

# By identifying the value of cp associated with the smallest cross-validated classification error, 
# we can create the minimum-error tree.

# Alternatively, we can produce the "best-pruned" tree which is the smalles tree with an error rate that is within 
# one standard deviation of the minimum error rate.

# ------ Demonstration of the pruning process to optimize the complexity of the tree ----------

# G
# We first grow the full tree by using the rpart function.
# We set the options cp = 0, minsplit = 2, and minbucket = 1
# The minsplit option specifies the minimum number of observations in the parent node that can be split further
# The minbucket option specifies the minimum number of observations that are allowed in the leaf node
# These settings ensure that the largest possible tree will be produced.
# We plot the full tree using the prpr function.
# Again, to ensure consistency of cross-validation results, we specify a random seed of 1 using the set.see function


set.seed(1)
full_tree <- rpart(HELOC ~ ., data = train.df, method = "class", cp=0, minsplit = 2, minbucket = 1)
prp(full_tree, type = 1, extra = 1, under = TRUE)

# H
# To identify the value of cp that is associated with the smallest cross-validated classification error, 
# we use the printcp function to display the complexity parameter

printcp(full_tree)

# Observe the resulting complexity parameter table.
# The Cp column shows complexity parameter in decreasing order
# The nsplit column shows the number of splits for each tree
# The number of leaf nodes for each tree = nsplit + 1
# The rel error column shows the fraction of misclassified cases for each tree relative to the fraction
# of misclassified cases in the root node if all classes are classified into the predominant class
# Notice that the last tree has a relative errors of 0 because it is fully grown tree whose leaf nodes
# only contain cases that belong to the same class; therefore, there is no misclassified cases.
# The xerror column shows the cross-validation errors associated with each candidate tree.
# It is the recommended measure for identifying the tree that can potentially perform well on new data sets
# Notice that cross-validation errors decrease initially as the classification tree becomes more
# complex and then increases after a certain point. 
# This is common and indicative of of the overfitting problems with complex tree models
# The tree which has the lowest cross-validation error is the "minimum error tree"
# The xstd column can be used to identify the "best-pruned" tree, which is the smallest tree with an
# error that is within one standard error of the minimum-error tree.



# I.
# Use the "prune" function to create the pruned tree by using the cp value associated with the 
# best pruned or minimum error tree (or middle)
# Note that the cp values provided in R results are rounded to seven digits after the decimal points.
# TO rnsure that we DO NOT use a cp value that is less than the actual cp value, we use a cp value
# that is slightly larger than the cp value displayed in the table but lower than the cp value for the
# next smaller tree.

# Now, display the pruned tree using the prp function


pruned_tree <- prune(full_tree, cp=0.0215054)
# pruned_tree <- prune(full_tree, cp=0.0122449)
prp(pruned_tree, type = 1, extra = 1, under = TRUE)

# The figure displays the pruned tree. Note that this is a much simpler tree with fewer branches, compared to the default tree

# J
# Now, predict the class memberships of the observations in the validation dataset 
# using "predict" function. We set the "type" option to "class" so that the class membership
#is produced instead of probability

predicted_class <- predict(pruned_tree, valid.df, type = "class")

#K
# The confusion matrix can be created by comparing the predicted class memberships and 
# actual class memberships of the validation data set. We use the confusioMatrix
# function to produce the confusion matrix and various performance measures.
# The " positive = "1" option specifies 1 as the target class 

confusionMatrix(predicted_class, valid.df$HELOC, positive = "1")

# Review the resulting confusion matrix. It shows results using the default cutoff rate of 0.5

# In this particular example, the default cutoff value is much higher than the proportion of target
# cases in the data set, which is 0.26
# By lowering the cutoff value to be close to the actual class distribution, we will be able to
# classify more cases into the target class and improve the sensitivity measure.

# L

# To evaluate the predictive performance of the classification model using a different cutoff value 
# in R, we first need to compute the probability of each validation case belonging to the target class
# instead of its class membership.
# For this, in the predict function, set the type option equal to "prob" to predict the probability values


predicted_prob <- predict(pruned_tree, valid.df, type = "prob")
head(predicted_prob)

# the head() will display a few columns -- the first column lists the probabilities belonging 
# to the Class 0 and the second column lists probabilities of cases belonging to Class 1
# To determine the class memberhips of cases using a cutoff value other than the default value of 0.5,
# we compare the values in the second column to the new cutoff value.

# M
# To construct a confusion matrix using the new cutoff value of 0.26, we use the "ifelse" function to determine the
# class memberships. We use the "as.factor" function to convert the class membership to factor,
# which is the same data type as the target variable.

confusionMatrix(as.factor(ifelse(predicted_prob[,2]>0.26, '1', '0')), valid.df$HELOC, positive = "1")


# The resulting confusion matrix provides the performance measures of the pruned decision tree using
# the cutoff value of 0.26. It should have a higher sensitivity value (Check)

# To evaluate model performance independent of the cutoff value, we now examine the 
# Cumulative Lift Chart, Decile Charts, and the ROC curve

# N
# First convert the the target variable t0 a numerical data type as required by the gains package
# For this, use the as.numeric function.
# Generate the cumulative lift using the "gains" function. It requires two inputs: Actual class 
# memberships and predicted target class probabilities
# Since the class of interest is "1", we need to refer to column two for the predicted target class probabilities. 
# 

valid.df$HELOC <- as.numeric(as.character(valid.df$HELOC))
gains_table <- gains(valid.df$HELOC, predicted_prob[,2])
gains_table


# Note that gains function generates few groups instead of the default 10 groups,
  # one for each unique probability value

# Lift Chart
plot(c(0, gains_table$cume.pct.of.total*sum(valid.df$HELOC)) ~ c(0, gains_table$cume.obs), 
     xlab = "# of cases", ylab = "Cumulative", main = "Lift Chart", type = "l")  # Why is there error in the last lift chart?? Type "L" and not "1"

# Decile Chart 

barplot(gains_table$mean.resp/mean(valid.df$HELOC), names.arg = gains_table$depth, xlab="Percentils", ylab = "Lift", 
        ylim=c(0,3), main = "Decile-Wise Lift CHart")

# ROC chart
roc_object <- roc(valid.df$HELOC, predicted_prob[,2])
plot.roc(roc_object)
auc(roc_object)

# Apply model to new data
# Use the "predict" function to produce the predicted class memberships and probabilities for the new cases

myScoreData <- read.csv("HELOC_Score.csv")
predicted_class_score <- predict(pruned_tree, myScoreData, type = "class")
predicted_class_score

predicted_class_prob <- predict(pruned_tree, myScoreData, type = "prob")
predicted_class_prob

# End of Classification Tree Example -------
