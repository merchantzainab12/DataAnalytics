# Set (wd)

# A. Install Packages

install.packages("caret", dependencies =c("Depends", "Suggests"))
install.packages("rpart")
install.packages("rpart.plot")
install.packages("forecast")
install.packages("gains")
library(caret); library(rpart); library(rpart.plot); library(forecast); library(gains)

#B.  Read Data File into a Data frame and label it as myrtdata.df

myrtdata.df <- read.csv("BalanceData.csv")

# C. Partition: By setting the random seed to 1, we will generate the same partitions 
# Create partitions 70 - 30 split

set.seed(1)
trainr.index <- sample(c(1:dim(myrtdata.df)[1]), dim(myrtdata.df)[1]*0.7)  
trainr.df <- myrtdata.df[trainr.index, ]
validr.df <- myrtdata.df[-trainr.index, ]

# D. 
# Use the rpart function to generate the default classification tree labeled "default_tree"
# Within the rpart function, specify the model structure, data source, and method.
# The method = "anova" option tells the function to build a regression tree to estimate a numerical target value
# To view the details of the cross-validation results, use the summary function
# To ensure consistency of the cross-validation results, use the set.seed function to mix the random seed to 1

set.seed(1)
default_rtree <- rpart(Balance ~ ., data = trainr.df, method = "anova")
summary(default_rtree)

# E.
# To view the regression tree visually, use the prp function(# The type option is set equal to 1 so that all nodes 
# except the leaf nodes are labeled in the tree diagram
# The extra option is set equal to 1 so that the number of observations that fall into each node are displayed.
# The under option is set equal to TRUE in order to put the number of cases under each decision node in the diagram

options(scipen=999)

prp(default_rtree, type=1, extra = 1, under = TRUE)# Here, use "one" for type, and not "L"

# 
# To find the optimal decision tree, a common practice is to grow the full tree and then prune it to a less-complex tree
# based on the prediction errors produced by the cross-validation process of the rpart function.
# By identifying the value of the complexity parameter (cp) associated with the smallest cross-validated prediction error,
# we can create the minimum error tree.
# Let us now examine the pruning process to optimize the complexity of the tree

# F.
# First grow the full tree by using the rpart function
# For this, we set the options cp equals 0. minsplit equal to 2 and minbucket equal to 1
# These settings ensure that the largest possible tree will be produced
# Then plot the full tree using  the prp function
# Again, to ensure consistency of the cross-validation results, set the random seed to 1


set.seed(1)
full_rtree <- rpart(Balance ~ ., data = trainr.df, method = "anova", cp = 0, minsplit = 2, minbucket =1)
prp(full_rtree, type=1, extra=1, under=TRUE)

# The full tree will be complex and may take time to see the output

# G.
# To identify the value of cp that is associated with the smallest cross-validate prediction error, use the printcp function

printcp(full_rtree)

# Due to the complexity of the full tree, many (> 200 for this example) would be displayed
# The column namess are: cp, nsplit, relerror, xerror, and xstd
# The nsplit column shows the number of split for each tree
# The rel error column shows the number of splits for each tree, relative to the prediction error of the root node if all cases
# are given the predicted balance that equals the average of all balances.
# The prediction performance of the trees needs to be evaluated by inspecting the cross-validation errors associated with 
# each tree -- -- see the xerror column
# the xstd column (standard error) can be used to identify the best-pruned tree, which is the smallest tree whose
# cross-validation error falls within one standard error of the minimum error tree.
# Next, use the value of cp associated with the second tree to produce "best-pruned" tree.

# H.
# Use the prune function to create the best-pruned tree by using the cp value associated as described above.
# To avoid issues caused by rounding, use a cp value that is slightly higher than the cp value associated with the best 
# pruned tree as above, but lower than that of the next smaller tree.

pruned_rtree <- prune(full_rtree, cp=0.0474651940645)
prp(pruned_rtree, type=1, extra=1, under = TRUE)

# Review above tree for rules

# I.
# To predict the average balances of the observations in the validation data set use the predict function:

predicted_value <- predict(pruned_rtree, validr.df)

#J.
# To evaluate the performance our regression tree, compute the performance measures for prediction using the "accuracy" function.
# The accuracy function requires two arguments: Predicted values and actual values


accuracy(predicted_value, validr.df$Balance)

# This will produce various performance measures: ME, RMSE, MAE, MPE, MAPE 

# K.
# Finally, to score New Data -- inport the data into a dataframe
# use the predict function to produce the prediced average for the new cases using the regression tree as below

Data_toscore <- read.csv("BalanceScore.csv")
predicted_values <- predict(pruned_rtree, Data_toscore)  
predicted_values

# Review the results -- 
# Question: which variables have most impact?

# Now, one can create Lift and Decile Charts

#  




