# Set working directory
# Setwd( ) 

# ------ Install packages and deploy libraries

install.packages("leaps", "forecast")
library(leaps); library(forecast)

# -------------------------------------
# Read dataset
car.df <- read.csv("ToyotaCorolla.csv")

# select variable for regression

selected.var <- c(3,4,7,8,9,10,12,13,14,17,18)

# partition data

set.seed(1) # set seed for reproducing the partition
# use first 1000 rows of data 
train.index <- sample(c(1:1000), 600)
train.df <- car.df[train.index, selected.var]
valid.df <- car.df[-train.index, selected.var]

# use lm() to run a linear regression of Price on all the predictors on the
# training set (it will automatically turn Fuel_Type into dummies).
# use . after ~ to include all the remaining columns in train.df as predictors.

car.lm <- lm(Price ~ ., data = train.df)
# use options () to ensure numbers are not displayed in scientific notation.
options(scipen = 999)
summary(car.lm)

#
# ----------------------------------------------------------------------------
#
#       Table 6.5: Exhaustive Search reducing predictors
#
# -------------------------------------------------------------------------

# 
# Code for best subset

## use regsubsets() in package leaps to run the exhaustive search.

# unlike with lm, categorical predictors must be turned into dummies manually.


# create dummies for fuel type for training data
Fuel_Type <- as.data.frame(model.matrix(~ 0 +Fuel_Type, data=train.df))
# replace Fuel_Type column with 3 dummies
Fuel_Type[1:10,]
dim(Fuel_Type)

# replace Fuel_Type column with dummies
train.df <- cbind(train.df[,-4], Fuel_Type[,])
head(train.df)

# Run Exhaustive search method using regsubsets;  
# nvmax = maximum size of subsets to examin: 12): 
# nbest = number of subsets of each size to record: 1)

search <- regsubsets(Price ~ ., data = train.df, nbest = 1, nvmax = dim(train.df)[2], method = "exhaustive") 
# ignore warning messages

sum <- summary(search)

# show models
sum$which

# show matrices


sum$rsq
sum$adjr2
sum$cp

# Now, Choose appropriate predictors and build the model using "lm" as before using training data and. 
# assess model accuracy using the validation data set (which will require converting / using the dummy variables as below)

#  ---------- create dummies for fuel type for validation data -- (renmove commengts as listed below) -------------
#Fuel_Type_v <- as.data.frame(model.matrix(~ 0 +Fuel_Type, data=valid.df)) # remove comment
# replace Fuel_Type column with 3 dummies
#Fuel_Type_v[1:10,] #  remove comment
#dim(Fuel_Type_v) #remove comment
#valid.df_v <- cbind(valid.df[,-4], Fuel_Type_v[,]) # remove comment
#dim(valid.df_v) # remove comment

# ------------------- Complete creation of dummy variables for the validation dataset --------------


# ---------- Completed illustration of the  Exhaustive Search Method ------------------


# -------------------- Demonstration of Stepwise Regression ----

car.df <- read.csv("ToyotaCorolla.csv") # read the file (again)


# use first 1000 rows of data
# select variable for regression

selected.var <- c(3,4,7,8,9,10,12,13,14,17,18)

# partition data

set.seed(1) # set seed for reproducing the partition
train.index <- sample(c(1:1000), 600)
train.df <- car.df[train.index, selected.var]
valid.df <- car.df[-train.index, selected.var]

# use lm() to run a linear regression of Price on all the predictors on the
# training set (it will automatically turn Fuel_Type into dummies).
# use . after ~ to include all the remaining columns in train.df as predictors.

car.lm <- lm(Price ~ ., data = train.df)
# use options () to ensure numbers are not displayed in scientific notation.
options(scipen = 999)
summary(car.lm)

# 
# ----------------------------------------------------------------------------
#
#       Table 6.6: Backward Elimination for reducing predictors
#
# -------------------------------------------------------------------------


# use step() to run stepwise regression
car.lm.step <-step(car.lm, direction = "backward")
summary(car.lm.step) # which variables did it drop?
# (no need to specify search range)

# ------------ Backward Elimination Accuracy -----------------

car.lm.step.pred <- predict(car.lm.step, valid.df)
accuracy(car.lm.step.pred, valid.df$Price)

# 
# ----------------------------------------------------------------------------
#
#       Table 6.7: Forward Selection   for reducing predictors
#
# -------------------------------------------------------------------------

# 
# use step() to run stepwise regression (available in lm).
# set direction = forward
# Specify the initial model (here, model with no predictors), and the bottom of the # search range (here with no predictors) and the top (here with all predictors)
# create model with no predictors
car.lm.null <- lm(Price~1, data = train.df)

# Use step() to run forward selection
car.lm.step <- step(car.lm.null, scope = list(lower=car.lm.null, upper=car.lm), direction = "forward")

summary(car.lm.step) # which variables were added


# ----------------------------------------------------------------------------
#
#       Table 6.8: Stepwise Regression for Reducing Predictors
#
# -------------------------------------------------------------------------
car.lm.step <- step(car.lm, direction = "both")
summary(car.lm.step)# Which variables were added/dropped?

