# setwd()

# Install libraries neuralnet and MASS (if you need to try various datasets)
install.packages("neuralnet")
library(neuralnet)

####################################################################

###  Regression Model using Neural Network using R       

###################################################################

# Dataset: BostonHousing

set.seed (1234)

bostondata.df <- read.csv("BostonHousing.csv")
str(bostondata.df)
# Normalize Data

max_boston <- apply(bostondata.df,2,max)
min_boston <- apply(bostondata.df,2,min)
boston_scaled.df <- scale(bostondata.df, center = min_boston, scale = max_boston - min_boston)

max_boston
min_boston
head(boston_scaled.df)

# since we are building a regression model, we should not be using the "CAT..MEDV"
# so, remove the variable "CAT..MEDV"

boston_scaled.df <- boston_scaled.df[, - 14] # removes the variable CAT.MEDV
head(boston_scaled.df)

# Sample Data

index = sample(1:nrow(bostondata.df), round(0.7*nrow(bostondata.df)))
train_boston <- as.data.frame(boston_scaled.df[index,])
valid_boston <- as.data.frame(boston_scaled.df[-index,])

dim(train_boston) # Note that the above sampling method used "rounding"
dim(valid_boston) 

head(train_boston)
head(valid_boston)

data_names <- names(boston_scaled.df)

# Build an NNW model and plot it 

boston_nw1 <- neuralnet(MEDV ~ ., data = train_boston, hidden = 10, linear.output = T)
plot(boston_nw1)

# Apply NNW model to teh validation data

predict_boston <- compute(boston_nw1, valid_boston)

predict_boston$net.result # Shows the outputs -- but these are in scaled units

# To find the accuracy of the model, # We need to convert it back to the original units

# We need to multiply the results by [Max - Min]and then add Min


multiplier <-max(bostondata.df$MEDV) - min(bostondata.df$MEDV)

# test that this works.



# Now, apply the multiplier to the output of predict_boston$net.result

predicted_valid_boston <- predict_boston$net.result * multiplier + min(bostondata.df$MEDV)

predicted_valid_boston # These are the predicted MEDVs for the validation data

# Now apply the same for the scaled validation data

valid_boston_MEDV <- as.data.frame((valid_boston$MEDV)*multiplier + min(bostondata.df$MEDV))

valid_boston_MEDV

# Now, find the MSE (sum of square of errors/no of observation)

MSE_valid_boston_nnw <- sum((valid_boston_MEDV - predicted_valid_boston)^2)/nrow(valid_boston_MEDV)
MSE_valid_boston_nnw

# This is the error using Neural Network.

## --------- Run linear regression model -----(in scaled or original units)------
# Here, we use the scaled data

boston_reg <- lm(MEDV ~., data = train_boston)
summary(boston_reg)

predicted_valid_boston_lm <- predict(boston_reg, valid_boston)

# Convert back to original units using the multiplier

predicted_valid_boston_reg <- predicted_valid_boston_lm * multiplier + min(bostondata.df$MEDV)

predicted_valid_boston_reg # These are the predicted MEDVs for the validation data

# We have already converted the valid data to original units earlier; so, we will use that

# Now, to find the MSE


MSE_valid_boston_reg <- sum((valid_boston_MEDV - predicted_valid_boston_lm)^2)/nrow(valid_boston_MEDV)


MSE_valid_boston_reg

# So, we can now compare the NNW and Regression Models

MSE_valid_boston_nnw
MSE_valid_boston_reg

# Notice that the NNW provides higher accuracy!
-------------------------------------------------------------
# Suggested Exercises:
# 1. Try NNW models with different number of nodes
# 2. Build a Regression Tree model -- what are the top 4 variables to use?
# 3. Use these 4 variables to build a linear regression model
# 4. Use these 4 variables to build a Neural Network regression model
# 5. Compare the NNW, CART, and Linear Regression models -- 
# 4. Which is the best model for deployment? Explain Why?




























