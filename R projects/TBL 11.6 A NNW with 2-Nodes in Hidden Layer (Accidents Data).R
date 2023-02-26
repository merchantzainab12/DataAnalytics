# setwd()

#    Table 11.6: A Neural Network with Two Nodes in the Hidden Layer (Accidents Data)
##
# #

# library(neuralnet,nnet,caret, e1071)

library(neuralnet)
library(nnet) ## nnet () not used here as it does not support multilayer networks
library(caret)
library(e1071)

# Read accidents data
accidentsnn.df <- read.csv("accidentsnn.csv")

View(accidentsnn.df)

# Explore Data
#
dim(accidentsnn.df) # 5 columns
summary(accidentsnn.df) # What can you surmise from this summary?
class(accidentsnn.df)
names(accidentsnn.df)
head(accidentsnn.df)
tail(accidentsnn.df)
names(accidentsnn.df)
t(t(names(accidentsnn.df)))
class(accidentsnn.df$MAX_SEV_IR) # Outcome Variable -- Note that its int and would need to be converted into dummy varaibles
class(accidentsnn.df$SUR_COND) # Input Predictor -- with different surface conditions; would need to create dummy variables
levels(accidentsnn.df$MAX_SEV_IR)
#
#---------- Data Visualization---------------

# # What visualizations would have been helpful? Try some appropriate visualizations and see if you get some insights ...
# 
# Note that the data is not scaled in this example.

# selected variables ()
vars <- c("ALCHL_I", "PROFIL_I_R", "VEH_INVL") # These are the predictors
c(vars)

# Partition the Data
# 
set.seed(1024)
training=sample(row.names(accidentsnn.df), dim(accidentsnn.df)[1]*0.6)
validation=setdiff(row.names(accidentsnn.df),training)


# dim(training) and dim(validation) will be NULL --> These are just indices

# when output y has multiple classes, they will need to be converted to dummy variables
# The variables "Attributes" SUR_COND and MAX_SEV_IR are converted to dummy variables

trainData <-cbind(accidentsnn.df[training,c(vars)],
                  class.ind(accidentsnn.df[training,]$SUR_COND),
                  class.ind(accidentsnn.df[training,]$MAX_SEV_IR))

names(trainData) <- c(vars, paste("SUR_COND_", c(1, 2, 3, 4, 9),sep = ""), paste("MAX_SEV_IR_", c(0,1,2), sep = ""))
                      
names(trainData)
dim(trainData) # 11 columns -- This is because data is augmented with additional dummy variable for SUR_COND and MAX_SEV_IR

# Notice that the paste command adds "suffix" to the variable names.

# Similarly, do the same or the validation as well

validData <- cbind(accidentsnn.df[validation,c(vars)],
                   class.ind(accidentsnn.df[validation,]$SUR_COND),
                   class.ind(accidentsnn.df[validation,]$MAX_SEV_IR))
                   
                      
# names(validData) <- c(vars, paste("SUR_COND_", c(1, 2, 3, 4, 9), sep = ""), paste("MAX_SEV_IR_", c(0,1,2), sep = ""))

names(validData) <- c(vars, 
                      paste("SUR_COND_", c(1, 2, 3, 4, 9), sep=""), paste("MAX_SEV_IR_", c(0, 1, 2), sep=""))


# --- If above gives error, change the value in set.seed () ---

names(validData)

## run nn with 2 hidden nodes

# use hidden - with a vector of integers specifying number of hidden nodes in each layer

nn <- neuralnet(MAX_SEV_IR_0+MAX_SEV_IR_1+MAX_SEV_IR_2 ~ 
                  ALCHL_I + PROFIL_I_R + VEH_INVL + SUR_COND_1 +SUR_COND_2
                + SUR_COND_3 + SUR_COND_4, data = trainData, hidden = 2)    

# Plot the Neural Network 
plot(nn, rep = "best") # Review the Neural Network

# Print the  Weights
nn$weights
# Display predictions
prediction(nn)

# Now, use this neural net to predict -- and see how well it fits the training and validation data
# Note that traindata has several dummy variables

training.prediction <- compute(nn,trainData[,-c(8:11)]) # applies the NNW to training data; Note that SUR_COND_9 and Target variables are not used

training.class <- apply(training.prediction$net.result,1,which.max)-1 # 

confusionMatrix(as.factor(training.class),as.factor(accidentsnn.df[training,]$MAX_SEV_IR))

# Now, predict using validation dataset

validation.prediction <- compute(nn,validData[,-c(8:11)]) # applies the NNW to Validation with SUR_COND_9 and Target variables removed

validation.class <- apply(validation.prediction$net.result,1,which.max)-1

confusionMatrix(as.factor(validation.class), as.factor(accidentsnn.df[validation,]$MAX_SEV_IR))

## -- End of 11.6 exercise

###
# Table 11.7 : Confusion Matrix for NNW for accident data, with 2 nodes in hidden layer
###
confusionMatrix(as.factor(training.class),as.factor(accidentsnn.df[training,]$MAX_SEV_IR))
confusionMatrix(as.factor(validation.class), as.factor(accidentsnn.df[validation,]$MAX_SEV_IR))

## End of Table 11.7 

####------- Suggested Exercises--------------
# 1. Create a NNW with three nodes and compare the confusion matrix

# 2. Create a new observation and score it using this Neural Network

# 3. Which, if any, data have been scaled in this dataset?
