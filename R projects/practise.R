library(readr)
UniversalBank_3_ <- read_csv("UniversalBank.csv")
View(UniversalBank_3_)
library(neuralnet)
personal_loan <- UniversalBank_3_$`Personal Loan`
cbind(UniversalBank_3_,UniversalBank_3_$`Personal Loan`)
new.df <- UniversalBank_3_[,c(-1,-5,-10)]

max_UB <- apply(new.df,2,max)
min_UB <- apply(new.df,2,min)
new_scaled <- scale(new.df, center = min_UB, scale = max_UB - min_UB)

new_scaled <- cbind(new_scaled,personal_loan)
new_df<-as.data.frame(new_scaled)

set.seed(1234) 
train.index <- sample(c(1:dim(new_scaled)[1]), dim(new_scaled)[1]*0.6)  
train.d <- new_scaled[train.index, ]
valid.d <- new_scaled[-train.index, ]
train.d
n2 <- neuralnet(personal_loan ~ Age + Experience + Income, data = train.d, hidden = 2,act.fct = "logistic")
plot(n2)
validation.prediction <- compute(n2,valid.d) # applies the NNW to Validation with SUR_COND_9 and Target variables removed
pv1 <- validation.prediction$net.result # get the probabilities
predv1 <- ifelse(pv1>0.5,1,0) # convert probabilities into classification
tabvl <- table(predv1, new_df[-train.index,]$personal_loan)

confusionMatrix(as.factor(predv1), as.factor(new_df[-train.index,]$personal_loan))
