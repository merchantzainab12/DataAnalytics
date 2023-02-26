


#### Table 5.5
library(caret)
library(e1071)

owner.df <- read.csv("ownerExample.csv",stringsAsFactors=TRUE)

confusionMatrix(as.factor(ifelse(owner.df$Probability>0.5, 'owner', 'nonowner')), 
                owner.df$Class)
confusionMatrix(as.factor(ifelse(owner.df$Probability>0.25, 'owner', 'nonowner')), 
                owner.df$Class)
confusionMatrix(as.factor(ifelse(owner.df$Probability>0.75, 'owner', 'nonowner')), 
                owner.df$Class)

## ----------Validated ---------------

#### Figure 5.4

# replace data.frame with your own
df <- read.csv("liftExample.csv")


# create empty accuracy table
accT = c() 

# compute accuracy per cutoff
for (cut in seq(0,1,0.1)){
  cm <- confusionMatrix(as.factor(1 * (df$prob > cut)), as.factor(df$actual))
  accT = c(accT, cm$overall[1])
}

# You may get some warning messages in execution of above code

# plot accuracy
plot(accT ~ seq(0,1,0.1), xlab = "Cutoff Value", ylab = "", type = "l", ylim = c(0, 1))
lines(1-accT ~ seq(0,1,0.1), type = "l", lty = 2)
legend("topright",  c("accuracy", "overall error"), lty = c(1, 2), merge = TRUE)

## -----------Validated --------------------------------

#### Figure 5.5

library(pROC)
r <- roc(df$actual, df$prob)
plot.roc(r)

# compute auc
auc(r)

## --------Validated -----------------

#### Figure 5.6

# first option with 'caret' library:
library(caret)
df <- read.csv("liftExample.csv")
lift.example <- lift(relevel(as.factor(actual), ref="1") ~ prob, data = df)
xyplot(lift.example, plot = "gain")

# Second option with 'gains' library:
library(gains)
df <- read.csv("liftExample.csv")
gain <- gains(df$actual, df$prob, groups=dim(df)[1])
plot(c(0, gain$cume.pct.of.total*sum(df$actual)) ~ c(0, gain$cume.obs), 
     xlab = "# cases", ylab = "Cumulative", type="l")
lines(c(0,sum(df$actual))~c(0,dim(df)[1]), col="gray", lty=2)

## ------------- Validated -------------------------


#### Figure 5.7

# use gains() to compute deciles. 
# when using the caret package, deciles must be computed manually. 

gain <- gains(df$actual, df$prob)
barplot(gain$mean.resp / mean(df$actual), names.arg = gain$depth, xlab = "Percentile", 
        ylab = "Mean Response", main = "Decile-wise lift chart")

# ----- Validated ------------------------ 

