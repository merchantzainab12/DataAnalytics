Tayko.df <- read.csv("Tayko.csv", header = TRUE)
View(Tayko.df)

train.index<- sample(c(1:2000),0.6*2000)
train.df <- Tayko.df[train.index,]
valid.df <- Tayko.df[-train.index,]
install.packages("leaps","forecast")
library(leaps)
library(forecast)
search <- regsubsets(Spending ~ ., data = train.df, nbest = 1, nvmax = dim(train.df)[2], method = "exhaustive") 
sum<-summary(search)

tayko.lm.null <- lm(Spending~1, data = train.df)
tayko.lm <- lm(Spending ~ ., data = train.df)
# Use step() to run forward selection
tayko.lm.step.forw <- step(tayko.lm.null, scope = list(lower=tayko.lm.null, upper=tayko.lm), direction = "forward")

tayko.lm.step.back <- step(tayko.lm,direction = "backward")
summary(tayko.lm.step.forw)

tayko.lm.step.both <- step(tayko.lm,direction = "both")
summary(tayko.lm.step.back)
summary(tayko.lm.step.both)

tayko.final <- lm(formula = Spending ~  Freq + last_update_days_ago + Address_is_res + 
                    US + Gender.male + Web.order, data = train.df)
