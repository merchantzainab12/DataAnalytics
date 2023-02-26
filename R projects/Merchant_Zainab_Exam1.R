library(readr)
Dataset.df <- read_csv("EX1-Dataset-1.csv")
View(Dataset.df)
#1A)
cat_level <- as.factor(Dataset.df$CAT)
levels(cat_level) 

#1B)
summary(Dataset.df)

#1C)
cor(Dataset.df[c(1,2,3,4)])

#############################3

#2A)
boxplot(Dataset.df$Target, xlab="Target")
#2b)
library(GGally)
ggpairs(Dataset.df[, c(1:4)])


#############################

#3A)
set.seed(1234)
#3B
train.index <- sample(row.names(Dataset.df), dim(Dataset.df)[1]*0.6) 
train.df <- Dataset.df[train.index,]
valid.index<- setdiff(row.names(Dataset.df), train.index)
valid.df <- Dataset.df[valid.index,]

#3C
mean(train.df$Target)
mean(valid.df$Target)

##################################
#4A)
dataset.lm <- lm(Target ~ ., data = train.df)
summary(dataset.lm)

#4B)

library(forecast)

dataset.lm.pred <- predict(dataset.lm, valid.df)
all.residuals <- valid.df$Target - dataset.lm.pred
accuracy(dataset.lm.pred, valid.df$Target)
length(all.residuals[which(all.residuals > -1406 & all.residuals < 1406)])/400
hist(all.residuals, breaks = 25, xlab = "Residuals", main ="")

#4C
new.data <- read.csv("Unknown.csv")
pred <- predict(dataset.lm, newdata = new.data)
pred
