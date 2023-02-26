#Q1
library(readr)
BH2data <- read_csv("BH2data.csv")
View(BH2data)

#a 
mean(BH2data$AGE) #68.4246
min(BH2data$NOX) #0.385

#b
hist(BH2data$`CAT. MEDV`, main = "Histogram of CAT.MEDV", xlab = "CAT.MEDV")

#c
set.seed(1234)
train.index <- sample(c(1:dim(BH2data)[1]), dim(BH2data)[1]*0.6)  
train.df <- BH2data[train.index,]
valid.df <- BH2data[-train.index,]

#d
library(rpart); library(rpart.plot)
full.ct <- rpart(`CAT. MEDV` ~ ., data = train.df, method = "class", cp = 0, minsplit = 2, minbucket=1)
rpart.plot(full.ct)

#e
printcp(full.ct)
# cp =0.06
pruned.ct <- prune(full.ct,cp = 0.06)
rpart.plot(pruned.ct)

#f
library(caret)
pruned.ct.pred.valid <- predict(pruned.ct,valid.df,type = "prob")
pred.valid<-ifelse(pruned.ct.pred.valid[,2]>0.5,1,0)
confusionMatrix(as.factor(pred.valid), as.factor(valid.df$`CAT. MEDV`))

#g
newdata <- read_csv("newBH2data1.csv")
View(newdata)
pruned.ct.pred.new <- predict(pruned.ct,newdata,type = "prob")
pred_probability<-pruned.ct.pred.new[,2] #0.877
pruned.ct.pred.new <- ifelse(pruned.ct.pred.new[,2]>0.5,1,0)
pruned.ct.pred.new



#Q2
library(readr)
Cereals <- read_csv("Cereals.csv")
View(Cereals)
Cereals.df <- Cereals[,-1] #removing names column

# normalized distance:
Cereals.df.norm <- sapply(Cereals.df, scale)

# run kmeans algorithm 
set.seed(2)
km <- kmeans(Cereals.df.norm, 4)

#see cluster memberships
memb<-km$cluster
data_with_memb <-cbind(Cereals,memb)
View(data_with_memb)

# Find cluster memberships
km$cluster[which(Cereals$name=="Trix")]
km$cluster[which(Cereals$name=="All-Bran")]
km$cluster[which(Cereals$name=="Corn_Flakes")]

# Find size of each cluster
km$size
