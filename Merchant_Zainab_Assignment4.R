library(readr)
Cosmetics <- read_csv("Cosmetics.csv")
View(Cosmetics)

install.packages("arules")

library(arules)
library(arulesViz)

cosmetics.mat <- as.matrix(Cosmetics[, -1]) 
cosmetics.trans <- as(cosmetics.mat, "transactions") # converts binary into list format
inspect(cosmetics.trans)

rules <- apriori(cosmetics.trans, 
                 parameter = list(supp= 200/4000, conf = 0.5, target = "rules")) 

inspect(sort(rules, by = "lift"))


#problem 2
library(readr)
titanicdata <- read_csv("titanicdata.csv")
titanic.mat<- as.matrix(titanicdata[,-1])

View(titanic.mat)

titanicdata <- read_csv("titanicdata.csv")
rule_default <-apriori(titanicdata[2:5], appearance = list(default = "lhs", rhs=c("Survived=Yes","Survived=No")))

inspect(rule_default)
rule_param <- apriori(titanicdata[2:5], 
                      parameter=list(minlen=2, supp=0.003, conf=0.8)
                      ,appearance = list(default = "lhs", rhs=c("Survived=Yes","Survived=No")))

inspect(rule_param)


#1
library(readr)
Labs <- read_csv("Labs.csv")
LabsData <- na.exclude(Labs)
Labs.df.norm <- sapply(LabsData[-1], scale)

#2
pairs(LabsData[,2:5])

#3
set.seed(2)
km <- kmeans(Labs.df.norm, 3)

km$cluster

plot(c(0), xaxt = 'n', ylab = "", type = "l", 
     ylim = c(min(km$centers), max(km$centers)), xlim = c(0, 8))

axis(1, at = c(1:8), labels = names(Labs.df.norm))

for (i in c(1:3))
  lines(km$centers[i,], lty = i, lwd = 2, col = ifelse(i %in% c(1, 3, 5),
                                                       "black", "dark grey"))
text(x = 0.5, y = km$centers[, 1], labels = paste("Cluster", c(1:3)))
#4
km$centers

#5
km$cluster[which(Labs$Name=="AstraZeneca PLC")]
