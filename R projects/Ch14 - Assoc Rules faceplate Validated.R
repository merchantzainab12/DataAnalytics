
# Chapter - 14 "Association Rules"

# setwd()

# install packages and Load Libraries

install.packages("arules")

library(arules)
library(arulesViz)

#  --------------   Table 14.4  -------------------------

fp.df <- read.csv("Faceplate.csv")

class(fp.df) # This shows that this is a data frame
str(fp.df) # This will show the binary version of the faceplate data by column names

View(fp.df)

# remove first column and convert data frame to matrix
fp.mat <- as.matrix(fp.df[, -1])
View(fp.mat)

# convert the binary incidence matrix into a transactions database (list format)
fp.trans <- as(fp.mat, "transactions") # This converts to "transaction" class (e.g., binary matrix or data frame)
fp.trans
inspect(fp.trans) # This shows the data in list format
fp.trans@itemInfo # provides the items in the sataset -- there are 6 items

## get rules
# when running apriori(), include the minimum support, minimum confidence, and target as arguments. 
rules <- apriori(fp.trans)
rules <- apriori(fp.trans, parameter = list(supp = 0.2, conf = 0.5, target = "rules")) # --> generates 18 rules
inspect(rules)
inspect(head(sort(rules, by = "lift"), n = 6))

# plot(rules) -- not in Shmueli book

plot(rules)
# inspect the first six rules, sorted by their lift
inspect(head(sort(rules, by = "lift"), n = 6))

# -----------------Table 14.6 completed ------------------------

#  ---------------Practice Exercises --------------------------

##   Practice Class Exercises -- Try to find rules with various values of support and confidence
##   Summarize results / findings


# -----------------------------------------------------------------

