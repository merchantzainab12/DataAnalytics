
# Chapter - 14 "Association Rules"

# setwd()

# install packages and Load Libraries

install.packages("arules")

library(arules)
library(arulesViz)

#  --------------   Table 14.5  -------------------------

# Exercise: Convert the fifty random transactions into binary forlat.
# Below, we use the data that is already converted in teh binary format.

random50.df <- read.csv("fiftytransactions.csv")
View(random50.df)

class(random50.df) # This shows that this is a data frame
str(random50.df) # This will show the binary version 


# remove first column and convert data frame to matrix
random50.mat <- as.matrix(random50.df[, -1])
View(random50.mat)


#  convert the binary incidence matrix into a transactions database

random.trans <- as(random50.mat, "transactions") # converts binary into list format
inspect(random.trans)

# plot data (Not in Shmueli Book)
itemFrequencyPlot(random.trans) # creates bar charts for item frequency


# Run apriori algorithm (function)

# Specify parameters in a list format
# Change the support by changing supp = fraction, and confidence
# set target = 'rules'

rules <- apriori(random.trans) # Notice that "0" rules are returned.
rules <- apriori(random.trans, 
                 parameter = list(supp= 2/50, conf = 0.7, target = "rules"))

# Support could also be expressed as a fraction - as below
rules <- apriori(random.trans, 
                 parameter = list(supp= 0.04, conf = 0.7, target = "rules"))

# 54 rules are generated

# inspect rules
inspect(sort(rules, by = "lift"))

# -----------------------------------------------------------------

