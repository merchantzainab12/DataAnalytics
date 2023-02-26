## --------------------------------
#            Table 2.14 
# --------------------------
# Read the new.data file or recreate using a data frame. OR try to create a sheet using R itsef
#
new.data <- read.csv("new.data.csv")
View(new.data)

pred <- predict(reg, newdata = new.data)
pred


##  -----------------   End of 2.14 ------------------ 