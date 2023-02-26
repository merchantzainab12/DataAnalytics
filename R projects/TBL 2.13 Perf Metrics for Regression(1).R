# -------------------------------------------------

##Table 2.13
install.packages('forecast', dependencies = TRUE)
library(forecast)
# compute accuracy on training set
accuracy(reg$fitted.values, train.data$TOTAL.VALUE)
# compute accuracy on prediction set
pred <- predict(reg, newdata = valid.data)
accuracy(pred, valid.data$TOTAL.VALUE)

## --------------------------------