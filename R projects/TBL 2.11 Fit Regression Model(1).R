# ----------------------------------------------------------

#              Table 2.11  

##Table 2.11 - Fit regression model on training data
reg <- lm(TOTAL.VALUE ~ .-TAX, data = housing.df, subset = train.rows) # removed Tax variable
tr.res <- data.frame(train.data$TOTAL.VALUE, reg$fitted.values, reg$residuals)
head(tr.res)

# --------------------------------