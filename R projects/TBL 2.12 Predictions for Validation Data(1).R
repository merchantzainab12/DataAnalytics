# --------------------------------
#             Table 2.12


#Table 2.12 - Predictions on validation data
pred <- predict(reg, newdata = valid.data)
vl.res <- data.frame(valid.data$TOTAL.VALUE, pred, residuals = valid.data$TOTAL.VALUE - pred)
head(vl.res)

# -------------------------------------------------