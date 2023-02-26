##Table 2.7
#convert a few entries for bedrooms to NA's. 
#Then we impute these missing values using the median of the remaining values.
rows.to.missing <- sample(row.names(housing.df), 10)
housing.df[rows.to.missing,"BEDROOMS"] <- NA
summary(housing.df$BEDROOMS) # Now we have 10 NA's

# replace the missing values using the median of the remaining values.
# use median() with na.rm = TRUE to ignore missing values when computing the median.
housing.df[rows.to.missing,]$BEDROOMS <- median(housing.df$BEDROOMS, na.rm = TRUE)
summary(housing.df$BEDROOMS)
# -------------------------------------------