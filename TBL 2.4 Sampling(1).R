# random sample of 5 observations
set.seed(123)
s <- sample(row.names(housing.df),5)
housing.df[s,]

# oversample houses with over rows
s <- sample(row.names(housing.df),5, prob = ifelse(housing.df$ROOMS > 10, 0.9, 0.1))
housing.df[s,]
