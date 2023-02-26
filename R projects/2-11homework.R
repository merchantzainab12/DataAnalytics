Toyota.df <- read.csv("ToyotaCorolla.csv", header = TRUE)# Load Data
toyota_num <- Toyota.df[,c(3:7)]
heatmap(cor(toyota_num))
toyota_dummy <- model.matrix(~0 + Fuel_Type + Color , data = Toyota.df)
train.rows <- sample(row.names(housing.df), dim(housing.df)[1]*0.6) 