housing.df <- read.csv("WestRoxbury.csv", header = TRUE)# Load Data
dim(housing.df) # find the dimension of data frame
head(housing.df) # show the first six rows
View(housing.df) # show all the data in a new tab

# Practice showing different subsets of the data
housing.df[1:10,1] # show the first 10 rows of the first column only
housing.df[1:10,] # show the first 10 rows of each of the columns
housing.df[5,1:10] # show the fifth row of the first 10 columns

housing.df[5, c(1:2, 4, 8:10)] # show the fifth row of some columns
housing.df[,1] # show the whole first column
housing.df$TOTAL.VALUE # a different way to show the whole first column
length(housing.df$TOTAL.VALUE) # find the length of the first column
mean(housing.df$TOTAL.VALUE) # find the mean of the first column
summary(housing.df)# find summary statistics for each column

# Notice that "Remodel" is read as a character.
# To read it as a factor, set stringsAsFactors=TRUE while reading the data

housing.df <- read.csv("WestRoxbury.csv", header = TRUE, stringsAsFactors=TRUE)# Load Data
summary(housing.df)# find summary statistics for each column
# Notice the output -- Remodel has now 3 categories: None, Old, and Recent
