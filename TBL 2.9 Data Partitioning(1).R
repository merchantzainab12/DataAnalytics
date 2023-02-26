# ----------------------------------------------
##               Table 2.9 Data Partitioning in R
# ----------------------------------------------
set.seed(1)

# -------------------------------------------------------------
## 1. Partitioning into training (60%) and validation (40%)
# randomly sample 60% of the row IDs for training; the remaining 40% serve as validation
# Note that dim(housing.df)[1] will give the number of rows in the dataset

train.rows <- sample(row.names(housing.df), dim(housing.df)[1]*0.6) 

# Note that instead, one could use nrow(housing.df) as well. As below.
# train.rows2 <- sample(row.names(housing.df), nrow(housing.df)*0.6)

# collect alll the columns with training rows ID in to training set.

train.data <- housing.df[train.rows,] 
dim(train.data)

# assign row IDs that are not already in the training set, into validation

valid.rows <- setdiff(rownames(housing.df), train.rows)
valid.data <- housing.df[valid.rows,]
dim(valid.data)

# alternative code for vaidation (works only when row names are numeric):
# collect all the columns without train rows ID into validation set
# valid.data <- housing.df[-train.rows,] # This will not work as the row names are given/non-numeric

# Create Data Frames from this dataset

train.data60.df <- train.data
valid.data40.df <- valid.data

# Named these as 60 40 to indicate that partitioned size. Below, we are going to split data in 50-30-20 proportion

# ----------------------------------------------------------------


## partitioning into training (50%), validation (30%), test (20%)
# randomly sample 50% of the row IDs for training
train.rows <- sample(rownames(housing.df), dim(housing.df)[1]*0.5)
# randomly sample 30% of the row IDs for validation (setdiff used to draw records not in training set)
valid.rows <- sample(setdiff(rownames(housing.df), train.rows), dim(housing.df)[1]*0.3)

test.rows <- setdiff(rownames(housing.df), union(train.rows, valid.rows))

train.data <- housing.df[train.rows, ]
valid.data <- housing.df[valid.rows, ]
test.data <- housing.df[test.rows, ]

# ----------------------------------------------------------