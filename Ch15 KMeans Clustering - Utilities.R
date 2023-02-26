# Chapter - 15.5: Cluster Analysis -- K- Means

# Set the working directory first before proceeding further


################################################################
##
### K-Means Clustering 
###
############################################################

#### Table 15.9

# load and preprocess data 
utilities.df <- read.csv("Utilities.csv")
row.names(utilities.df) <- utilities.df[,1]
utilities.df <- utilities.df[,-1]

# normalized distance:
utilities.df.norm <- sapply(utilities.df, scale)
row.names(utilities.df.norm) <- row.names(utilities.df) 

# run kmeans algorithm 
set.seed(2)
km <- kmeans(utilities.df.norm, 6)


# "6" Specifies the desired number of clusters =6 

# show cluster membership
km$cluster



## ---------------------------------------------------

#### Table 15.10: Cluster Centroids and Squared Distances for k-means with k = 6

# centroids
km$centers
# Centroids = each cluster has a vector of variables means

# within-cluster sum of squares
km$withinss
# If a cluster is a singleton, within-cluster distance = 0

# Cluster Size
km$size
# shows number of observations in each cluster

## -------------------------------------------------------------

#### Figure 15.5: Visual Representation (Profle Plot) of Cluster Centroids

# plot an empty scatter plot
plot(c(0), xaxt = 'n', ylab = "", type = "l", 
     ylim = c(min(km$centers), max(km$centers)), xlim = c(0, 8))

# label x-axes
axis(1, at = c(1:8), labels = names(utilities.df))

# plot centroids
for (i in c(1:6))
  lines(km$centers[i,], lty = i, lwd = 2, col = ifelse(i %in% c(1, 3, 5),
                                                       "black", "dark grey"))

# name clusters
text(x = 0.5, y = km$centers[, 1], labels = paste("Cluster", c(1:6)))

## ------------------------------------------------- ------------------

#### Table 15.11; Euclidean Distance Between Cluster Centroids ---

dist(km$centers)
 # ----------------------------- -------------



##   ------Fig. 15.6: Scree Plot: Comparing different choices of k in terms of overall average within-cluster distance-
# Note this code is not in the text book

# Lets' say we want to find the best number for cluster k in the range 2 - 15
# Assign the largest value to the variable expt below:

expt <- 15 
wss <- numeric(expt)
for (k in 1:expt) wss[k] <- sum(kmeans(utilities.df.norm, centers = k, nstart = 25)$withinss)
plot(1:expt, wss, type = "b", xlab = "No of Clusters", ylab = "Within Sum of Squares")


### Suggested Exercises

# 1. Based on review of this scree plot, what is the optimum number of clusters (OPT)?  
# 2. Segment the data for for OPT-1, OPT, OPT+1, and compare characteristics of these 3 clusters 

### End of Code for K-Means -----------

