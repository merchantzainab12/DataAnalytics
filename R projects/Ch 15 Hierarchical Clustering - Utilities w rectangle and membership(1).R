# Chapter - 15: Cluster Analysis


# Set the working directory first before proceeding further

# Read and review data
utilities.df <- read.csv("Utilities.csv")
tria<-read.csv("utilities.csv")
summary(utilities.df)

# ------------------Fig 15.1: Scatter Plot of Fuel Cost Vs Sales ----------------------------------

plot(utilities.df$Fuel_Cost ~ utilities.df$Sales,
     xlab="sales",ylab="FuelCost", xlim=c(2000,20000))
text(x=utilities.df$Sales,y=utilities.df$Fuel_Cost,
     labels=utilities.df$Company,pos=4,cex = 0.8,srt=20,offset = 0.2)

# Alternative with ggplot

library(ggplot2)
ggplot(utilities.df,aes(y=Fuel_Cost,x=Sales))+geom_point()+
  geom_text(aes(label=paste("",Company)),size=4,hjust=0.0,angle=15)+
  ylim(0.25,2.25)+xlim(3000,18000)


# -------Table 15.2: R-Code for computing distance between records 

# set row names to the utilities column
row.names(utilities.df) <- utilities.df[,1]
# Now rownames(utilities.df) will show that row names are changed to names of utilities, instead of 1, 2, 3, 4 ,...
# The he dataframe will now have row names instead of 1,2,3, etc.

# So, now we can remove the "company" column
utilities.df <- utilities.df[,-1]
summary(utilities.df)

# The dimension is 22 X 8 instead of 22 X 9
# Verify this by using dim(utilities.df) and names(utilities.df),etc.
# 
# ------------- compute Euclidean distance --------------
# (to compute other distance measures, change the value in method = )
# "dist" function computes Distance Matrix. Check syntax with ?dist
# This function computes and returns the distance matrix computed by 
# using the specified distance measure to compute the distances between the rows of a data matrix
# Usage is: dist(x, method = "euclidean", diag = FALSE, upper = FALSE, p = 2)

d <- dist(utilities.df, method = "euclidean")
d

# Note that it also prints the row names.

##### ---------------Table 15.4: Code for normalizing data and computing distances--------

# normalize input variables

utilities.df.norm <- sapply(utilities.df, scale)

# ?scale --> Scaling and Centering of Matrix-like Objects
# scale is generic function whose default method centers and/or scales the columns of a numeric matrix.
# Scale Usage: scale(x, center = TRUE, scale = TRUE)
# Compare utilities.df and utilities.df.norm --> The latter are normalized values
# You can look at data utilities.df.norm --> It has 0 mean.

summary(utilities.df.norm)

# Exercise -- Verify that standard deviation = 1

# add row names: utilities
row.names(utilities.df.norm) <- row.names(utilities.df) 

# compute normalized distance based on variables Sales (Column 6) and FuelCost (column 8)
d.norm <- dist(utilities.df.norm[,c(6,8)], method = "euclidean")
d.norm

# ------------------Exercise-1:  Plot the normalized Fuel COst Vs Sales ---------
# ----------------- Exercise-2: Write code to recreate Table 15.3 ------------



#-----------------------------------------------------------



##########################################
#
# Hierarchical Clustering
#
##########################################
#

# ----- Figure 15.3: Code for running hierarchical clustering and generating a dendogram ----

# compute normalized distance based on all 8 variables

d.norm <- dist(utilities.df.norm, method = "euclidean")
d.norm

# Clustering 
# in hclust() set argument method =  
# to "ward.D", "single", "complete", "average", "median", or "centroid"
# Using single method

hc_single <- hclust(d.norm, method = "single")
plot(hc_single, hang = -1, ann = FALSE)
rect.hclust(hc_single, k=2, border = "red") # Cut the dendrogram into 2 clusters

#  parameter hang = 1 causes x-axis labels to hang below the 0 line
#  ann = FALSE turns off annotation (plot and axis titles) 

# -----------------------------
# Using 'average method
#
hc_average <- hclust(d.norm, method = "average")
plot(hc_average, hang = -1, ann = FALSE)
rect.hclust(hc_average, k=2, border = "red") # Cut the dendrogram into 2 clusters
#
#------------------------------------------
#
# Using "complete" method
#
hc_complete <- hclust(d.norm, method = "complete")
plot(hc_complete, hang = -1, ann = FALSE)
rect.hclust(hc_complete, k=3, border = "red") # Cut the dendrogram into 2 clusters

#
# ----------------------------------------
#
# Median method
#
hc_median <- hclust(d.norm, method = "median")
plot(hc_median, hang = -1, ann = FALSE)
rect.hclust(hc_median, k=2, border = "red") # Cut the dendrogram into 2 clusters
#
#---------------------------------------
#
# Centroid Method
#
hc_centroid <- hclust(d.norm, method = "centroid")
plot(hc_centroid, hang = -1, ann = FALSE)
rect.hclust(hc_centroid, k=2, border = "red") # Cut the dendrogram into 2 clusters

#

hc_ward <- hclust(d.norm, method = "ward.D")
plot(hc_ward, hang = -1, ann = FALSE)
rect.hclust(hc_ward, k=4, border = "red") # Cut the dendrogram into 2 clusters
# ---------------------------------------------

##

## -----------Table 15.6: Computing Cluster Membership by cutting the dendogram -------

memb <- cutree(hc_single, k = 6)
memb

memb <- cutree(hc_average, k = 6)
memb


# Add cluster membership to the dataset

cluster_average <-  memb 

data_with_cluster_average <-cbind(utilities.df,cluster_average)

View(data_with_cluster_average)

# --------------------- 


## -------- ------------- ------------------


## ------------Figure 15.4: Code for creating  heatmap  -----------

# 

# set labels as cluster membership and utility name

row.names(utilities.df.norm) <- paste(memb, ": ", row.names(utilities.df), sep = "")

# plot heatmap 
# rev() reverses the color mapping to large = dark
heatmap(as.matrix(utilities.df.norm), Colv = NA, hclustfun = hclust, 
        col=rev(paste("gray",1:99,sep="")))


 

## ---------- plotting is validated ------ write code to plot color heatmap  -------------------
#
# End of Hierarchical Clustering Illustration

#  ----------  Suggested Exercises -------------

# 1. Shuffle data and find clusters. Does this impact cluster membership?
# 2. Create and compare heatmaps for results from other clustering methods. What differences are observed?
# 3. Create a data frame with cluster memberships obtained from each method. Use this dataset and find clusters
#     using the k-means method (k - 3, 4, 5). Compare memberships with hierarchical clustering.
