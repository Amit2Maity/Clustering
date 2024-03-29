rm(list =ls())
setwd("C:/Users/User02/Google Drive/Business Analytics/Business Analytics Video/Moodle Upload/10. Clustering/Clustering Case on R/")

##Read the data in the file
cust_data<-read.csv("cluster.csv")
### Select the requried columns for clustering

head(cust_data)

cust_data<- cust_data[-c(1)]
summary(cust_data)

cust_data_f<- scale(cust_data)

head(cust_data_f)

dist.res=dist(cust_data_f,method = "euclidean")

hc<- hclust(dist.res,method="complete")

#Visulize of hclust

plot(hc,labels=FALSE,hang=-1)

rect.hclust(hc,k=3,border = 2:3)


####K-means clustering

#install.packages("vegan")
#install.packages("permute")

library(vegan)
library(permute)
library(lattice)
fit <- cascadeKM(scale(cust_data, center = TRUE, scale = TRUE), 1, 10, iter = 1000)
plot(fit, sortg = TRUE, grpmts.plot = TRUE)

calinski.best <- as.numeric(which.max(fit$results[2,]))
cat("Calinski criterion optimal number of clusters:", calinski.best, "\n")


# Also looking at the elbow chart 
mydata <- cust_data

#Determine the optimal cluster size based on within sum of squares
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))

for (i in 2:15) wss[i] <- sum(kmeans(mydata,centers=i)$withinss)

#Plot the elbow chart to determine optimal cluster
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares",col="mediumseagreen",pch=12)

# From elbow chart it looks like 4 clusters although the change is low after 2. So we can either consider 2 or 4. Lets stick with 2 as calinski criterion also suggests that.

###Run the kmeans algorithm to generate the clusters
k1<-kmeans(cust_data_f, 2)

k1

###See the clustering results
###Fetch the group means for each variable
k1$centers

###Fetch size/n of obs for the groups
k1$size

###Fetch the cluster for each obs
k1$cluster

cust_data$cluster=k1$cluster

View(cust_data)

#Silhoutte plot for checking how good are the clusters
library(cluster)

diss=daisy(cust_data_f)
sp=silhouette(cust_data$cluster,diss)
windows()
plot(sp)

# Analysing the clusters
aggregate(.~cluster, data=cust_data, mean)
library(ggplot2)
ggplot(cust_data,aes(cust_data$Pages_Per_Session,cust_data$Avg_Session_Duration, color=cluster)) + geom_point()
