cluster <- read.csv("E:/PGD Data Science/Clustering/Classroom Case Study-clustering/cluster.csv")
View(cluster)
summary(cluster)
summary(as.factor(cluster$Gender))
dim(cluster)
cust_data <- cluster[-c(1)]
dim(cust_data)

library(dplyr)
dataM=filter(cust_data,cust_data$Gender==0)
head(dataM)
DataF <- cust_data[cust_data$Gender == 1,]
DataM <- cust_data[cust_data$Gender == 0,]
summary(DataF$Avg_Session_Duration)
mean(DataF$Transaction)
summary(DataF$Transaction)

#Scaling the data
cust_scale <-scale(cust_data)
head(cust_scale)

dist.res = dist(cust_scale,method = "euclidean")

hc= hclust(dist.res,method = "complete") #Complete linkage (farthest point linkage)
#Visualize of hcust
plot(hc,labels = FALSE,hang = -1)


rect.hclust(hc,k=3,border = 2:3)

#K-means Clustering
library(vegan)
library(permute)
library(lattice)
fit <- cascadeKM(scale(cust_data,center = TRUE, scale = TRUE),1,10,iter = 1000)
plot(fit,sortg = TRUE,grpmts.plot = TRUE)
fit
calinski.best <- as.numeric(which.max(fit$results[2,]))
cat("Calinski criterion optimal number of clusters:", calinski.best, "\n")


#---------------------------------------------------------------------------------------
library(vegan)
library(permute)
library(lattice)
cust <- read.csv("E:/PGD Data Science/Clustering/Practice Case Study/Case Study 1/Wholesale customers data.csv")
View(customer)
fit <- cascadeKM(customer,1,10,iter = 1000)
plot(fit,sortg = TRUE,grpmts.plot = TRUE)
calinski.best <- as.numeric(which.max(fit$results[2,]))
cat("Calinski criterion optimal number of clusters:", calinski.best, "\n")

#Elbow Method
wss <-(nrow(cust)-1)*sum(apply(cust,2,var))
wss
for (i in 2:15){ wss[i]= sum(kmeans(cust,centers = i)$withinss)}
plot(1:15,wss, type = "b" , xlab = "No of Clusters", ylab = "Within Group Sum of Squares"
     ,col="mediumseagreen",pch=12)



K = kmeans(cust,centers = 5)
K$withinss
wss[3]

set.seed(4)
clustMod = kmeans(customer, centers = 2)
clustMod$centers
clustMod$size
clustMod$tot.withinss
customer$category = clustMod$cluster
View(customer)


#Silhoutte plot for checking how good are the clusters
library(cluster)
library(fpc)
diss=daisy(customer)
sp=silhouette(customer$category,diss)
windows()
plot(sp)
#plot 1
plotcluster(cust,customer$category)
#plot 2
clusplot(cust,customer$category,color=TRUE, shade=TRUE, labels=2, lines=0)
#plot 3
with(cust,pairs(cust,col=c(1:6)[customer$category]))

plot(cust,col=clustMod$cluster)
points(clustMod$centers,col=1:6,pch=8,cex=1)

aggregate(.~category, data=customer, mean)
library(ggplot2)
ggplot(customer,aes(customer$category,cust_data$Avg_Session_Duration, color=cluster)) + geom_point()


library(factoextra)
fviz_nbclust(cust, kmeans, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2)
