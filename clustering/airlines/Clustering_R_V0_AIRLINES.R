#SGN
#K-Means Clustering Project


#SGN
rm(list = ls())

##############################################################################################
###### Build an analytical model to create clusters of airline travellers ####################
##############################################################################################

#---------------------Step1: Loading the Data in R

airlines<-read.csv("AirlinesCluster.csv", header = TRUE, as.is = TRUE, na.strings = c(""))


#Understand the data type and summary of each coloumn
str(airlines)
summary(airlines)


#Checking missing values
as.data.frame(colSums(is.na(airlines)))



#Normalizing the Data for clustering
library(caret)
preproc<-preProcess(airlines)
airlinesNorm<-predict(preproc,airlines)
summary(airlinesNorm)


#Hierarchical Clustering
distan<-dist(airlinesNorm, method = "euclidean")
ClusterAirline<-hclust(distan, method = "ward.D")
plot(ClusterAirline)


#Assigning points to the clusters
AirlineCluster<-cutree(ClusterAirline, k = 5)
table(AirlineCluster)

airlines_h=data.frame(airlines,AirlineCluster)
airlines_h
write.csv(airlines_h,"airlines_h.csv",row.names = FALSE)
#Computing the average values of the cluster groups
MeanComp<-function(var, clustergrp, meas){
  z<-tapply(var, clustergrp, meas)
  print(z)
}

Bal_mean<-MeanComp(airlines$Balance, AirlineCluster, mean)
Bal_DaysSinceEnroll<-MeanComp(airlines$DaysSinceEnroll, AirlineCluster, mean)

#k-Means Clustersing
set.seed(88)
k<-5
AirlineCluster_K<-kmeans(airlinesNorm, centers = k, iter.max = 1000)
table(AirlineCluster_K$cluster)
AirlineCluster_K$centers


#finding the mean values of the variables
bal_mean_k=aggregate(airlines,by=list(AirlineCluster_K$cluster),mean)
bal_mean_k


airlines_k=data.frame(airlines,AirlineCluster_K$cluster)
write.csv(airlines_k,"airlines_k.csv",row.names = FALSE)
