#SGN
#K-Means Clustering Project


#SGN
rm(list = ls())

##############################################################################################
###### Build an analytical model to create clusters of stocks and further predict it #########
##############################################################################################

#---------------------Step1: Loading the Data in R

StockClus<-read.csv("StocksCluster.csv", header = TRUE, as.is = TRUE, na.strings = c(""))


#Understand the data type and summary of each coloumn
str(StockClus)
summary(StockClus)


#Checking missing values
as.data.frame(colSums(is.na(StockClus)))


#Q2. What proportion of the observations have positive returns in December?
table(StockClus$PositiveDec)
6324/(5256+6324)


#Q3. Correlations 
library(Hmisc)
Cor<-rcorr(as.matrix(StockClus))
Cor

#Q4. Training a logistic model
Model1<-glm(PositiveDec~., family = binomial(link = 'logit'), data = StockClus)
summary(Model1)



library(caret)
#Normalizing the Data for clustering 
preproc<-preProcess(StockClus)
StockClusNorm<-predict(preproc,StockClus)
summary(StockClusNorm)


#Hiearchical Clustering
distan<-dist(StockClusNorm, method = "euclidean")
ClusterStocks<-hclust(distan, method = "ward.D")
plot(ClusterStocks)


#Assigning points to the cluster
StockCluster<-cutree(ClusterStocks, k = 5)
table(StockCluster)

#Computing the average values of the cluster groups
MeanComp<-function(var, clustergrp, meas){
  z<-tapply(var, clustergrp, meas)
  print(z)
}

PD_mean<-MeanComp(StockClus$PositiveDec, StockCluster, mean)


#k-Means Clustersing
set.seed(88)
k<-5
StockClus_K<-kmeans(StockClusNorm, centers = k, iter.max = 1000)
table(StockClus_K$cluster)
StockClus_K$centers
StockClus_K$cluster
StockClus_K
stocks_k=data.frame(StockClus,StockClus_K$cluster)
write.csv(stocks_k,"stocks.csv",row.names = FALSE)

cluster1=stocks_k[stocks_k$StockClus_K.cluster==1,]
cluster1
