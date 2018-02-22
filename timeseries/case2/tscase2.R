#-----------------------------------------Time Series Forecasting---------------------------------------#



#Analytical Problem: To forecast the Sales (in thoushand units) of a Automobile company for the next 36 months

#----------------------------------Preparing the environment--------------------------------------------#

list.of.packages <- c("forecast", "ggplot2","MASS","caTools","sqldf","tseries")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos="http://cran.rstudio.com/")
library(forecast)
library(tseries)


#-----------------------------Setting the working directory-------------------------------------------#


data<-read.csv("1sales.csv",header = TRUE)
TSdata=data#To create a backup of original data


#---------------------------------Exploring the data-------------------------------------------------------#
head(TSdata)#
dim(TSdata)#We have 169 time series points (at a date level) and 2 vars(Date, Sales)
str(TSdata)
summary(TSdata)
colSums(is.na(TSdata))
names(TSdata)[c(1:2)]=c("Date","Sales")


#---------------------Transformation of the date data into time series------------------------------------#

TSdata=ts(TSdata[,1],start=c(01,03),end=c(12,16),frequency=16)
start(TSdata)
end(TSdata)
frequency(TSdata)
StructTS(TSdata)
TSdata

#2003,1 is the start date and 12 is the frequency of the time series (monthly series)
str(TSdata)