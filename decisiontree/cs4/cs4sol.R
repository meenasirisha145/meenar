#---------------------------------Decision trees----------------------------------------------#


#-------------------------Fitting Classification Trees----------------------------------------#

#-------------------------->Preparing the Working Environment

library(tree)
library(ISLR)
library(MASS)
library(randomForest)


data<-read.csv("RAW_DATA_CLASSIFICATION TREE.csv",stringsAsFactors=F)


#-------------------->Exploring the Data Set

head(data)
str(data)
colSums(is.na(data))
View(data)
data=data[,-c("Expense.Debt_TO_EBITDA.Debt_TO_Capital.Asset_TO_Size.log10_Asset","Size.EBITDA_Margin.Default_Flag"]

