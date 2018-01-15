#reading the data
census=read.csv("census.csv",na.strings = "")
str(census)
summary(census)
colSums(is.na(census))
library(caTools)
set.seed(3000)
sample=sample.split(census$over50k,0.6)
train=subset(census,sample==TRUE)
test=subset(census,sample==FALSE)
summary(train)
str(train)
str(test)

library(caret)
library(ggplot2)
library(rpart.plot)
library(pROC)
library(ROCR)
library(rpart)
library(randomForest)
library(caTools)
library(e1071)

cart1=rpart(over50k~.,data=train,method="class")
prp(cart1)#prp gives the decision tree
cart1

#1.change the iv's
#2.change the minbucket
#3.change the complexity parameter
predictcart1=predict(cart1,newdata = test,type="class")
table(predictcart1,test$over50k)
confusionMatrix(predictcart1,test$over50k)

predictcart2=predict(cart1,newdata=test)


cartroc=roc(response=test$over50k,predictor = predictcart2[,2],level=rev(levels(test$over50k)))
plot(cartroc)




###########RANDOM FOREST MODEL################
predictforest1=randomForest(over50k~.,data=train)
predictforest1

predforest1=predict(predictforest1,newdata = test,type="class")
table(predforest1,test$over50k)
confusionMatrix(predforest1,test$over50k)


##variable importance

vu=varUsed(predictforest1,count=TRUE)

vusorted = sort(vu, decreasing = FALSE, index.return = TRUE)
dotchart(vusorted$x, names(predictforest1$forest$xlevels[vusorted$ix]), main = "Variable Importance Chart_Splitting")


varImpPlot(predictforest1, main = "Variable Importance Chart_Impurity Red")
