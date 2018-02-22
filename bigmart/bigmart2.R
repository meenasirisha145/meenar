#-------------------------Fitting Regression Tree----------------------------------------#
data=read.csv("bigmarttrain.csv",na.strings = c(""))
data
str(data)
summary(data)
colSums(is.na(data))

as.data.frame(colSums(is.na(data)))
library(Amelia)
missmap(data, main = "Missing values vs Observed")

is.na(data$Outlet_Size)
table(data$Outlet_Size)

data$Item_Weight[is.na(data$Item_Weight)]=median(data$Item_Weight,na.rm=TRUE)
data$Outlet_Size[is.na(data$Outlet_Size)]="Medium"
data$Outlet_Establishment_Year=as.factor(data$Outlet_Establishment_Year)

str(data)
colSums(is.na(data))
#removing the columns item_identifier and outlet_identifier
data1=data[,-c(1,7)]

library(caTools)
spl=sample.split(data1$Item_Weight,0.7)
train=subset(data1,spl==TRUE)
valid=subset(data1,spl==FALSE)


#Fitting the train model 
tree.bigmart <- tree(Item_Outlet_Sales ~.,data= train)
summary(tree.bigmart)

plot(tree.bigmart)
text(tree.bigmart, pretty = 0)



#--------------------> Pruning the Decision Tree
cv.bigmart <- cv.tree(tree.bigmart)
cv.bigmart
plot(cv.bigmart$size, cv.bigmart$dev, type = 'b', main = "CV plot terminal node size vs Deviance")

#-------------------> if you still want to prune the tree
prune.bigmart <- prune.tree(tree.bigmart, best = 7)
plot(prune.bigmart)
text(prune.bigmart, pretty = 0)


#-------------------> if you still want to prune the tree
prune.bigmart1 <- prune.tree(tree.bigmart, best = 5)
plot(prune.bigmart1)
text(prune.bigmart1, pretty = 0)




yhat <- predict(tree.bigmart, newdata = valid)

plot(yhat,valid[,"Item_Outlet_Sales"])
abline(0, 1)
mean((yhat - valid[,"Item_Outlet_Sales"]) ^ 2)



#importing the test data
testdata=read.csv("bigmarttest.csv",na.strings = "")
colSums(is.na(testdata))
table(testdata$Outlet_Size)
testdata$Item_Weight[is.na(testdata$Item_Weight)]=median(testdata$Item_Weight,na.rm=TRUE)
testdata$Outlet_Size[is.na(testdata$Outlet_Size)]="Medium"
testdata$Outlet_Establishment_Year=as.factor(testdata$Outlet_Establishment_Year)


pred1 <- predict(tree.bigmart, newdata = testdata)




sub1=read.csv("sub.csv")
sub1$Item_Outlet_Sales=pred1
head(sub1)

write.csv(sub1,"submission2.csv",row.names = FALSE)

