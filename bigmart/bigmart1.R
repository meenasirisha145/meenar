##Big mart Sales##
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

data$Item_Weight[is.na(data$Item_Weight)]=mean(data$Item_Weight,na.rm=TRUE)
data$Outlet_Size[is.na(data$Outlet_Size)]="Medium"
data$Outlet_Establishment_Year=as.factor(data$Outlet_Establishment_Year)

str(data)
###Building the model##
data1=data[,-c(1,7)]

library(caTools)
spl=sample.split(data1$Item_Weight,0.7)
train=subset(data1,spl==TRUE)
valid=subset(data1,spl==FALSE)

names(data1)
model1=lm(Item_Outlet_Sales~1,data=data1)
summary(model1)

model2=lm(Item_Outlet_Sales~. ,data=data1)
summary(model2)


library(randomForest)
rfmodel=randomForest(Item_Outlet_Sales~Item_MRP+Outlet_Establishment_Year
                       ,data=data1)
summary(rfmodel)
testdata=read.csv("bigmarttest.csv",na.strings = "")
colSums(is.na(testdata))
table(testdata$Outlet_Size)
testdata$Item_Weight[is.na(testdata$Item_Weight)]=mean(testdata$Item_Weight,na.rm=TRUE)
testdata$Outlet_Size[is.na(testdata$Outlet_Size)]="Medium"
testdata$Outlet_Establishment_Year=as.factor(testdata$Outlet_Establishment_Year)


pred1=predict(rfmodel,newdata = testdata[,c("Item_MRP","Outlet_Establishment_Year")])


sub1=read.csv("sub.csv")
sub1$Item_Outlet_Sales=pred1
head(sub1)

write.csv(sub1,"submission.csv",row.names = FALSE)
