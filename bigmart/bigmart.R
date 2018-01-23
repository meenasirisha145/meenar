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
###Building the model##
data1=data[,-c(1,7)]

library(caTools)
spl=sample.split(data1$Item_Weight,0.7)
train=subset(data1,spl==TRUE)
valid=subset(data1,spl==FALSE)

names(data1)
model1=lm(Item_Outlet_Sales~1,data=train)
summary(model1)

model2=lm(Item_Outlet_Sales~. ,data=train)
summary(model2)


model3=lm(Item_Outlet_Sales~Item_Weight+Item_Fat_Content+Item_Visibility+Item_MRP+
            Outlet_Establishment_Year+Outlet_Size+Outlet_Location_Type+Outlet_Type,data=train)
summary(model3)

model4=lm(Item_Outlet_Sales~Item_Weight+Item_Visibility+Item_MRP+
            Outlet_Establishment_Year+Outlet_Size+Outlet_Location_Type+Outlet_Type,data=train)
summary(model4)

model5=lm(Item_Outlet_Sales~Item_Weight+Item_MRP+
            Outlet_Establishment_Year+Outlet_Size+Outlet_Location_Type+Outlet_Type,data=train)
summary(model5)

model6=lm(Item_Outlet_Sales~Item_MRP+Outlet_Establishment_Year+
            Outlet_Size+Outlet_Location_Type+Outlet_Type,data=train)
summary(model6)

pred=predict(model6,newdata = valid)
valid$prediction=pred
library(Metrics)
rmse(valid$Item_Outlet_Sales,pred)

testdata=read.csv("bigmarttest.csv",na.strings = "")
colSums(is.na(testdata))
table(testdata$Outlet_Size)
testdata$Outlet_Size[is.na(testdata$Outlet_Size)]="Medium"

pred=predict(model6,newdata = testdata[,c("Item_MRP","Outlet_Establishment_Year",
                                          "Outlet_Size","Outlet_Location_Type",
                                          "Outlet_Type")])
pred
sub1=read.csv("sub.csv")
sub1$Item_Outlet_Sales=pred
head(sub1)
write.csv(sub1,"submission.csv",row.names = FALSE)
