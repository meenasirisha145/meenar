data=read.csv("elantra.csv",stringsAsFactors = FALSE,header = TRUE)
data
str(data)
summary(data)
colSums(is.na(data))
traindata=subset(data,Year<=2012)
traindata
testdata=subset(data,Year>2012)
testdata
nrow(traindata)
nrow(testdata)



####building a model
model1=lm(ElantraSales~Unemployment+CPI_all+ CPI_energy + Queries,data=traindata)
summary(model1)


##model includes month
model2=lm(ElantraSales~Month+Unemployment+CPI_all+ CPI_energy + Queries,data=traindata)
summary(model2)


data$Month=as.factor(data$Month)
str(data)

traindata$Month=as.factor(traindata$Month)
str(traindata)
#after making month as factor

model3=lm(ElantraSales~Month+Unemployment+CPI_all+ CPI_energy + Queries,data=traindata)
summary(model3)

#removing the variable queries

model4=lm(ElantraSales~Month+Unemployment+CPI_all+ CPI_energy ,data=traindata)
summary(model4)
#prediction
testdata$Month=as.factor(testdata$Month)
pred_sales=predict(model4,newdata=testdata)
testdata$pred=pred_sales
testdata

SSE=sum((testdata$ElantraSales-testdata$pred)^2)
SSE

summary(abs(pred_sales-testdata$ElantraSales))

testdata[which.max(abs(pred_sales-testdata$ElantraSales)),]


library(car)
as.data.frame(vif(model4))              
              
#model4=lm(ElantraSales~Month+Unemployment+CPI_all ,data=traindata)
#summary(model4)
