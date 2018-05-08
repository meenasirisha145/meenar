#Load the Data
train=read.csv("train.csv",stringsAsFactors = F,na.strings = "")
test=read.csv("test.csv",stringsAsFactors = F,na.strings = "")
str(train)
summary(train)
str(test)
dim(train)
dim(test)
test$stroke=0
full=rbind(train,test)
colSums(is.na(full))
hist(full$bmi)
mean(full$bmi,na.rm=T)
median(full$bmi,na.rm=T)
full$bmi[is.na(full$bmi)]=median(full$bmi,na.rm=T)
sum(is.na(full$bmi))

library(ggplot2)

ggplot(data=full, aes( x=gender, fill=smoking_status)) + geom_bar(position = 'stack') + 
  labs(title='position=stack')
ggplot(data=full, aes( x=Residence_type, fill=smoking_status)) + geom_bar(position = 'stack') + 
  labs(title='position=stack')
ggplot(data=full, aes( x=work_type, fill=smoking_status)) + geom_bar(position = 'stack') + 
  labs(title='position=stack')
ggplot(data=full, aes( x=ever_married, fill=smoking_status)) + geom_bar(position = 'stack') + 
  labs(title='position=stack')

table(full$smoking_status)

full$smoking_status[is.na(full$smoking_status)]="never smoked"
colSums(is.na(full))
str(full)

for (i in c(2,4,5,6,7,8,11)){
  full[,i]=as.factor(full[,i])
  
}

train1=full[1:43400,]
test1=full[43401:62001,]
dim(test1)
str(test1)
str(train1)
table(train$stroke)
table(train1$stroke)
test1=subset(test1,select = -c(12))
str(test1)

library(caTools)
spl=sample.split(train1,SplitRatio = 0.7)
train2=subset(train1,spl==TRUE)
valid=subset(train1,spl==FALSE)
str(train2)
str(valid)

#Building a model



library(rpart)
myformula=stroke~gender+age+hypertension+heart_disease+ever_married+work_type+Residence_type+avg_glucose_level+bmi+smoking_status
model1=rpart(myformula,data=train2)
summary(model1)

pred=predict(model1,newdata = valid)
valid$predictions=pred
table(valid$stroke,valid$predictions)




library(ROCR)

pred <- prediction(train1$predictions, train1$stroke)
perf <- performance(pred,"tpr","fpr")
plot(perf)
str(perf)
cutoffs <- data.frame(cut=perf@alpha.values[[1]], fpr=perf@x.values[[1]], 
                      tpr=perf@y.values[[1]])

(cutoffs)


library(pROC)
my_roc <- roc(train1$predictions, train1$stroke)
coords(my_roc, "best", ret = "threshold")

pred1=predict(model1,newdata = test1)
head(pred1)
pred1=ifelse(pred1>0.01465965,1,0)
sub=read.csv("sample_submission_1.csv",stringsAsFactors = FALSE)
head(sub)
sub$stroke=pred1
sub
