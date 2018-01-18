#simple example in CT
train1=data.frame(rollno=c(1,2,3),gender=c('m','m','f'),plays=c('play','noplay','play'))
train1
library(rpart)
mytree1=rpart(plays~gender,data=train1,method="class")
mytree1


train2=data.frame(rollno=c(1,2,3,4),gender=c('m','m','f','f'),plays=c('play','noplay','play','play'))
train2
mytree2=rpart(plays~gender,data=train2,method="class")
mytree2


rpart(plays~gender,data=train2,method="class",minsplit=1,minbucket=1)


train3=data.frame(rollno=c(1,2,3,4,5,6,7),gender=c('m','m','f','m','m','f','f'),plays=c('play','play','play','play','play','noplay','noplay'))
train3
mytree3=rpart(plays~gender,data=train3,method="class",minsplit=1,minbucket=1)
mytree3
table(train3$gender,train3$play)
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
fancyRpartPlot(mytree3)

train3$married=c('married','Single','married','married','married','single','single')

mytree4=rpart(plays~gender+married,data=train3,method="class",minsplit=1,minbucket=1)
mytree4
fancyRpartPlot(mytree4)

train3


mytree5=rpart(plays~ gender+married,data=train3,method="class",minsplit=2,minbucket=1,cp=-1)
mytree5
fancyRpartPlot(mytree5)

predict(mytree4,newdata = data.frame(gender="m",married="single"))
