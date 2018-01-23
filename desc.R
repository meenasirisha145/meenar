origin=c("usa","usa","india","india","india","china","china","china")
invoice=c(100,100,10000,12000,10000,10,20,30)
x=data.frame(origin,invoice)
x$origin=as.factor(x$origin)
x
aggregate(x$invoice,by=list(x$origin),FUN=min)

library(psych)
describe.by(x,group = x$origin)
library(doBy)
summaryBy(invoice~origin,data=x,FUN = function(x){c(m=mean(x),s=sd(x))})
library(Hmisc)
describe(x)
