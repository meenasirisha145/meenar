#Loading the Data
data=read.csv("irr.csv",stringsAsFactors = FALSE)
head(data)
months=40
library(FinCal)
#There is a function called IRR in Fincal package to calculate internal rate of return
for (i in 1:nrow(data)){
  
  cf0 =-1*(data$Yield[i])
  cf1=c(rep(data$Installment[i],(i-1)),rep(data$After.Lifting[i],(months-(i-1))))
  cf2=c(cf0,cf1)
  irrcal=irr(cf2)
  data$cal[i]=irrcal
}
head(data)
data
write.csv(data,"solirr.csv",row.names=FALSE)




