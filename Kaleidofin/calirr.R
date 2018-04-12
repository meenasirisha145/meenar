#Loading the Data
data=read.csv("irr.csv",stringsAsFactors = FALSE)
head(data)

for (i in nrow(data)){
  irr=data$Interest.Premium/data$Totalamount.to.pay
  data$irr=irr
}
head(data)

library(FinCal)
cf1=c(-20300,rep(5000,3),rep(7000,37))

irr(cf1)
IRR=function(Duration,inst_lft,inst_nonlft,lft_time)
{
  yield=Duration*inst_lft+(lft_time-1)*1000
  totalamt=(lft_time-1)*inst_lft+(Duration-lft_time+1)*inst_nonlft
  irr=(-yield+totalamt)/totalamt
  return(irr)
}



