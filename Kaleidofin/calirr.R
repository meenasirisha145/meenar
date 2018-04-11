#Loading the Data
data=read.csv("irr.csv",stringsAsFactors = FALSE)
data
library(FinCal)
irr(data$Interest.Premium)
