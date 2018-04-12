
# Load required libraries

library(tm)
library(RTextTools)
library(e1071)
library(dplyr)
library(caret)
# Library for parallel processing
library(doMC)
registerDoMC(cores=detectCores())  # Use all available cores
data=read.csv("paytmtweet.csv",stringsAsFactors = FALSE)
glimpse(data)
