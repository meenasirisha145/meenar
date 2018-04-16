
# Load required libraries

library(tm)
library(RTextTools)
library(e1071)
library(dplyr)
library(caret)
# Library for parallel processing
library(doMC)
registerDoMC(cores=detectCores())  # Use all available cores
data=read.csv("sentiment.csv",stringsAsFactors = FALSE)
glimpse(data)
data$sentiment=as.factor(data$sentiment)

corpus <- Corpus(VectorSource(data$text))
# Inspect the corpus
corpus

rmlist=c("@paytm","paytm","paytmMall","paytmBank","Paytm","xfxfxaxaxfxfxaxa","https","paytmcar","paytmbank")
# Use dplyr's  %>% (pipe) utility to do this neatly.
corpus.clean <- corpus %>%
  tm_map(content_transformer(tolower)) %>% 
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(removeWords, stopwords(kind="en")) %>%
  tm_map(stripWhitespace) %>%
  tm_map(stemDocument) %>%
  tm_map(removeWords,rmlist)
corpus.clean
dtm <- DocumentTermMatrix(corpus.clean)
# Inspect the dtm
inspect(dtm[40:50, 10:15])


df.train <- data[1:300,]
df.test <- data[301:500,]

dtm.train <- dtm[1:300,]
dtm.test <- dtm[301:500,]

corpus.clean.train <- corpus.clean[1:300]
corpus.clean.test <- corpus.clean[301:500]

dim(dtm.train)

onefreq <- findFreqTerms(dtm.train, 1)
length((onefreq))

dtm.train.nb <- DocumentTermMatrix(corpus.clean.train, control=list(dictionary = onefreq))

dim(dtm.train.nb)

dtm.test.nb <- DocumentTermMatrix(corpus.clean.test, control=list(dictionary = onefreq))

dim(dtm.test.nb)
dtm.train.nbmat=as.matrix(dtm.train.nb)


classifier <- naiveBayes(dtm.train.nbmat,as.factor(data[1:300,3]))
pred=predict(classifier,newdata=df.test)
table(df.test[,3],pred)

