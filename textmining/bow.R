library(dplyr)
library(tm)
library(wordcloud)
library(plotrix)
library(dendextend)
library(ggplot2)
library(ggthemes)
library(RWeka)

data=read.csv("paytmtweet.csv",stringsAsFactors = FALSE)
glimpse(data)
nrow(data)
tweets=data$text
head(tweets)
tweetcorpus=Corpus(VectorSource(tweets))
tweetcorpus

#In order to make use of this corpus, we need to transform its contents as follows.
library(SnowballC)
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
tweetcorpus <- tm_map(tweetcorpus, toSpace, "/")
tweetcorpus <- tm_map(tweetcorpus, toSpace, "@")
tweetcorpus <- tm_map(tweetcorpus, toSpace, "\\|")

tweetcorpus <- tm_map(tweetcorpus, content_transformer(tolower))
tweetcorpus <- tm_map(tweetcorpus, removePunctuation)
tweetcorpus <- tm_map(tweetcorpus, stemDocument)
tweetcorpus <- tm_map(tweetcorpus, removeWords, stopwords("english"))
tweetcorpus <- tm_map(tweetcorpus, stripWhitespace)
tweetcorpus <- tm_map(tweetcorpus, removeNumbers)
tweetcorpus[1]$content
rmlist=c("@paytm","paytm","paytmMall","paytmBank","Paytm","xfxfxaxaxfxfxaxa")
tweetcorpus <- tm_map(tweetcorpus, removeWords, rmlist)
tweetcorpus[1]$content


dtm=DocumentTermMatrix(tweetcorpus,control=list(weighting=weightTfIdf, minWordLength=2, minDocFreq=5))
dtm
?DocumentTermMatrix
dim(dtm)
colnames(dtm)[100:110]
inspect(dtm[1:10,100:110])

inspect(dtm)
class(dtm)
dtm1=TermDocumentMatrix(tweetcorpus)
m=as.matrix(dtm1)
v <- sort(rowSums(m),decreasing=TRUE)
v
d <- data.frame(word = names(v),freq=v)
head(d, 10)

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

findFreqTerms(dtm, lowfreq = 4)
findAssocs(dtm, terms = "women", corlimit = 0.3)

barplot(d[1:10,]$freq, las = 2, names.arg = d[1:10,]$word,
        col ="lightblue", main ="Most frequent words",
        ylab = "Word frequencies")



