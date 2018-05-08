library(dplyr)
library(tm)
library(wordcloud)
library(plotrix)
library(dendextend)
library(ggplot2)
library(ggthemes)


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
rmlist=c("@paytm","paytm","paytmMall","paytmBank","Paytm","xfxfxaxaxfxfxaxa","https","paytmcar","paytmbank")
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


# remove sparse terms
dtm2 <- removeSparseTerms(dtm, sparse=0.95)
m2 <- as.matrix(dtm2)
# cluster terms
distMatrix <- dist(scale(m2))
fit <- hclust(distMatrix, method="ward")
plot(fit)
# cut tree into 10 clusters
rect.hclust(fit, k=10)
(groups <- cutree(fit, k=10))

# set a fixed random seed
set.seed(122)
# k-means clustering of tweets
k <- 5
kmeansResult <- kmeans(m2, k)
# cluster centers
round(kmeansResult$centers, digits=3)

for (i in 1:k) {
  cat(paste("cluster ", i, ": ", sep=""))
  s <- sort(kmeansResult$centers[i,], decreasing=T)
  cat(names(s)[1:5], "\n")
}
