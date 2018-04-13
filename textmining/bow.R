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

findFreqTerms(dtm, lowfreq = 4)
findAssocs(dtm, terms = "women", corlimit = 0.3)

barplot(d[1:10,]$freq, las = 2, names.arg = d[1:10,]$word,
        col ="lightblue", main ="Most frequent words",
        ylab = "Word frequencies")

dtm
sparse <- removeSparseTerms(dtm, 0.99)


score.sentiment = function(tweetcorpus, pos.words, neg.words)
  
{
  
  require(plyr)
  require(stringr)
  
  scores = laply(tweetcorpus, function(tweet, pos.words, neg.words) {
    word.list = str_split(tweet, '\\s+') # splits the tweets by word in a list
    
    words = unlist(word.list) # turns the list into vector
    
    pos.matches = match(words, pos.words) ## returns matching 
    #values for words from list 
    neg.matches = match(words, neg.words)
    
    pos.matches = !is.na(pos.matches) ## converts matching values to true of false
    neg.matches = !is.na(neg.matches)
    
    score = sum(pos.matches) - sum(neg.matches) # true and false are 
    #treated as 1 and 0 so they can be added
    
    return(score) 
    
  }, pos.words, neg.words )
  
  scores.df = data.frame(score=scores, text=tweets)
  
  return(scores.df)
  
}

pos <- scan("positive-words.txt",what = 'character')
neg<- scan("negative-words.txt",what = 'character')  
analysis=score.sentiment(tweetcorpus,pos,neg)  
analysis
length(analysis)
dim(analysis)
colnames(analysis)
analysis$score
analysis$sentiment=ifelse(analysis$score>0,"positive",ifelse(analysis$score<0,"negative","neutral"))
head(analysis)
write.csv(analysis,"sentiment.csv",row.names = FALSE)