library("twitteR")
library("ROAuth")
library(stringr)
library(xlsx)
library(plyr)
api_key<- "AsWp2GC9HD68ul5hXh0vIfOAL"
api_secret <- "dBQz5HeITyFsmcqQO2xcPsWTMlVLN85DUPPhtUHep5n73HWaiJ"
access_token <- "964058868992086016-vjVnFGDqFF1wEtng3qfiWKQZjKuSY4A"
access_token_secret <- "o5I3NCIaHP49VoW7VzzpnhI7vlzfTA2khdqdFGwOM4b04"

library("httr")
twitteR:::setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)
twitteR::searchTwitter("analytics")

#set the working directory
neg = scan("negative-words.txt", what="character", comment.char=";")
pos = scan("positive-words.txt", what="character", comment.char=";")

neg = c(neg, 'wtf')

score.sentiment = function(tweets, pos.words, neg.words)
  
{
  
  require(plyr)
  require(stringr)
  
  scores = laply(tweets, function(tweet, pos.words, neg.words) {
    
    
    
    tweet = gsub('https://','',tweet) # removes https://
    tweet = gsub('http://','',tweet) # removes http://
    tweet=gsub('[^[:graph:]]', ' ',tweet) ## removes graphic characters 
    #like emoticons 
    tweet = gsub('[[:punct:]]', '', tweet) # removes punctuation 
    tweet = gsub('[[:cntrl:]]', '', tweet) # removes control characters
    tweet = gsub('\\d+', '', tweet) # removes numbers
    tweet=str_replace_all(tweet,"[^[:graph:]]", " ") 
    
    tweet = tolower(tweet) # makes all letters lowercase
    
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
#Now let’s get some tweets and analyze them. Note, if your computer is slow or old, you can lower the number of tweets to process. Just change n= to a lower number like 100 or 50

tweets = twitteR::searchTwitter('Attacks on Syria',n=2500)
Tweets.text = lapply(tweets,function(t)t$getText()) # gets text from Tweets

analysis = score.sentiment(Tweets.text, pos, neg) # calls sentiment function

hist(analysis$score)

install.packages("openxlsx") 
library(openxlsx)

write.xlsx(analysis, "myResults.xlsx")

Sys.setenv("R_ZIPCMD" = "path/to/zip.exe")
installr::install.rtools() 
