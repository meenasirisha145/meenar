######TWITTER ANALYSIS#############

#ROAuth: Provides an interface to the OAuth 1.0 specification, allowing users to 
#authenticate via OAuth to the server of their choice.

#Twitter: Provides an interface to the Twitter web API.


install.packages("twitteR")
install.packages("ROAuth")
library("twitteR")
library("ROAuth")
# Download "cacert.pem" file
download.file(url="http://curl.haxx.se/ca/cacert.pem",destfile="cacert.pem")

#create an object "cred" that will save the authenticated object that we can 
#use for later sessions
cred <- OAuthFactory$new(consumerKey='pqbb2rRvz0wkSLWv2qYFJ4vAC',
                         consumerSecret='HKcZHHeqJY94VU83nawfGBefcK49ZEH58lVkrhbpmebZWMelIC',
                         requestURL='https://api.twitter.com/oauth/request_token',
                         accessURL='https://api.twitter.com/oauth/access_token',
                         authURL='https://api.twitter.com/oauth/authorize')

#Executing the next step generates an output --> To enable the connection, 
#please direct your web browser to: <hyperlink> . Note:  You only need to do this part once
cred$handshake(cainfo="cacert.pem")
5621800


#save for later use for Windows
save(cred, file="twitter authentication.Rdata")



#EXTRACT TWEETS
#Load “twitter authentication.Rdata” file in your session and run registerTwitterOAuth. 

load("twitter authentication.Rdata")
registerTwitterOAuth(cred)
#Twitter Authentication with R
install.packages("httr")
library("httr")
Consumer_key<- "AsWp2GC9HD68ul5hXh0vIfOAL"
Consumer_secret <- "dBQz5HeITyFsmcqQO2xcPsWTMlVLN85DUPPhtUHep5n73HWaiJ"
access_token <- "964058868992086016-vjVnFGDqFF1wEtng3qfiWKQZjKuSY4A"
access_token_secret <- "o5I3NCIaHP49VoW7VzzpnhI7vlzfTA2khdqdFGwOM4b04"

setup_twitter_oauth(Consumer_key,Consumer_secret,access_token,access_token_secret)

search.string <- "#datascience"
no.of.tweets <- 200

tweets <- searchTwitter(search.string, n=no.of.tweets, cainfo="cacert.pem", lang="en")
tweets
