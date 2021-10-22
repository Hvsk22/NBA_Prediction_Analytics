library(Rcpp)
library(twitteR)
library(NLP)
library(RColorBrewer)
library(tm)
library(wordcloud)

###### Secret Keys stored in GIT Secrets ###############

consumerKey=Sys.getenv("CONSUMER_KEY") #accessing the secrets from git
consumerSecret=Sys.getenv("CONSUMER_SECRET_KEY")
accessToken=Sys.getenv("ACCESS_TOKEN")
accessTokenSecret= Sys.getenv("ACCESS_TOKEN_SECRET")

#########################################################

####### Authenticating connection to Twitter using Secrets #################

setup_twitter_oauth(consumerKey,consumerSecret,accessToken,accessTokenSecret)

############################################################################

###### Search and store 10,000 Tweets with #NBA in Language English ############
tweets <- searchTwitter("#NBA", n=1000, lang="en")
tweets.text <- sapply(tweets, function(x) x$getText())

##### Storing cleaned data for 10K tweets for use in Shiny ########
save(tweets.text, file = "./Data/tweets_10K_clean.RData")
