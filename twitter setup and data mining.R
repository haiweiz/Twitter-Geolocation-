library(rjson)
library(bit64)
library(httr)
library(devtools)
library(twitteR)
library(ROAuth)
library(streamR)

## Connect to twitter account 
requestURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"
consumerKey <- "1onR7BMwlK4sjMj9aum29yS1e"
consumerSecret <- "SVtfnL3fyIlb4piPI9AoeFHVyfqEka1wHN0DrwNaU5U1iZX4nF"
my_oauth <- OAuthFactory$new(consumerKey = consumerKey, consumerSecret = consumerSecret, 
                             requestURL = requestURL, accessURL = accessURL, authURL = authURL)
my_oauth$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))

save(my_oauth, file = "my_oauth.Rdata")


load("my_oauth.Rdata")

filterStream("tweets.json", 
             track=c("#rstats", "#DataScience"),
             timeout=600, oauth=my_oauth,locations=c(-125,25,-66,50))
tweets.df <- parseTweets("tweets.json", simplify=FALSE)

## Write table into RDS file
saveRDS(tweets.df,"data.RDS")
saveRDS(tweets.geo,"data_geo.RDS")
tweets.geo <- tweets.df[-which(is.na(tweets.df$full_name)),]

#get friends and followers
user <- getUser("Rbloggers")
user$toDataFrame()
friends <- Rbloggers$getFriends() # who this user follows
followers <- Rbloggers$getFollowers(retryOnRateLimit=1000) # this user's followers
saveRDS(friends,file = "friends.rds")
Rbloggers
saveRDS(Rbloggers_followers_df,file = "followers.rds")
