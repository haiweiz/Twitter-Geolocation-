library(sp)
library(RgoogleMaps)
library(ggplot2)
library(ggmap)
library(maptools)
library(datasets)
library(tigris)
library(wordcloud)
library(wordcloud2)
library(twitteR)
library(topicmodels)
library(data.table)
library(twitteR)
library(tm)
library(devtools)
library(sentiment)
library(car)
source("http://bioconductor.org/biocLite.R")
biocLite("Rgraphviz")
library(Rgraphviz)

#Enable Pings to the Twitter API
consumer_key <- "1onR7BMwlK4sjMj9aum29yS1e"
consumer_secret <- "SVtfnL3fyIlb4piPI9AoeFHVyfqEka1wHN0DrwNaU5U1iZX4nF"
access_token <- "2977662430-XH3adnIZRnA5N2hRz3XJfIafdttFG3C454H49OM"
access_secret <- "aiuaGR658BgK8R9fei3ZCrmslTcHt0SYrAzlXYlW1xC4Q"
options(httr_oauth_cache=T) #This will enable the use of a local file to cache OAuth access credentials between R sessions.
setup_twitter_oauth(consumer_key,
                    consumer_secret,
                    access_token,
                    access_secret)

#Get users' data of Rbloggers
Rbloggers <- getUser("Rbloggers")
location(Rbloggers)

#Use timeline to retrieve users' tweets
tweets <- userTimeline("Rbloggers",n=3000)
head(tweets) 
tweets.df <- twListToDF(tweets)
#test tweets, show the #19 tweet
tweets.df[19, c("id", "created", "screenName", "replyToSN","favoriteCount", "retweetCount", "longitude", "latitude", "text")]

# print tweet #19 and make text fit for slide width
writeLines(strwrap(tweets.df$text[19], 60))

# build a corpus, and specify the source to be character vectors
myCorpus <- Corpus(VectorSource(tweets.df$text))
# convert to lower case
myCorpus <- tm_map(myCorpus, content_transformer(tolower))
# remove URLs
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
myCorpus <- tm_map(myCorpus, content_transformer(removeURL))
# remove anything other than English letters or space
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
myCorpus <- tm_map(myCorpus, content_transformer(removeNumPunct))
# remove stopwords
myStopwords <- c(setdiff(stopwords('english'), c("r", "big")),
                 "use", "see", "used", "via", "amp")
myCorpus <- tm_map(myCorpus, removeWords, myStopwords)
# remove extra whitespace
myCorpus <- tm_map(myCorpus, stripWhitespace)
# keep a copy for stem completion later
myCorpusCopy <- myCorpus

myCorpus <- tm_map(myCorpus, stemDocument) # stem words
writeLines(strwrap(myCorpus[[19]]$content, 60))

stemCompletion2 <- function(x, dictionary){
  x <- unlist(strsplit(as.character(x), " "))
  x <- x[x != ""]
  x <- stemCompletion(x, dictionary=dictionary)
  x <- paste(x, sep="", collapse=" ")
  PlainTextDocument(stripWhitespace(x))
}

myCorpus <- Corpus(VectorSource(myCorpus))
writeLines(strwrap(myCorpus[[19]]$content, 60))

#Create term document matrix
tdm <- TermDocumentMatrix(myCorpus,control = list(wordLengths = c(1, Inf)))
tdm

idx <- which(dimnames(tdm)$Terms %in% c("r", "data", "stat"))
as.matrix(tdm[idx, 21:30])

#Get every word's frequency, I choose 50 whihc means over 50 is regarded as high-frequency
freq.terms <- findFreqTerms(tdm, lowfreq = 50)
print(freq.terms)

term.freq <- rowSums(as.matrix(tdm))
term.freq <- subset(term.freq, term.freq >= 50)
df <- data.frame(term = names(term.freq), freq = term.freq)

#plot the frequency of high-frequency words
ggplot(df, aes(x=term, y=freq)) + geom_bar(stat="identity") +
  xlab("Terms") + ylab("Count") + coord_flip() +
  theme(axis.text=element_text(size=7))

#word cloud
m <- as.matrix(tdm)
# calculate the frequency of words and sort it by frequency
word.freq <- sort(rowSums(m), decreasing = T)
# colors
pal <- brewer.pal(9, "BuGn")[-(1:4)]
View(word.freq)
#test word.freq
word.freq[2]

# plot word cloud
wordcloud(words = names(word.freq), freq = word.freq, min.freq = 10,
          random.order = F, colors = pal)

#word association
# which words are associated with 'r'?
findAssocs(tdm, "r", 0.2)

# which words are associated with 'data'?
findAssocs(tdm, "data", 0.2)
#plot
plot(tdm, term = freq.terms, corThreshold = 0.1, weighting = T)

#Topic models
dtm <- as.DocumentTermMatrix(tdm)
library(topicmodels)
lda <- LDA(dtm, k = 8) # find 8 topics
term <- terms(lda, 7) # first 7 terms of every topic
(term <- apply(term, MARGIN = 2, paste, collapse = ", "))
topics <- topics(lda) # 1st topic identified for every document (tweet)
topics <- data.frame(date=as.IDate(tweets.df$created), topic=topics)
ggplot(topics, aes(date, fill = term[topic])) +
  geom_density(position = "stack")

install_github("okugami79/sentiment140")

sentiments <- sentiment(tweets.df$text)
table(sentiments$polarity)

# sentiment plot
sentiments$score <- 0
sentiments$score[sentiments$polarity == "positive"] <- 1
sentiments$score[sentiments$polarity == "negative"] <- -1
sentiments$date <- as.IDate(tweets.df$created)
result <- aggregate(score ~ date, data = sentiments, sum)
plot(result, type = "l")

# select top retweeted tweets
table(tweets.df$retweetCount)
selected <- which(tweets.df$retweetCount >= 50)
tweets.df$text[selected]
# plot them
dates <- strptime(tweets.df$created, format="%Y-%m-%d")
plot(x=dates, y=tweets.df$retweetCount, type="l", col="grey",
     xlab="Date", ylab="Times retweeted")
colors <- rainbow(10)[1:length(selected)]
points(dates[selected], tweets.df$retweetCount[selected],
       pch=19, col=colors)
text(dates[selected], tweets.df$retweetCount[selected],
     tweets.df$text[selected], col=colors, cex=.9)

#Users' data Analysis
#lm(Rbloggers_followers_df1$statusesCount~Rbloggers_followers_df1$followersCount)
#m <- lm(Rbloggers_followers_df1$followersCount~Rbloggers_followers_df1$statusesCount+
          #Rbloggers_followers_df1$favoritesCount+Rbloggers_followers_df1$friendsCount)
#summary(m)
#anova(m)
#scatterplotMatrix(~Rbloggers_followers_df1$followersCount+Rbloggers_followers_df1$statusesCount+
 #                   Rbloggers_followers_df1$favoritesCount+Rbloggers_followers_df1$friendsCount)


