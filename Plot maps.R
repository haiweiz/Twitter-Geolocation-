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

#organize data 
Rbloggers_followers_df <- readRDS("followers.rds")
#Remove none location
Rbloggers_followers_df1 <- subset(Rbloggers_followers_df,location!="")
#Remove any instances of %
Rbloggers_followers_df1$location <- gsub("%","",Rbloggers_followers_df1$location)
length(Rbloggers_followers_df1$location)
#sample from the big data set
samplelocation <- Rbloggers_followers_df1$location[seq(1,20001,40)]
#sl for state and slw for world
sl <- na.omit(samplelocation)
sll <- geocode(sl)
sll <- geocode(sl[100:200])
length(sl)
sll <- subset(sll,sll$lon!="NA")
sll <- subset(sll,sll$lon<(-66)&sll$lon>-125&sll$lat>25&sll$lat<50)
sll
points <- data.frame(x=as.numeric(sll$lon),y=as.numeric(sll$lat))

slw <- subset(sll,sll$lon!="NA")
length(slw)
pointsw <- data.frame(x=as.numeric(slw$lon),y=as.numeric(slw$lat))

#Clean geocoding result
map.data <- map_data("state")

ggplot(map.data) + 
  geom_map(aes(map_id = region), 
           map = map.data, 
           fill = "white",       
           color = "grey20", size = 0.5) + 
  expand_limits(x = map.data$long, y = map.data$lat) +       
  theme(axis.line = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(),           
        axis.title = element_blank(), 
        panel.background = element_blank(), 
        panel.border = element_blank(),           
        panel.grid.major = element_blank(), 
        plot.background = element_blank(),           
        plot.margin = unit(0 * c(-1.5, -1.5, -1.5, -1.5), "lines")) + 
  geom_point(data = points,       
             aes(x = x, y = y), size = 5, 
             alpha = 1/5, color = "darkblue") 

#world map
map.data <- map_data("world")

ggplot(map.data) + 
  geom_map(aes(map_id = region), 
           map = map.data, 
           fill = "white",       
           color = "grey20", size = 0.5) + 
  expand_limits(x = map.data$long, y = map.data$lat) +       
  theme(axis.line = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(),           
        axis.title = element_blank(), 
        panel.background = element_blank(), 
        panel.border = element_blank(),           
        panel.grid.major = element_blank(), 
        plot.background = element_blank(),           
        plot.margin = unit(0 * c(-1.5, -1.5, -1.5, -1.5), "lines")) + 
  geom_point(data = pointsw,       
             aes(x = x, y = y), size = 3, 
             alpha = 1/5, color = "darkblue") 


#active users
activelocation <- Rbloggers_followers_df1$location[Rbloggers_followers_df1$statusesCount>10000]
sl2 <- na.omit(samplelocation)
sll2 <- geocode(sl2)
sll2 <- subset(sll,sll$lon!="NA")
sll2 <- subset(sll,sll$lon<(-66)&sll$lon>-125&sll$lat>25&sll$lat<50)
sll2
points2 <- data.frame(x=as.numeric(sll2$lon),y=as.numeric(sll2$lat))

#Clean geocoding result
map.data <- map_data("state")

ggplot(map.data) + 
  geom_map(aes(map_id = region), 
           map = map.data, 
           fill = "white",       
           color = "grey20", size = 0.5) + 
  expand_limits(x = map.data$long, y = map.data$lat) +       
  theme(axis.line = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(),           
        axis.title = element_blank(), 
        panel.background = element_blank(), 
        panel.border = element_blank(),           
        panel.grid.major = element_blank(), 
        plot.background = element_blank(),           
        plot.margin = unit(0 * c(-1.5, -1.5, -1.5, -1.5), "lines")) + 
  geom_point(data = points2,       
             aes(x = x, y = y), size = 3, 
             alpha = 1/5, color = "red") 


#Hashtag analysis
hashtag <- readRDS("data.RDS")
hashtag1 <- subset(hashtag,lat!="")
#Remove any instances of %
hashtag1$location <- gsub("%","",hashtag1$location)
length(hashtag1$location)
samplelocation3 <- hashtag1$location

points3 <- data.frame(x=as.numeric(hashtag1$lon),y=as.numeric(hashtag1$lat))

#Clean geocoding result
map.data <- map_data("state")

ggplot(map.data) + 
  geom_map(aes(map_id = region), 
           map = map.data, 
           fill = "white",       
           color = "grey20", size = 0.5) + 
  expand_limits(x = map.data$long, y = map.data$lat) +       
  theme(axis.line = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(),           
        axis.title = element_blank(), 
        panel.background = element_blank(), 
        panel.border = element_blank(),           
        panel.grid.major = element_blank(), 
        plot.background = element_blank(),           
        plot.margin = unit(0 * c(-1.5, -1.5, -1.5, -1.5), "lines")) + 
  geom_point(data = points3,       
             aes(x = x, y = y), size = 4, 
             alpha = 1/5, color = "black") 

View(hashtag1)
