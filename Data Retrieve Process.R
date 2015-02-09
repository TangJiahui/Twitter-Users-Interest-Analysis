#set the working directory (files will be saved/loaded # from this directory)


library(twitteR)
library(ROAuth)


#options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))

#window users need to get this file

download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem")
requestURL <- "https://api.twitter.com/oauth/request_token"
accessURL = "https://api.twitter.com/oauth/access_token"
authURL = "https://api.twitter.com/oauth/authorize"
consumerKey = ""
consumerSecret =""
Cred <- OAuthFactory$new(consumerKey=consumerKey, consumerSecret=consumerSecret, requestURL=requestURL, accessURL=accessURL, authURL=authURL)

Cred$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl") )


#Should then see this
#Open the URL on your web browser and take note of the PIN
To enable the connection, please direct your web browser to:
https://api.twitter.com/oauth/authorize?oauth_token=XXX
when complete, record the PIN given to you and provide it
here: (ENTER PIN)
#save the authentication file
save(Cred, file="twitter_auth.Rdata")
registerTwitterOAuth(Cred)

#subsequently we can load the file without going through
#the whole process again

load("twitter_auth.Rdata")
registerTwitterOAuth(Cred)


# for a certain twitter account
user <- userTimeline('FoodPornsx',n=500)
type <- rep("food",length(user))
user <- twListToDF(user)
user <- user$text
user <- cbind(user,type)
write.csv(user,"FoodPornsx.csv")

#rbind all data retrieved from diff accounts for news
ABC <- read.csv("ABC.csv")
BBCWorld <- read.csv("BBCWorld.csv")
BloombergNews <- read.csv("BloombergNews.csv")
CdnPress <- read.csv("CdnPress.csv")
ChannelNewsAsia <- read.csv("ChannelNewsAsia.csv")
FoxNews <- read.csv("FoxNews.csv")
LatestAusNews <- read.csv("LatestAusNews.csv")
nytimes <- read.csv("nytimes.csv")
STcom <- read.csv("STcom.csv")
TheSunNewspaper <- read.csv("TheSunNewspaper.csv")


news <- rbind(ABC,BBCWorld,BloombergNews,CdnPress,ChannelNewsAsia,FoxNews,LatestAusNews,
              nytimes,STcom,TheSunNewspaper)



#=========remove http links===============

food <- food[,-1]
food <- as.matrix(food)
food <- as.data.frame(food)
food[,1] <- as.character(food[,1])

for (i in 1:length(food[,1])){
  food[i,1] <- gsub('http\\S+\\s*', '', food[i,1])
  food[i,1] <- gsub('[\r\n\t]', '', food[i,1])
}

write.csv(food,"food.csv")

