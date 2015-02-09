

#=============================TRAINING===============================

library("e1071")
library("RTextTools")
library("wordcloud")

options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))



set.seed(1)



art <- read.csv("arts.csv",header=T,na.string="")
art <- na.omit(art)
art <- as.character(art$user)
art <- sample(art,1500)
art_cat <- rep("arts",length(art))
art_data <-cbind(art,art_cat)

business <- read.csv("business.csv",header=T)
business <- as.character(business$user)
business <-sample(business,1500)
bus_cat <- rep("business",length(business))
bus_data <-cbind(business,bus_cat)

food <- read.csv("food.csv",header=T)
food <- as.character(food$user)
food <- sample(food,1500)
food_cat <- rep("food",length(food))
food_data <-cbind(food,food_cat)

fashion <- read.csv("fashion.csv",header=T)
fashion <- as.character(fashion$user)
fashion <- sample(fashion,1500)
fashion_cat <- rep("fashion",length(fashion))
fashion_data <-cbind(fashion,fashion_cat)

music <- read.csv("music.csv",header=T)
music <- as.character(music$user)
music <- sample(music,1500)
music_cat <- rep("music",length(music))
music_data <-cbind(music,music_cat)

news <- read.csv("news.csv",header=T)
news <- as.character(news$user)
news <- sample(news,1500)
news_cat <- rep("news",length(news))
news_data <-cbind(news,news_cat)

politics <- read.csv("politics.csv",header=T)
politics <- as.character(politics$user)
politics <- sample(politics,1500)
politics_cat <- rep("politics",length(politics))
politics_data <-cbind(politics,politics_cat)

sport <- read.csv("sports.csv",header=T)
sport <- as.character(sport$user)
sport <- sample(sport, 1500)
sport_cat <- rep("sport",length(sport))
sport_data <-cbind(sport,sport_cat)

technology <- read.csv("technology.csv",header=T)
technology <- as.character(technology$user)
technology <- sample(technology,1500)
tech_cat <- rep("technology",length(technology))
tech_data <-cbind(technology,tech_cat)

total <- rbind(art_data,bus_data,politics_data,food_data,sport_data,tech_data,music_data,fashion_data,news_data)


#==========testing=========
## There is another file to do testing for each individual category
## this part is not used eventually as our testing method.

art <- read.csv("arts.csv",header=T)
business <- read.csv("business.csv",header=T)
food <- read.csv("food.csv",header=T)
fashion <- read.csv("fashion.csv",header=T)
music <- read.csv("music.csv",header=T)
news <- read.csv("news.csv",header=T)
politics <- read.csv("politics.csv",header=T)
sport <- read.csv("sports.csv",header=T)
technology <- read.csv("technology.csv",header=T)

previous <- rbind(art,business,food,fashion,music,news,politics,sport,technology)
cat <- as.data.frame(previous$type)
user <- as.character(previous$user)
previous <- cbind(user,cat)
remain <- previous[!(previous[,1] %in% total[,1]),]
remain <- as.data.frame(remain)

sam <- sample.int(length(remain[,1]),1000)
remain <-remain[sam,]


correct <- 0
for (i in 1:length(remain[,1])){
  text <- remain[i,1]
  result <- predictTest(text, mat, svm_classifier) 
  if (result == as.character(remain[i,2])){
    correct = correct+1 }
 }
cat("Number of correct classification: ", correct)




#==========================Train the Classifier===================

matrix <- create_matrix(total[,1], language="english", removeStopwords=T, removeNumbers=T,stemWords=T, toLower=T, removePunctuation=T, removeSparseTerms=0.998)
mat <- as.matrix(matrix)
svm_classifier = svm(mat,as.factor(total[,2]))

save(svm_classifier, file = "model_1500_with_other.rda")




#===========================FUNCTIONS==============================
# tweets <- get_tweets("BBCWorld")

predictTest <- function(test_user, mat, classifier){
  train_mat = mat[1:2,]
  train_mat[,1:ncol(train_mat)] = 0
  
  test_matrix = create_matrix(test_user, language="english", removeStopwords=T, removeNumbers=T,stemWords=T, toLower=T, removePunctuation=T)
  test_mat <- as.matrix(test_matrix)
  flag <- F
  for(col in colnames(test_mat)){
    if(col %in% colnames(train_mat))
    {
      flag <- T
      train_mat[2,col] = test_mat[1,col];
    }
  }
  if (flag == F)
  {
    "others"
  }
  else{
  
    #test_mat = as.matrix(t(test_mat))
    row.names(train_mat)[1] = ""
    row.names(train_mat)[2] = test_user
    p <- predict(classifier, train_mat[1:2,])
    as.character(p[2])
  }
}

get_tweets <- function(user_name){
  user <- userTimeline(user_name,n=50)
  user <-twListToDF(user)
  tweets <- as.data.frame(user$text)
  tweets[,1] <- as.character(tweets[,1])
  for (i in 1:length(tweets[,1])){
    tweets[i,1] <- gsub('http\\S+\\s*', '', tweets[i,1])
    tweets[i,1] <- gsub('[\r\n\t]', '', tweets[i,1])
  }
  na.omit(tweets)
}

# user_table <- get_result_table("David_Cameron")

get_result_table <- function(user_name){
  times <- as.data.frame(rep.int(0,10))
  percentage <- as.data.frame(rep.int(0,10))
  classes <- c("others","arts","business","food","fashion","music","news","politics","sport","technology")
  result_table <- cbind(classes, times,percentage)
  colnames(result_table) <- c("classes", "times","percentage")
  tweets <- get_tweets(user_name)
  num_tweets = 0;
  for (i in 1:length(tweets[,1])){
    current <- tweets[i,1]
    result <- predictTest(current, mat, svm_classifier)
    # print(result)
    for (j in 1:10){
      if (result_table[j,1]==result){
        result_table[j,2]=result_table[j,2]+1
        num_tweets = num_tweets + 1
      }
    }
  }
  for (j in 1:10){
    result_table[j,3] = result_table[j,2]/num_tweets
  }
  print_word_cloud(result_table)
  result_table[order(-result_table[,3]),]
}


# areas <- interestedArea("BBCWorld")

interestedArea <- function(user_name){
  result_table <- get_result_table(user_name)
  list <- result_table[c(1,2,3,4,5),]
  list <- list[,1]
  as.data.frame(list)
}

print_word_cloud(user_table)

print_word_cloud <- function(table){
  classes <- c("others","arts","business","food","fashion","music","news","politics","sport","technology")
  data <- table[,2]
  wordcloud(classes,data,colors=c(1:10),ordered.colors=T)
}

make_friend <- function(userA, userB){
  areaA <- interestedArea(userA)
  areaB <- interestedArea(userB)
  common <- merge(areaA,areaB,all=F)
  if (length(common)==0){
    cat("No common interested area!")
  }
  else{
    cat("The common interested areas betwwen ",userA," and ",userB,":\n")
  }
  common
}



filter <- function(topics,user_name){
  tweets <- get_tweets(user_name);
  for (i in 1:length(tweets[,1])){
    current <- tweets[i,1]
    result <- predictTest(current, mat, svm_classifier)
    for (j in 1:length(topics)){
      if (result == topics[j]){
        cat(current,"\n")
        break
      }

    }
  }
}

#============================TESTING===================================
library(twitteR)
library(ROAuth)
# 
# window users need to get this file
# 
download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem")
requestURL <- "https://api.twitter.com/oauth/request_token"
accessURL = "https://api.twitter.com/oauth/access_token"
authURL = "https://api.twitter.com/oauth/authorize"
# 
# 
consumerKey = ""
consumerSecret =""
# 
# 
Cred <- OAuthFactory$new(consumerKey=consumerKey, consumerSecret=consumerSecret, requestURL=requestURL, accessURL=accessURL, authURL=authURL)
# 
Cred$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl") )
# 
# 
# #Should then see this
# #Open the URL on your web browser and take note of the PIN
# To enable the connection, please direct your web browser to:
#   https://api.twitter.com/oauth/authorize?oauth_token=XXX
# when complete, record the PIN given to you and provide it
# here: (ENTER PIN)
# #save the authentication file
save(Cred, file="twitter_auth.Rdata")
registerTwitterOAuth(Cred)
# 
# #subsequently we can load the file without going through
# #the whole process again

load("twitter_auth.Rdata")
registerTwitterOAuth(Cred)
load(file = "model_1500.rda")


# 
# #test
# make_friend("BarackObama","David_Cameron")
get_result_table("David_Cameron")
get_result_table("BarackObama")

interested_topics= c("news")
filter(interested_topics,"David_Cameron")
