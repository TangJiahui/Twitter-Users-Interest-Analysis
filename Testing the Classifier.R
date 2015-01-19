set.seed(1)
art <- read.csv("arts.csv",header=T)
business <- read.csv("business.csv",header=T)
food <- read.csv("food.csv",header=T)
fashion <- read.csv("fashion.csv",header=T)
music <- read.csv("music.csv",header=T)
news <- read.csv("news.csv",header=T)
politics <- read.csv("politics.csv",header=T)
sports <- read.csv("sports.csv",header=T)
technology <- read.csv("technology.csv",header=T)

cat<-as.data.frame(art$type)
user<-as.character(art$user)
art<-cbind(user,cat)
art_remain<-(art[!(art[,1] %in% art_data[,1]),])
art_remain[,1] <- as.character(art_remain[,1])

cat<-as.data.frame(business$type)
user<-as.character(business$user)
business<-cbind(user,cat)
business_remain<-business[!(business[,1] %in% bus_data[,1]),]
business_remain[,1] <- as.character(business_remain[,1])

cat<-as.data.frame(food$type)
user<-as.character(food$user)
food<-cbind(user,cat)
food_remain<-food[!(food[,1] %in% food_data[,1]),]
food_remain[,1] <- as.character(food_remain[,1])

cat<-as.data.frame(fashion$type)
user<-as.character(fashion$user)
fashion<-cbind(user,cat)
fashion_remain<-fashion[!(fashion[,1] %in% fashion_data[,1]),]
fashion_remain[,1] <- as.character(fashion_remain[,1])

cat<-as.data.frame(music$type)
user<-as.character(music$user)
music<-cbind(user,cat)
music_remain<-music[!(music[,1] %in% music_data[,1]),]
music_remain[,1] <- as.character(music_remain[,1])

cat<-as.data.frame(news$type)
user<-as.character(news$user)
news<-cbind(user,cat)
news_remain<-news[!(news[,1] %in% news_data[,1]),]
news_remain[,1] <- as.character(news_remain[,1])

cat<-as.data.frame(politics$type)
user<-as.character(politics$user)
politics<-cbind(user,cat)
politics_remain<-politics[!(politics[,1] %in% politics_data[,1]),]
politics_remain[,1] <- as.character(politics_remain[,1])

cat<-as.data.frame(sports$type)
user<-as.character(sports$user)
sports<-cbind(user,cat)
sports_remain<-sports[!(sports[,1] %in% sports_data[,1]),]
sports_remain[,1] <- as.character(sports_remain[,1])

cat<-as.data.frame(technology$type)
user<-as.character(technology$user)
technology<-cbind(user,cat)
technology_remain<-technology[!(technology[,1] %in% tech_data[,1]),]
technology_remain[,1] <- as.character(technology_remain[,1])

sam1 <-sample.int(length(art_remain[,1]),100)
art_remain<-art_remain[sam1,]

sam2 <-sample.int(length(business_remain[,1]),100)
business_remain<-business_remain[sam2,]

sam3 <-sample.int(length(food_remain[,1]),100)
food_remain<-food_remain[sam3,]

sam4 <-sample.int(length(fashion_remain[,1]),100)
fashion_remain<-fashion_remain[sam4,]

sam5 <-sample.int(length(music_remain[,1]),23)
music_remain<-music_remain[sam5,]

sam6 <-sample.int(length(news_remain[,1]),100)
news_remain<-news_remain[sam6,]

sam7 <-sample.int(length(politics_remain[,1]),100)
politics_remain<-politics_remain[sam7,]

sam8 <-sample.int(length(sports_remain[,1]),100)
sports_remain<-sports_remain[sam8,]

sam9 <-sample.int(length(technology_remain[,1]),100)
technology_remain<-technology_remain[sam9,]

correct <- 0
for (i in 1:length(art_remain[,1])){
  text <- art_remain[i,1]
  if (text != "") {
  result <- predictTest(text, mat, nb_classifier) 
  if (result == as.character(art_remain[i,2])){
    correct = correct+1 }}
}
cat("Number of correct classification of arts: ", correct) 

correct <- 0
for (i in 1:length(business_remain[,1])){
  text <- business_remain[i,1]
  if (text != "") {
  result <- predictTest(text, mat, nb_classifier) 
  if (result == as.character(business_remain[i,2])){
    correct = correct+1 }}
}
cat("Number of correct classification of business: ", correct) 

correct <- 0
for (i in 1:length(food_remain[,1])){
  text <- food_remain[i,1]
  if (text != "") {
  result <- predictTest(text, mat, nb_classifier) 
  if (result == as.character(food_remain[i,2])){
    correct = correct+1 }}
}
cat("Number of correct classification of food: ", correct) 

correct <- 0
for (i in 1:length(fashion_remain[,1])){
  text <- fashion_remain[i,1]
  if (text != "") {
  result <- predictTest(text, mat, nb_classifier) 
  if (result == as.character(fashion_remain[i,2])){
    correct = correct+1 }}
}
cat("Number of correct classification of fashion: ", correct) 

correct <- 0
for (i in 1:length(music_remain[,1])){
  text <- music_remain[i,1]
  if (text != "") {
  result <- predictTest(text, mat, nb_classifier) 
  if (result == as.character(music_remain[i,2])){
    correct = correct+1 }}
}
cat("Number of correct classification of music: ", correct) 

correct <- 0
for (i in 1:length(news_remain[,1])){
  text <- news_remain[i,1]
  if (text != "") {
  result <- predictTest(text, mat, nb_classifier) 
  if (result == as.character(news_remain[i,2])){
    correct = correct+1 }}
}
cat("Number of correct classification of news: ", correct) 

correct <- 0
for (i in 1:length(politics_remain[,1])){
  text <- politics_remain[i,1]
  if (text != "") {
  result <- predictTest(text, mat, nb_classifier) 
  if (result == as.character(politics_remain[i,2])){
    correct = correct+1 }}
}
cat("Number of correct classification of politics: ", correct) 

correct <- 0
for (i in 1:length(sports_remain[,1])){
  text <- sports_remain[i,1]
  if (text != "") {
  result <- predictTest(text, mat, nb_classifier) 
  if (result == as.character(sports_remain[i,2])){
    correct = correct+1 }}
}
cat("Number of correct classification of sports: ", correct) 

correct <- 0
for (i in 1:length(technology_remain[,1])){
  text <- technology_remain[i,1]
  if (text != "") {
  result <- predictTest(text, mat, nb_classifier) 
  if (result == as.character(technology_remain[i,2])){
    correct = correct+1 }}
}
cat("Number of correct classification of technology: ", correct) 