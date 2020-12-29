if(!require(tidyverse)) 
  install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) 
  install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) 
  install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(tidytext)) 
  install.packages("tidytext", repos = "http://cran.us.r-project.org")
if(!require(wordcloud)) 
  install.packages("wordcloud", repos = "http://cran.us.r-project.org")
if(!require(wordcloud2)) 
  install.packages("wordcloud2", repos = "http://cran.us.r-project.org")
if(!require(RColorBrewer)) 
  install.packages("RColorBrewer", repos = "http://cran.us.r-project.org")
if(!require(tm)) 
  install.packages("tm", repos = "http://cran.us.r-project.org")


dl <- tempfile()
download.file("https://raw.githubusercontent.com/yhung615/Hotel-Capstone/main/tripadvisor_hotel_reviews.csv",dl)
rawdata <- read.csv(dl, sep = ",")

rating <- rawdata %>%
  group_by(Review) %>%
  mutate(
    reviewNum = row_number(),
    vocab = str_split(Review, " ")) %>%
  ungroup() %>%
  unnest_tokens(word, Review)

data <- rawdata %>% rownames_to_column()


sentiment_data <- data  %>% 
  unnest_tokens(word,Review) %>% 
  anti_join(stop_words) %>% 
  inner_join(get_sentiments("bing"))%>%    ##Retrieve sentiment values for keywords##
  count(Rating ,index= rowname, sentiment)%>%
  spread(sentiment, n, fill = 0) %>% 
  mutate(sentiment = positive-negative)   ##Generate overall sentiment values##


####################General Statistics##################################################


str(sentiment_data)

dim(sentiment_data)

n_distinct(sentiment_data$Rating)

####################Data Visualization##################################################


##Sentiment Distribution##

sentiment_data %>% ggplot(aes(x=sentiment)) + 
  geom_bar(stat = "count",binwidth = .5)+
  ggtitle("Distribution of Total sentiment counts in Ratings")+
  ylab("Count") + xlab("Sentiment Values")
sentiment_data$sentiment <- as.factor(sentiment_data$sentiment)

mean(sentiment_data$sentiment)

median(sentiment_data$sentiment)

quantile(sentiment_data$sentiment)

##Rating Distribution##

sentiment_data %>% group_by(Rating) %>% summarize(n = n()) %>%
  ggplot(aes(x = Rating, y = n)) + 
  geom_point() + geom_line() +
  scale_y_log10() + 
  ggtitle("Hotel Rating Distribution") + 
  ylab("Count") + xlab("Rating")

##Sentiment Distribution for individual Ratings##
sentiment_data %>%  ggplot(aes(index, sentiment, fill = Rating)) +
                    geom_col(show.legend = FALSE, width = 2) + 
                    facet_wrap(.~Rating, ncol = 5, scales = "free_x") +
                    ggtitle("Sentiment Variations in Different Rating Levels") +
                    xlab("Indexed Keywords") + ylab("Sentiment Values")

##Words distribution for both sentiments##

#We first generate an alternative sentiment data from our main dataset
#for alternative data visualization purposes##

alt_sentiment <- rawdata %>% 
  filter(Rating %in% c(1,5)) %>% 
  unnest_tokens(word, Review) %>% 
  anti_join(stop_words) %>% 
  inner_join(get_sentiments("bing"))

## A cumulation of popular words used for positive and negative 
#sentiments is shown using this code below##

alt_sentiment %>% group_by(sentiment, Rating) %>% 
  count(word) %>%
  top_n(10) %>% 
  ggplot(., aes(reorder(word, n), n, fill = Rating)) +
  geom_col(show.legend = T) +
  coord_flip() +
  facet_wrap(~sentiment, scales = "free_y") +
  xlab("Words") +
  ylab("frequency") +
  ggtitle("Word Usage")


##Word Cloud##

###1-Tiered Rating###

##Filtering out everything except for 1-star ratings##
worst_senti <- alt_sentiment %>% filter(Rating == 1) 

##making sure everything is correct and acknowledging this alternate dataset's parameters##
str(worst_senti) 

##Processing alternate dataset for generating wordclouds##
##Load dataset into Corpus##
neg_doc <- VCorpus(VectorSource(worst_senti$word)) 

##Replace special characters##
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
neg_doc <- tm_map(neg_doc, toSpace, "/")
neg_doc <- tm_map(neg_doc, toSpace, "@")
neg_doc <- tm_map(neg_doc, toSpace, "\\|")

##Apply all as lower case##
neg_doc <- tm_map(neg_doc,content_transformer(tolower))

##Removing words that's irrevelant to save processing space##
neg_doc <- tm_map(neg_doc, removeNumbers)
neg_doc <- tm_map(neg_doc, removeWords, stopwords("English"))
neg_doc <- tm_map(neg_doc, removeWords, 
                  c("hotel", "rooms", "room", "service", "staff", "resort", "location"))

##Removing punctuation, extra white spaces, and text stem##
neg_doc <- tm_map(neg_doc, removePunctuation)
neg_doc <- tm_map(neg_doc, stripWhitespace)
neg_doc <- tm_map(neg_doc, stemDocument)

##Turn text into matrix##
tdm <- TermDocumentMatrix(neg_doc)
m <- as.matrix(tdm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(b_word = names(v),freq=v)
head(d, 10) #Shows first 10 words & sentiments##


###5-Tiered Rating###

##Filtering out everything except for 5-star ratings##
best_senti <- alt_sentiment %>% filter(Rating == 5)

##making sure everything is correct and acknowledging this alternate dataset's parameters##
str(best_senti) 

##Processing alternate dataset for generating wordclouds##
##Load dataset into Corpus##
pos_doc <- VCorpus(VectorSource(best_senti$word)) 

##Replace special characters##
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
pos_doc <- tm_map(pos_doc, toSpace, "/")
pos_doc <- tm_map(pos_doc, toSpace, "@")
pos_doc <- tm_map(pos_doc, toSpace, "\\|")

##Apply all as lower case##
pos_doc <- tm_map(pos_doc,content_transformer(tolower))

##Removing words that's irrevelant to save processing space##
pos_doc <- tm_map(pos_doc, removeNumbers)
pos_doc <- tm_map(pos_doc, removeWords, stopwords("english"))
pos_doc <- tm_map(pos_doc, removeWords,
                  c("hotel", "rooms", "room", "service", "staff", "resort", "location"))

##Removing punctuation, extra white spaces, and text stem##
pos_doc <- tm_map(pos_doc, removePunctuation)
pos_doc <- tm_map(pos_doc, stripWhitespace)
pos_doc <- tm_map(pos_doc, stemDocument)

##Turn text into matrix##
tdm1 <- TermDocumentMatrix(pos_doc)
m1 <- as.matrix(tdm1)
v1 <- sort(rowSums(m1),decreasing=TRUE)
d1 <- data.frame(g_word = names(v1),freq=v1)
head(d1, 10) #Shows first 10 words & sentiments##

##Word Cloud - 1 star rating##
set.seed(2222)
wordcloud(words = d$b_word, freq = d$freq, min.freq = 1,
          max.words=100, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

##Word Cloud - 5 star rating##
set.seed(3333)
wordcloud(words = d1$g_word, freq = d1$freq, min.freq = 1,
          max.words=100, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

####################Model Preparation##################################################

##We drop the index to declutter the dataset for sentiment analysis and further modelling algorithms##

drop <- c("index")
sentiment_data <- sentiment_data[,!(names(sentiment_data) %in% drop)]

##Modelling Approach##

set.seed(1234, sample.kind="Rounding")
test_index <- initial_split(sentiment_data, prop = 0.7,
                            strata = "Rating")
train <- training(test_index)
test <- testing(test_index)

fitctrl <- trainControl(method = "cv", number = 10)

## Cross-Validation C5.0 ##
train_c5 <- train(as.factor(Rating) ~., data = train, method = "C5.0", trControl = fitctrl)

pred_c5 <- predict(train_c5, test)

####
#testing difference
diff <- (test$Rating)-(as.integer(pred_c5))
#Mean Squared Error
mse <- mean((diff)^2)
print(mse)
#Mean absolute error
mae <- mean(abs(diff))
print(mae)
#Root Mean Squared Error
rmse <- sqrt(mse)
print(rmse)


## Cross-Validation rf ##

c5_rf <- train(as.factor(Rating)~., data = train, method = "rf", trControl = fitctrl)

c5_rf_pred <- predict(c5_rf, test)

####
#testing
diff <- (test$Rating)-(as.integer(c5_rf_pred))
#Mean Squared Error
mse <- mean((diff)^2)
print(mse)
#Mean absolute error
mae <- mean(abs(diff))
print(mae)
#Root Mean Squared Error
rmse <- sqrt(mse)
print(rmse)

## SVMLinear2 Model ##

svm_linear <- train(as.factor(Rating)~., data = train, method = "svmLinear2", trControl = fitctrl)

svm_pred <- predict(svm_linear, test)

####
#testing
diff <- (test$Rating)-(as.integer(svm_pred))
#Mean Squared Error
mse <- mean((diff)^2)
print(mse)
#Mean absolute error
mae <- mean(abs(diff))
print(mae)
#Root Mean Squared Error
rmse <- sqrt(mse)
print(rmse)

## KNN ##
knn <- train(as.factor(Rating)~., 
             data = train, 
             method = "knn", 
             tuneGrid = data.frame(k = seq(1,5,2)))

knn$bestTune

knn$finalModel

knn_pred <- predict(knn, test)

#testing 
diff <- (test$Rating)-(as.integer(knn_pred))
#Mean Squared Error
mse <- mean((diff)^2)
print(mse)
#Mean absolute error
mae <- mean(abs(diff))
print(mae)
#Root Mean Squared Error
rmse <- sqrt(mse)
print(rmse)

## lm model ##

#Regression model training  
#linear regression model is implemented using the lm() function. 
#Rating is the dependent variable in the model and all other variables are used as predictors.
linear_reg <- lm(Rating ~. , data = train[-4])

#summary of the model
print(summary(linear_reg))

##plot Residual and fitted.
plot(linear_reg,1)

##we can tell that the distribution on this chart is not clearly distributed.

#Prediction of model
pred_lm <- predict(linear_reg,test,type = "response")

#testing
diff <- test$Rating - pred_lm
#Mean Squared Error
mse <- mean((diff)^2)
print(mse)
#Mean absolute error
mae <- mean(abs(diff))
print(mae)
#Root Mean Squared Error
rmse <- sqrt(mse)
print(rmse)

## Data Testing ##

## Example of prediction failure ##
print(test[10,1])
print(pred_lm[10])

## Example of prediction success ##
print(test[980,1])
print(pred_lm[980])

## Validation ##

## 10-Fold 1-time Cross Validation ##
set.seed(321)
train.control <- trainControl(method = "cv", number=10)
model <- train(Rating~., data = sentiment_data, method = "lm", trControl = train.control)
print(model)


## 10-fold 5-time Cross Validation ##
set.seed(321)
train.control <- trainControl(method = "repeatedcv", number=10, repeats = 5)
model <- train(Rating~., data = sentiment_data, method = "lm", trControl = train.control)
print(model)

