
library(rtweet)
library(httr)
library(tidyverse)
library(caret)

### First part of code demonstrates twitter scrapping, not necessary to run
### as we already saved a csv of a dataset from earlier

### Put project name, consumer and secret keys, access token and secret
### Tokens are already saved in RDS files

# appname <- ""
# key <- ""
# secret <- ""
# atoken <- ""
# asecret<- ""


### Saved Tokens to RDS files

# saveRDS(appname, file = "appname.rds")
# saveRDS(key, file = "key.rds")
# saveRDS(secret, file = "secret.rds")
# saveRDS(atoken, file = "atoken.rds")
# saveRDS(asecret, file = "asecret.rds")

### create token
### reads keys from RDS files

twitter_token <- create_token(
  app = readRDS("appname.rds"),
  consumer_key =  readRDS("key.rds"),
  consumer_secret =  readRDS("secret.rds"),
  access_token =  readRDS("atoken.rds"),
  access_secret =  readRDS("asecret.rds"))

## search for tweets involving america and europe
rt <- search_tweets("america", n = 500)
rt2 <- search_tweets("europe", n = 500)

# select needed columns, mutate and rename full_text col to be a label
rt <-  rt %>% select(text, full_text) %>% mutate(full_text = "yes")
names(rt)[2] <- "usa"
rt2 <- rt2 %>% select(text, full_text) %>% mutate(full_text = "no")
names(rt2)[2] <- "usa"

#create our dataset
data <- rbind(rt, rt2)
data$ID <- seq.int(nrow(data))

#save dataset
write.csv(data,"~/nguyen_jason/us_eu_tweets.csv", row.names = FALSE)



####ACTUAL ML SCRIPT BELOW


data <- read_csv("~/nguyen_jason/us_eu_tweets.csv")
data_labels <- data %>% select(ID, usa)

# Tokenize by term and bigram and get TFs by status_id
data_counts <- map_df(1:2, # map iterates over a list, in this case the list is 1:2
                      ~ unnest_tokens(data, word, text, 
                                      token = "ngrams", n = .x)) %>% # .x receives the values for the list
  anti_join(stop_words, by = "word") %>%
  count(ID, word, sort = TRUE)

# Get only terms and bigrams with a TF > 10 
words_10 <- data_counts %>%
  group_by(word) %>%
  summarise(n = n()) %>% 
  filter(n >= 10) %>%
  select(word) %>%
  na.omit()

# Create a document term matrix (DTM)
data_dtm <- data_counts %>%
  right_join(words_10, by = "word") %>%
  bind_tf_idf(word, ID, n) %>%
  cast_dtm(ID, word, tf_idf)

# Add labels to DTM and filter for complete cases
data_engineered <- data_dtm %>% 
  as.matrix() %>% 
  as.data.frame() %>% 
  mutate(ID = as.numeric(dimnames(data_dtm)[[1]])) %>% 
  right_join(data_labels) %>% 
  filter(complete.cases(.))

# Create training set based on an 80/20 split
training_set <- data_engineered %>% slice_sample(prop =.8)

# Create testing set
test_set <- data_engineered %>% anti_join(training_set, by="ID") %>% select(-ID)

# Create training set with No ID
training_set_no_id <- training_set %>% select(-ID)


# Set train control to repeated cross-validation 
fitControl <- trainControl(
  method = "repeatedcv", # set method to repeated cross-validation 
  number = 5, # Divide data into 5 sub-sets
  repeats = 5) # Repeat the cross-validation 5 times

# Train SVM model 
svm_mod <- train(usa ~ ., # predict china based on the remaining data 
                 data = training_set_no_id, # identify the training data set 
                 method = "svmLinearWeights2", # specify the algorithm to be used 
                 trControl = fitControl, # import the train control parameters 
                 tuneGrid = data.frame(cost = 1, 
                                       Loss = 0, 
                                       weight = 1)) # provide tuning parameters (basically ignore these)

# Predict against test set 
svm_pred <- test_set %>% 
  select(-usa) %>% # get rid of the label so it's a fair prediction 
  predict(svm_mod, newdata = .) # use the pre-trained model (svm_mod) to predict the label for the test set data


# Get confusion matrix, agreement, and accuracy values 
confusionMatrix(svm_pred,as.factor(test_set$usa))
