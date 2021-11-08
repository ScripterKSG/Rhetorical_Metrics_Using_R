
#import libraries
library(tidyverse)
library(stringr)
library(tidytext)
library(caret)
library(pROC)

#get dataset,
#add unique row IDs
#and label yes/no for spam
data <- read_csv("~/datasets/spam_ham.csv") %>% 
  select(Type, Message) %>%
  unique()

data$ID <- seq.int(nrow(data))

data_labels <- data %>% mutate(spam = ifelse(Type=="spam","yes","no")) %>% select(ID,spam)


data_counts <- map_df(1:2, # map iterates over a list, in this case the list is 1:2
                      ~ unnest_tokens(data, word, Message, 
                                      token = "ngrams", n = .x)) %>% # .x receives the values for the list
  anti_join(stop_words, by = "word") %>%
  count(ID, word, sort = TRUE)

words_10 <- data_counts %>%
  group_by(word) %>%
  summarise(n = n()) %>% 
  filter(n >= 10) %>%
  select(word) %>%
  na.omit()

data_dtm <- data_counts %>%
  right_join(words_10, by = "word") %>%
  bind_tf_idf(word, ID, n) %>%
  cast_dtm(ID, word, tf_idf)

data_engineered <- data_dtm %>% 
  as.matrix() %>% 
  as.data.frame() %>% 
  mutate(ID = as.numeric(dimnames(data_dtm)[[1]])) %>% 
  right_join(data_labels) %>% 
  filter(complete.cases(.))  


training_set <- data_engineered %>% slice_sample(prop =.8)

# Create testing set
test_set <- data_engineered %>% anti_join(training_set, by="ID") %>% select(-ID)

# Create training set with No ID
training_set_no_id <- training_set %>% select(-ID)



fitControl <- trainControl(
  method = "repeatedcv", # set method to repeated cross-validation 
  number = 5, # Divide data into 5 sub-sets
  repeats = 5) # Repeat the cross-validation 5 times

# Train SVM model 
svm_mod <- train(spam ~ ., # predict spam based on the remaining data 
                 data = training_set_no_id, # identify the training data set 
                 method = "svmLinearWeights2", # specify the algorithm to be used 
                 trControl = fitControl, # import the train control parameters 
                 tuneGrid = data.frame(cost = 1, 
                                       Loss = 0, 
                                       weight = 1)) # provide tuning parameters (basically ignore these)

# Predict against test set 
svm_pred <- test_set %>% 
  select(-spam) %>% # get rid of the label so it's a fair prediction 
  predict(svm_mod, newdata = .) # use the pre-trained model (svm_mod) to predict the label for the test set data


# Get confusion matrix, agreement, and accuracy values 
confusionMatrix(svm_pred,as.factor(test_set$spam))
