
#import libraries
library(tidyverse)
library(stringr)
library(tidytext)
library(caret)
library(pROC)

#get dataset and label yes/no for spam
data <- read_csv("~/datasets/spam_ham.csv") %>% 
  select(Type, Message) %>%
  mutate(spam = ifelse(Type=="spam","yes","no")) %>%
  unique()

#add unique row IDs
data$ID <- seq.int(nrow(data))


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
  right_join(data) %>% 
  filter(complete.cases(.))  


training_set <- data_engineered %>% slice_sample(prop =.8)

# Create testing set
test_set <- data_engineered %>% anti_join(training_set, by="ID") %>% select(-ID)

# Create training set with No ID
training_set_no_id <- training_set %>% select(-ID)
