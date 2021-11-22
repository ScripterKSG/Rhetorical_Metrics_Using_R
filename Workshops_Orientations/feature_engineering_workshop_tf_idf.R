# Feature Engineering Workshop: TF-IDF ------------------------------------

# This workshop oriented you to feature engineering using TF-IDF & data segmenting 


# Pre-Flight --------------------------------------------------------------

# Load libraries 
library(tidyverse)
library(stringr)
library(tidytext)


# Load data
data <- read_csv("~/datasets/football_tweets.csv") %>% select(status_id,school,text) %>% unique()

# Create binary label 
data_labels <- data %>% mutate(ut = ifelse(school=="ut","yes","no")) %>% select(status_id,ut)

# Engineer TF-IDF Features ------------------------------------------------

# Tokenize by term and bigram and get TFs by status_id
data_counts <- map_df(1:2, # map iterates over a list, in this case the list is 1:2
                      ~ unnest_tokens(data, word, text, 
                                      token = "ngrams", n = .x)) %>% # .x receives the values for the list
  anti_join(stop_words, by = "word") %>%
  count(status_id, word, sort = TRUE)

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
  bind_tf_idf(word, status_id, n) %>%
  cast_dtm(status_id, word, tf_idf)

# Add labels to DTM and filter for complete cases
data_engineered <- data_dtm %>% 
  as.matrix() %>% 
  as.data.frame() %>% 
  mutate(status_id = as.numeric(dimnames(data_dtm)[[1]])) %>% 
  right_join(data_labels) %>% 
  filter(complete.cases(.))


#  Create Train & Test Sets -----------------------------------------------

# Create training set based on an 80/20 split
training_set <- data_engineered %>% slice_sample(prop =.8)

# Create testing set
test_set <- data_engineered %>% anti_join(training_set, by="status_id") %>% select(-status_id)

# Create training set with No ID
training_set_no_id <- training_set %>% select(-status_id)