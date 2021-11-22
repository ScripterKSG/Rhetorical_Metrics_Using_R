# Feature Engineering: One-Hot --------------------------------------------

# This workshop is a demo of Keras One-Hot Encoding 

# Pre-Flight --------------------------------------------------------------

# load libraries 
library(keras)
library(tidyverse)
library(stringr)

# Load data
data <- read_csv("~/datasets/football_tweets.csv") %>% select(status_id,school,text) %>% unique()

# Create binary label 
data_labels <- data %>% mutate(ut = ifelse(school=="ut","yes","no")) %>% select(status_id,ut)

# Separate and clean text 
text <- data$text %>% str_replace_all(., "[^[:alnum:]]", " ")



# Feature Engineering -----------------------------------------------------

# Set max number of features 
max_features <- 100

# Set up tokenizer based on max number of features 
tokenizer <- text_tokenizer(num_words = max_features)

# Tokenize and feature engineer 
tokenizer %>% 
  fit_text_tokenizer(text)

# Check document count | confirm = data
tokenizer$document_count

# Create One Hot Results data frame 
one_hot_results <- texts_to_matrix(tokenizer, text, mode = "binary") %>% as.data.frame() %>% cbind()

# Create final data frame with ids and label 
data_engineered <- data_labels %>% cbind(one_hot_results) 


#  Create Train & Test Sets -----------------------------------------------

training_set <- data_engineered %>% slice_sample(prop =.8)

test_set <- data_engineered %>% anti_join(training_set, by="status_id") %>% select(-status_id)

training_set_no_id <- training_set %>% select(-status_id)