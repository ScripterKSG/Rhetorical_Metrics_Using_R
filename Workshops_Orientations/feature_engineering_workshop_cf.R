# Feature Engineering Workshop: Custom Features ---------------------------

# This workshop orients you to using summative content analysis to create custom features. 

# Pre-Flight --------------------------------------------------------------

# Load libraries 
library(tidyverse)
library(stringr)
library(tidytext)


# Load data &  create lables 
data <- read_csv("~/datasets/football_tweets.csv") %>% 
  select(status_id,school,text) %>% 
  mutate(ut = ifelse(school=="ut","yes","no")) %>% 
  unique()


# Custom Features ---------------------------------------------------------

# Use summative content analysis to do feature engineering 
data_engineered <- data %>% 
  mutate(text = str_replace_all(text,"[^[:graph:]]", " "),
         text =tolower(text),
         texas = str_count(text,"texas"),
         u_t = str_count(text, "ut"),
         bevo = str_count(text, "bevo"),
         longhorn = str_count(text, "longhorn"),
         aggie = str_count(text, "aggie"),
         tamu = str_count(text,"tamu"),
         lsu = str_count(text, "lsu"),
         tiger = str_count(text, "tiger")) %>% 
  select(status_id,ut,texas:tiger)

#  Create Train & Test Sets -----------------------------------------------


# Create training set based on an 80/20 split
training_set <- data_engineered %>% slice_sample(prop =.8)

# Create testing set
test_set <- data_engineered %>% anti_join(training_set, by="status_id") %>% select(-status_id)

# Create training set with No ID
training_set_no_id <- training_set %>% select(-status_id)

