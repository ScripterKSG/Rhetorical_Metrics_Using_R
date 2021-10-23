
# Summative Content Analysis Workshop  ------------------------------------

# This is the summative content analysis workshop. 


# Pre-Flight --------------------------------------------------------------

#load libraries 
library(tidyverse)
library(stringr)
library(tidytext)
library(textstem)

#load data
spam_data <- read_csv("~/datasets/spam_ham.csv")


# Basic Demo --------------------------------------------------------------

# Get money terms 
money_terms <- read_csv("~/datasets/money_terms.csv")

# Paste to REGEX query 
money_terms_regex <- paste0(money_terms$terms,collapse = "|")

# Get a money text 
spam_data$Message[12] 

# Check that money text 
spam_data$Message[12] %>% str_detect(.,money_terms_regex)

# Try again
spam_data$Message[12] %>% tolower() %>% str_detect(.,money_terms_regex)


# Get fancy ---------------------------------------------------------------

# Create a function that gets stems and lemmas and then creates the regex query for you. 
regexify <- function(x){
  stems <- stem_words(x)
  lemmas <- lemmatize_words(x)
  c(stems,lemmas,x) %>% unique %>% 
    paste0(.,collapse = "|")
}

# Apply regexify function to money_terms 
money_terms_better_regex <- regexify(money_terms$terms)


# Apply to all data
spam_data_money <- spam_data %>%
  mutate(Message = tolower(Message),
         money = ifelse(str_detect(Message,money_terms_better_regex),1,0))

# Explore by class
spam_data_money %>% 
  count(Type,money) %>% 
  pivot_wider(names_from = money,values_from=n,names_prefix="money_") %>% 
  mutate(n=money_0+money_1,
         money_percent = money_1/n)

# RQ: Are spam texts more likely to be about money than non-spam texts? 

# Test for equality of proportions 
prop.test(c(35,13),c(577,79))

# 6.1% of non-spam texts are about money, whereas 16.5% of spam texts are about money. 
# A two-sample test for equality of proportions was conducted to evaluate if there is a significant difference in these proportions.  
# The results indicate that spam texts are more likely to be about money than non-spam texts (x^2 = 9.58, p < 0.01).


# Your Turn ---------------------------------------------------------------

# Research question: Are spam or ham texts more likely to request a return phone call or text message? 