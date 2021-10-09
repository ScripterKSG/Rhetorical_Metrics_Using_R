library(tidyverse)
library(tidytext)
library(textstem)
library(fuzzyjoin)
library(dabestr)

#We want frequency of negative fear words
nrc <- get_sentiments("nrc")

#import data
data <- read_csv("~/datasets/gop_debates.csv")

nrc_analysis <- data %>%
  unnest_tokens(word, text, token = "words") %>% 
  inner_join(nrc)

nrc_analysis %>% count(who,sentiment) %>% pivot_wider(names_from = "sentiment", values_from=n)
