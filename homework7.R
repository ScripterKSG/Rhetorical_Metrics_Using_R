library(tidyverse)
library(tidytext)
library(textstem)
library(fuzzyjoin)
library(dabestr)

#We want frequency of negative fear words
nrc <- get_sentiments("nrc")

#import data
data <- read_csv("~/datasets/gop_debates.csv")

#adds unique row id
data$ID <- seq.int(nrow(data))

#filters out for fear words said during debate
nrc_analysis <- data %>% 
  unnest_tokens(word, text, token = "words") %>% 
  regex_inner_join(nrc) %>%
  filter(sentiment == "fear") %>%
  group_by(ID, word.x) %>% 
  ungroup() %>% 
  group_by(ID)

#counts total number of fear words said by each speaking turn
nrc_total <- nrc_analysis %>%
  count(who, sentiment)

#combined data of initial data and total number of fear words in each turn
combined <- data %>% left_join(nrc_total)

#dabest works its magic
nrc_dabest <- combined %>% select(who, n) %>% 
  filter(!is.na(n)) %>%
  dabest(x = who,
         y= n,
         idx= c("TRUMP", "BUSH", "CRUZ", "FIORINA"),
         paired = FALSE)

nrc_dabest %>% mean_diff()

nrc_dabest %>% mean_diff() %>% plot
