library(tidyverse)
library(tidytext)
library(textstem)
library(fuzzyjoin)
library(dabestr)

#import data, add row IDs to each text, currently using stem analysis
data <- read_csv("~/nguyen_jason/spam_ham.csv")%>% mutate(id=row.names(.))
bing_pos_stem <- get_sentiments("bing") %>% filter(sentiment=="positive") %>% mutate(word=stem_words(word))
bing_neg_stem <- get_sentiments("bing") %>% filter(sentiment=="negative") %>% mutate(word=stem_words(word))

#bing_neg_lem <- get_sentiments("bing") %>% filter(sentiment=="negative") %>% mutate(word=lemmatize_words(word))
#bing_pog_lem <- get_sentiments("bing") %>% filter(sentiment=="positive") %>% mutate(word=lemmatize_words(word))

pos_analysis <- data %>% 
  unnest_tokens(word, Message, token = "words") %>% 
  mutate(word=stem_words(word)) %>% 
  inner_join(bing_pos_stem) %>% 
  add_count(id) %>% 
  select(id,Type,n) %>% 
  unique()

neg_analysis <- data %>% 
  unnest_tokens(word, Message, token = "words") %>% 
  mutate(word=stem_words(word)) %>% 
  inner_join(bing_neg_stem) %>% 
  add_count(id) %>% 
  select(id,Type,n) %>% 
  unique()

#Pos average
pos_analysis %>% group_by(Type) %>% 
  summarise(ave_fear = mean(n))

#Neg average
neg_analysis %>% group_by(Type) %>% 
  summarise(ave_fear = mean(n))

pos_dabest <- pos_analysis %>% 
  dabest(x = Type,
         y= n,
         idx= c("spam","ham"),
         paired = FALSE)

neg_dabest <- neg_analysis %>% 
  dabest(x = Type,
         y= n,
         idx= c("spam","ham"),
         paired = FALSE)


pos_dabest %>% mean_diff()
neg_dabest %>% mean_diff()

pos_dabest %>% mean_diff() %>% plot()
neg_dabest %>% mean_diff() %>% plot()
