
# Sentiment Analysis Workshop ---------------------------------------------

#PRE-FLIGHT 

#Load Libraries 
library(tidyverse)
library(tidytext)

# Explore Sentiment Dictionaries ------------------------------------------
afin <- get_sentiments("afinn")
bing <- get_sentiments("bing")
nrc <- get_sentiments("nrc")


# Build example dataframe -------------------------------------------------
data <- data.frame(id=1:4,text=c("I’m so happy.","Your music is bad, and you should feel bad.", "I’m super excited to be in this wonderful world today which is the best of days.", "Don’t worry. Be happy.")) %>% 
  mutate(text = as.character(text))

# Exploratory Analysis ----------------------------------------------------

# Tokenize by word 
data %>%
  filter(id==1) %>% 
  unnest_tokens(word, text, token = "words")

# Analyze with AFIN
data %>%
  filter(id==1) %>% 
  unnest_tokens(word, text, token = "words") %>% 
  left_join(afin)

# Again, but with an auto-filter for unknown words
data %>%
  filter(id==1) %>% 
  unnest_tokens(word, text, token = "words") %>% 
  inner_join(afin)

# Again, but with another sentence
data %>%
  filter(id==2) %>% 
  unnest_tokens(word, text, token = "words") %>% 
  inner_join(afin)


# Again, but with another dictionary (bing)
data %>%
  filter(id==2) %>% 
  unnest_tokens(word, text, token = "words") %>% 
  inner_join(bing)


# Again, but with another dictionary (nrc)
data %>%
  filter(id==2) %>% 
  unnest_tokens(word, text, token = "words") %>% 
  inner_join(nrc)



# Comparative Analyses  ---------------------------------------------------

# Batch analysis
afin_analysis <- data %>%
  #filter(id==1) removed! 
  unnest_tokens(word, text, token = "words") %>% 
  inner_join(afin)

#Aggregate the data
afin_analysis %>% group_by(id) %>% summarise(sent_score = sum(value))


# Batch analysis
nrc_analysis <- data %>%
  unnest_tokens(word, text, token = "words") %>% 
  inner_join(nrc)

#Aggregate the data
nrc_analysis %>% count(id,sentiment) %>% pivot_wider(names_from = "sentiment", values_from=n)


# Batch analysis
bing_analysis <- data %>%
  unnest_tokens(word, text, token = "words") %>% 
  inner_join(bing)


#Aggregate the data
bing_analysis %>% 
  count(id,sentiment) %>% 
  pivot_wider(names_from = "sentiment", values_from=n) 


#Aggregate the data (get rid of NAs)
bing_analysis %>% 
  count(id,sentiment) %>% 
  pivot_wider(names_from = "sentiment", values_from=n) %>% 
  mutate(positive = replace_na(positive,0), negative = replace_na(negative,0)) %>% 
  mutate(sent_score = positive + negative*-1)

