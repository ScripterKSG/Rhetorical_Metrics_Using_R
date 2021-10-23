
# Sentiment Analysis Workshop ---------------------------------------------


# Pre-Flight --------------------------------------------------------------

#Load Libraries 
library(tidyverse)
library(tidytext)
library(textstem)
library(fuzzyjoin)
library(dabestr)

# Get AFIN sentiment dictionary
afin <- get_sentiments("afinn")


# Improving Performance ---------------------------------------------------

# Create Happy Problems DF
happy_problems <- data.frame(word=c("happy","happily","happier", "happiest"))

# Afin sentiment analysis
happy_problems %>% left_join(afin)

# Stemmed Analysis 
happy_problems_stemmed <- happy_problems %>% mutate(word=stem_words(word))
afin_stemmed <- afin %>% mutate(word=stem_words(word))
happy_problems_stemmed %>% left_join(afin_stemmed)

# Lemmitized Analysis 
happy_problems_lemma <- happy_problems %>% mutate(word=lemmatize_words(word))
afin_lemma  <- afin %>% mutate(word=lemmatize_words(word))
happy_problems_lemma  %>% left_join(afin_lemma)

# Fuzzy join
happy_problems %>% regex_left_join(afin)

# Fuzzy join 2.0
happy_problems %>% regex_left_join(afin) %>% group_by(word.x) %>% summarise(value=mean(value))

# Compare 
happy_problems_stemmed %>% left_join(afin_stemmed)
happy_problems_lemma  %>% left_join(afin_lemma)
happy_problems %>% regex_left_join(afin) %>% group_by(word.x) %>% summarise(value=mean(value))


# Research Question -------------------------------------------------------

# Is there a meaningful difference in average tweet sentiment scores when TAMU and LSU are compared to UT? 

#Load Data
data <- read_csv("~/datasets/football_tweets.csv")

# Conduct Sentiment Analysis  ---------------------------------------------

# Get Aggregate scores 
afin_anlysis <- data %>% 
  unnest_tokens(word, text, token = "words") %>% 
  regex_inner_join(afin) %>% 
  group_by(status_id, word.x) %>% 
  summarise(value = mean(value)) %>% 
  ungroup() %>% 
  group_by(status_id) %>% 
  summarise(sent_score = sum(value))

# Combine with full dataset 
data_afin <- data %>% left_join(afin_anlysis)


# Get mean values 
data_afin %>% group_by(school) %>% filter(!is.na(sent_score)) %>% summarise(ave=mean(sent_score))


# Mean Difference Analysis ------------------------------------------------

# Set up dabest
afin_dabest <- data_afin %>% select(school, sent_score) %>% 
  filter(!is.na(sent_score)) %>% 
  dabest(x = school,
         y= sent_score,
         idx= c("ut", "tamu", "lsu"),
         paired = FALSE)

# Get mean diff
afin_dabest %>% mean_diff()

# Plot mean diff
afin_dabest %>% mean_diff() %>% plot()


# Answer ------------------------------------------------------------------

# There are neither measurable nor meaningful differences in average sentiment score when TAMU and LSU are compared to UT.
# The average UT sentiment score is 2.36. 
# The average TAMU sentiment score is 2.14, with a mean difference compared to UT of -0.216 (95% CI: -1.2 to 0.802)
# The average LSU sentiment score is 2.03, with a mean difference compared to UT of -0.329 (95% CI: -1.22 to 0.582)


# Challenge Questions -----------------------------------------------------

# 1 What did I do? (comment lines 57 to 65)

# 2 Is there a meaningful mean difference in sentiment scores for UT when comparing the stemming and lemmaziting approaches to th