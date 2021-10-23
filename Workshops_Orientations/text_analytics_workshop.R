
# Text Analytics Workshop --------------------------------------------------

# PRE-FLIGHT 

# load libraries 
library(tidyverse)
library(tidytext)

# load data
data <- read_csv("~/datasets/football_tweets.csv")



# Explore data by word frequency ------------------------------------------

# View most common words in all tweets
data %>%
  #Tokenize by word
  unnest_tokens(word, text) %>%
  # Count and sort by word 
  count(word, sort = TRUE) 

# View most common words excluding stop words
data %>%
  unnest_tokens(word, text) %>%
  #USe anti_join() to remove stop words 
  anti_join(stop_words) %>%
  count(word, sort = TRUE) 

#Visualize frequency of most common words occurring more frequently than 75 times
data %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE) %>%
  # Exclude words that appear fewer than 75 times
  filter(n > 75) %>%
  # Reorder descending
  mutate(word = reorder(word, n)) %>%
  #plot data
  ggplot(aes(x=word, y=n)) +
  # Add column geometry 
  geom_col() +
  #Remove the x axis labels 
  xlab(NULL) +
  #Flip coordinates so X data is on the Y axis
  coord_flip()


#Save word data for subsequent analyses
data_word_n <- data %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(school, word, sort = TRUE)

#Visualize frequency of most common words occurring more frequently than 20 times by school
data_word_n %>%
  filter(n > 20) %>%  
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill=school)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()+
  # Separate charts by school using facet_wrap()
  facet_wrap(~school, ncol = 2, scales = "free_y")


#Save for future use
word_freq <- data_word_n %>%
  filter(n > 20) %>%  
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill=school)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()+
  # Separate charts by school using facet_wrap()
  facet_wrap(~school, ncol = 2, scales = "free_y")



# TF-IDF (Better than word frequencies) -----------------------------------

# Get wordcount by school compared to total count of each word & visualize [6]
total_words <- data_word_n %>% 
  group_by(school) %>% 
  summarize(total = sum(n))

# Create a new dataframe combining information on total number of words per tweet word frequency counts
data_word_n <- left_join(data_word_n, total_words)

# Get term frquency (TF),  invverse document frequency (IDF), and TF-IDF 
data_word_n <- data_word_n %>%
  # Calculates TF, IDF, and TF-IDF from word totals and TFs
  bind_tf_idf(word, school, n)

# Display dataframe to see some results 
data_word_n

# Sort dataframe by TF-IDF
data_word_n %>%
  select(-total) %>%
  arrange(desc(tf_idf))

# See most unique TAMU words 
data_word_n %>%
  select(-total) %>%
  arrange(desc(tf_idf)) %>% 
  filter(school == "tamu")


# Plot TF-IDF 
data_word_n %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(school) %>% 
  slice(1:15) %>% 
  ungroup() %>%
  ggplot(aes(word, tf_idf, fill = school)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~school, ncol = 2, scales = "free") +
  coord_flip()

#Google search letter/number combinations for an interesting finding 

# Show word frequency chart | Compare TF vs. TF-IDF to see how TF-IDF finds more unique words 
word_freq


# Ngrams ------------------------------------------------------------------
# View most common words in all tweets
data %>%
  #Tokenize by word
  unnest_tokens(word, text) %>%
  # Count and sort by word 
  count(word, sort = TRUE) 

# View most common bigrams in all tweets
data %>%
  #Tokenize by bigram
  unnest_tokens(bigram, text, token = "ngrams", n=2) %>%
  # Count and sort by word 
  count(bigram, sort = TRUE) 

# View most common trigrams in all tweets
data %>%
  #Tokenize by trigram
  unnest_tokens(trigram, text, token = "ngrams", n=3) %>%
  # Count and sort by word 
  count(trigram, sort = TRUE) 


# Your Turn ---------------------------------------------------------------

# Use bigram TF-IDFs scores to identify and chart the most unique bigrams for each school
# Hint: Lines 84-125 + Lines 141-146