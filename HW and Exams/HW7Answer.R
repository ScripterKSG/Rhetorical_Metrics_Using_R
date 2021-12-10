library(tidyverse)
library(tidytext)
library(textdata)
library(textstem)
library(dabestr)

# load data
data <- read_csv("~/datasets/gop_debates.csv") %>% filter(who %in% c("BUSH","CRUZ","FIORINA","TRUMP")) %>% mutate(id=row.names(.))

# load sentiment dictionary & filter for fear words 
nrc_fear_lemma <- get_sentiments("nrc") %>% filter(sentiment=="fear") %>% mutate(word=lemmatize_words(word))


# Get Aggregate scores 
fear_anlysis <- data %>% 
  unnest_tokens(word, text, token = "words") %>% 
  mutate(word=lemmatize_words(word)) %>% 
  inner_join(nrc_fear_lemma) %>% 
  add_count(id) %>% 
  select(id,who,n) %>% 
  unique()

# Get averages
fear_anlysis %>% group_by(who) %>% 
  summarise(ave_fear = mean(n))

# Mean Difference Analysis ------------------------------------------------

# Set up dabest
fear_dabest <- fear_anlysis %>% 
  dabest(x = who,
         y= n,
         idx= c("TRUMP","BUSH","CRUZ","FIORINA"),
         paired = FALSE)

# Get mean diff
fear_dabest %>% mean_diff()

# Plot mean diff
fear_dabest %>% mean_diff() %>% plot()
