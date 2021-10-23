
# Framegram Workshop -----------------------------------------------------

# This is the framegram workshop for 330c. 


# Pre-flight --------------------------------------------------------------

# Load libraries 
library(tidyverse)
library(tidytext)
library(textstem)

# Load data
data <- read_csv("~/datasets/gop_debates.csv")


# Trigrams ----------------------------------------------------------------

# Visualize most commonly occurring terms
data %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE) %>%
  filter(n > 200) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x=word, y=n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

# Visualize most commonly occurring trigrams (take 1)
data %>%
  unnest_tokens(trigram, text, token = "ngrams", n=3) %>% 
  anti_join(stop_words) %>%
  count(trigram, sort = TRUE) %>%
  filter(n > 200) %>%
  mutate(trigram = reorder(trigram, n)) %>%
  ggplot(aes(x=trigram, y=n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()


# Visualize most commonly occurring trigrams (take 2)
data %>%
  unnest_tokens(trigram, text, token = "ngrams", n=3) %>% 
  #anti_join(stop_words) %>%
  count(trigram, sort = TRUE) %>%
  filter(n > 200) %>%
  mutate(trigram = reorder(trigram, n)) %>%
  ggplot(aes(x=trigram, y=n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

# Visualize most commonly occurring trigrams (take 3)
data %>%
  unnest_tokens(trigram, text, token = "ngrams", n=3) %>% 
  #anti_join(stop_words) %>%
  count(trigram, sort = TRUE) %>%
  filter(n > 100) %>%
  mutate(trigram = reorder(trigram, n)) %>%
  ggplot(aes(x=trigram, y=n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()



# Framegrams --------------------------------------------------------------

#stringr::str_detect()
?str_detect()

str_detect("chicken", "c")

str_detect("chicken", "z")


# Economy framegram 
data %>%
  unnest_tokens(trigram, text, token = "ngrams", n=3) %>% 
  #anti_join(stop_words) %>%
  count(trigram, sort = TRUE) %>%
  filter(str_detect(trigram,"economy")) %>% 
  filter(n > 2) %>% # note filter change 
  mutate(trigram = reorder(trigram, n)) %>%
  ggplot(aes(x=trigram, y=n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()


# Stem and lemmatize 
stem_words("economy")
lemmatize_words("economy")


# Economy framegram (stemmed)
data %>%
  unnest_tokens(trigram, text, token = "ngrams", n=3) %>% 
  #anti_join(stop_words) %>%
  count(trigram, sort = TRUE) %>%
  filter(str_detect(trigram,"economi|economy")) %>% 
  filter(n > 2) %>% # note filter change 
  mutate(trigram = reorder(trigram, n)) %>%
  ggplot(aes(x=trigram, y=n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()


# Now with synonyms 
stem_strings("economy money finance")
lemmatize_strings("economy money finance")

# Save terms for later
econ_terms <- "economi|monei|financ|economy|money|finance"

# Economy framegram (with added terms)
data %>%
  unnest_tokens(trigram, text, token = "ngrams", n=3) %>% 
  #anti_join(stop_words) %>%
  count(trigram, sort = TRUE) %>%
  filter(str_detect(trigram,econ_terms)) %>% 
  filter(n > 3) %>% # note filter change 
  mutate(trigram = reorder(trigram, n)) %>%
  ggplot(aes(x=trigram, y=n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()



# Addressing Stop Words ----------------------------------------------------

#stringr::str_count()
?str_count()

str_detect("chicken", "c")

str_count("chicken", "c")

#paste
paste(stop_words$word)

#paste
paste(stop_words$word, collapse = "|")


# Economic terms framegram (remove double stopwords) 
data %>%
  unnest_tokens(trigram, text, token = "ngrams", n=3) %>% 
  #anti_join(stop_words) %>%
  count(trigram, sort = TRUE) %>%
  filter(str_detect(trigram,econ_terms)) %>% 
  filter(str_count(trigram,paste(stop_words$word, collapse = "|")) < 1) %>% 
  filter(n > 0) %>% # note filter change 
  mutate(trigram = reorder(trigram, n)) %>%
  ggplot(aes(x=trigram, y=n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

# WTF, eh?!  
str_count("finance",paste(stop_words$word, collapse = "|"))

str_count("finance","in")

str_count("finance","\\bin\\b") # \\b = word boundary

stop_words_bounded <- paste0("\\b", stop_words$word, "\\b", collapse = "|")

str_count("finance", stop_words_bounded)

# Economic terms framegram (remove double stopwords) 
data %>%
  unnest_tokens(trigram, text, token = "ngrams", n=3) %>% 
  #anti_join(stop_words) %>%
  count(trigram, sort = TRUE) %>%
  filter(str_detect(trigram,econ_terms)) %>% 
  filter(str_count(trigram,stop_words_bounded) < 1) %>% 
  filter(n > 2) %>% # note filter change 
  mutate(trigram = reorder(trigram, n)) %>%
  ggplot(aes(x=trigram, y=n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()


# Top 15 Economic terms framegram (remove double stopwords) 
data %>%
  unnest_tokens(trigram, text, token = "ngrams", n=3) %>% 
  #anti_join(stop_words) %>%
  count(trigram, sort = TRUE) %>%
  filter(str_detect(trigram,econ_terms)) %>% 
  filter(str_count(trigram,stop_words_bounded) < 1) %>% 
  mutate(trigram = reorder(trigram, n)) %>%
  slice(1:15) %>% # top 10 
  ggplot(aes(x=trigram, y=n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()



# Your Turn ---------------------------------------------------------------

# What are the most common 15 framegrams for national security issues in the GOP debate data set?