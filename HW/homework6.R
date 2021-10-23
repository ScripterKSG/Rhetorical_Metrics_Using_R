# Load libraries 
library(tidyverse)
library(tidytext)
library(textstem)

# Load data
data <- read_csv("~/datasets/gop_debates.csv")

# find and stores stem of immigration and lemmatized immgration and other synonyms
stem_strings("immigration foreign deportation alien citizen")
lemmatize_strings("immigration foreign deportation alien citizen")
immigration_terms <- "immigr|immigration|foreign|deport|deportation|alien|citizen"

# deals with removing stop words
paste(stop_words$word, collapse = "|")
stop_words_bounded <- paste0("\\b", stop_words$word, "\\b", collapse = "|")

# filters out data to only have trump, cruz and rubio's speaking turns
data_Speakers <- data %>%
  filter(who == "TRUMP" | who == "CRUZ" | who == "RUBIO") %>%
  group_by(who, text)

# gets total trigram data
data_trigram_n <- data_Speakers %>%
  unnest_tokens(trigram, text, token = "ngrams", n=3) %>% 
  count(trigram, sort = TRUE)

# total number of trigrams said by each speaker
total_trigrams <- data_trigram_n %>% 
  group_by(who) %>% 
  summarize(total = sum(n))

# combined trigram data and total number of trigrams
combined_data <- left_join(data_trigram_n, total_trigrams)

combined_tf_idf <- combined_data  %>%
  # Calculates TF, IDF, and TF-IDF from word totals and TFs
  bind_tf_idf(trigram, who, n)

# graphs lemmaitzed framegrams for trump, cruz, rubio
combined_tf_idf %>%
  filter(str_detect(trigram,immigration_terms)) %>% 
  filter(str_count(trigram,stop_words_bounded) < 1) %>% 
  mutate(trigram = reorder(trigram, n)) %>%
  group_by(who) %>%
  slice(1:10) %>% # top 10 
  ungroup() %>%
  ggplot(aes(x=trigram, y=tf_idf)) +
  geom_col() +
  xlab(NULL) +
  facet_wrap(~who, ncol = 3, scales = "free") +
  coord_flip()


# The most salient differences involve Cruz and Rubio overall having more lemmatized trigrams about immigration 
# than Trump. Trump has the lowest frequency of mentioning the topic of immigration.
# Moreover, Cruz talks about immigration law and deportation more often than the Rubio and Trump.
# On the other hand, Rubio most commonly references a legal immgration system and is more positive about
# immigration overall, with words such as celebrate and reform being part of his trigrams. Finally,
# Trump seems to have a very negative opinion on immigration, as the few times he discusses it he
# talks about stopping immigration, how it is horribly illegal.
