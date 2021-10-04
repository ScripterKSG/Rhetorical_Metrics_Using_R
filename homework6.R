# Load libraries 
library(tidyverse)
library(tidytext)
library(textstem)

# Load data
data <- read_csv("~/datasets/gop_debates.csv")

# find and stores stem of immigration and lemmatized immgration
stem_strings("immigration")
lemmatize_strings("immigration")
immigration_terms <- "immigr|immigration"

# deals with removing stop words
paste(stop_words$word, collapse = "|")
stop_words_bounded <- paste0("\\b", stop_words$word, "\\b", collapse = "|")

# filters out data to only have trump, cruz and rubio's speaking turns
data_Speakers <- data %>%
  filter(who == "TRUMP" | who == "CRUZ" | who == "RUBIO") %>%
  group_by(who, text)

# graphs lemmaitzed framegrams for trump, cruz, rubio
data_Speakers %>%
  unnest_tokens(trigram, text, token = "ngrams", n=3) %>% 
  count(trigram, sort = TRUE) %>%
  filter(str_detect(trigram,immigration_terms)) %>% 
  filter(str_count(trigram,stop_words_bounded) < 1) %>% 
  mutate(trigram = reorder(trigram, n)) %>%
  group_by(who) %>%
  slice(1:10) %>% # top 10 
  ungroup() %>%
  ggplot(aes(x=trigram, y=n)) +
  geom_col() +
  xlab(NULL) +
  facet_wrap(~who, ncol = 3, scales = "free") +
  coord_flip()
