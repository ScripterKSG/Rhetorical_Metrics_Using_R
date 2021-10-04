# Load libraries 
library(tidyverse)
library(tidytext)
library(textstem)
library(gridExtra)

# Load data
data <- read_csv("~/datasets/gop_debates.csv")

stem_strings("immigration")
lemmatize_strings("immigration")
immigration_terms <- "immigr|immigration"

paste(stop_words$word, collapse = "|")
stop_words_bounded <- paste0("\\b", stop_words$word, "\\b", collapse = "|")
  
data_TRUMP <- data %>%
  filter(who == "TRUMP")

data_CRUZ <- data %>%
  filter(who == "CRUZ")

data_RUBIO <- data %>%
  filter(who == "RUBIO")

plot_TRUMP <- data_TRUMP %>%
  unnest_tokens(trigram, text, token = "ngrams", n=3) %>% 
  count(trigram, sort = TRUE) %>%
  filter(str_detect(trigram,immigration_terms)) %>% 
  filter(str_count(trigram,stop_words_bounded) < 1) %>% 
  mutate(trigram = reorder(trigram, n)) %>%
  slice(1:10) %>% # top 10 
  ggplot(aes(x=trigram, y=n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

plot_CRUZ <- data_CRUZ %>%
  unnest_tokens(trigram, text, token = "ngrams", n=3) %>% 
  count(trigram, sort = TRUE) %>%
  filter(str_detect(trigram,immigration_terms)) %>% 
  filter(str_count(trigram,stop_words_bounded) < 1) %>% 
  mutate(trigram = reorder(trigram, n)) %>%
  slice(1:10) %>% # top 10 
  ggplot(aes(x=trigram, y=n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

plot_RUBIO <- data_RUBIO %>%
  unnest_tokens(trigram, text, token = "ngrams", n=3) %>% 
  count(trigram, sort = TRUE) %>%
  filter(str_detect(trigram,immigration_terms)) %>% 
  filter(str_count(trigram,stop_words_bounded) < 1) %>% 
  mutate(trigram = reorder(trigram, n)) %>%
  slice(1:10) %>% # top 10 
  ggplot(aes(x=trigram, y=n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()


grid.arrange(plot_TRUMP, plot_CRUZ, plot_RUBIO, ncol=3)
