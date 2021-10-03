# Load libraries 
library(tidyverse)
library(tidytext)
library(textstem)

# Load data
data <- read_csv("~/datasets/gop_debates.csv")


stem_strings("immigration")
lemmatize_strings("immigration")
immigration_terms <- "immigr|immigration"

paste(stop_words$word, collapse = "|")
stop_words_bounded <- paste0("\\b", stop_words$word, "\\b", collapse = "|")


data %>%
  unnest_tokens(trigram, text, token = "ngrams", n=3) %>% 
  #anti_join(stop_words) %>%
  count(trigram, sort = TRUE) %>%
  filter(str_detect(trigram,immigration_terms)) %>% 
  filter(str_count(trigram,stop_words_bounded) < 1) %>% 
  mutate(trigram = reorder(trigram, n)) %>%
  slice(1:10) %>% # top 10 
  ggplot(aes(x=trigram, y=n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()
