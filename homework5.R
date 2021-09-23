#imports packages from the tidyverse and quanteda
library(tidyverse)
library(tidytext)

#reads debate data, changes column names to have date and speaker
data <- read.csv("~/datasets/gop_debates.csv", stringsAsFactors=FALSE)
names(data)[1] <- "date"
names(data)[3] <- "speaker"

#save word data, grouped by date spoken
data_word_n <- data %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(date, word, sort = TRUE)

#total number of words said during each date (aka each debate)
total_words <- data_word_n %>% 
  group_by(date) %>% 
  summarize(total = sum(n))


