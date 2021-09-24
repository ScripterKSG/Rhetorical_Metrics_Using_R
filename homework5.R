#imports packages from the tidyverse and tidytext
library(tidyverse)
library(tidytext)

#reads debate data, changes column names to have date and speaker
data <- read_csv("~/datasets/gop_debates.csv")
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

#new dataframe from data_word_n and total_words combined
combined_data <- left_join(data_word_n, total_words)

# Get term frquency (TF),  invverse document frequency (IDF), and TF-IDF 
combined_data  <- combined_data  %>%
  # Calculates TF, IDF, and TF-IDF from word totals and TFs
  bind_tf_idf(word, date, n)

# Plot TF-IDF 
combined_data  %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(date) %>% 
  slice(1:10) %>% 
  ungroup() %>%
  ggplot(aes(word, tf_idf, fill = date)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~date, ncol = 3, scales = "free") +
  coord_flip()


# #stores most common words said during each date
# word_freq <- data_word_n %>%
#   mutate(word = factor(word, levels = rev(unique(word)))) %>% 
#   group_by(date) %>% 
#   slice(1:10) %>% 
#   ungroup() %>%
#   ggplot(aes(x = reorder(word, n), y = n, fill=date)) +
#   geom_col() +
#   xlab(NULL) +
#   coord_flip()+
#   # Separate charts by date using facet_wrap()
#   facet_wrap(~date, ncol = 3, scales = "free_y")
