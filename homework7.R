library(tidyverse)
library(tidytext)
library(textstem)
library(fuzzyjoin)
library(dabestr)

#We want frequency of negative fear words
nrc <- get_sentiments("nrc")

#import data
data <- read_csv("~/datasets/gop_debates.csv")

#adds unique row id
data$ID <- seq.int(nrow(data))


#### lemmatized analysis, not used
#nrc_lem <- nrc %>% mutate(word=lemmatize_words(word))
#nrc_lem_analysis <- data %>% 
#  unnest_tokens(word, text, token = "words") %>% 
#  left_join(nrc_lem) %>%
#  filter(sentiment == "fear") %>%
#  group_by(ID, word) %>% 
#  ungroup() %>% 
#  group_by(ID)

#nrc_total <- nrc_lem_analysis %>%
#  count(who, sentiment)




#stemmed analysis
nrc_stem <- nrc %>% mutate((word=stem_words(word)))
#filters out for fear words said during debate
nrc_stem_analysis <- data %>% 
  unnest_tokens(word, text, token = "words") %>% 
  left_join(nrc_stem) %>%
  filter(sentiment == "fear") %>%
  group_by(ID, word) %>% 
  ungroup() %>% 
  group_by(ID)

#counts total number of fear words said by each speaking turn
nrc_total <- nrc_stem_analysis %>%
  count(who, sentiment)



#### regex inner join, not used
#nrc_analysis <- data %>% 
#  unnest_tokens(word, text, token = "words") %>% 
#  regex_inner_join(nrc) %>%
#  filter(sentiment == "fear") %>%
#  group_by(ID, word.x) %>% 
#  ungroup() %>% 
#  group_by(ID)

#nrc_total <- nrc_analysis %>%
#  count(who, sentiment)






#combined data of initial data and total number of fear words in each turn
combined <- data %>% left_join(nrc_total)

#changes all NAs in numeric columns (in this case, n) to 0
combined <- combined %>% mutate_if(is.numeric , replace_na, replace = 0)

#dabest works its magic
nrc_dabest <- combined %>% select(who, n) %>% 
  dabest(x = who,
         y = n,
         idx= c("TRUMP", "BUSH", "CRUZ", "FIORINA"),
         paired = FALSE)

nrc_dabest %>% mean_diff()

nrc_dabest %>% mean_diff() %>% plot



#There is a meaningful difference in the frequency of fear words per talking turn for Bush, Cruz
#and Fiorina when compared to Trump. None of the confidence intervals contain 0, and
#the unpaired mean differences were 0.684, 1.26 and 1.32 for Bush, Cruz and Fiorina respectively.
#Overall, we conclude Trump had a lower frequencies of fear words than these 3 speakers.
#I will note that the results between stemmed and lemmatized analysis were very similar, but
#going through the data I felt stemmed analysis was more appropriate and accurate.