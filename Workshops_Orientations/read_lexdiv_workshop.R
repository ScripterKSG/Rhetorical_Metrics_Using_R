# Readability and Lexical Diversity Workshop -------------------------------

#PRE-FLIGHT 

#Load Libraries 
library(dplyr)
library(tidyverse)
library(quanteda)
library(quanteda.textstats) #Due to a recent package update this line is not in the video. You will need to run it for this script to work. 

#Load Data
data <- read_csv("~/datasets/football_tweets.csv")


# Readability Metrics ------------------------------------------------------

#Look at the text of two tweets
data$text[c(4,13)]

# Open documentation to view available scores 
?textstat_readability

# Get a Flesch Kincaid for those two tweets 
textstat_readability(data$text[c(4,13)],measure = "Flesch.Kincaid")

# Create new column in data with Flesh, Flesch Kincaid, and SMOG scores. 
data <- data %>% 
  bind_cols(textstat_readability(.$text,measure = c("Flesch","Flesch.Kincaid","SMOG")))


# Lexical Diversity Metrics -----------------------------------------------

#View documentation 
?textstat_lexdiv

# Get TTR
textstat_lexdiv(data$text, measure = "TTR")

# Corpus Assemble! 
corpus <- data %>% corpus(docid_field = "status_id", unique_docnames = FALSE)

# Tokenize Corpus
corpus_tokens <- corpus %>% tokens()

# Create document feature matrix 
corpus_dfm <- corpus_tokens %>% dfm(remove = stopwords('en'))

# Pick a tweet to Look at
data$text[4]

# Get TTR for a that tweet
textstat_lexdiv(corpus_dfm[4,], measure = "TTR")

# Pick another tweet to look at
data$text[10]

# Get TTR for a that tweet
textstat_lexdiv(corpus_dfm[10,], measure = "TTR")

# Get multiple lexical diversity measures for the entire dataset. 
lexdiv <- corpus_dfm %>% textstat_lexdiv(measure=c("TTR","C"))

# Join lexdiv to data
full_data <- data %>% left_join(lexdiv, by=c("status_id" = "document"))

# Get lexdiv values K 
lexdiv <- textstat_lexdiv(corpus_dfm,measure=c("TTR","C","R","K")) %>% mutate(document = as.numeric(document))

# Join lexdiv to data
full_data <- data %>% left_join(lexdiv, by=c("status_id" = "document")) %>% unique()


# Summarize Data ----------------------------------------------------------

# Get average readability (FOG) by school
full_data %>% 
  group_by(school) %>% 
  summarise(ave_readability = mean(Flesch))

# Get average lexical diversity (TTR) by school
full_data %>% 
  group_by(school) %>% 
  summarise(ave_lexdiv = mean(TTR))

# Try again
full_data %>% 
  group_by(school) %>% 
  summarise(ave_lexdiv = mean(TTR, na.rm = TRUE))


# Your turn ---------------------------------------------------------------

# Determine which school's tweets are easiest to read based on Coleman-Liau Grade Level. 
data <- data %>% 
  bind_cols(textstat_readability(.$text,measure = c("Coleman.Liau.grade")))

full_data <- data %>% left_join(lexdiv, by=c("status_id" = "document")) %>% unique()

full_data %>% 
  group_by(school) %>% 
  summarise(ave_readability = mean(Coleman.Liau.grade))

# Determine which school's tweets are the most lexical diverse based on Dugast's Uber Index. 

lexdiv <- textstat_lexdiv(corpus_dfm,measure=c("TTR","C","R","K","U")) %>% mutate(document = as.numeric(document))
lexdiv <- lexdiv[is.finite(rowSums(lexdiv))]
full_data <- data %>% left_join(lexdiv, by=c("status_id" = "document")) %>% unique()


full_data %>% 
  group_by(school) %>% 
  summarise(ave_lexdiv = mean(U, na.rm = TRUE))

