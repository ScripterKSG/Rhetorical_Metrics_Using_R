# Lexical Diversity Demo --------------------------------------------------
# This is a demonstration of different ways to calculate TTR


# Pre-flight --------------------------------------------------------------

#Load libraries 
library(tidyverse)
library(textstem)



# The horrible way --------------------------------------------------------
stem_words("America's")
stem_words("jobs")
stem_words("recovery")
stem_words("hit") # eff this!


# The less horrible (but still horrible) way ------------------------------

# Save sentence as a string
sentence <- "America's jobs recovery hit a major roadblock in August as the Delta variant threatened the labor market recovery, and the US economy added far fewer jobs than expected."

# Get stems
stem_strings(sentence) #doable, but not for more than a few sentences.



# The even less horrible (but still not great) way ------------------------

# Create a function (verb) that counts the unique number of stems in a string
unique_stems <- function(x){
  temp <- stem_strings(x)
  temp <- as.list(strsplit(temp, '\\s+')[[1]])
  temp %>% unique() %>% length()}

# Apply to sentence 
unique_stems(sentence)

# Get sentence TTR
unique_stems(sentence)/28

# Get full article
article <- readChar("~/nguyen_jason/npr_article.txt", nchars=50000)

# Create a function to get the number of words
n_words <- function(x){
  as.list(strsplit(x, '\\s+')[[1]]) %>% length()
}

# Get article TTR
unique_stems(article)/n_words(article)


# Improved Metrics --------------------------------------------------------

# Herdan’s C

# Get Herdan's C for the sentence
log(unique_stems(sentence))/log(28)

# Get Herdan's C for the article (aka logTTR)
log(unique_stems(article))/log(n_words(article))
