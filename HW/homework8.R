#load libraries 
library(tidyverse)
library(stringr)
library(tidytext)
library(textstem)

#load data
data <- read_csv("~/datasets/utreddit.csv")

#dictionary with synonyms of word course, also some extra added words like professor and content
sched <- read_csv("~/nguyen_jason/HW/HW8_Dictionaries/Course_Scheduling.csv")

#dictionary with synonyms of word majors, also added words like choosing, picking, choice
major <- read_csv("~/nguyen_jason/HW/HW8_Dictionaries/Major_Selection.csv")

#dictionary with synonyms of word policy
policy <- read_csv("~/nguyen_jason/HW/HW8_Dictionaries/Policy_Questions.csv")

#dictionary of synonyms of financial aid, very small because words like money or finance can apply
#to several different topics so I avoided them
finance <- read_csv("~/nguyen_jason/HW/HW8_Dictionaries/Financial_Aid.csv")

#dictionary of synonyms of house, housing and added words like dorm and apartments
house <- read_csv("~/nguyen_jason/HW/HW8_Dictionaries/Housing.csv")

#dictionary of food, dinning, restaurants
food <- read_csv("~/nguyen_jason/HW/HW8_Dictionaries/Food.csv")

#dictionary of synonyms of entertainment and added words like sports, game, play
entertain <- read_csv("~/nguyen_jason/HW/HW8_Dictionaries/Entertainment.csv")

#dictionary of synonyms of humor
humor <- read_csv("~/nguyen_jason/HW/HW8_Dictionaries/Humor.csv")


# Create a function that gets stems and lemmas and then creates the regex query for you. 
regexify <- function(x){
  stems <- stem_words(x)
  lemmas <- lemmatize_words(x)
  c(stems,lemmas,x) %>% unique %>% 
    paste0(.,collapse = "|")
}


#Applying our regexify function to all our dictionaries
better_sched <- regexify(sched$term)
better_major <- regexify(major$term)
better_policy <- regexify(policy$term)
better_finance <- regexify(finance$term)
better_house <- regexify(house$term)
better_food <- regexify(food$term)
better_entertain <- regexify(entertain$term)
better_humor <- regexify(humor$term)


#apply our dictionaries to our data in the same order we created/regexify them
#checks both the title and post_text of the UTreddit post by pasting them together
#into one message
data <- data %>%
  mutate(Message = paste(tolower(title), tolower(post_text), sep = " "),
         sched = ifelse(str_detect(Message,better_sched),1,0))

data <- data %>%
  mutate(Message = paste(tolower(title), tolower(post_text), sep = " "),
         major = ifelse(str_detect(Message,better_major),1,0))

data <- data %>%
  mutate(Message = paste(tolower(title), tolower(post_text), sep = " "),
         policy = ifelse(str_detect(Message,better_policy),1,0))

data <- data %>%
  mutate(Message = paste(tolower(title), tolower(post_text), sep = " "),
         finaid = ifelse(str_detect(Message,better_finance),1,0))

data <- data %>%
  mutate(Message = paste(tolower(title), tolower(post_text), sep = " "),
         housing = ifelse(str_detect(Message,better_house),1,0))

data <- data %>%
  mutate(Message = paste(tolower(title), tolower(post_text), sep = " "),
         food = ifelse(str_detect(Message,better_food),1,0))

data <- data %>%
  mutate(Message = paste(tolower(title), tolower(post_text), sep = " "),
         entertain = ifelse(str_detect(Message,better_entertain),1,0))

data <- data %>%
  mutate(Message = paste(tolower(title), tolower(post_text), sep = " "),
         humor = ifelse(str_detect(Message,better_humor),1,0))




