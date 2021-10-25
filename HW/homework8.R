#load libraries 
library(tidyverse)
library(stringr)
library(tidytext)
library(textstem)
library(dplyr)

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

#adds all terms to one value, used for the "Other" code
all_terms <- paste(better_sched,
                   better_major,
                   better_policy,
                   better_finance,
                   better_house,
                   better_food,
                   better_entertain,
                   better_humor,
                   sep = "|")


#apply our dictionaries to our data in the same order we created/regexify them
#this checks both the title and post_text of the UTreddit post by pasting them together
#into one message
data <- data %>%
  mutate(Message = paste(tolower(title), tolower(post_text), sep = " "),
         sched = ifelse(str_detect(Message,better_sched),1,0),
         major = ifelse(str_detect(Message,better_major),1,0),
         policy = ifelse(str_detect(Message,better_policy),1,0),
         finaid = ifelse(str_detect(Message,better_finance),1,0),
         housing = ifelse(str_detect(Message,better_house),1,0),
         food = ifelse(str_detect(Message,better_food),1,0),
         entertain = ifelse(str_detect(Message,better_entertain),1,0),
         humor = ifelse(str_detect(Message,better_humor),1,0))


#For Other code, done a bit differently since we check for UTreddit posts with NONE of
#the content analysis terms
data <- data %>%
  mutate(Message = paste(tolower(title), tolower(post_text), sep = " "),
         other = ifelse(str_detect(Message,all_terms, negate = TRUE),1,0))

#total number of UTposts in our dataset
total_posts = count(data)

#Summarize, dplyer, gets percentages of each type of post in our data set
percentages <- data %>%
  summarise(Course = sum(sched)/total_posts,
            Major = sum(major)/total_posts,
            Policy = sum(policy)/total_posts,
            FinAid = sum(finaid)/total_posts,
            Housing = sum(housing)/total_posts,
            Food = sum(food)/total_posts,
            Entertain = sum(entertain)/total_posts,
            Humor = sum(humor)/total_posts,
            Other = sum(other)/total_posts)

#displays percentages
percentages

#I will note that our dictionaries may be lacking in some aspects, but some posts are
#too short/specific to classify. For example, one post is titled
#"Waffles on sticks at new place in the Union" with no text_post
#This is clearly talking about food but none of those words strongly fit the "food" schema 

