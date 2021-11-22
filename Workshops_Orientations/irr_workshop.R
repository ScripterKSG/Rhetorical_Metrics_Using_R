
# IRR Workshop ------------------------------------------------------------

# This is the inter-rater reliability workshop. 


# Pre-Flight --------------------------------------------------------------

# load libraries 
library(tidyverse)
library(tidytext)
library(stringr)
library(textstem)
library(irr)

# load data
data <- read_csv("~/datasets/spam_samp.csv")


# Quick Analysis ----------------------------------------------------------

# Auto-code for love 
data_compare <- data %>%
  mutate(Message = tolower(Message),
         robot_love = ifelse(str_detect(Message,"love"),1,0))

# Check agreement 
agree(data.frame(data_compare$Love,data_compare$robot_love))

# Check reliability 
kappa2(data.frame(data_compare$Love,data_compare$robot_love))

# Diagnose issues 
data_compare %>% 
  mutate(agree = Love + robot_love) %>% 
  filter(agree == 1)


# Improvements ------------------------------------------------------------

# Auto-code for love 
data_compare <- data %>%
  mutate(Message = tolower(Message),
         robot_love = ifelse(str_detect(Message,"love|luv") & !str_detect(Message,"god|jesus"),1,0))

# Check agreement 
agree(data.frame(data_compare$Love,data_compare$robot_love))

# Check reliability 
kappa2(data.frame(data_compare$Love,data_compare$robot_love))

# Diagnose issues 
data_compare %>% 
  mutate(agree = Love + robot_love) %>% 
  filter(agree == 1)


# Improvements ------------------------------------------------------------

# Auto-code for love 
data_compare <- data %>%
  mutate(Message = tolower(Message),
         robot_love = ifelse(str_detect(Message,"love|luv") & !str_detect(Message,"god|jesus|lovely smell|friendship"),1,0))

# Check agreement 
agree(data.frame(data_compare$Love,data_compare$robot_love))

# Check reliability 
kappa2(data.frame(data_compare$Love,data_compare$robot_love))

# Diagnose issues 
data_compare %>% 
  mutate(agree = Love + robot_love) %>% 
  filter(agree == 1)
