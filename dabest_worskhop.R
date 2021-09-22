# Dabest Workshop ---------------------------------------------------------
# 330c dabestr workshop


# Pre-Flight --------------------------------------------------------------

# Load libraries 
library(tidyverse)
library(dabestr)

#Create Data
jar <- rep(c("red", "blue"), times = c(84,37)) %>% as.factor()


# Red vs. Blue ------------------------------------------------------------

#Simulate 100 draws of 10
samples <- replicate(100, sample(jar,10) %>%  # take 10 beads out of the jar 100 times 
                       table()) %>% t() %>% # create a table of the results and transpose those results 
  data.frame() %>% # make results into a dataframe 
  mutate(percent_blue = blue/10, percent_red = red/10) # use mutate to get the percent red and percent blue


# Estimate the % blue
mean(samples$percent_blue)

# Estimate the % red
mean(samples$percent_red)

# Estimate the difference between blue and red
mean(samples$percent_red) - mean(samples$percent_blue)



# This time with confidence! ----------------------------------------------

# Make data long form
samples_long <- samples %>% pivot_longer(cols = c("percent_blue", "percent_red"))

# Uses dabestr to calculate the mean_diff between red and blue
red_blue_mean_diff <- samples_long %>%
  dabest(name, value, 
         idx = c("percent_blue", "percent_red"), 
         paired = FALSE) %>% mean_diff()


# View results
red_blue_mean_diff

# Plot results 
red_blue_mean_diff %>% plot()



# A whole new jar ---------------------------------------------------------


#Create Data
new_jar <- rep(c("red", "blue"), times = c(55,58)) %>% as_factor()

#Simulate 100 draws of 10
samples <- replicate(100, sample(new_jar,10) %>%  # take 10 beads out of the jar 100 times 
                       table()) %>% t() %>% # create a table of the results and transpose those results 
  data.frame() %>% # make results into a dataframe 
  mutate(percent_blue = blue/10, percent_red = red/10) # use mutate to get the percent red and percent blue

# Make data long form
samples_long <- samples %>% pivot_longer(cols = c("percent_blue", "percent_red"))

# Uses dabestr to calculate the mean_diff between red and blue
red_blue_mean_diff <- samples_long %>%
  dabest(name, value, 
         idx = c("percent_blue", "percent_red"), 
         paired = FALSE) %>% mean_diff()


# View results
red_blue_mean_diff

# Plot results 
red_blue_mean_diff %>% plot()



# Your Turn ---------------------------------------------------------------

# Research question: Is there a meaningful difference in the average NDC scores of LSU and TAMU when compared to UT? 
#There is a meaningful difference as 0 is not in anyu of the 95% CI

# Prepare data
data <- read_csv("~/datasets/football_tweets.csv")
data <- data %>% 
  bind_cols(textstat_readability(.$text,measure = c("Coleman.Liau.grade")))
# Calculate mean difference 
meanDiff <- data %>%
  dabest(school, Coleman.Liau.grade, 
         idx = c("ut", "lsu", "tamu"), 
         paired = FALSE) %>% mean_diff()

meanDiff
# Visualize data 
meanDiff %>% plot()  

