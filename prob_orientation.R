# Probability Orientation --------------------------------------------------

# This script is part of the basic intro to probability in RHE 330C. 
# Note: Many calculations in the script are random. That means the numbers will be slightly different when you run them at home. 

# Pre-Flight --------------------------------------------------------------

#Load Libraries 
library(tidyverse)
library(dslabs)


# The Mystery jar ---------------------------------------------------------

# Take multiple polls
take_poll(10)



# Tiny Toy Model jar ------------------------------------------------------

# Create the jar 
jar <- rep(c("red", "blue"), times = c(2,3)) %>% as.factor()
jar

#Calculate the percent red
2/5

#Calculate the percent blue
3/5

#Sample 1 bead at random
sample(jar, 1)

#Same 3 beads at random (what % red?)
sample(jar, 3)

# What if we try 10 times? 
replicate(10, sample(jar, 3)) %>% table() %>% prop.table() #different code in video, but same effect

# What if we try 1000 times? 
replicate(1000, sample(jar, 3)) %>% table() %>% prop.table() #different code in video, but same effect

# What if we try 10,000 times? 
replicate(10000, sample(jar, 3)) %>% table() %>% prop.table() #different code in video, but same effect

# Let's do it 10,000 times and see what each result looks like 
samples <- replicate(10000, sample(jar,3) %>%  # take 3 beads out of the jar 10,000 times 
                       table()) %>% t() %>% # create a table of the results and transpose those results 
  data.frame() %>% # make results into a dataframe 
  mutate(percent_blue = blue/3, percent_red = red/3) # use mutate to get the percent red and percent blue


# What did we learn? 
summary(samples)
hist(samples$percent_blue)
hist(samples$percent_red)


# Jumbo jar ---------------------------------------------------------------
# Create the jar 
jumbo_jar <- rep(c("red", "blue"), times = c(84,37)) %>% as.factor()
jumbo_jar

84/(84+37) # percent red
37/(84+37) # percent blue

jumbo_samples <- replicate(10000, sample(jumbo_jar,10) %>%  # take 10 beads out of the jar 10,000 times 
                             table()) %>% t() %>% # create a table of the results and transpose those results 
  data.frame() %>% # make results into a dataframe 
  mutate(percent_blue = blue/10, percent_red = red/10) # use mutate to get the percent red and percent blue

summary(jumbo_samples)
hist(jumbo_samples$percent_blue)
hist(jumbo_samples$percent_red)


# How confident are we? ---------------------------------------------------

# Calculate average Red %
x_hat_red <- mean(jumbo_samples$percent_red)

# Calculate standard error for Red
se_hat_red <- sqrt(x_hat_red * (1 - x_hat_red) / 10000)

# calculate lower and upper bounds for the 95% Confidence Interval 
c(x_hat_red - 1.96 * se_hat_red, x_hat_red + 1.96 * se_hat_red)


# Calculate average Blue %
x_hat_blue<- mean(jumbo_samples$percent_blue)

# Calculate standard error for Blue
se_hat_blue <- sqrt(x_hat_blue * (1 - x_hat_blue) / 10000)

# calculate lower and upper bounds for the 95% Confidence Interval 
c(x_hat_blue - 1.96 * se_hat_blue, x_hat_blue + 1.96 * se_hat_blue)
