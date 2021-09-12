#Homework 3

#imports packages from the tidyverse and quanteda
library(tidyverse)
library(quanteda)
library(quanteda.textstats)

#sets working directory inside datasets/gop_frags
setwd("~/datasets/gop_frags/")

#defines a variable files which stores a list of the files in the working
#directory
files <- list.files()

#gets the data from files and uses it to create gop_df, which is data
#the date and text organized in columns
data <- map(files,function(x) read_csv(x))
gop_data <- map2(files,data, function(x,y) cbind(x,y))
gop_df <- do.call(rbind,gop_data)
names(gop_df)[1] <- "date"

#uses gop_df to create data df1, which has the speaker and text length added
df1 <- gop_df %>% 
  separate(date,"date",sep = "\\.") %>% 
  separate(text, "speaker", sep = ":", remove = FALSE) %>% 
  mutate(text_length = nchar(text))

#add readability metric columns to df1
df1 <- df1 %>% 
  bind_cols(textstat_readability(.$text,measure = c("Flesch","Flesch.Kincaid","SMOG")))

#displays speakers and their average complexity
df1 %>% 
  group_by(speaker) %>% 
  summarise(ave_readability = mean(Flesch))

