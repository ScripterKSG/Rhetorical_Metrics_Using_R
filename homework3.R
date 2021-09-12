#Homework 3

#imports packages from the tidyverse and quanteda
library(tidyverse)
library(quanteda)
library(quanteda.textstats)

#sets working directory inside home/datasets/gop_frags
setwd("~/datasets/gop_frags/")

#defines a variable files which stores a list of the files in the working
#directory since gop_frags is a folder with several csv files in it
files <- list.files()

#gets the data from files and uses it to create gop_data
#and then gop_df then the date and text is organized in columns
data <- map(files,function(x) read_csv(x))
gop_data <- map2(files,data, function(x,y) cbind(x,y))
gop_df <- do.call(rbind,gop_data)
names(gop_df)[1] <- "date"

#uses gop_df to create data df1, which has the speaker and text length added
df1 <- gop_df %>% 
  separate(date,"date",sep = "\\.") %>% 
  separate(text, "speaker", sep = ":", remove = FALSE) %>% 
  mutate(text_length = nchar(text))

#add readability metric columns to df1 which analyzes each
#speaking turn in several different metrics
df1 <- df1 %>% 
  bind_cols(textstat_readability(.$text,measure = c("Flesch","Flesch.Kincaid","SMOG")))

#stores speakers and their average complexity by Flesch metric in df2
#arrange verb ensures its sorted in descending complexity
df2 <- df1 %>% 
  group_by(speaker) %>% 
  summarise(ave_readability = mean(Flesch)) %>%
  arrange(desc(ave_readability))

#displays descending complexity info from df2
df2

