#Homework 3

#imports packages from the tidyverse and quanteda
library(tidyverse)
library(quanteda)
library(quanteda.textstats)
library(dabestr)

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
  bind_cols(textstat_readability(.$text,measure = c("Flesch")))

#removes unneeded columns of date, document and text length
#df2 is the avg speaking complexity (Flesch) of each speaking turn
df2 <- df1[ -c(1,4,5)]

#gets mean speaking difference of Trump compared to Cruz,
#Walker and Bush. Complexity metric is Flesch 
meanDiff <- df2 %>%
  dabest(speaker, Flesch, 
         idx = c("TRUMP", "CRUZ", "WALKER", "BUSH"), 
         paired = FALSE) %>% mean_diff()

#view results
meanDiff

#plot results
meanDiff %>% plot()  
