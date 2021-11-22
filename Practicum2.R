#import libraries for machine learning
library(tidyverse)
library(stringr)
library(tidytext)
library(caret)
library(pROC)

#import data, replace 1 and 0 under hedge with yes and no for ease of use
#filter out pmid and sent columns
data <- read_csv("~/nguyen_jason/hedge_data.csv") %>% 
  select(text, hedge) %>% 
  mutate(hedge = ifelse(hedge=="1","yes","no")) %>% unique()

#add unique row IDs
data$ID <- seq.int(nrow(data))


# Terms were chosen from manually analyzing the text and referencing "Features of Academic Writing"
# There is a space in front of " but" to filter out words like contribute and distribute, which have but

# Content Analysis for feature engineering
data_engineered <- data %>% 
  mutate(text = str_replace_all(text,"[^[:graph:]]", " "),
         text =tolower(text),
         seem = str_count(text,"seem"),
         suggest = str_count(text, "suggest"),
         may = str_count(text,"may"),
         could = str_count(text, "could"),
         might = str_count(text, "might"),
         probable = str_count(text, "probable"),
         potential = str_count(text, "potential"),
         but = str_count(text, " but"),
         assume = str_count(text, "assume")) %>% 
  select(ID,hedge,seem:assume)


# Create training set based on an 80/20 split
training_set <- data_engineered %>% slice_sample(prop =.8)

# Create testing set
test_set <- data_engineered %>% anti_join(training_set, by="ID") %>% select(-ID)

# Create training set with No ID
training_set_no_id <- training_set %>% select(-ID)


### Machine Learning attempts

# Train knn model
knnfit <- train(hedge ~ ., 
                data = training_set_no_id,
                method = "knn",
                tuneLength = 7)

# Predict against test set 
knn_pred <- test_set %>% select(-hedge) %>% predict(knnfit, newdata = ., type = 'prob')

# Set Train Control
fitControl <- trainControl(method = "repeatedcv", 
                           number = 10, 
                           repeats = 5, 
                           classProbs = TRUE, 
                           summaryFunction = twoClassSummary)

# Train nb model
nb_mod = train(hedge ~ ., 
               data = training_set_no_id, 
               method="naive_bayes", 
               trControl = fitControl, 
               tuneGrid = expand.grid(usekernel=TRUE,laplace=0,adjust=1))

# Predict against test set 
nb_pred <- test_set %>% select(-hedge) %>% predict(nb_mod, newdata = ., type = 'prob')

# Train nn model
nnetFit <- train(hedge ~ ., 
                 data = training_set_no_id,
                 method = "nnet",
                 metric = "ROC",
                 trControl = fitControl,
                 tuneLength = 3,
                 verbose = FALSE)

# Predict against test set 
nn_pred <- test_set %>% select(-hedge) %>% predict(nnetFit, newdata = ., type = 'prob')

# Get area under the curve for our AI and each model
knn_roc <- roc(test_set$hedge,knn_pred$yes)
knn_roc

nb_roc <- roc(test_set$hedge,nb_pred$yes)
nb_roc

nn_roc <- roc(test_set$hedge,nn_pred$yes)
nn_roc



# Compare Curves ----------------------------------------------------------

ggroc(list(knn=knn_roc,nb=nb_roc,nnet=nn_roc), legacy.axes = TRUE)

