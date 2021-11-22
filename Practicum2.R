#import libraries
library(tidyverse)
library(stringr)
library(tidytext)
library(caret)
library(pROC)

#import data, replace 1 and 0 under hedge with yes and no
data <- read_csv("~/nguyen_jason/hedge_data.csv") %>% 
  select(text, hedge) %>% 
  mutate(hedge = ifelse(hedge=="1","yes","no")) %>% unique()

#add unique row IDs
data$ID <- seq.int(nrow(data))



# Much of the given text refer to AUC, p values, area under the curve and ci (confidence intervals)
# However, it is difficult to account for this as both hedged and unhedged texts
# refer to these statistical methods

#There is a space in front of " but" to filter out words like contribute and distribute, which have but
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


### Machine Learning attempts with knn, nb, nn
knnfit <- train(hedge ~ ., 
                data = training_set_no_id,
                method = "knn",
                tuneLength = 7)

# Predict against test set 
knn_pred <- test_set %>% select(-hedge) %>% predict(knnfit, newdata = ., type = 'prob')


fitControl <- trainControl(method = "repeatedcv", 
                           number = 10, 
                           repeats = 5, 
                           classProbs = TRUE, 
                           summaryFunction = twoClassSummary)


nb_mod = train(hedge ~ ., 
               data = training_set_no_id, 
               method="naive_bayes", 
               trControl = fitControl, 
               tuneGrid = expand.grid(usekernel=TRUE,laplace=0,adjust=1))

nb_pred <- test_set %>% select(-hedge) %>% predict(nb_mod, newdata = ., type = 'prob')

nnetFit <- train(hedge ~ ., 
                 data = training_set_no_id,
                 method = "nnet",
                 metric = "ROC",
                 trControl = fitControl,
                 tuneLength = 3,
                 verbose = FALSE)

# Predict against test set 
nn_pred <- test_set %>% select(-hedge) %>% predict(nnetFit, newdata = ., type = 'prob')


knn_roc <- roc(test_set$hedge,knn_pred$yes)
knn_roc

nb_roc <- roc(test_set$hedge,nb_pred$yes)
nb_roc

nn_roc <- roc(test_set$hedge,nn_pred$yes)
nn_roc



# Compare Curves ----------------------------------------------------------

ggroc(list(knn=knn_roc,nb=nb_roc,nnet=nn_roc), legacy.axes = TRUE)

