# ML Workflow Workshop ----------------------------------------------------

# This workshop demonstrates ML workflow tasks


# Pre-Flight --------------------------------------------------------------

# load libraries 
library(tidyverse)
library(caret)
library(pROC)

# source training sets (custom features)
source("feature_engineering_workshop_cf.R")



# Compare Models ----------------------------------------------------------

# The following trains and classifies using three models (KNN, Naive Bayes, and Neural Net)


# Train KNN ---------------------------------------------------------------

# Train the KNN model 
knnfit <- train(ut ~ ., 
                data = training_set_no_id,
                method = "knn",
                tuneLength = 7)

# Predict against test set 
knn_pred <- test_set %>% select(-ut) %>% predict(knnfit, newdata = ., type = 'prob')


# Naive Bayes -------------------------------------------------------------
# Set train control 
fitControl <- trainControl(method = "repeatedcv", 
                           number = 10, 
                           repeats = 5, 
                           classProbs = TRUE, 
                           summaryFunction = twoClassSummary)

# Train the NB Model
nb_mod = train(ut ~ ., 
               data = training_set_no_id, 
               method="naive_bayes", 
               trControl = fitControl, 
               tuneGrid = expand.grid(usekernel=TRUE,laplace=0,adjust=1))

nb_pred <- test_set %>% select(-ut) %>% predict(nb_mod, newdata = ., type = 'prob')

# Train NNet --------------------------------------------------------------

# Train the NN Model 
nnetFit <- train(ut ~ ., 
                 data = training_set_no_id,
                 method = "nnet",
                 metric = "ROC",
                 trControl = fitControl,
                 tuneLength = 3,
                 verbose = FALSE)

# Predict against test set 
nn_pred <- test_set %>% select(-ut) %>% predict(nnetFit, newdata = ., type = 'prob')


# Get ROCs ----------------------------------------------------------------

knn_roc <- roc(test_set$ut,knn_pred$yes)
knn_roc

nb_roc <- roc(test_set$ut,nb_pred$yes)
nb_roc

nn_roc <- roc(test_set$ut,nn_pred$yes)
nn_roc



# Compare Curves ----------------------------------------------------------

ggroc(list(knn=knn_roc,nb=nb_roc,nnet=nn_roc), legacy.axes = TRUE)



# Your Turn ---------------------------------------------------------------

# Compare the model performance for KNN, NB, and NNet using TF-IDF feature engineering 