# Modeling: The SVM -------------------------------------------------------

# Orientation to the Support Vector Machine and Caret 

# Pre-Flight --------------------------------------------------------------
# load libraries 
library(caret)

# With TF-IDF -------------------------------------------------------------

# source training sets
source("~/nguyen_jason/Workshops_Orientations/feature_engineering_workshop_tf_idf.R")

# SVM Linear Weights 2 ----------------------------------------------------

# Set train control to repeated cross-validation 
fitControl <- trainControl(
  method = "repeatedcv", # set method to repeated cross-validation 
  number = 5, # Divide data into 5 sub-sets
  repeats = 5) # Repeat the cross-validation 5 times

# Train SVM model 
svm_mod <- train(ut ~ ., # predict ut based on the remaining data 
                 data = training_set_no_id, # identify the training data set 
                 method = "svmLinearWeights2", # specify the algorithm to be used 
                 trControl = fitControl, # import the train control parameters 
                 tuneGrid = data.frame(cost = 1, 
                                       Loss = 0, 
                                       weight = 1)) # provide tuning parameters (basically ignore these)

# Predict against test set 
svm_pred <- test_set %>% 
  select(-ut) %>% # get rid of the label so it's a fair prediction 
  predict(svm_mod, newdata = .) # use the pre-trained model (svm_mod) to predict the label for the test set data


# Get confusion matrix, agreement, and accuracy values 
confusionMatrix(svm_pred,as.factor(test_set$ut))




# Clear all ---------------------------------------------------------------

rm(list = ls())


# With Custom Features ----------------------------------------------------

# source training sets
source("feature_engineering_workshop_cf.R")

# SVM Linear Weights 2 ----------------------------------------------------

# Set train control to repeated cross-validation 
fitControl <- trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = 5)

# Train SVM
svm_mod <- train(ut ~ ., 
                 data = training_set_no_id,
                 method = "svmLinearWeights2",
                 trControl = fitControl,
                 tuneGrid = data.frame(cost = 1, 
                                       Loss = 0, 
                                       weight = 1))

# Predict against test set 
svm_pred <- test_set %>% select(-ut) %>% predict(svm_mod, newdata = .)


# Get confusion matrix, agreement, and accuracy values 
confusionMatrix(svm_pred,as.factor(test_set$ut))

