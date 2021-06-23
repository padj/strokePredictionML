# nnet_model
###############################################################################
# -----------------------------------------------------------------------------
# SCRIPT:
# Name:       nnet_model.R
# Date:       23 June 2021
# Version:    1.0.0
# Authors:    thomas.padgett
#
# Description:
#             Imports prepped, feature-engineered test and train data. Trains
#             a neural network, tested on the test set. 
#
# Change notes:
#             N/A
#
# -----------------------------------------------------------------------------
###############################################################################

#### Preamble ####

set.seed(10) # Ensure repeatibility
library(pROC)
library(caret)
library(e1071) #required within caret::train()

#### Function definitions ####


#### Import data ####

train <- read.csv("data/trainDataOversampled_featEng_v1.csv")
test <- read.csv("data/testDataOversampled_featEng_v1.csv")

#### Model ####
# Define the training control method. 
# K-fold cross validation (number = folds)
ctrl <- caret::trainControl(method = "repeatedcv", 
                     number = 20, 
                     repeats = 5, 
                     search = "grid", 
                     classProbs = FALSE)

# Train the neural network
nnet_basic <- caret::train(as.factor(stroke) ~ .,
                    data = train,
                    method = "nnet",
                    metric = "Kappa",
                    trControl = ctrl)

print(nnet_basic) # Review the model summary

# Apply the model to the test set
predicted <- predict(nnet_basic, test)

# Calculate the Confusion Matrix and statistics surrounding the performance of 
# our model
CM <- caret::confusionMatrix(data = predicted, 
                             reference = as.factor(test$stroke), 
                             positive='1')
print(CM) # view confusion matrix

# It's also common to score the performance of the model using the area under 
# the ROC curve metric (AUC). We can calculate the ROC directly
ROC_test = pROC::roc(response=test$stroke, predictor=ordered(predicted))
print(ROC_test) # This can be plotted also

AUC_test = pROC::auc(response=test$stroke, predictor=ordered(predicted))
print(AUC_test) # A score of 1 is very good, a score of 0.5 is very bad. 
