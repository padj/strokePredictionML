# XGB_models
###############################################################################
# -----------------------------------------------------------------------------
# SCRIPT:
# Name:       XGB_models.R
# Date:       28 June 2021
# Version:    1.0.0
# Authors:    thomas.padgett
#
# Description:
#             Imports prepped, test and train data. Trains using various XGB
#             methods within caret. Tested on the test set. Performance is 
#             reviewed using the confusion matrix and AUC metric.
#
# Change notes:
#             N/A
#
# -----------------------------------------------------------------------------
###############################################################################

#### Preamble ####

set.seed(10) # Ensure repeatability
library(pROC)
library(caret)
library(e1071) #required within caret::train()
library(xgboost)
library(plyr)
library(h2o)
library(gbm)


# We'll use this later to define where we output to.
output_location <- 'output/XGB/'

#### Function definitions ####


#### Import data ####

train_file <- "data/trainDataOversampled_featEng_v1.csv"
test_file <- "data/testDataOversampled_featEng_v1.csv"

train <- read.csv(train_file)
test <- read.csv(test_file)


#### Model ####

xgb_models <- c('xgbLinear', 'xgbTree', 'gbm')
# 'gbm_h2o' <- doesn't seem to work.
# Note: xgbDART seems to take forever. 'xgbDART', 

model <- list()
modellist <- list()

# Define the training control method. 
# K-fold cross validation (number = folds)
model$ctrl <- caret::trainControl(method = "repeatedcv", 
                                  number = 5, 
                                  repeats = 3,
                                  verboseIter = FALSE)


# Train the model
for (mdl in xgb_models) {
  
  print(paste0('running ', mdl))
  
  xgb <- caret::train(as.factor(stroke) ~ .,
                   data = train,
                   method = mdl,
                   metric = "Kappa",
                   trControl = model$ctrl,
                   verbose = 0)
  
  modellist[[toString(mdl)]] <- xgb
}

# This bit allows us to plot and assess the different models 
results <- resamples(modellist)
summary(results)

# We can now save the plot
image_name <- 'xgb_dotplot'
png(paste0(output_location,image_name,'.png'), width = 800, height = 600)
dotplot(results)
dev.off()


# take best performing
model$xgb <- modellist$xgbLinear

# Apply the model to the test set
predicted <- predict(model$xgb, test)

# Calculate the Confusion Matrix and statistics surrounding the performance of 
# our model
model$CM <- caret::confusionMatrix(data = predicted, 
                                   reference = as.factor(test$stroke), 
                                   positive='1')
print(model$CM) # view confusion matrix

# It's also common to score the performance of the model using the area under 
# the ROC curve metric (AUC). We can calculate the ROC directly
ROC_val = pROC::roc(response=test$stroke, predictor=ordered(predicted))
print(ROC_val) # This can be plotted also

# Or just spit out the AUC metric
AUC_val = pROC::auc(response=test$stroke, predictor=ordered(predicted))
print(AUC_val) # A score of 1 is very good, a score of 0.5 is very bad. 
model$AUC <- AUC_val

# Add train and test file names for reference and reproducibility. 
model$train_file <- train_file 
model$test_file <- test_file

# Now that the model is developed/trained, we should save the model, along with
# information about it for use later. 
# The list "model" contains the trained model, the control file, the 
# confusion matrix and the AUC metric, as well as the names of the train and 
# test files used in development of the model.
output_model_location <- 'scripts/MLModels/'
output_model_name <- 'xgb_model.RData'
save(model, file=paste0(output_model_location,output_model_name))
