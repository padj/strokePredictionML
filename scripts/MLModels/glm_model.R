# glm_model.R
###############################################################################
# -----------------------------------------------------------------------------
# SCRIPT:
# Name:       glm_model.R
# Date:       28 June 2021
# Version:    1.0.0
# Authors:    thomas.padgett
#
# Description:
#             Imports prepped, test and train data. Trains using a glm
#             within caret. Tested on the test set. Performance is 
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

output_model_location <- 'scripts/MLModels/model_RData/'
output_model_name <- 'glm_model_Oversampled_FEng_cDP_int.RData'

#### Function definitions ####


#### Import data ####

train_file <- "data/trainData_Oversampled_FEng_cDP_int_v1.csv"
test_file <- "data/testData_Oversampled_FEng_cDP_int_v1.csv"

train <- read.csv(train_file)
test <- read.csv(test_file)

# if trainDataOversampled_featEng_v1.csv
# remove variables found to be insignificant.
# train <- within(train, rm(ever_married_no, smoking_status_formerly_smoked, 
#                          residence_type_rural, work_type_never_worked))




#### Model ####

model <- list()


# Define the training control method. 
# K-fold cross validation (number = folds)
model$ctrl <- caret::trainControl(method = "repeatedcv", 
                                  number = 4, 
                                  repeats = 3)

model$glm <- caret::train(as.factor(stroke) ~ .,
                    data = train,
                    method = "glm",
                    metric = "Kappa",
                    trControl = model$ctrl,
                    family = "binomial")

# Apply the model to the test set
predicted <- predict(model$glm, test)

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
save(model, file=paste0(output_model_location,output_model_name))
