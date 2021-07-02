# rf_model
###############################################################################
# -----------------------------------------------------------------------------
# SCRIPT:
# Name:       rf_model.R
# Date:       24 June 2021
# Version:    1.0.0
# Authors:    thomas.padgett
#
# Description:
#             Imports prepped, feature-engineered test and train data. Trains
#             a random forest, tested on the test set. Performance is reviewed
#             using the confusion matrix and AUC metric.
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

# We'll use this later to define where we output to.
output_location <- 'output/rf/'
dotplot_name <- 'rf_dotplot_Oversampled_FEng_binned_cDP_num'

output_model_location <- 'scripts/MLModels/model_RData/'
output_model_name <- 'rf_model_Oversampled_FEng_binned_cDP_num.RData'

#### Function definitions ####


#### Import data ####

train_file <- "data/trainData_Oversampled_FEng_binned_cDP_num_v1.csv"
test_file <- "data/testData_Oversampled_FEng_binned_cDP_num_v1.csv"

train <- read.csv(train_file)
test <- read.csv(test_file)

#### Model ####
model <- list()
mtry <- sqrt(ncol(train))
tuneGrid <- expand.grid(.mtry=mtry)
modellist <- list()

# Define the training control method. 
# K-fold cross validation (number = folds)
model$ctrl <- caret::trainControl(method = "repeatedcv", 
                                  number = 10, 
                                  repeats = 3, 
                                  search = "grid", 
                                  classProbs = FALSE)

# Train the random forest
# with different ntree parameters
for (ntree in c(10,50,100,250,500,1000,2000)) {
  
  rf <- caret::train(as.factor(stroke) ~ .,
                          data = train,
                          method = "rf",
                          metric = "Kappa",
                          tuneGrid = tuneGrid,
                          trControl = model$ctrl)
  
  modellist[[toString(ntree)]] <- rf
}

# This bit allows us to plot and assess the different models 
results <- resamples(modellist)
summary(results)

# We can now save the plot
png(paste0(output_location,dotplot_name,'.png'), width = 800, height = 600)
dotplot(results)
dev.off()


# Review the dotplot and select the model that performs best. In  this case, 
# the version with nTree = 500 performs well (although all perform well).
# Set the best version as the final version
model$rf <- modellist$'500'

# Apply the model to the test set
predicted <- predict(model$rf, test)

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

