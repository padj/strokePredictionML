# dataPrep_binned
###############################################################################
# -----------------------------------------------------------------------------
# SCRIPT:
# Name:       dataPrep_binned.R
# Date:       28 June 2021
# Version:    1.0.0
# Authors:    thomas.padgett
#
# Description:
#             Import, explore, explain, clean, and manipulate base data for 
#             the strokePredictionML models. Outputs prepared data. Includes 
#             binned numeric values. Original data: 
#             https://www.kaggle.com/fedesoriano/stroke-prediction-dataset
#
# Change notes:
#             N/A
#
# -----------------------------------------------------------------------------
###############################################################################

#### Preamble ####

set.seed(1)
library(modeest)
library(imbalance)


#### Function definitions ####


#### Import data ####

dataLoc = "data/healthcare-dataset-stroke-data.csv"
data <- read.csv(dataLoc)
data_orig <- data #back up of original data


#### Data cleaning ####
# As in dataPrep.R

data$bmi <- as.numeric(data$bmi)
totalMissing <- colSums(is.na(data)) 

# Not including adding in averages. instead substitute NA for -99 in bmi.
data$bmi <- as.numeric(lapply(data$bmi, function(x) {if (is.na(x)) 
{x=-99} else {x=x}}))

data$gender <- factor(x=data$gender, levels=unique(data$gender))
data$ever_married <- factor(x = data$ever_married, 
                            levels = unique(data$ever_married))
data$work_type <- factor(x = data$work_type,
                         levels = unique(data$work_type))
data$Residence_type <- factor(x = data$Residence_type,
                              levels = unique(data$Residence_type))
data$smoking_status <- factor(x = data$smoking_status,
                              levels = unique(data$smoking_status))

# sort numeric categories into bins using manual breaks. Outputs are naturally
# of class factor
data$age_binned <- as.integer(cut(data$age, 
                                  breaks=c(-100,18,32,42,50,56,64,76,100)))
data$bmi_binned <- as.integer(cut(data$bmi, 
                                  breaks=c(-100,10,20,25,30,35,40,50,100)))
data$glucose_binned <- as.integer(cut(data$avg_glucose_level, 
                                      breaks=c(50,65,75,85,95,110,135,200,300)))
data <- data[c(-3, -9, -10)]

# comvert factors to numerics
data$ever_married <- as.integer(data$ever_married)
data$work_type <- as.integer(data$work_type)
data$Residence_type <- as.integer(data$Residence_type)
data$smoking_status <- as.integer(data$smoking_status)
data$gender <- as.integer(data$gender)


#### Oversampling ####
# ML algorithms need a balanced dataset, meaning a relatively equal number of 
# true and false stroke patients. We can use the imbalanceRatio function within
# the imbalance package to assess the balance of the data
print(imbalance::imbalanceRatio(data, 'stroke')) # shows that only 5% of the 
# patients suffered stroke, therefore not balanced. 

# We can use the imbalance package to oversample the data, effectively creating
# new patients that suffered stroke to balance the dataset.

# First split the data into test and train sets using createDataPartition
idx = createDataPartition(data$stroke, p = 0.8, list = FALSE)
train = data[idx, ]
test = data[-idx, ]
train <- train[,-1] # removed id

# oversample the data (1:1 ratio)
train_oversampled <- imbalance::oversample(train, 
                                           classAttr = "stroke", 
                                           ratio = 1, 
                                           method = "MWMOTE")

# Check the imbalance of each set
print(imbalance::imbalanceRatio(test, 'stroke'))
print(imbalance::imbalanceRatio(train, 'stroke'))
print(imbalance::imbalanceRatio(train_oversampled, 'stroke'))


#### Output data ####
write.csv(train_oversampled, 'data/trainDataOversampled_binned_cDP_v1.csv', row.names=FALSE)
write.csv(test, 'data/testDataOversampled_binned_cDP_v1.csv', row.names=FALSE)
