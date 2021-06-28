# dataPrep_selection
###############################################################################
# -----------------------------------------------------------------------------
# SCRIPT:
# Name:       dataPrep_selection.R
# Date:       23 June 2021
# Version:    1.0.0
# Authors:    thomas.padgett
#
# Description:
#             Variation on dataPrep to select most important variables based
#             on varImp of initial nnet and rf models. 
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
library(caret)


#### Function definitions ####


#### Import data ####

dataLoc = "data/healthcare-dataset-stroke-data.csv"
data <- read.csv(dataLoc)
data_orig <- data #back up of original data


#### Data cleaning ####
# As in dataPrep.R

data$bmi <- as.numeric(data$bmi)
totalMissing <- colSums(is.na(data)) 

avgBMI <- round(mean(na.omit(data$bmi)),1)
data$bmi <- as.numeric(lapply(data$bmi, function(x) {if (is.na(x)) 
{x=avgBMI} else {x=x}}))
rm(avgBMI)

avgSmokeS <- modeest::mfv(data$smoking_status) 
data$smoking_status <- lapply(data$smoking_status, function(x) {if 
  (x=="Unknown") {x=avgSmokeS} else {x=x}})
rm(avgSmokeS)

data$gender <- factor(x=data$gender, levels=unique(data$gender))
data$ever_married <- factor(x = data$ever_married, 
                            levels = unique(data$ever_married))
data$work_type <- factor(x = data$work_type,
                         levels = unique(data$work_type))
data$Residence_type <- factor(x = data$Residence_type,
                              levels = unique(data$Residence_type))
data$smoking_status <- factor(x = data$smoking_status,
                              levels = unique(data$smoking_status))

#### Feature Engineering ####
# As in dataPrep.R

data$male <- sapply(data$gender, function(x) {if (x=="Male") {x=1} else {x=0}})
data$female <- sapply(data$gender, function(x) {if (x=="Female") 
{x=1} else {x=0}})
data <- data[-2]

# And similar for other factor categories:
data$ever_married_yes <- sapply(data$ever_married, function(x) {if (x=="Yes") 
{x=1} else {x=0}})
data$ever_married_no <- sapply(data$ever_married, function(x) {if (x=="No") 
{x=1} else {x=0}})
data <- data[-5]

data$smoking_status_smokes <- sapply(data$smoking_status, 
                                     function(x) {if (x=="smokes") 
                                       {x=1} else {x=0}})
data$smoking_status_never_smoked <- sapply(data$smoking_status, 
                                           function(x) {if (x=="never smoked") 
                                             {x=1} else {x=0}})
data$smoking_status_formerly_smoked <- sapply(data$smoking_status, 
                                              function(x) {if 
                                                (x=="formerly smoked") {x=1} else {x=0}})
data <- data[-9]

data$residence_type_rural <- sapply(data$Residence_type, 
                                    function(x) {if (x=="Rural") {x=1} else {x=0}})
data$residence_type_urban <- sapply(data$Residence_type, 
                                    function(x) {if (x=="Urban") {x=1} else {x=0}})
data <- data[-6]

data$work_type_self_employed <- sapply(data$work_type, 
                                       function(x) {if (x=="Self-employed") 
                                         {x=1} else {x=0}})
data$work_type_private <- sapply(data$work_type, function(x) {if (x=="Private") 
{x=1} else {x=0}})
data$work_type_govt <- sapply(data$work_type, function(x) {if (x=="Govt_job") 
{x=1} else {x=0}})
data$work_type_child <- sapply(data$work_type, function(x) {if (x=="children") 
{x=1} else {x=0}})
data$work_type_never_worked <- sapply(data$work_type, function(x) 
{if (x=="Never_worked") {x=1} else {x=0}})
data <- data[-5]

# Then convert nums to ints:
data$male <- as.integer(data$male)
data$female <- as.integer(data$female)
data$ever_married_yes <- as.integer(data$ever_married_yes)
data$ever_married_no <- as.integer(data$ever_married_no)
data$smoking_status_smokes <- as.integer(data$smoking_status_smokes)
data$smoking_status_never_smoked<- as.integer(data$smoking_status_never_smoked)
data$smoking_status_formerly_smoked <- as.integer(data$smoking_status_formerly_smoked)
data$residence_type_rural <- as.integer(data$residence_type_rural)
data$residence_type_urban <- as.integer(data$residence_type_urban)
data$work_type_self_employed <- as.integer(data$work_type_self_employed)
data$work_type_private <- as.integer(data$work_type_private)
data$work_type_govt <- as.integer(data$work_type_govt)
data$work_type_child <- as.integer(data$work_type_child)
data$work_type_never_worked <- as.integer(data$work_type_never_worked)

#### Feature Selection ####
# Initial rf and nnet models can be used to determine the importance of each 
# variable and subsequently remove unimportant variables which may mislead or
# 'muddy' the modelling.

load('scripts/MLModels/rf_model.RData')
output_location <- 'output/rf/'
image_name <- 'rf_varImp_plot'
png(paste0(output_location,image_name,'.png'), width = 800, height = 600)
plot(varImp(model$rf), # varImp() from caret
     main='Variable importance from initial random forest model') 
dev.off()

load('scripts/MLModels/nnet_model.RData')
output_location <- 'output/nnet/'
image_name <- 'nnet_varImp_plot'
png(paste0(output_location,image_name,'.png'), width = 800, height = 600)
plot(varImp(model$nnet),
     main='Variable importance from initial neural network model')
dev.off()

# A comparison of the two plots shows that the rf model values 'age' as the 
# most important variable, whereas the nnet values 'avg_glucose_level' the 
# highest and 'age' as the second least important. 

# Removing age and rerunning both rf and nnet
data <- within(data, rm(ever_married_no))
data <- within(data, rm(work_type_child, work_type_never_worked))
data <- within(data, rm(male))
data <- within(data, rm(residence_type_rural))

#### Oversampling ####
# ML algorithms need a balanced dataset, meaning a relatively equal number of 
# true and false stroke patients. We can use the imbalanceRatio function within
# the imbalance package to assess the balance of the data
print(imbalance::imbalanceRatio(data, 'stroke')) # shows that only 5% of the 
# patients suffered stroke, therefore not balanced. 

# We can use the imbalance package to oversample the data, effectively creating
# new patients that suffered stroke to balance the dataset.

#First shuffle data for good mix of factors at all parts of data set
shuffle_index <- sample(1:nrow(data))
data <- data[shuffle_index, ]

# Oversampling should only be applied to data used for training NOT testing, and
# therefore we need to split the data into test and train sets first 
# (80/20 split)
splits <- sample(1:2, size=nrow(data), prob=c(0.8,0.2), replace=TRUE)
train <- data[splits==1,]
test <- data[splits==2,]
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
write.csv(train_oversampled, 'data/trainDataOversampled_featEng_selected_v1.csv', row.names=FALSE)
write.csv(test, 'data/testDataOversampled_featEng_selected_v1.csv', row.names=FALSE)