# dataPrep_cDP
###############################################################################
# -----------------------------------------------------------------------------
# SCRIPT:
# Name:       dataPrep_cDP.R
# Date:       23 June 2021
# Version:    1.1.0
# Authors:    thomas.padgett
#
# Description:
#             Duplicate of dataPrep.R utilising the createDataPartition 
#             function (caret). Exploration. 
#             Original data: 
#             https://www.kaggle.com/fedesoriano/stroke-prediction-dataset
#
# Change notes:
#             Now calcs and outputs *_num and *_int versions of train/test.
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
# Data insights and cleaning.
str(data) # gives a good insight into the data.
# from this we can see that the data contains 12 variables (11 clinical 
# features), of mixed types. 

# We can also see that bmi is reported as a character rather than a numeric. 
# We can convert using:
data$bmi <- as.numeric(data$bmi)
# Note the warning message showing coerced NAs.

# It's likely that there are other NAs in the data as strings, which aren't 
# accounted for and should be true NAs. We can check for string versions of 
# 'N/A' using unique() 
unique(data$gender)
unique(data$ever_married)
unique(data$work_type)
unique(data$Residence_type)
unique(data$smoking_status)
# From this we can see there are no string NAs, which is great.

# We need to know how much data are missing.
totalMissing <- colSums(is.na(data)) # gives total number of missing data 
# points. 
print(totalMissing) # We can see there are 201 missing bmi values.

# We can fill in these missing bmi values with the average
avgBMI <- round(mean(na.omit(data$bmi)),1)
data$bmi <- as.numeric(lapply(data$bmi, function(x) {if (is.na(x)) 
{x=avgBMI} else {x=x}}))
rm(avgBMI) #remove the variable to keep a tidy environment
# This is a cheap and easy way of filling in missing data but it isn't great.
# We'll explore better ways of dealing with missingness later. 

# Using summary(data), we can see that many patients' smoking status is 
# reported as unknown (i.e. missing). We will replace all these unknowns with 
# the mode.
avgSmokeS <- modeest::mfv(data$smoking_status) 
data$smoking_status <- lapply(data$smoking_status, function(x) {if 
  (x=="Unknown") {x=avgSmokeS} else {x=x}})
rm(avgSmokeS)

# Character categories are often not as useful as they could be. What can 
# possibly make our future job easier by converting the character categories 
# to factors:
data$gender <- factor(x=data$gender, levels=unique(data$gender))
data$ever_married <- factor(x = data$ever_married, 
                            levels = unique(data$ever_married))
data$work_type <- factor(x = data$work_type,
                         levels = unique(data$work_type))
data$Residence_type <- factor(x = data$Residence_type,
                              levels = unique(data$Residence_type))
data$smoking_status <- factor(x = data$smoking_status,
                              levels = unique(data$smoking_status))

str(data) # See the cleaned data


#### Feature Engineering ####
# Often the format of the raw data doesn't lend itself to ML. We can manipulate
# the raw data, splitting factors of multiple levels into separate binary 
# categories that display the same data. Note that new binary categories are 
# likely to be mutually exclusive with others.

# For example, we can remove the gender category and implement two new 
# categories: male (0/1) and female (0/1)
data$male <- sapply(data$gender, function(x) {if (x=="Male") {x=1} else {x=0}})
data$female <- sapply(data$gender, function(x) {if (x=="Female") 
{x=1} else {x=0}})
data <- data[-2] # This removes the original gender column.

# And similar for other factor categories:
data$ever_married_yes <- sapply(data$ever_married, function(x) {if (x=="Yes") 
{x=1} else {x=0}})
data$ever_married_no <- sapply(data$ever_married, function(x) {if (x=="No") 
{x=1} else {x=0}})
data <- data[-5]

data$smoking_status_smokes <- sapply(data$smoking_status, 
                                     function(x) {if (x=="smokes") {x=1} else {x=0}})
data$smoking_status_never_smoked <- sapply(data$smoking_status, 
                                           function(x) {if (x=="never smoked") {x=1} else {x=0}})
data$smoking_status_formerly_smoked <- sapply(data$smoking_status, 
                                              function(x) {if (x=="formerly smoked") {x=1} else {x=0}})
data <- data[-9]

data$residence_type_rural <- sapply(data$Residence_type, 
                                    function(x) {if (x=="Rural") {x=1} else {x=0}})
data$residence_type_urban <- sapply(data$Residence_type, 
                                    function(x) {if (x=="Urban") {x=1} else {x=0}})
data <- data[-6]

data$work_type_self_employed <- sapply(data$work_type, 
                                       function(x) {if (x=="Self-employed") {x=1} else {x=0}})
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

str(data) # Check feature engineered data

#### Oversampling ####
# ML algorithms need a balanced dataset, meaning a relatively equal number of 
# true and false stroke patients. We can use the imbalanceRatio function within
# the imbalance package to assess the balance of the data
print(imbalance::imbalanceRatio(data, 'stroke')) # shows that only 5% of the 
# patients suffered stroke, therefore not balanced. 

# Two versions - numeric and integer.
data_num <- data
data_num$hypertension <- as.numeric(data_num$hypertension)
data_num$heart_disease <- as.numeric(data_num$heart_disease)
data_num$male <- as.numeric(data_num$male)
data_num$female <- as.numeric(data_num$female)
data_num$ever_married_yes <- as.numeric(data_num$ever_married_yes)
data_num$ever_married_no <- as.numeric(data_num$ever_married_no)
data_num$smoking_status_smokes <- as.numeric(data_num$smoking_status_smokes)
data_num$smoking_status_never_smoked<- as.numeric(data_num$smoking_status_never_smoked)
data_num$smoking_status_formerly_smoked <- as.numeric(data_num$smoking_status_formerly_smoked)
data_num$residence_type_rural <- as.numeric(data_num$residence_type_rural)
data_num$residence_type_urban <- as.numeric(data_num$residence_type_urban)
data_num$work_type_self_employed <- as.numeric(data_num$work_type_self_employed)
data_num$work_type_private <- as.numeric(data_num$work_type_private)
data_num$work_type_govt <- as.numeric(data_num$work_type_govt)
data_num$work_type_child <- as.numeric(data_num$work_type_child)
data_num$work_type_never_worked <- as.numeric(data_num$work_type_never_worked)

data_int <- data
data_int$hypertension <- as.integer(data_int$hypertension)
data_int$heart_disease <- as.integer(data_int$heart_disease)
data_int$male <- as.integer(data_int$male)
data_int$female <- as.integer(data_int$female)
data_int$ever_married_yes <- as.integer(data_int$ever_married_yes)
data_int$ever_married_no <- as.integer(data_int$ever_married_no)
data_int$smoking_status_smokes <- as.integer(data_int$smoking_status_smokes)
data_int$smoking_status_never_smoked<- as.integer(data_int$smoking_status_never_smoked)
data_int$smoking_status_formerly_smoked <- as.integer(data_int$smoking_status_formerly_smoked)
data_int$residence_type_rural <- as.integer(data_int$residence_type_rural)
data_int$residence_type_urban <- as.integer(data_int$residence_type_urban)
data_int$work_type_self_employed <- as.integer(data_int$work_type_self_employed)
data_int$work_type_private <- as.integer(data_int$work_type_private)
data_int$work_type_govt <- as.integer(data_int$work_type_govt)
data_int$work_type_child <- as.integer(data_int$work_type_child)
data_int$work_type_never_worked <- as.integer(data_int$work_type_never_worked)

# We can use the imbalance package to oversample the data, effectively creating
# new patients that suffered stroke to balance the dataset.

# First split the data into test and train sets using createDataPartition
idx = createDataPartition(data_num$stroke, p = 0.8, list = FALSE)
train_num = data_num[idx, ]
test_num = data_num[-idx, ]
train_num <- train_num[,-1] # removed id

idx = createDataPartition(data_int$stroke, p = 0.8, list = FALSE)
train_int = data_int[idx, ]
test_int = data_int[-idx, ]
train_int <- train_int[,-1] # removed id

# oversample the data (1:1 ratio)
train_oversampled_num <- imbalance::oversample(train_num, 
                                           classAttr = "stroke", 
                                           ratio = 1, 
                                           method = "MWMOTE")

train_oversampled_int <- imbalance::oversample(train_int, 
                                               classAttr = "stroke", 
                                               ratio = 1, 
                                               method = "MWMOTE")

#### Output data ####
write.csv(train_oversampled_int, 'data/trainData_Oversampled_FEng_cDP_int_v1.csv', row.names=FALSE)
write.csv(test_int, 'data/testData_Oversampled_FEng_cDP_int_v1.csv', row.names=FALSE)

write.csv(train_oversampled_num, 'data/trainData_Oversampled_FEng_cDP_num_v1.csv', row.names=FALSE)
write.csv(test_num, 'data/testData_Oversampled_FEng_cDP_num_v1.csv', row.names=FALSE)

















