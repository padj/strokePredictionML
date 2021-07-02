
model_types <- c('glm', 'rf', 'nnet', 'xgb')
data_location <- 'scripts/MLModels/model_RData/'

for (model_type in model_types) {
  data_names <- list.files(data_location, pattern = model_type)

  output <- data.frame(matrix(ncol = 6, nrow = 13))
  output[1,1] <- model_type
  output[1,2] <- 'Accuracy'
  output[1,3] <- 'Kappa'
  output[1,4] <- 'Sensitivity'
  output[1,5] <- 'Specificity'
  output[1,6] <- 'AUC'
  
  for (i in 1:length(data_names)) {
    name <- data_names[i]
    load(paste0(data_location,name))
  
    name <- substr(name[1],nchar(model_type)+8,nchar(name[1])-6)
    output[i+1,1] <- name
    output[i+1,2] <- model$CM$overall[1] # accuracy
    output[i+1,3] <- model$CM$overall[2] # kappa
    output[i+1,4] <- model$CM$byClass[1] # sensitivity 
    output[i+1,5] <- model$CM$byClass[2] # specificity
    output[i+1,6] <- model$AUC # AUC
  }

file_name <- paste0(model_type,'_table_output.csv')
write.csv(output, paste0('output/', file_name), quote = FALSE,
          row.names = FALSE)
  
} 




