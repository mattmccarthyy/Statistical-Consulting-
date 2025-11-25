###################################################################################################################################################################################
#==================================================================================================================================================================================
##Train/Test/Validation Generic R script##

##Random Split
split_data = function(data){
  set.seed(100) #Needed for reproducibility
  n = nrow(data)
  indices = sample(1:n) #Randomly shuffles row indexes
  
  #Compute split sizes
  train_size = floor(0.6*n) 
  validation_size = floor(0.2*n)
  test_size = n-train_size-validation_size
  
  #Split indices
  train_index = indices[1:train_size]
  validation_index = indices[(train_size + 1):(train_size + validation_size)]
  test_index = indices[(train_size + validation_size +1):n]
  
  #Create splits in data
  train = data[train_index, ,drop=FALSE]
  validation = data[validation_index, , drop=FALSE]
  test = data[test_index, , drop=FALSE]
  
  #Assign to global environment
  assign("train", train, envir = .GlobalEnv)
  assign("validation", validation, envir = .GlobalEnv)
  assign("test", test, envir = .GlobalEnv)
}
