###################################################################################################################################################################################
#==================================================================================================================================================================================

##MSE/MAE/RMSE##

#Train Set
train$pred = predict(gamma_model, type="response")

#MSE
mse_gamma = mean((train$gross_amount - train$pred)^2)

#MAE
mae_gamma = mean(abs(train$gross_amount - train$pred))

#RMSE
rmse_gamma = sqrt(mse_gamma)

#Validation Set
validation$pred = predict(gamma_model, newdata=validation, type="response")

#MSE
mse_gamma_val = mean((validation$gross_amount - validation$pred)^2)

#MAE
mae_gamma_val = mean(abs(validation$gross_amount - validation$pred))

#RMSE
rmse_gamma_val = sqrt(mse_gamma_val)

#Compare results
results = data.frame(
  Dataset = c("Train", "Validation"),
  MSE = c(mse_gamma, mse_gamma_val),
  MAE = c(mae_gamma, mae_gamma_val),
  RMSE = c(rmse_gamma, rmse_gamma_val)
)
print(results)

#Validation errors are very similar to training errors so no evidence of overfitting

#===============================================================================================================

##Gini
#Measures how well model ranks policyholders by risk

#Function to calculate Gini
gini = function(actual, pred) {
  gini = function(a, p) {
    df = data.frame(actual = a, pred = p)
    df = df[order(df$pred, decreasing = TRUE), ]
    df$cum_actual = cumsum(df$actual)
    gini_sum = sum(df$cum_actual) / sum(df$actual) - (length(a) + 1) / 2
    return(gini_sum / length(a))
  }
  
  gini_model = gini(actual, pred)
  gini_perfect = gini(actual, actual)
  
  return(gini_model / gini_perfect)
}


#Compute Gini for train and validation sets

gini_train = gini(train$gross_amount, train$pred)
gini_validation = gini(validation$gross_amount, validation$pred)

gini_train #0.2830227
gini_validation #0.2814459
#Model has moderate ranking power 
#Values are almost the same so model generalises well
