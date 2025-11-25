rm(list = ls())

library(tidyverse)
library(xgboost)
library(Matrix)
library(SHAPforxgboost)
set.seed(123)
n_cores <- parallel::detectCores() - 1  # keep one core free, without this code takes incredibly long to run. 

claims_severity <- readRDS(url("https://github.com/mattmccarthyy/Statistical-Consulting-/raw/refs/heads/main/data/processed/claims_severity.rds")) 

#########################################################################################################
## 1). Prepare target and predictor variables
#########################################################################################################
data <- claims_severity

# Log target for stability
data$log_gross <- log(data$gross_amount)

# Train/test split (80/20 by row, may need to update this to 60/20/20).
n <- nrow(data)
train_idx <- sample(seq_len(n), size = floor(0.8 * n))

train <- data[train_idx, ]
test  <- data[-train_idx, ]

# Design matrices: xgboost will one-hot encode factors via model.matrix
X_train <- sparse.model.matrix(
  log_gross ~ . - gross_amount - net_amount - policy_id - vehicle_id - 1,
  data = train
)
X_test <- sparse.model.matrix(
  log_gross ~ . - gross_amount - net_amount - policy_id - vehicle_id - 1,
  data = test
)

y_train <- train$log_gross
y_test <- test$log_gross

dtrain <- xgb.DMatrix(data = X_train, label = y_train)
dtest  <- xgb.DMatrix(data = X_test, label = y_test)



#########################################################################################################
## 2). Hyperparameter tuning (shallow and regularised)
#########################################################################################################
param_grid <- expand.grid(
  max_depth = c(2, 3, 4),
  eta = c(0.03, 0.05, 0.1),
  min_child_weight = c(1, 5, 10),
  subsample = c(0.7, 0.9),
  colsample_bytree = c(0.7, 0.9)
)

best_rmse <- Inf
best_param <- NULL
best_nrounds <- NULL

for (i in seq_len(nrow(param_grid))) {
  p <- param_grid[i, ]
  params <- list(
    booster = "gbtree",
    objective = "reg:squarederror",
    eval_metric = "rmse",
    max_depth = p$max_depth,
    eta = p$eta,
    min_child_weight = p$min_child_weight,
    subsample = p$subsample,
    colsample_bytree = p$colsample_bytree,
    nthread = n_cores
  )
  
  cv <- xgb.cv(
    params = params,
    data = dtrain,
    nrounds = 800, # Keeping this low for now. If early stopping isn't breached before 800 we will increase.
    nfold = 5,
    early_stopping_rounds = 50,
    verbose = 0
  )
  
  mean_rmse <- min(cv$evaluation_log$test_rmse_mean)
  best_iter <- cv$best_iteration
  
  if (mean_rmse < best_rmse) {
    best_rmse <- mean_rmse
    best_param <- params
    best_nrounds <- best_iter
  }
}
# 28 minutes. 


#########################################################################################################
## 3). Fit final GBDT on full training set
#########################################################################################################
watchlist <- list(train = dtrain, eval = dtest)

gbdt_sev <- xgb.train(
  params = best_param,
  data = dtrain,
  nrounds = best_nrounds,
  watchlist = watchlist,
  print_every_n = 50
)



#########################################################################################################
## 4). Predictions and performance vs log-scale target
#########################################################################################################
# Predictions on log scale
log_pred_test <- predict(gbdt_sev, newdata = dtest)

# Back-transform to gross_amount
gross_pred_test <- exp(log_pred_test)

# Observed on original scale
gross_obs_test <- test$gross_amount

# Metrics (original scale)
rmse <- sqrt(mean((gross_pred_test - gross_obs_test)^2))
mae <- mean(abs(gross_pred_test - gross_obs_test))

# Metrics on log scale (closer to Gamma GLM log-link comparison)
rmse_log <- sqrt(mean((log_pred_test - y_test)^2))
mae_log  <- mean(abs(log_pred_test - y_test))



#########################################################################################################
## 5). SHAP explanations
#########################################################################################################
###############################################################
# 5.1). Global importance
###############################################################
# Prepare SHAP values 
set.seed(123)

# Sample rows from the sparse training matrix
shap_sample_idx <- sample(seq_len(nrow(X_train)), size = 1000)

# Convert sparse -> dense numeric matrix (NOT data.frame / tibble)
X_train_sample <- as.matrix(X_train[shap_sample_idx, ])

# Compute SHAP values
shap_values <- shap.prep(
  xgb_model = gbdt_sev,
  X_train = X_train_sample
)

# Summary plot
shap.plot.summary(shap_values)



###############################################################
# 5.2). Dependence
###############################################################
# Example (to be tweaked later): effect of vehicle_value on log severity
shap.plot.dependence(
  data_long = shap_values,
  x = "vehicle_value",
  color = "vehicle_value"
)

# Example (to be tweaked later): effect of age
shap.plot.dependence(
  data_long = shap_values,
  x = "age",
  color = "age"
)


