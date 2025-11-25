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
