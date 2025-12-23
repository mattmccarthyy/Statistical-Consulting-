###################################################################################################################################################################################
#==================================================================================================================================================================================

##LASSO Model##

#Load libraries
library(glmnet)
library(dplyr)

#Determine categorical variable names
cat_policy_lasso = names(policy_frequency)[!sapply(policy_frequency, is.numeric)]; cat_policy_lasso

#Convert categorical variables to factors
categorical_vars = c("gender", "marital", "employment", "occupation",
                     "area", "province", "primary_usage", "overnight_parking",
                     "security_device", "occasional_commercial", 
                     "transmission", "fuel", "body_type", "vehicle_power", "engine_cc_missing",
                     "reported_mileage_missing")

train[categorical_vars] = lapply(train[categorical_vars], as.factor)

#Define response and predictor matrix
y = train$n_claims #Response
offset_var = log(train$exposure) #Offset for exposure

#Remove unwanted predictors from training data
predictors = train %>%
  dplyr::select(-policy_id, -cal_year, -n_claims, -exposure)

#Create dummy variables for factors (0/1)
X = model.matrix(~ ., data = predictors)[, -1]  #remove intercept column

#Fit LASSO Poisson regression with 10 fold cross-validation
set.seed(100)
cv_poisson = cv.glmnet(
  x = X,
  y = y,
  family = "poisson",
  offset = offset_var,
  alpha = 1, #1 = LASSO Regularisation
  nfolds = 10,
)

#Review lambda (penalty) by creating a plot
par(mfrow = c(1, 1))
plot(cv_poisson) #Shows trade off between number of predictors, Poisson deviance and strength of penalisation term

#Extract nonzero coefficients from cv_lasso - these are the predictors for the GLM
best_lambda_pois = cv_poisson$lambda.min
lasso_coef = coef(cv_poisson, s = best_lambda_pois)
nonzero = lasso_coef[lasso_coef[, 1] != 0, , drop = FALSE] #Remove coefficients with value 0
print(nonzero) #View non-zero coefficients

#Refit a Poisson GLM using LASSO-selected variables
#Convert model matrix to data frame
X_df = as.data.frame(as.matrix(X))
X_df$n_claims = y
X_df$exposure = train$exposure

#Clean names to ensure no incorrect naming 
names(X_df) = make.names(names(X_df))

#Extract selected variable names (excluding intercept)
selected_vars = make.names(rownames(nonzero)[-1])

#Remove unwanted variables 
selected_vars = setdiff(selected_vars, make.names(c("cal_year", "policy_id")))

##Store and print final selected variable names for future reference
final_selected_vars = selected_vars
cat("Final selected predictors (after removing unwanted vars):\n")
print(final_selected_vars)

#Build and fit Poisson GLM
formula_text = paste(
  "n_claims ~",
  paste(final_selected_vars, collapse = " + "),
  "+ offset(log(exposure))"
)
formula = as.formula(formula_text)

lasso_model = glm(formula, data = X_df, family = poisson(link="log"))

##LASSO model summary 
summary(lasso_model)

##AIC for LASSO model
AIC(lasso_model) #AIC=237001.1

#===========================================================================================================

##Finalised LASSO model

lasso_model
