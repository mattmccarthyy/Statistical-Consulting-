###############################################################################
# Gamma Interpretable Boosted GLM (IBLM)
###############################################################################
rm(list = ls())
set.seed(100)

library(IBLM)
library(xgboost)
library(splines)

claims_severity <- readRDS(
  url("https://github.com/mattmccarthyy/Statistical-Consulting-/raw/refs/heads/main/data/processed/claims_severity.rds")
)


###############################################################################
# Minimal Preprocessing 
###############################################################################
claims_severity$security_device <- NULL # Removing based on EDA

# These are ints but must be factors to match the Gamma GLM
claims_severity$occasional_commercial <- factor(claims_severity$occasional_commercial)
claims_severity$ncd_level <- factor(claims_severity$ncd_level)
claims_severity$reported_mileage_missing <- factor(claims_severity$reported_mileage_missing)
claims_severity$engine_cc_missing <- factor(claims_severity$engine_cc_missing)

# Age spline (df = 6) exactly as in GLM
age_ns <- ns(claims_severity$age, df = 6)
colnames(age_ns) <- paste0("age_ns", seq_len(ncol(age_ns)))
claims_severity <- cbind(claims_severity, age_ns)



###############################################################################
# Create design matrix with the exact GLM structure (incl. interaction)
###############################################################################
X <- model.matrix(
  ~ vehicle_power + usage + area + fuel + overnight_parking +
    age_ns1 + age_ns2 + age_ns3 + age_ns4 + age_ns5 + age_ns6 +
    engine_cc + occasional_commercial +
    ncd_level + vehicle_age + body_type +
    reported_mileage_missing + engine_cc_missing +
    vehicle_power:usage,
  data = claims_severity
) # Removed security_device predictor based off EDA.

# Drop intercept
X <- X[, colnames(X) != "(Intercept)", drop = FALSE]

preds <- colnames(X) # Using all cols as predictors now



###############################################################################
# Create dataset for modelling "dat", response and predictors only
###############################################################################
dat <- data.frame(gross_amount = claims_severity$gross_amount, X, check.names = FALSE)



###############################################################################
# Train / validation / test split (60/20/20)
###############################################################################
split_data <- function(data){
  n <- nrow(data)
  indices <- sample.int(n)
  
  train_size <- floor(0.6 * n)
  validation_size <- floor(0.2 * n)
  
  train_index <- indices[1:train_size]
  validation_index <- indices[(train_size + 1):(train_size + validation_size)]
  test_index <- indices[(train_size + validation_size + 1):n]
  
  train <- data[train_index, , drop = FALSE]
  validation <- data[validation_index, , drop = FALSE]
  test <- data[test_index, , drop = FALSE]
  
  assign("train", train, envir = .GlobalEnv)
  assign("validation", validation, envir = .GlobalEnv)
  assign("test", test, envir = .GlobalEnv)
}

split_data(dat)



###############################################################################
# IBLM needs a named list with "train" and "validate"
###############################################################################
df_list <- list(
  train = train,
  validate = validation
)



###############################################################################
# Fitting and Tuning Gamma IBLM
###############################################################################
gamma_dev_mean <- function(y, mu){
  y <- pmax(y, 1e-12); mu <- pmax(mu, 1e-12)
  mean(2 * ((y - mu) / mu - log(y / mu)))
}

fit_one <- function(g){
  m <- train_iblm_xgb(
    df_list,
    response_var = "gross_amount",
    family = "gamma",
    params = list(
      max_depth = g$md,
      eta = g$eta,
      min_child_weight = g$mcw,
      gamma = g$gam,
      subsample = g$subs,
      colsample_bytree = g$colsub,
      lambda = g$lam,
      alpha = g$alp
    ),
    nrounds = 800,
    early_stopping_rounds = 50,
    verbose = 0,
    strip_glm = FALSE
  )
  mu <- as.numeric(predict(m, newdata = validation))
  list(model = m, val = gamma_dev_mean(validation$gross_amount, mu))
}

# 2 * 3 * 4 * 3 * 2 * 2 * 3 * 3 = 3,888 fits, takes ages to run.
grid <- expand.grid(
  md = c(1,2),
  eta = c(0.02, 0.05, 0.1),
  mcw = c(10, 30, 50, 100),
  gam = c(0, 1, 5),
  subs = c(0.8, 1.0),
  colsub = c(0.8, 1.0),
  lam = c(1, 5, 10),
  alp = c(0, 1, 5)
)

best_val <- Inf; best_fit <- NULL; best_grid <- NULL

for(i in seq_len(nrow(grid))){
  fit <- fit_one(grid[i, ])
  if (fit$val < best_val){ 
    best_val <- fit$val; best_fit <- fit; best_grid <- grid[i, ] 
    }
}

best_grid
best_val



###############################################################################
# Final Gamma IBLM fit on train, with validation for early stopping. 
###############################################################################
df_list_final <- list(train = train, validate = validation)

iblm_gamma_final <- train_iblm_xgb(
  df_list_final,
  response_var = "gross_amount",
  family = "gamma",
  params = list(
    max_depth = best_grid$md,
    eta = best_grid$eta,
    min_child_weight = best_grid$mcw,
    gamma = best_grid$gam,
    subsample = best_grid$subs,
    colsample_bytree = best_grid$colsub,
    lambda = best_grid$lam,
    alpha = best_grid$alp
  ),
  nrounds = 2000,
  early_stopping_rounds = 50,
  verbose = 1,
  strip_glm = FALSE
)

mu_test <- as.numeric(predict(iblm_gamma_final, newdata = test))
gamma_dev_mean_1 <- gamma_dev_mean(test$gross_amount, mu_test); gamma_dev_mean_1



###############################################################################
# Saving Everything  
###############################################################################
saveRDS(test, file = file.path("R","Severity Modelling","GammaIBLM","test_gamma_iblm.rds"))
saveRDS(train, file = file.path("R","Severity Modelling","GammaIBLM","train_gamma_iblm.rds"))
saveRDS(validation, file = file.path("R","Severity Modelling","GammaIBLM","validation_gamma_iblm.rds"))
saveRDS(list(model = iblm_gamma_final, preds = preds, best_grid = best_grid, test_dev = gamma_dev_mean_1), file = file.path("R","Severity Modelling","GammaIBLM","iblm_gamma_final.rds"))
        
