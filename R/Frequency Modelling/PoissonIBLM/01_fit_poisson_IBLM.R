###############################################################################
# Poisson Interpretable Boosted GLM (IBLM)
###############################################################################
rm(list = ls())
set.seed(100)

library(IBLM)
library(xgboost)
library(splines)

policy_frequency_derived <- readRDS(
  url("https://github.com/mattmccarthyy/Statistical-Consulting-/raw/refs/heads/main/data/derived/policy_frequency_derived.rds")
)

###############################################################################
# Minimal Preprocessing
###############################################################################
# XGBoost won't take ordered factors, unordering occupation_risk5
# Everything else should already be accounted for. 
policy_frequency_derived$occupation_risk5 <- factor(
  policy_frequency_derived$occupation_risk5,
  ordered = FALSE
)

# Exposure proxy, can't use an offset (outlining this in report)
# IBLM cannot impose offset, including log(exposure) as a proxy covariate instead
policy_frequency_derived$log_exposure <- log(policy_frequency_derived$exposure)

# Age spline again on 6 df, removing age so we don't double up. 
age_ns <- ns(policy_frequency_derived$age, df = 6)
colnames(age_ns) <- paste0("age_ns", seq_len(ncol(age_ns)))
policy_frequency_derived <- cbind(policy_frequency_derived, age_ns)

###############################################################################
# Predictor Variables (define ONCE, after all engineered columns exist)
###############################################################################
preds <- c(
  "occupation_risk5",
  "ncd_level",
  "vehicle_age",
  "vehicle_power",
  "num_drivers",
  "province",
  "area",
  "fuel",
  "employment_missing",
  "security_device",
  "marital",
  "employment",
  "body_type",
  "transmission",
  "primary_usage",
  "occasional_commercial",
  "reported_mileage_missing",
  "engine_cc_missing",
  "engine_cc",
  "vehicle_value",
  "reported_mileage",
  colnames(age_ns),
  "log_exposure"
)

###############################################################################
# Create dataset for modelling "dat", response and predictors only
###############################################################################
dat <- policy_frequency_derived[, c("n_claims", preds)]

###############################################################################
# Train / validation / test split (60/20/20) as used in every other model
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
  assign("test1", test, envir = .GlobalEnv) # Changed to test 1, as slightly different columns than used in nb. Response is identical. Ensure this in comparison script. 
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
# Fitting Poisson IBLM
###############################################################################
N_ROUNDS <- 300
EARLY_STOP <- 25

iblm_poisson <- train_iblm_xgb(
  df_list,
  response_var = "n_claims",
  family = "poisson",
  params = list(
    max_depth = 2,
    eta = 0.05,
    subsample = 0.8,
    colsample_bytree = 0.8,
    min_child_weight = 50
  ),
  nrounds = N_ROUNDS,
  early_stopping_rounds = EARLY_STOP,
  verbose = 0,
  strip_glm = FALSE # Retaining the GLM component. Want this to be as comparable as possible.
)


###############################################################################
# Save fitted model and predictors for auditability later.
# Also saving test set for the comparison script. 
###############################################################################
saveRDS(list(model = iblm_poisson, preds = preds), file = file.path("R", "Frequency Modelling", "PoissonIBLM", "iblm_poisson_frequency.rds"))
saveRDS(test1, file = file.path("R", "Frequency Modelling", "PoissonIBLM", "test_iblm.rds"))
