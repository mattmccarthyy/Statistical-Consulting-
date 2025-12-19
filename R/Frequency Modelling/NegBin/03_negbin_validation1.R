###############################################################################
# Negative Binomial GLM – Feature Engineering + Final Model Only
###############################################################################
rm(list = ls())

library(MASS)
library(splines)
library(tidyverse)
library(IBLM)

set.seed(100)

###############################################################################
# Load data
###############################################################################
policy_frequency <- readRDS(
  url("https://github.com/mattmccarthyy/Statistical-Consulting-/raw/refs/heads/main/data/processed/policy_frequency.rds")
)


###############################################################################
# Helper Function. Brought it into script to save time.
###############################################################################
# Random Split
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
###############################################################################
# Feature engineering
###############################################################################
# Occupation 5-level lookup
occ_key <- read.csv(
  "https://raw.githubusercontent.com/mattmccarthyy/Statistical-Consulting-/refs/heads/main/data/derived/occupation_risk5_lookup.csv"
)
policy_frequency$occupation_risk5 <- factor(
  occ_key$occupation_risk5[match(policy_frequency$occupation, occ_key$occupation)],
  levels = c("Very Low","Low","Medium","High","Very High")
)

# Factors
policy_frequency$ncd_level <- factor(policy_frequency$ncd_level)
policy_frequency$vehicle_power <- factor(policy_frequency$vehicle_power)
policy_frequency$num_drivers <- factor(policy_frequency$num_drivers)

# Cap vehicle age
policy_frequency$vehicle_age <- pmin(policy_frequency$vehicle_age, 19)


###############################################################################
# Train / Validation / Test split
###############################################################################
split_data(policy_frequency) # external function
final_trainset <- rbind(train, validation)

###############################################################################
# Fit NB model 
###############################################################################
final_nb_M <- glm.nb(
  n_claims ~ 
    ns(age, df = 6) +
    occupation_risk5 +
    factor(primary_usage) +
    factor(vehicle_power) +
    factor(area) +
    factor(ncd_level) +
    factor(marital) +
    factor(occasional_commercial) +
    factor(employment_missing) +
    factor(body_type) +
    vehicle_age +
    offset(log(exposure)),
  data = final_trainset,
  link = "log"
)

summary(final_nb_M)


###############################################################################
# Fit NB model 
###############################################################################
# Baseline predictions for IBLM residual learner
final_trainset$mu_nb <- predict(final_nb_M, newdata = final_trainset, type = "response")
test$mu_nb <- predict(final_nb_M, newdata = test, type = "response")

# Residuals for the learner
final_trainset$res_nb <- (final_trainset$n_claims - final_trainset$mu_nb) / sqrt(final_trainset$mu_nb)

