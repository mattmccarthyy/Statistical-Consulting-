###############################################################################
# Negative Binomial GLM – Feature Engineering + Final Model Only
###############################################################################
rm(list = ls())

library(MASS)
library(splines)
library(tidyverse)

set.seed(100)

###############################################################################
# Load data
###############################################################################
policy_frequency <- readRDS(
  url("https://github.com/mattmccarthyy/Statistical-Consulting-/raw/refs/heads/main/data/processed/policy_frequency.rds")
)

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
    factor(primary_usage) +
    factor(vehicle_power) +
    factor(area) +
    factor(ncd_level) +
    factor(marital) +
    factor(occasional_commercial) +
    factor(employment_missing) +
    factor(body_type) +
    factor(gender) +
    vehicle_age +
    ns(age,df = 6):factor(primary_usage) +
    ns(age,df = 6):factor(gender) +
    offset(log(exposure)),
  data = final_trainset,
  link = "log"
)
summary(final_nb_M)


###############################################################################
# Check for overfitting: train vs validation performance
###############################################################################
# 1) Predictions
train$pred_M <- predict(final_nb_M, newdata = train, type = "response")
validation$pred_M <- predict(final_nb_M, newdata = validation, type = "response")

# 2) RMSE (scaled measure, comparable across sample sizes)
rmse_train_M <- sqrt(mean((train$n_claims - train$pred_M)^2))
rmse_valid_M <- sqrt(mean((validation$n_claims - validation$pred_M)^2))

rmse_train_M
rmse_valid_M

# 3) Scaled deviance (correct measure for comparing different sized datasets)
scaled_dev_train <- deviance(final_nb_M) / sum(train$exposure)

dev_valid_M <- 2 * sum(
  validation$n_claims * log((validation$n_claims + 1e-10) / validation$pred_M) -
    (validation$n_claims - validation$pred_M)
)

scaled_dev_valid <- dev_valid_M / sum(validation$exposure)

scaled_dev_train
scaled_dev_valid


