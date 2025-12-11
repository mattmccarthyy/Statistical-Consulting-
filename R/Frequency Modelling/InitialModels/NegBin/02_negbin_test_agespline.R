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
split <- split_data(policy_frequency) # external function



###############################################################################
# Fit NB model 
###############################################################################
final_nbA <- glm.nb(
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
    years_licensed +
    vehicle_age +
    factor(reported_mileage_missing) +
    factor(security_device) +
    ns(age,df = 6):factor(primary_usage) +
    ns(age,df = 6):factor(gender) +
    offset(log(exposure)),
  data = train,
  link = "log"
)

final_nbM <- glm.nb(
  n_claims ~ 
    ns(age, df = 8) +
    factor(primary_usage) +
    factor(vehicle_power) +
    factor(area) +
    factor(ncd_level) +
    factor(marital) +
    factor(occasional_commercial) +
    factor(employment_missing) +
    factor(body_type) +
    factor(gender) +
    years_licensed +
    vehicle_age +
    factor(reported_mileage_missing) +
    factor(security_device) +
    ns(age,df = 8):factor(primary_usage) +
    ns(age,df = 8):factor(gender) +
    offset(log(exposure)),
  data = train,
  link = "log"
)


###############################################################################
# Compare two NB GLMs on the validation set
###############################################################################
# Predictions on validation set
validation$pred_A <- predict(final_nbA, newdata = validation, type = "response")
validation$pred_M <- predict(final_nbM, newdata = validation, type = "response")

# RMSE
rmse_A <- sqrt(mean((validation$n_claims - validation$pred_A)^2))
rmse_M <- sqrt(mean((validation$n_claims - validation$pred_M)^2))

# NB deviance
dev_A <- 2 * sum(validation$n_claims * log((validation$n_claims + 1e-10) / validation$pred_A) -
                   (validation$n_claims - validation$pred_A))

dev_M <- 2 * sum(validation$n_claims * log((validation$n_claims + 1e-10) / validation$pred_M) -
                   (validation$n_claims - validation$pred_M))

c(rmse_A, rmse_M) # rmse M lower on out-of-sample data
c(dev_A, dev_M) # deviance M lower
AIC(final_nbA, final_nbM) # AIC M lower. (BIC Higher). 




