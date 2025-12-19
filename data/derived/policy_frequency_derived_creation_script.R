###############################################################################
# Building derived policy_frequency dataset (feature engineering only)
###############################################################################
rm(list = ls())
library(tidyverse)

set.seed(100)

###############################################################################
# Load cleaned current processed base dataset
###############################################################################
policy_frequency <- readRDS(
  url("https://github.com/mattmccarthyy/Statistical-Consulting-/raw/refs/heads/main/data/processed/policy_frequency.rds")
)

###############################################################################
# Feature engineering (just our Frequency EDA decisions in report)
###############################################################################
# 1). Occupation -> 5 risk buckets (ordered)
occ_key <- read.csv(
  "https://raw.githubusercontent.com/mattmccarthyy/Statistical-Consulting-/refs/heads/main/data/derived/occupation_risk5_lookup.csv",
  stringsAsFactors = FALSE
)

policy_frequency$occupation_risk5 <- factor(
  occ_key$occupation_risk5[match(policy_frequency$occupation, occ_key$occupation)],
  levels = c("Very Low", "Low", "Medium", "High", "Very High"),
  ordered = TRUE
)

# 2). NCD level as categorical rating factor
policy_frequency$ncd_level <- factor(policy_frequency$ncd_level)

# 3). Vehicle age capped at 19 
policy_frequency$vehicle_age <- pmin(policy_frequency$vehicle_age, 19)

# 4). Vehicle power as 3-level factor
policy_frequency$vehicle_power <- factor(
  policy_frequency$vehicle_power,
  levels = c("Low", "Med", "High")
)

# 5). Number of drivers as factor (1/2/3)
policy_frequency$num_drivers <- factor(policy_frequency$num_drivers, levels = c(1, 2, 3))

# 6). Fuel retained unaggregated as 4-level factor
policy_frequency$fuel <- factor(policy_frequency$fuel, levels = c("Diesel", "Petrol", "Hybrid", "EV"))

# 7). Territory factors retained as factors
policy_frequency$province <- factor(policy_frequency$province)
policy_frequency$area <- factor(policy_frequency$area, levels = c("Rural", "Suburban", "Urban"))

# 8). Employment missingness
policy_frequency$employment_missing <- factor(
  policy_frequency$employment_missing,
  levels = c(0, 1),
  labels = c("Reported", "Missing")
)

# 9). Security device 
policy_frequency$security_device <- factor(
  policy_frequency$security_device,
  levels = c(0, 1),
  labels = c("No Security Device", "Security Device")
)

# 10). Keep other categorical predictors as factors
policy_frequency$marital <- factor(policy_frequency$marital)
policy_frequency$employment <- factor(policy_frequency$employment)
policy_frequency$occupation <- factor(policy_frequency$occupation)   # retained for reference; model uses occupation_risk5
policy_frequency$body_type <- factor(policy_frequency$body_type)
policy_frequency$transmission <- factor(policy_frequency$transmission)
policy_frequency$primary_usage <- factor(policy_frequency$primary_usage)
policy_frequency$occasional_commercial <- factor(policy_frequency$occasional_commercial, levels = c(0, 1))

# 11). Additional Missingness flags (0/1 indicators), keeping in case we go into more detail with them later, but likely won't be included in further models. 
policy_frequency$reported_mileage_missing <- as.integer(policy_frequency$reported_mileage_missing)
policy_frequency$engine_cc_missing <- as.integer(policy_frequency$engine_cc_missing)

# 12). Predictors we don't want to/can't include in models from EDA
policy_frequency <- policy_frequency[
  , !names(policy_frequency) %in% c("gender", "overnight_parking")
]

###############################################################################
# Saving derived dataset
###############################################################################
policy_frequency_derived <- policy_frequency
# glimpse(policy_frequency_derived) # All as expected
# colSums(is.na(policy_frequency_derived)) # No NA's created

saveRDS(policy_frequency_derived, file = "policy_frequency_derived.rds")
