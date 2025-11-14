rm(list = ls())

###################################################################################################
# Installing and attaching packages we use.
###################################################################################################

# install.packages("readr")
# install.packages("dplyr")
# install.packages("conflicted")


library(readr) # Using tidyverse to clean the data, following "An R approach to Data Cleaning and Wrangling for Educational Research".
library(dplyr)
library(conflicted)
conflict_prefer("filter", "dplyr", quiet = TRUE) # Use dplyr filter by default, otherwise we may run into issues with filter() if we use MASS for example. 
conflict_prefer("lag", "dplyr", quiet = TRUE) # Had to add this due to conflicts later on.



###################################################################################################
# Loading in Data.
###################################################################################################

policy_url <- "https://raw.githubusercontent.com/mattmccarthyy/Statistical-Consulting-/refs/heads/main/data/processed/policy.csv"
claims_url <- "https://raw.githubusercontent.com/mattmccarthyy/Statistical-Consulting-/refs/heads/main/data/raw/motor_claims_100k_2025-10-22.csv"

policy <- read_csv(policy_url, show_col_types = FALSE)
claims <- read_csv(claims_url, show_col_types = FALSE)
claims_raw <- claims

# na_rate(): quick missingness scanner, gives a fast read on where our NAs are concentrated to guide repairs or feature pruning. 
na_rate <- function(df) sapply(df, function(x) mean(is.na(x))) |> sort(TRUE)

# Inital Checks
# head(claims)
str(claims)
# glimpse(claims)
# summary(claims)

# NA Study
na_rate(claims)[1:6]*100 # only vehicle power to deal with. 0.3% NA's (240 claims, very inconsequential)



###################################################################################################
# Adding Missingness Flags to Column (vehicle_power) Featuring NA's.
###################################################################################################
claims <- claims %>%
  mutate(
    vehicle_power_missing = is.na(vehicle_power)
  )



###################################################################################################
# Checking for Missingness Effects. 
###################################################################################################
claims %>%
  group_by(vehicle_power_missing) %>%
  summarise(
    avg_net_amount = mean(net_amount),
    n = n()
  )
# Average claim is actually lower, this flag most likely irrelevant.
# We will not be using this flag.
# Insufficient credibility, no adverse selection, parsimony, all reasons to remove this flag. 



###################################################################################################
# Impute Vehicle Power.
###################################################################################################
# If policy id in twice, take first non-NA vehicle_power entry.
claims <- claims %>% 
  group_by(policy_id) %>%
  mutate(vehicle_power = ifelse(is.na(vehicle_power), first(na.omit(vehicle_power)), vehicle_power)) %>%
  ungroup()
sum(is.na(claims$vehicle_power)) # 72 NA's left. 

# Fallback to grouped mode (Categorical variable)
claims <- claims %>%
  group_by(body_type, fuel, transmission) %>%
  mutate(vehicle_power = ifelse(is.na(vehicle_power), names(which.max(table(vehicle_power))), vehicle_power)) %>%
  ungroup()
sum(is.na(claims$vehicle_power)) # No NA's left. 



###################################################################################################
# Removing Missingness Flag. Won't use it. 
###################################################################################################
claims <- claims %>%
  select(-vehicle_power_missing)



###################################################################################################
# Duplicate Investigation. 
###################################################################################################
sum(duplicated(claims)) # No exact duplicates.

# Count how many policy_id + occurrence_date combinations appear multiple times
claims %>%
  group_by(policy_id, occurrence_date) %>%
  summarise(n = n(), .groups = "drop") %>%
  filter(n > 1) %>%
  summarise(n_duplicate_combos = n(), total_duplicate_rows = sum(n))

# View the duplicate claims to see what varies
claims %>%
  group_by(policy_id, occurrence_date) %>%
  filter(n() > 1) %>%
  arrange(policy_id, occurrence_date) %>%
  select(policy_id, occurrence_date, gross_amount, net_amount, everything())

# Check if vehicle_id differs (different vehicles = separate claims)
claims %>%
  group_by(policy_id, occurrence_date) %>%
  filter(n() > 1) %>%
  arrange(policy_id, occurrence_date) %>%
  select(policy_id, occurrence_date, vehicle_id, gross_amount, net_amount)
# Vehicle id is the same, so these are not separate claims occurring on the same day.
# Likely revised claim amounts, keep the highest amount (gross), (should be the final settlement)

# Keeping highest gross_amount
claims <- claims %>%
  group_by(policy_id, occurrence_date) %>%
  slice_max(gross_amount, n = 1, with_ties = FALSE) %>%
  ungroup()

# Count how many policy_id + occurrence_date combinations appear multiple times (again)
claims %>%
  group_by(policy_id, occurrence_date) %>%
  summarise(n = n(), .groups = "drop") %>%
  filter(n > 1) %>%
  summarise(n_duplicate_combos = n(), total_duplicate_rows = sum(n))
# No more duplicates



###################################################################################################
# Feature Engineering. 
###################################################################################################
# Join relevant policy features to claims for severity modelling

# Matched by policy_id x cal_year
# FEATURES ADDED TO CLAIMS (from policy dataset):
# 1. Demographics (risk factors for severity):
#    -marital: Marital status may affect claim severity
#    -employment: Employment type could indicate income/vehicle usage patterns
#    -occupation: Different occupations may have different severity profiles
#    -years_licensed: Driving experience, will have to decide between this and age, likely using age.
#    -licensing_age: Age when first licensed (proxy for experience quality)
# 2. Vehicle characteristics:
#    -vehicle_age: Older vehicles may have higher/lower repair costs
#    -reported_mileage: Higher mileage may indicate wear/tear affecting severity
# 3. Policy features:
#    -num_drivers: Multiple drivers may affect severity patterns
#    -exposure: Time exposed to risk (useful for weighting/offsets)
# 4. Data quality flags:
#    -employment_missing: Flag for missing employment data
#    -reported_mileage_missing: Flag for missing mileage data
#    -engine_cc_missing: Flag for missing engine data
#
# FEATURES EXCLUDED (from policy dataset):
# 1. Agg claim vars (DATA LEAKAGE):
#    -n_claims, sum_net, sum_gross, avg_net, avg_gross
#    -These aggregate the target variable we're trying to predict
#    -Including them would cause overfitting/data leakage
# 2. Dupe cols (already in claims):
#    -gender, age, area, province, body_type, fuel, transmission,
#      vehicle_power, engine_cc, vehicle_value, usage, overnight_parking,
#      occasional_commercial, security_device, ncd_level

claims <- claims %>%
  left_join(
    policy %>%
      select(
        policy_id,
        cal_year,
        # New demographic features
        marital,
        employment,
        occupation,
        years_licensed,
        licensing_age,
        # New vehicle features
        vehicle_age,
        reported_mileage,
        # Policy features
        num_drivers,
        exposure,
        # Missingness flags
        employment_missing,
        reported_mileage_missing,
        engine_cc_missing
      ),
    by = c("policy_id", "cal_year")
  )

# Check the join results
sum(is.na(claims$marital)) 
# If marital NA, the entire join failed for that specific row. 
# marital is arbitrary, we could have checked any joined attribute

# Checking which claims are unmatched, more robust than above. 
unmatched <- claims %>%
  filter(is.na(marital) & is.na(employment) & is.na(years_licensed))
nrow(unmatched)

# Check the unmatched claims
unmatched %>%
  select(policy_id, cal_year, occurrence_date, gross_amount, net_amount) %>%
  head(20)

# Are these policy_ids completely missing from policy dataset?
unmatched_policy_ids <- unmatched %>%
  distinct(policy_id) %>%
  pull(policy_id)
policy %>%
  filter(policy_id %in% unmatched_policy_ids) %>%
  select(policy_id, cal_year) %>%
  head(20)
# Cal_year is the attribute that isn't matching.
# This is simply a data quality issue.
# We could adjust for nearest policy year, and match these up (quite difficult)
# Since it is only ~0.16% of data, for simplicity I am simply removing these policies. 
claims <- claims %>%
  filter(!is.na(marital))



###################################################################################################
# Further Dataset Check
###################################################################################################
# Final NA check
na_rate(claims) # No NA's to deal with

# Check target variable (net_amount / gross_amount)
sum(claims$net_amount < 0, na.rm = TRUE) # 0 claims < 0. All good. 
sum(claims$net_amount == 0, na.rm = TRUE) # 103 zero net_amounts, MUST INVESTIGATE
sum(claims$gross_amount < claims$net_amount, na.rm = TRUE) # 0 gross < net, all good. 

# Final dupe check. 
sum(duplicated(claims[, c("policy_id", "occurrence_date")])) # 0 Duplicates.

# Check target variable.
summary(claims$net_amount) # All good

# Investigate zero net_amount claims
claims %>%
  filter(net_amount == 0) %>%
  select(policy_id, occurrence_date, gross_amount, net_amount, cal_year) %>%
  head(20)

# Check if gross amounts also 0. 
claims %>%
  filter(net_amount == 0) %>%
  summarise(
    also_zero_gross = sum(gross_amount == 0),
    non_zero_gross = sum(gross_amount > 0)
  )
# Gross amounts are all positive. This is problematic. 
# Again, incredibly small amount of data (0.12%), removing for simplicity.
claims <- claims %>%
  filter(net_amount > 0)



###################################################################################################
# Please Note
###################################################################################################
# Due to huge dimension of this dataset, going to save rare category analysis for the modelling step.



###################################################################################################
# Dataset Formatting (for rds)
###################################################################################################
claims_severity <- claims %>%
  mutate(
    # Convert character columns to factors
    usage = factor(usage),
    area = factor(area),
    province = factor(province),
    overnight_parking = factor(overnight_parking),
    body_type = factor(body_type),
    vehicle_power = factor(vehicle_power),
    fuel = factor(fuel),
    transmission = factor(transmission),
    gender = factor(gender),
    marital = factor(marital),
    employment = factor(employment),
    occupation = factor(occupation),
    
    # Convert logical/numeric to integer where appropriate
    cal_year = as.integer(cal_year),
    occasional_commercial = as.integer(occasional_commercial),
    security_device = as.integer(security_device),
    vehicle_id = as.integer(vehicle_id),
    ncd_level = as.integer(ncd_level),
    num_drivers = as.integer(num_drivers),
    employment_missing = as.integer(employment_missing),
    reported_mileage_missing = as.integer(reported_mileage_missing),
    engine_cc_missing = as.integer(engine_cc_missing),
    
    # Ensure numeric columns are numeric (should already be, but explicit)
    engine_cc = as.numeric(engine_cc),
    vehicle_value = as.numeric(vehicle_value),
    age = as.numeric(age),
    gross_amount = as.numeric(gross_amount),
    net_amount = as.numeric(net_amount),
    years_licensed = as.numeric(years_licensed),
    licensing_age = as.numeric(licensing_age),
    vehicle_age = as.numeric(vehicle_age),
    reported_mileage = as.numeric(reported_mileage),
    exposure = as.numeric(exposure)
    
    # occurrence_date staying as Date
    # policy_id staying as character
  )

str(claims_severity) # All good.



###################################################################################################
# Exporting
###################################################################################################
saveRDS(claims_severity, "data/claims_severity.rds")
write.csv(policy, "data/claims.csv", row.names = FALSE)




