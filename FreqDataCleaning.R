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


policy_url <- "https://raw.githubusercontent.com/mattmccarthyy/Statistical-Consulting-/main/data/motor_policy_year_100k_2025-10-22.csv"
policy_raw <- read_csv(policy_url, show_col_types = FALSE)
policy <- policy_raw


# na_rate(): quick missingness scanner, gives a fast read on where our NAs are concentrated to guide repairs or feature pruning. 
na_rate <- function(df) sapply(df, function(x) mean(is.na(x))) |> sort(TRUE)

# Inital Checks
# head(policy)
str(policy)
# glimpse(policy)
# summary(policy)
# EDA Complete in previosu script

# NA Study
na_rate(policy)[1:6]*100 # percentage NA entries, ignoring avg claims for now



###################################################################################################
# Adding Missingness Flags to Columns Featuring NA's.
###################################################################################################
policy <- policy %>%
  mutate(
    employment_missing = is.na(employment),
    reported_mileage_missing = is.na(reported_mileage),
    engine_cc_missing = is.na(engine_cc),
  )


###################################################################################################
# Checking for Missingness Effects. 
###################################################################################################
# Engine_cc_missing check for any noticeable effect of missingness:
policy %>%
  group_by(engine_cc_missing) %>%
  summarise(
    avg_claims = mean(n_claims / exposure),
    avg_age = mean(age),
    n = n()
  )
# Claim frequency ~1.5% higher for cases where engine_cc was NA.
# Quick significance test (assumping Poisson)
SE = sqrt(0.275/2093 + 0.271/351877)
Diff = 0.275 - 0.271
z = Diff/SE
p_value <- 2*pnorm(-abs(z));
# p_value # Not statistically significant (p > 0.7)
# 1.5% difference likely just random noise. 
# Flag won't hurt to keep but won't help to keep. Keeping for now, unlikely to be used in the model. May remove later. 


# Check for any noticeable effect of missingness:
policy %>%
  group_by(employment_missing) %>%
  summarise(
    avg_claims = mean(n_claims / exposure),
    avg_age = mean(age),
    n = n()
  )
# ~12.9% higher claim rate when employment is missing
# Ages near identical (45.3 vs. 45.1) => not just an age effect. 
# Checking statistical significance (assuming Poisson)
SE = sqrt(0.271/350415 + 0.306/3555)
Diff = 0.306 - 0.271
z = Diff/SE
p_value = 2*pnorm(-z); p_value
# Statistically significant (p<0.001).
# Missing employment is MNAR - the missingness is informative. Possible reasons:
# 1). Unemployment Stigma: People may avoid reporting unemployment.
# 2). Irregular employment: Gig workers, self-employed with variable income.
# 3). Behavioural correlation: Those who skip employment questions might be riskier in general. 


# Checking for systematic differences in reported_mileage cases which were NA.
policy %>%
  group_by(reported_mileage_missing) %>%
  summarise(
    avg_claims = mean(n_claims / exposure),
    avg_age = mean(age),
    n = n()
  )
# 13.7% higher claim rate when mileage is missing. Suggests MNAR*, the missingness itself is informative. (*Missingness Not At Random).
# Possible explanations:
# 1). Strategic non-reporting: High-mileage drivers omit data to avoid higher premiums.
# 2). Behavioural correlation: People who don't report mileage might also be riskier drivers in general.
# 3). Usage patterns: Missing data may cluster in certain high-risk usage types.

# Important notes:
# Ages are nearly identical (45.3 vs. 45.5), so this is not just an age effect. 
# Sample size is so small, that despite the higher risk, the effect will be small.
# Must test with and without the effect, need to price the lower risk against including extra predictor.

# After modelling, must check coefficient of reported_mileage_missing:
# Positive & significant => MNAR (strategic non-reporting). 
# Not significant => Suggests MCAR* (random data issues)



###################################################################################################
# Deduplication. This bit is very hard. I regret doing this section. 
###################################################################################################
# Check if duplicate records have SAME or DIFFERENT n_claims
duplicate_claims_check <- policy %>%
  group_by(policy_id, cal_year) %>%
  filter(n() > 1) %>%
  summarise(
    n_records = n(),
    n_claims_values = list(n_claims),
    all_same_claims = n_distinct(n_claims) == 1,
    total_if_sum = sum(n_claims),
    value_if_max = max(n_claims),
    .groups = "drop"
  )

# Checking how many duplicate groups have identical n_claims?
table(duplicate_claims_check$all_same_claims)
# 67% have identical n_claims =? max() = sum(), doesn't matter which we use.
# 33% differ, problematic subset we have to deal with carefully.
# These are not independent claims, they're the same policy at different times.
# Using max() for n_claims, sum_net, sum_gross and exposure, pattern strongly suggests that
# duplicates are incomplete/stale records rather than genuinely independent claims, using max() is
# ethically sound. 

# Look at examples
head(duplicate_claims_check, 20)


# Performing de-duplication
policy <- policy %>%
  mutate(
    employment_missing = is.na(employment),
    reported_mileage_missing = is.na(reported_mileage),
    engine_cc_missing = is.na(engine_cc)
  )

cat("\nBefore deduplication:", nrow(policy), "rows\n")


###################################################################################################
# Deduplication
###################################################################################################
policy <- policy %>%
  group_by(policy_id, cal_year) %>%
  arrange(desc(exposure), .by_group = TRUE) %>%  # Added .by_group = TRUE for clarity. Explained in document. 
  summarise(
    # Categorical variables: take entry with largest exposure (most representative)
    gender = first(gender),
    marital = first(marital),
    employment = first(employment),
    occupation = first(occupation),
    area = first(area),
    province = first(province),
    body_type = first(body_type),
    fuel = first(fuel),
    transmission = first(transmission),
    primary_usage = first(primary_usage),
    overnight_parking = first(overnight_parking),
    vehicle_power = first(vehicle_power),
    
    # Logical variables: TRUE if any record is TRUE
    occasional_commercial = any(occasional_commercial),
    security_device = any(security_device),
    employment_missing = any(employment_missing),
    reported_mileage_missing = any(reported_mileage_missing),
    engine_cc_missing = any(engine_cc_missing),
    
    # Numeric variables: exposure-weighted mean
    age = sum(age * exposure) / sum(exposure),
    years_licensed = sum(years_licensed * exposure) / sum(exposure),
    engine_cc = sum(engine_cc * exposure, na.rm = TRUE) / sum(exposure[!is.na(engine_cc)]),
    vehicle_age = sum(vehicle_age * exposure) / sum(exposure),
    vehicle_value = sum(vehicle_value * exposure) / sum(exposure),
    reported_mileage = sum(reported_mileage * exposure, na.rm = TRUE) / sum(exposure[!is.na(reported_mileage)]),
    ncd_level = round(sum(ncd_level * exposure) / sum(exposure)),
    num_drivers = round(sum(num_drivers * exposure) / sum(exposure)),
    
    # Claims and exposure: max (most complete record, avoid double-counting)
    n_claims = max(n_claims),
    sum_net = max(sum_net),
    sum_gross = max(sum_gross),
    exposure = max(exposure),
    
    .groups = "drop"
  ) %>%
  mutate(
    avg_net = ifelse(n_claims > 0, sum_net / n_claims, 0),
    avg_gross = ifelse(n_claims > 0, sum_gross / n_claims, 0)
  )
353970 - 347937
# Removed 6033 rows.

# This created floating-point precision errors in licensing_age := age - years_licensed, causing some entries to be 16.999 instead of 17. 
# Round age-related variables to 2 decimal places (reasonable precision)
policy <- policy %>%
  mutate(
    age = round(age, 2),
    years_licensed = round(years_licensed, 2),
    licensing_age = age - years_licensed
  )
# This became an issue in data-quality checks, fixing here. 



###################################################################################################
# Addressing engine_cc NA's
###################################################################################################
# Missingness flag created at beginning of script. 

# If there are other years with this policy id, take value from the first one of them with engine_cc entry. 
policy <- policy %>%
  group_by(policy_id) %>% # Groups data by policy id's (individual drivers), if a policy id appears in multiple years, all rows are treated as a group. 
  mutate(engine_cc = ifelse(is.na(engine_cc), 
                            first(na.omit(engine_cc)), # If engine_cc is NA, replaces it with the first non-missing value in the group.
                            engine_cc)) %>% # If all values are NA, leaves them as NA, this explains the 81 remaining. 
  ungroup()
summary(policy$engine_cc)[7] # Only 81 NA's remain.

# Grouped median imputation
policy <- policy %>%
  group_by(body_type, vehicle_power, transmission, fuel) %>%
  mutate(engine_cc = ifelse(is.na(engine_cc), 
                            median(engine_cc, na.rm = TRUE), 
                            engine_cc)) %>%
  ungroup()

# Check if any NAs remain (due to sparse groups)
summary(policy$engine_cc)[7] # None left.



###################################################################################################
# Addressing employment NA's
###################################################################################################
# Missingness flag created at beginning of script.

# Convert NA's to "Unknown".
policy$employment[is.na(policy$employment)] = "Unknown"
table(policy$employment) # All NA's have been flagged and converted to type "Unknown"



###################################################################################################
# Addressing reported_mileage NA's
###################################################################################################
# Missingness flag created at beginning of script.

# Imputing with within-driver Median (ethically conservative) - see notes for explanation.
policy <- policy %>%
  group_by(policy_id) %>%
  mutate(reported_mileage = ifelse(is.na(reported_mileage), 
                                   median(reported_mileage, na.rm = TRUE), 
                                   reported_mileage)) %>%
  ungroup()

# For remaining NA's use MEDIAN
policy <- policy %>%
  mutate(reported_mileage = ifelse(is.na(reported_mileage), 
                                   median(reported_mileage, na.rm = TRUE), 
                                   reported_mileage))
# Almost all NA's removed from dataset. Still have to do claims related ones. 
summary(policy$reported_mileage) # No NA's.



###################################################################################################
# Ensuring exposure in (0, 1] after aggregation
###################################################################################################
summary(policy$exposure) # All in allowed range



###################################################################################################
# Investigating booting low exposure entries
###################################################################################################
# Categorising polcies by exposure and examining claim frequencies.
exposure_bands <- policy %>%
  mutate(
    exposure_band = case_when(
      exposure < 0.1 ~ "< 1.2 months",
      exposure < 0.25 ~ "1.2-3 months",
      exposure < 0.5 ~ "3-6 months",
      exposure < 0.75 ~ "6-9 months",
      TRUE ~ "9-12 months"
    ),
    # Convert to factor with correct ordering, this is just so the table displays correctly. 
    exposure_band = factor(exposure_band, 
                           levels = c("< 1.2 months", "1.2-3 months", "3-6 months", "6-9 months", "9-12 months"))
  ) %>%
  group_by(exposure_band) %>%
  summarise(
    n_policies = n(),
    pct_of_total = n() / nrow(policy) * 100,
    total_exposure = sum(exposure),
    n_claims = sum(n_claims),
    avg_frequency = sum(n_claims) / sum(exposure),
    .groups = "drop"
  )

print(exposure_bands)
# The shortest policies (<1.2 months) have the lowest frequency (0.248) but may indicate data-quality issues.
# Only removing 9,693 policy years (2.79% of data), retaining 97.21% of data for modelling by removing these.
# If we left them, we would be slighlty underestimating risk, optimal to remove them. 

# Keeping the 1.2-3 month segment. They feature the highest average frequency (0.293 vs. 0.272 baseline => 7.7% increase)
# Represents real risk pattern (likely mid-term cancellations of high-risk churners)
# Contains 12,841 policies (5.13%), enough data for the model to learn this effect. 
# Exposure offset should properly account for shorter duration. 

# Applying filter to remove the non-representative low-exposure entries.
policy <- policy %>% filter(exposure >= 0.1)



###################################################################################################
# Response Variable Checks
###################################################################################################
# Ensure n_claims is non-negative integer
summary(policy$n_claims)
cat("Any negative claims:", sum(policy$n_claims < 0), "\n")
cat("Any non-integer claims:", sum(policy$n_claims != round(policy$n_claims)), "\n")

# Claim frequency distribution
table(policy$n_claims)
round(sum(policy$n_claims) / sum(policy$exposure), 4) # Overall frequency rate.



###################################################################################################
# Data Quality Checks
###################################################################################################

# Age consistency: years_licensed should never exceed age
age_issues <- policy %>%
  filter(years_licensed > age)
nrow(age_issues) # No. of polcies where years_licensed > age. 0 as expected.

# Minimum driving age check (typically 17 in Ireland)
sum(policy$age - policy$years_licensed < 17) # Number of drivers licensed before age 17. 0 as expected. 
summary(policy$age - policy$years_licensed)

# Vehicle value checks
sum(policy$vehicle_value > 100000) # 617 vehicles worth more than €100,000. Large amount but likely represenative. We can refine this later. 
summary(policy$vehicle_value) # None <0, all seems reasonable

# NCD level range
table(policy$ncd_level)
sum(table(policy$ncd_level)) # 338,244, accounts for all policies as expected. 
summary(policy$ncd_level) # All reasonable.



###################################################################################################
# Rare Category Analysis (for potential combining)
###################################################################################################

categorical_variables <- c("gender", "marital", "employment", "occupation", "area", "province", "body_type", "fuel", "transmission",
                           "primary_usage", "overnight_parking", "vehicle_power")

for(var in categorical_variables) {
  tab <- table(policy[[var]])
  rare_categories <- tab[tab < nrow(policy) * 0.01]
  if(length(rare_categories) > 0) {
    cat("\n", var, ":\n", sep="")
    print(rare_categories)
  }
}
# Only 2 out of 12 are rare categories. Data is well-distributed.

# Deciding what to do with the rare category "occupation == Pop Star".
policy %>%
  filter(occupation == "Pop Star") %>%
  summarise(
    n = n(),
    frequency = sum(n_claims) / sum(exposure)
  )
policy %>%
  filter(occupation == "Other") %>%
  summarise(
    n = n(),
    frequency = sum(n_claims) / sum(exposure)
  )
policy %>%
  summarise(
    n = n(),
    frequency = sum(n_claims) / sum(exposure)
  )
# Test if difference is statistically significant
SE <- sqrt(0.284/1320 + 0.251/13596)
z <- (0.284 - 0.251) / SE; z
p_value <- 2 * pnorm(-abs(z)); p_value
# Difference is statistically significant (p<0.05)

# Pop-stars exhibit higher risk (13.9%), but only make up 0.39% of data.
# Their effect is statistically significant (from above), but has minimal impact. 
# Aiming for parsimony, we can collapse this into "Other", which is already an existing category.
# Other does have lower expected frequency, but saving the extra parameter is likely profitable. 
# Concern of adverse selection (underpricing attracting more Pop-Stars), however, rarity of this job category makes this unlikely.
# By collapsing, we are preferring parsimony over precision. 
# Collapsing "Pop-Star" to "Other"
policy <- policy %>%
  mutate(occupation = ifelse(occupation == "Pop Star", "Other", occupation))

# Deciding what to do with the rare category "fuel == EV".
# Check EV claim frequency
policy %>%
  group_by(fuel) %>%
  summarise(
    n = n(),
    frequency = sum(n_claims) / sum(exposure),
    avg_vehicle_value = mean(vehicle_value)
  ) %>%
  arrange(desc(frequency))
# EV's do exhibit higher risk, but regardless:
# EV's exhibit different risk profiles (different acceleration, repair costs, driver demographics)
# Growing market segment (important for future pricing)
# 1,693 is sufficient for GLM



###################################################################################################
# Cleaning global environment
###################################################################################################
rm(list = setdiff(ls(), c("policy", "policy_raw")))



###################################################################################################
# Dataset Prep before Freq. Modelling (Saving modelling decisions for that script, just formatting columns)
###################################################################################################

# Make "obvious types" explicit integers
policy <- policy %>%
  mutate(
    cal_year = as.integer(cal_year),
    n_claims = as.integer(n_claims),
    ncd_level = as.integer(ncd_level),
    num_drivers = as.integer(num_drivers)
  )

# Make character variables factors.
policy <- policy %>%
  mutate(
    gender = factor(gender),
    marital = factor(marital),
    employment = factor(employment),
    occupation = factor(occupation),
    area = factor(area),
    province = factor(province),
    body_type = factor(body_type),
    fuel = factor(fuel),
    transmission = factor(transmission),
    primary_usage = factor(primary_usage),
    overnight_parking = factor(overnight_parking),
    security_device = factor(security_device),
    vehicle_power = factor(vehicle_power)
  )

# Make logical T/F variables numeric 0/1 (1 == TRUE, 0 == FALSE)
policy <- policy %>%
  mutate(
    occasional_commercial = as.integer(occasional_commercial), # logical => 0/1 (FALSE = 0, TRUE = 1)
    security_device = as.integer(security_device == "TRUE"), # factor("FALSE","TRUE") => 0/1 via explicit comparison
    employment_missing = as.integer(employment_missing),
    reported_mileage_missing = as.integer(reported_mileage_missing),
    engine_cc_missing = as.integer(engine_cc_missing),
  )



###################################################################################################
# Final Dataset Verification
###################################################################################################

nrow(policy) # Total Observations
min(policy$cal_year) # Date range
n_distinct(policy$policy_id) # Number of unique policies

# Check for any remaining NAs
colSums(is.na(policy)) # All dealt with.

# Verify no duplicates
sum(duplicated(policy[c("policy_id", "cal_year")])) # 0 duplicates

# At this point, our data is clean, and ready for modelling or using in the severity feature engineering.
# Just to make it easier for people not involved in cleaning the data, removing columns which shouldn't be included
# in frequency modelling before writing table to Git.



###################################################################################################
# Final Frequency Specific Dataset Modification
###################################################################################################
policy_frequency <- policy %>%
  select(-sum_net, -sum_gross, -avg_net, -avg_gross)
# Don't want leakage, these can't be used in frequency models.



###################################################################################################
# Exporting
###################################################################################################
saveRDS(policy_frequency, "data/policy_frequency.rds")
