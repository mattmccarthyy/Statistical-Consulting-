#================================================================================================================================================

##Fairness Section

policy_frequency = readRDS(url("https://github.com/mattmccarthyy/Statistical-Consulting-/raw/refs/heads/main/data/processed/policy_frequency.rds"))
claims_severity = readRDS(url("https://github.com/mattmccarthyy/Statistical-Consulting-/raw/refs/heads/main/data/processed/claims_severity.rds"))
View(policy_frequency)

split_data(policy_frequency, "freq")
split_data(claims_severity, "sev")

library(splines)
library(MASS)

fair_nb = glm.nb(n_claims ~ ns(age, df = 6) + factor(primary_usage) + 
                   factor(vehicle_power) + factor(area) + factor(ncd_level) + 
                   factor(marital) + factor(occasional_commercial) + factor(employment_missing) + 
                   factor(body_type) + years_licensed + vehicle_age + 
                   factor(reported_mileage_missing) + factor(security_device) + offset(log(exposure)) +
                   ns(age,df=6):factor(primary_usage), 
                 data = freq_train, init.theta = 1.775461299, link = "log")

fair_gamma = glm(gross_amount ~ factor(vehicle_power) + factor(usage) + 
                   factor(area) + factor(fuel) + factor(overnight_parking) + 
                   ns(age, df = 6) + engine_cc + factor(security_device) + factor(occasional_commercial) + 
                   factor(reported_mileage_missing) + factor(ncd_level) + vehicle_age + 
                   factor(body_type) + factor(engine_cc_missing) + factor(vehicle_power):factor(usage) + 
                   factor(usage):engine_cc, family = Gamma(link = "log"), data = sev_train)

split_data(policy_frequency, "freq")
freq_train

split_data(claims_severity, "sev")
sev_train

sev_train$sev_pred = predict(gamma_model, type="response")
freq_train$freq_pred = predict(fair_nb, type="response")

#Aggregate severity predictions to policy level
library(dplyr)

sev_policy = sev_train %>%
  group_by(policy_id) %>%
  summarise(sev_pred = sum(sev_pred))

avg_sev = mean(sev_train$sev_pred)

freq_train = freq_train %>%
  left_join(sev_policy, by="policy_id") %>%
  mutate(sev_pred = ifelse(is.na(sev_pred), avg_sev, sev_pred))

#Compute Pure Premium
freq_train$pure_premium = freq_train$freq_pred*freq_train$sev_pred

#Actual Losses
actual_losses = sev_train %>%
  group_by(policy_id) %>%
  summarise(actual_loss = sum(gross_amount))

freq_train = freq_train %>%
  left_join(actual_losses, by = "policy_id") %>%
  mutate(actual_loss = ifelse(is.na(actual_loss), 0, actual_loss))

#DIR
avg_pp = freq_train %>%
  group_by(gender) %>%
  summarise(avg_pp = mean(pure_premium), .groups="drop")

DIR = avg_pp$avg_pp[avg_pp$gender == "M"] / avg_pp$avg_pp[avg_pp$gender =="F"]
DIR #1.07758

#Loss Ratio Parity
LR = freq_train %>%
  group_by(gender) %>%
  summarise(
    actual = mean(actual_loss),
    predicted = mean(pure_premium),
    LR = actual / predicted
  )
LR

#Calibration by Group
freq_train = freq_train %>%
  mutate(decile = ntile(pure_premium, 10))

calibration = freq_train %>%
  group_by(gender, decile) %>%
  summarise(
    actual = mean(actual_loss),
    predicted = mean(pure_premium),
    calibration = actual - predicted,
    .groups = "drop"
  )
calibration




