###################################################################################################################################################################################
#==================================================================================================================================================================================

##Quasi Poisson GLM##

#==========================================================================================================================================================================================

##Split Data

split_data(policy_frequency)

#===========================================================================================================================================================================================

##Selection Loops

#Load in MASS and splines
library(MASS)
library(splines)

#Null Model - intercept and offset of log(exposure)
null_qp = glm(n_claims~1 + offset(log(exposure)), family=quasipoisson(link="log"), data=train)

#Null Model Summary
summary(null_qp) #AIC=NA

#Full model - all predictors using the spline for age as determined above
full_qp = glm(
  n_claims~ns(age, df=6) + factor(gender) + factor(marital) + factor(employment) + 
    factor(occupation) + factor(area) + factor(province) + factor(body_type) + 
    factor(fuel) + factor(transmission) + factor(primary_usage) + factor(overnight_parking) +
    factor(vehicle_power) + factor(occasional_commercial) + factor(security_device) + factor(employment_missing) +
    factor(reported_mileage_missing) + factor(engine_cc_missing) + years_licensed + engine_cc + vehicle_age + 
    vehicle_value + reported_mileage + factor(ncd_level) + factor(num_drivers) + licensing_age + offset(log(exposure)), family=quasipoisson(link="log"),
  data=train)

#Full Model Summary
summary(full_qp) #AIC=NA

#Forward Selection Model
forward_qp = stepAIC(
  object = null_qp, 
  scope = list(lower = formula(null_qp), upper = formula(full_qp)), 
  direction = "forward", 
  trace = TRUE  #Shows progress of variables included in each iteration
)

#Forward Selection Model Summary
summary(forward_qp) ##AIC not defined

#==========================================================================================================================

