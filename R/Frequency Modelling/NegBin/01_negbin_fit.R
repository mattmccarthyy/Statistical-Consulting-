###################################################################################################################################################################################
#==================================================================================================================================================================================

##Negative Binomial GLM##

#==========================================================================================================================================================================================

##Split Data

split_data(policy_frequency)

#===========================================================================================================================================================================================

##Selection Loops

#Load in MASS and splines
library(MASS)
library(splines)

#Null Model - intercept and offset of log(exposure)
null_nb = glm.nb(n_claims~1 + offset(log(exposure)), link="log", data=train)

#Null Model Summary
summary(null_nb) #AIC=240368

#Full model - all predictors using the spline for age as determined above
full_nb = glm.nb(
  n_claims~ns(age, df=6) + factor(gender) + factor(marital) + factor(employment) + 
    factor(occupation) + factor(area) + factor(province) + factor(body_type) + 
    factor(fuel) + factor(transmission) + factor(primary_usage) + factor(overnight_parking) +
    factor(vehicle_power) + factor(occasional_commercial) + factor(security_device) + factor(employment_missing) +
    factor(reported_mileage_missing) + factor(engine_cc_missing) + years_licensed + engine_cc + vehicle_age + 
    vehicle_value + reported_mileage + factor(ncd_level) + factor(num_drivers) + licensing_age + offset(log(exposure)), link="log",
  data=train)

#Full Model Summary
summary(full_nb) #AIC=234354
BIC(full_nb)

#Forward Selection Model
forward_nb = stepAIC(
  object = null_nb, 
  scope = list(lower = formula(null_nb), upper = formula(full_nb)), 
  direction = "forward", 
  trace = TRUE  #Shows progress of variables included in each iteration
)

#Forward Selection Model Summary
summary(forward_nb) #AIC=234317

##Only ran forward selection as loop took a very long time to run

best_nb = forward_nb

best_nb = glm.nb(formula = n_claims ~ ns(age, df = 6) + factor(primary_usage) + 
                   factor(vehicle_power) + factor(area) + factor(ncd_level) + 
                   factor(marital) + factor(occasional_commercial) + factor(employment_missing) + 
                   factor(body_type) + factor(gender) + years_licensed + vehicle_age + 
                   factor(reported_mileage_missing) + factor(security_device) + 
                   offset(log(exposure)), data = train, init.theta = 1.643789121, 
                 link = "log")
summary(best_nb)
BIC(best_nb)
#===================================================================================================================================================

##Interactions

#Test reduction in AIC for each 2 way combination of predictors in best_poisson

#Define predictors 
predictors_nb = c("vehicle_power",
                  "primary_usage",
                  "area",
                  "age",
                  "security_device",
                  "occasional_commercial",
                  "ncd_level",
                  "vehicle_age",
                  "body_type",
                  "reported_mileage_missing",
                  "marital",
                  "gender",
                  "employment_missing",
                  "years_licensed")

#Baseline model 
base_nb = glm.nb(n_claims ~ 1 + offset(log(exposure)), data = train, link="log")

#2 way combinations
com_nb = combn(predictors_nb, 2, simplify = FALSE)

#Function to fit model and compute AIC reduction
test_com_nb = function(vars) {
  formula = as.formula(paste("n_claims ~", paste(vars, collapse = " + ")))
  model = glm.nb(formula, data = train, link="log")
  aic = AIC(model)
  reduction = AIC(base_nb) - aic
  data.frame(var1 = vars[1],
             var2 = vars[2],
             AIC = aic,
             AIC_reduction = reduction)
}

#Apply across all combinations
results_nb = do.call(rbind, lapply(com_nb, test_com_nb))

#Sort by largest AIC reduction
results_sorted_nb = results_nb[order(-results_nb$AIC_reduction), ]
print(results_sorted_nb)

#Use selection loops to determine which interactions to include

#Define candidate interactions 
candidates_nb = paste(
  "ns(age, df=6):factor(marital)",
  "factor(marital):years_licensed",
  "factor(primary_usage):ns(age,df=6)",
  "factor(vehicle_age):ns(age, df=6)",
  "factor(vehicle_power):ns(age, df=6)",
  "factor(area):ns(age, df=6)",
  "factor(vehicle_age):years_licensed",
  "factor(body_type):ns(age, df=6)",
  "factor(gender):ns(age,df=6)",
  sep = " + "
)

scope_formula_nb = as.formula(paste(". ~ . +", candidates_nb))

#Forward Selection
forward_nb_int = stepAIC(best_nb, scope = list(upper = update(best_nb, scope_formula_nb)), 
                              direction = "forward", trace = TRUE)
summary(forward_nb_int) #AIC=233387

nb_model = forward_nb_int

#LRT to see if interactions improve the model
anova(best_nb, nb_model, test="LRT") #Interactions 

#=================================================================================================================================

##Final Negative Binomial GLM

nb_model = glm.nb(formula = n_claims ~ ns(age, df = 6) + factor(primary_usage) + 
                    factor(vehicle_power) + factor(area) + factor(ncd_level) + 
                    factor(marital) + factor(occasional_commercial) + factor(employment_missing) + 
                    factor(body_type) + factor(gender) + years_licensed + vehicle_age + 
                    factor(reported_mileage_missing) + factor(security_device) + 
                    ns(age, df = 6):factor(gender) + ns(age, df = 6):factor(marital) + 
                    ns(age, df = 6):factor(primary_usage) + offset(log(exposure)), 
                  data = train, init.theta = 1.775461299, link = "log")
summary(nb_model)
BIC(nb_model)

#==========================================================================================================================================

##Multicollinearity

library(car)
vif(nb_model) #There are aliased coefficients in model - need to resolve
alias(nb_model)

#===============================================================================================================================================

##Resolve aliased coefficients

#Test nb model with no interactions
nb_model_no_int = glm.nb(formula = n_claims ~ ns(age, df = 6) + factor(primary_usage) + 
                    factor(vehicle_power) + factor(area) + factor(ncd_level) + 
                    factor(marital) + factor(occasional_commercial) + factor(employment_missing) + 
                    factor(body_type) + factor(gender) + years_licensed + vehicle_age + 
                    factor(reported_mileage_missing) + factor(security_device) + offset(log(exposure)), 
                  data = train, init.theta = 1.775461299, link = "log")
vif(nb_model_no_int)
alias(nb_model_no_int) #No aliasing

#Add in interactions one by one
#age:gender
#age:marital
#age:primary usage

nb_model_int1 = glm.nb(formula = n_claims ~ ns(age, df = 6) + factor(primary_usage) + 
                           factor(vehicle_power) + factor(area) + factor(ncd_level) + 
                           factor(marital) + factor(occasional_commercial) + factor(employment_missing) + 
                           factor(body_type) + factor(gender) + years_licensed + vehicle_age + 
                           factor(reported_mileage_missing) + factor(security_device) + offset(log(exposure)) +
                            ns(age,df=6):factor(gender), 
                         data = train, init.theta = 1.775461299, link = "log")
vif(nb_model_int1)
alias(nb_model_int1) #No aliasing

nb_model_int2 = glm.nb(formula = n_claims ~ ns(age, df = 6) + factor(primary_usage) + 
                         factor(vehicle_power) + factor(area) + factor(ncd_level) + 
                         factor(marital) + factor(occasional_commercial) + factor(employment_missing) + 
                         factor(body_type) + factor(gender) + years_licensed + vehicle_age + 
                         factor(reported_mileage_missing) + factor(security_device) + offset(log(exposure)) +
                         ns(age,df=6):factor(marital), 
                       data = train, init.theta = 1.775461299, link = "log")
vif(nb_model_int2)
alias(nb_model_int2) #Aliased coefficients - ignore interaction

nb_model_int3 = glm.nb(formula = n_claims ~ ns(age, df = 6) + factor(primary_usage) + 
                         factor(vehicle_power) + factor(area) + factor(ncd_level) + 
                         factor(marital) + factor(occasional_commercial) + factor(employment_missing) + 
                         factor(body_type) + factor(gender) + years_licensed + vehicle_age + 
                         factor(reported_mileage_missing) + factor(security_device) + offset(log(exposure)) +
                         ns(age,df=6):factor(primary_usage), 
                       data = train, init.theta = 1.775461299, link = "log")
vif(nb_model_int3)
alias(nb_model_int3) #No aliasing

#Combine interactions
final_nb = glm.nb(formula = n_claims ~ ns(age, df = 6) + factor(primary_usage) + 
                    factor(vehicle_power) + factor(area) + factor(ncd_level) + 
                    factor(marital) + factor(occasional_commercial) + factor(employment_missing) + 
                    factor(body_type) + factor(gender) + years_licensed + vehicle_age + 
                    factor(reported_mileage_missing) + factor(security_device) + offset(log(exposure)) +
                    ns(age,df=6):factor(primary_usage) + ns(age,df=6):factor(gender), 
                  data = train, init.theta = 1.775461299, link = "log")
vif(final_nb) #Years_licensed has high gvif - may need to revisit
alias(final_nb) #No aliased coefficients

#=====================================================================================================================================

##Final NB Model (No aliasing)
final_nb = glm.nb(formula = n_claims ~ ns(age, df = 6) + factor(primary_usage) + 
                    factor(vehicle_power) + factor(area) + factor(ncd_level) + 
                    factor(marital) + factor(occasional_commercial) + factor(employment_missing) + 
                    factor(body_type) + factor(gender) + years_licensed + vehicle_age + 
                    factor(reported_mileage_missing) + factor(security_device) + offset(log(exposure)) +
                    ns(age,df=6):factor(primary_usage) + ns(age,df=6):factor(gender), 
                  data = train, init.theta = 1.775461299, link = "log")
summary(final_nb) #AIC=233395

anova(best_nb, final_nb, test="LRT")
