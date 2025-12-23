###################################################################################################################################################################################
#==================================================================================================================================================================================

##Hurdle Model##

##Load in library
library(pscl)
library(MASS)
library(splines)

#==================================================================================================================================================================

##Selection Loops

#Null Model - intercept and offset of log(exposure)
null_hurdle = hurdle(n_claims~1, dist="poisson", zero.dist="binomial", offset=log(exposure), data=train)

#Null Model AIC
AIC(null_hurdle) #AIC=244749.7

#Full model - all predictors using the spline for age as previously determined 
full_hurdle = hurdle(
  n_claims~age_spline + factor(gender) + factor(marital) + factor(employment) + 
    factor(occupation) + factor(area) + factor(province) + factor(body_type) + 
    factor(fuel) + factor(transmission) + factor(primary_usage) + factor(overnight_parking) +
    factor(vehicle_power) + factor(occasional_commercial) + factor(security_device) + factor(employment_missing) +
    factor(reported_mileage_missing) + factor(engine_cc_missing) + years_licensed + engine_cc + vehicle_age + 
    vehicle_value + reported_mileage + factor(ncd_level) + factor(num_drivers) + licensing_age, dist="poisson", zero.dist="binomial", offset=log(exposure),
  data=train)
AIC(full_hurdle)

#===============================================================================================================

##Note: Selection loops don't work in the same way as for regular glm
##Apply poisson hurdle glm to poisson_model
##Apply negative binomial hurdle glm to nb_model

#=================================================================================================================================
##Poisson Hurdle Model

poisson_model = glm(n_claims ~ ns(age, df = 6) + factor(primary_usage) + 
                      factor(vehicle_power) + factor(area) + factor(ncd_level) + 
                      factor(marital) + factor(gender) + factor(occasional_commercial) + 
                      factor(employment_missing) + factor(body_type) + years_licensed + 
                      vehicle_age + factor(reported_mileage_missing) + factor(security_device) + 
                      ns(age, df = 6):factor(gender) + ns(age, df = 6):factor(marital) + 
                      factor(primary_usage):years_licensed + ns(age, df = 6):factor(area), 
                    family = poisson(link = "log"), data = train, offset = log(exposure))
summary(poisson_model) #AIC=235166

#Need to define spline prior to model for hurdle model
#Hurdle model not working with the interaction terms in the poisson model due to the risk of multicollinearity
#Apply to model with no interactions

train$age_spline = ns(train$age, df = 6)
hurdle_poisson_mod = hurdle(n_claims ~ age_spline + factor(primary_usage) + 
                              factor(vehicle_power) + factor(area) + factor(ncd_level) + 
                              factor(marital) + factor(gender) + factor(occasional_commercial) + 
                              factor(employment_missing) + factor(body_type) + years_licensed + 
                              vehicle_age + factor(reported_mileage_missing) + factor(security_device), 
                              dist="poisson", zero.dist="binomial", offset=log(exposure), data=train)
summary(hurdle_poisson_mod) #238585.7

hurdle_final_poisson = hurdle(n_claims ~ age_spline + factor(primary_usage) + 
                                factor(vehicle_power) + factor(area) + factor(ncd_level) + 
                                factor(marital) + factor(gender) + factor(occasional_commercial) + 
                                factor(employment_missing) + factor(body_type) + years_licensed + 
                                vehicle_age + factor(reported_mileage_missing) + factor(security_device) +
                                age_spline:factor(gender) + factor(primary_usage):years_licensed + age_spline:factor(area), 
                              dist="poisson", zero.dist="binomial", offset=log(exposure), data=train)
AIC(hurdle_final_poisson)

install.packages("lmtest")
library(lmtest)

lrtest(hurdle_poisson_mod, hurdle_final_poisson)
#==========================================================================================================================

##Negative Binomial Hurdle Model

best_nb = glm.nb(n_claims ~ ns(age, df = 6) + factor(primary_usage) + 
                    factor(vehicle_power) + factor(area) + factor(ncd_level) + 
                    factor(marital) + factor(occasional_commercial) + factor(employment_missing) + 
                    factor(body_type) + factor(gender) + years_licensed + vehicle_age + 
                    factor(reported_mileage_missing) + factor(security_device) + 
                    ns(age, df = 6):factor(gender) + ns(age, df = 6):factor(marital) + 
                    ns(age, df = 6):factor(primary_usage) + offset(log(exposure)), 
                  data = train, init.theta = 1.775461299, link = "log")
AIC(best_nb)#AIC=233386.7

hurdle_nb_mod = hurdle(n_claims ~ ns(age, df = 6) + factor(primary_usage) + 
                         factor(vehicle_power) + factor(area) + factor(ncd_level) + 
                         factor(marital) + factor(occasional_commercial) + factor(employment_missing) + 
                         factor(body_type) + factor(gender) + years_licensed + vehicle_age + 
                         factor(reported_mileage_missing) + factor(security_device) + 
                         offset(log(exposure)), data = train, dist="negbin", zero.dist="binomial")
AIC(hurdle_nb_mod) #AIC=234336

final_nb = glm.nb(n_claims ~ ns(age, df = 6) + factor(primary_usage) + 
                    factor(vehicle_power) + factor(area) + factor(ncd_level) + 
                    factor(marital) + factor(occasional_commercial) + factor(employment_missing) + 
                    factor(body_type) + factor(gender) + years_licensed + vehicle_age + 
                    factor(reported_mileage_missing) + factor(security_device) + offset(log(exposure)) +
                    ns(age,df=6):factor(primary_usage) + ns(age,df=6):factor(gender), 
                  data = train, init.theta = 1.775461299, link = "log")
summary(final_nb)

hurdle_final_nb = hurdle(n_claims ~ ns(age, df = 6) + factor(primary_usage) + 
                           factor(vehicle_power) + factor(area) + factor(ncd_level) + 
                           factor(marital) + factor(occasional_commercial) + factor(employment_missing) + 
                           factor(body_type) + factor(gender) + years_licensed + vehicle_age + 
                           factor(reported_mileage_missing) + factor(security_device) + offset(log(exposure)) +
                           ns(age,df=6):factor(primary_usage) + ns(age,df=6):factor(gender), 
                         data = train, dist="negbin", zero.dist="binomial")
AIC(hurdle_final_nb)

#LRT
lrtest(hurdle_nb_mod, hurdle_final_nb)
#==============================================================================================================================
