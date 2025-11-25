###################################################################################################################################################################################
#==================================================================================================================================================================================

##Severity GLM##
library(splines)
library(ggplot2)

##Load in cleaned data set 
claims_severity = readRDS(url("https://github.com/mattmccarthyy/Statistical-Consulting-/raw/refs/heads/main/data/processed/claims_severity.rds"))
View(claims_severity)

#============================================================================================================================================================================================

##Split Data
split_data(claims_severity)

#=====================================================================================================

##Selection Loops - Use same spline for age

#Load in library
library(MASS)

#Null model
null_model_s = glm(net_amount ~1, family=Gamma(link="log"), data=train)

#Null model summary
summary(null_model_s) #AIC=987075

#Full model
full_model_s = glm(
  net_amount~factor(usage)+factor(occasional_commercial)+factor(area)+factor(province)+
    factor(overnight_parking)+factor(security_device)+factor(body_type)+ factor(vehicle_power)+
    factor(fuel)+factor(transmission)+engine_cc+vehicle_value+factor(gender)+factor(ncd_level)+years_licensed+
    licensing_age+vehicle_age+reported_mileage+num_drivers+factor(employment_missing)+
    factor(reported_mileage_missing)+factor(engine_cc_missing)+ns(age, df=6),
  family=Gamma(link="log"), data=train
)

#Full model summary
summary(full_model_s) #AIC=983846

#Forward selection loop
forward_model_s = stepAIC(
  object = null_model_s, 
  scope = list(lower = formula(null_model_s), upper = formula(full_model_s)), 
  direction = "forward", 
  trace = TRUE  #Shows progress of variables included in each iteration
)

#Forward selection model summary
summary(forward_model_s) #AIC=983834

#Backward elimination loop
backward_model_s = stepAIC(
  object = full_model_s,
  direction = "backward", 
  trace = TRUE  #Shows progress of variables included in each iteration          
)

#Backward elimination model summary
summary(backward_model_s) #AIC=983834

#Bidirectional stepwise selection loop
stepwise_model_s = stepAIC(
  object = full_model_s,
  direction = "both",   #Allows both forward and backward selection
  trace = TRUE         
)

#Stepwise model summary
summary(stepwise_model_s) #AIC=983834

#====================================================================================================================================

##Interaction Terms
best_model_s = forward_model_s

#Save so loops don't have to be re-run
best_model_s = glm(net_amount ~ factor(vehicle_power) + factor(usage) + 
                     factor(area) + factor(fuel) + factor(overnight_parking) + 
                     ns(age, df = 6) + engine_cc + factor(security_device) + factor(occasional_commercial) + 
                     factor(ncd_level) + vehicle_age + factor(body_type) + factor(reported_mileage_missing) + 
                     factor(engine_cc_missing), family = Gamma(link = "log"), 
                   data = train)

#Test reduction in AIC for each 2 way combination of predictors in best model

#Define predictors
predictors = c("vehicle_power",
                "usage",
                "area",
                "fuel",
                "overnight_parking",
                "age",
                "engine_cc",
                "security_device",
                "occasional_commercial",
                "ncd_level",
                "vehicle_age",
                "body_type",
                "engine_cc_missing",
                "reported_mileage_missing")

#Baseline model 
base_model = glm(net_amount ~ 1, data = train, family = Gamma(link="log"))

#2-way combinations
combos = combn(predictors, 2, simplify = FALSE)

#Function to fit model and compute AIC reduction
test_combo = function(vars) {
  formula = as.formula(paste("net_amount ~", paste(vars, collapse = " + ")))
  model = glm(formula, data = train, family = Gamma(link="log"))
  aic = AIC(model)
  reduction = AIC(base_model) - aic
  data.frame(var1 = vars[1],
             var2 = vars[2],
             AIC = aic,
             AIC_reduction = reduction)
}

#Apply across all combinations
results = do.call(rbind, lapply(combos, test_combo))

#Sort by largest AIC reduction
results_sorted = results[order(-results$AIC_reduction), ]
print(results_sorted) #Vehicle power appears in all of the top 13 interaction terms

#Plot interactions
top_pairs = results_sorted

#Function to fit interaction model and plot
plot_interaction = function(var1, var2, data) {
  #Fit model with interaction
  formula = as.formula(paste("net_amount ~", var1, "*", var2))
  model = glm(formula, data = train, family = Gamma(link="log"))
  
  #Build grid of predictor values
  grid = expand.grid(
    var1 = unique(data[[var1]]),
    var2 = unique(data[[var2]])
  )
  names(grid) = c(var1, var2)
  
  # Predict fitted values
  grid$pred = predict(model, newdata = grid, type = "response")
  
  #Plot
  ggplot(grid, aes_string(x = var1, y = "pred", color = var2, group = var2)) +
    geom_line(size = 1.2) +
    labs(title = paste("Interaction:", var1, "x", var2),
         y = "Predicted net claims") +
    theme_minimal()
}

#Plot all interactions in a loop
plots = lapply(1:nrow(top_pairs), function(i) {
  plot_interaction(top_pairs$var1[i], top_pairs$var2[i], train)
})

#View all plots
for (i in 1:length(plots)) {
  print(plots[[i]])
}

#Manually:
#Add interactions to best model
best_model_s1 = glm(net_amount ~ factor(vehicle_power) + factor(usage) + 
                     factor(area) + factor(fuel) + factor(overnight_parking) + 
                     ns(age, df = 6) + engine_cc + factor(security_device) + factor(occasional_commercial) + 
                     factor(ncd_level) + vehicle_age + factor(body_type) + factor(reported_mileage_missing) + 
                     factor(engine_cc_missing) + factor(vehicle_power):factor(usage), family = Gamma(link = "log"), 
                   data = train)
summary(best_model_s1) #AIC=983826 - improvement
anova(best_model_s, best_model_s1, test="LRT") #p value = 0.009543 so include interaction

best_model_s2 = glm(net_amount ~ factor(vehicle_power) + factor(usage) + 
                      factor(area) + factor(fuel) + factor(overnight_parking) + 
                      ns(age, df = 6) + engine_cc + factor(security_device) + factor(occasional_commercial) + 
                      factor(ncd_level) + vehicle_age + factor(body_type) + factor(reported_mileage_missing) + 
                      factor(engine_cc_missing) + factor(vehicle_power):factor(usage) + factor(vehicle_power):factor(area), family = Gamma(link = "log"), 
                    data = train)
summary(best_model_s2) #AIC=983832 - disimprovement
anova(best_model_s1, best_model_s2, test="LRT") #p value = 0.0.6834 so don't include interaction

#Use selection loops to determine which interactions to include

#Define candidate interactions 
candidates = paste(
  "factor(vehicle_power):factor(usage)",
  "factor(vehicle_power):factor(area)",
  "factor(vehicle_power):ns(age, df=6)",
  "factor(vehicle_power):factor(fuel)",
  "factor(vehicle_power):factor(body_type)",
  "factor(vehicle_power):factor(overnight_parking)",
  "factor(usage):engine_cc",
  "factor(vehicle_power):factor(security_device)",
  "factor(area):engine_cc",
  sep = " + "
)

scope_formula = as.formula(paste(". ~ . +", candidates))

#Forward Selection
step_forward = stepAIC(best_model_s, scope = list(upper = update(best_model_s, scope_formula)), 
                        direction = "forward", trace = TRUE)
summary(step_forward)

#Bidirectional Stepwise Selection
step_both = stepAIC(best_model_s, 
                     scope = list(upper = update(best_model_s, scope_formula)),
                     direction = "both",
                     trace = TRUE)
summary(step_both)

##Best model including interactions: step_forward
best_model_severity = step_forward

#Save for future use to avoid running loops
best_model_severity = glm(net_amount ~ factor(vehicle_power) + factor(usage) + 
                         factor(area) + factor(fuel) + factor(overnight_parking) + 
                         ns(age, df = 6) + engine_cc + factor(security_device) + factor(occasional_commercial) + 
                         factor(ncd_level) + vehicle_age + factor(body_type) + factor(reported_mileage_missing) + 
                         factor(engine_cc_missing) + factor(vehicle_power):factor(usage), 
                       family = Gamma(link = "log"), data = train)
AIC(best_model_severity)#983826.4

#LRT
anova(best_model_s, best_model_severity, test="LRT") #Interactions Improve Fit

#Collinearity
library(car)
vif(best_model_severity)
alias(best_model_severity)

#=================================================================================================================================================

##Validation MSE, MAE, RMSE

#Train Set
train$pred = predict(best_model_severity, type="response")

#MSE
mse_train_s = mean((train$net_amount - train$pred)^2)

#MAE
mae_train_s = mean(abs(train$net_amount - train$pred))

#RMSE
rmse_train_s = sqrt(mse_train_s)

#Validation Set
validation$pred = predict(best_model_severity, newdata=validation, type="response")

#MSE
mse_validation_s = mean((validation$net_amount - validation$pred)^2)

#MAE
mae_validation_s = mean(abs(validation$net_amount - validation$pred))

#RMSE
rmse_validation_s = sqrt(mse_validation_s)

#Compare results
results = data.frame(
  Dataset = c("Train", "Validation"),
  MSE = c(mse_train_s, mse_validation_s),
  MAE = c(mae_train_s, mae_validation_s),
  RMSE = c(rmse_train_s, rmse_validation_s)
)
print(results)

#Validation errors are generally less than training errors

#===============================================================================================================

##Gini
#Measures how well model ranks policyholders by risk

#Function to calculate Gini
gini = function(actual, pred) {
  gini = function(a, p) {
    df = data.frame(actual = a, pred = p)
    df = df[order(df$pred, decreasing = TRUE), ]
    df$cum_actual = cumsum(df$actual)
    gini_sum = sum(df$cum_actual) / sum(df$actual) - (length(a) + 1) / 2
    return(gini_sum / length(a))
  }
  
  gini_model = gini(actual, pred)
  gini_perfect = gini(actual, actual)
  
  return(gini_model / gini_perfect)
}


#Compute Gini for train and validation sets

gini_train_s = gini(train$net_amount, train$pred)
gini_validation_s = gini(validation$net_amount, validation$pred)

gini_train_s #0.2665603
gini_validation_s #0.2647437
#Model has moderate ranking power 
#Values are almost the same so model generalises well

#===================================================================================================================================

##Room for Improvement in Model
##Cap claims??

par(mfrow=c(1,1))
hist(claims_severity$net_amount, main="Net Amount", col="blue", xlab="Net Amount")
summary(claims_severity$net_amount)
quantile(claims_severity$net_amount, c(0,0.50,0.75,0.90,0.95,0.99,0.995,1))

#===================================================================================================================================

##Plots

#Pearson Residuals vs Fitted
plot(fitted(best_model_severity), residuals(best_model_severity, type="pearson"),
     xlab="Fitted Values", ylab="Pearson Residuals",
     main = "Pearson Residuals vs Fitted")
abline(h=0, col="blue")

#Deviance Residuals vs Fitted
plot(fitted(best_model_severity), residuals(best_model_severity, type="deviance"),
     xlab="Fitted Values", ylab="Deviance Residuals",
     main = "Deviance Residuals vs Fitted")
abline(h=0, col="blue")

#QQ Plot of Raw Residuals
qqnorm(residuals(best_model_severity, type="response"), main="QQ Plot of Raw Residuals")
qqline(residuals(best_model_severity), col="blue")



#QQ Plot of Pearson Residuals
qqnorm(residuals(best_model_severity, type="pearson"), main="QQ Plot of Pearson Residuals")
qqline(residuals(best_model_severity), col="blue")

#QQ Plot of Deviance Residuals
qqnorm(residuals(best_model_severity, type="deviance"), main="QQ Plot of Deviance Residuals")
qqline(residuals(best_model_int_f), col="blue")

