###################################################################################################################################################################################
#==================================================================================================================================================================================

##Diagnostic Plots##

#Pearson Residuals vs Fitted
plot(fitted(gamma_model), residuals(gamma_model, type="pearson"),
     xlab="Fitted Values", ylab="Pearson Residuals",
     main = "Pearson Residuals vs Fitted")
abline(h=0, col="blue")

#Deviance Residuals vs Fitted
plot(fitted(gamma_model), residuals(gamma_model, type="deviance"),
     xlab="Fitted Values", ylab="Deviance Residuals",
     main = "Deviance Residuals vs Fitted")
abline(h=0, col="blue")

#QQ Plot of Raw Residuals
qqnorm(residuals(gamma_model, type="response"), main="QQ Plot of Raw Residuals")
qqline(residuals(gamma_model), col="blue")

#QQ Plot of Pearson Residuals
qqnorm(residuals(gamma_model, type="pearson"), main="QQ Plot of Pearson Residuals")
qqline(residuals(gamma_model), col="blue")

#QQ Plot of Deviance Residuals
qqnorm(residuals(gamma_model, type="deviance"), main="QQ Plot of Deviance Residuals")
qqline(residuals(gamma_model), col="blue")

hist(residuals(gamma_model, type="deviance"), main="Histogram of Deviance Residuals", xlab="Deviance Residuals")
