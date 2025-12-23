###################################################################################################################################################################################
#==================================================================================================================================================================================

##Diagnostic Plots##

#==========================================================================================================================================================================================

#Pearson Residuals vs Fitted
plot(fitted(final_nb), residuals(final_nb, type="pearson"),
     xlab="Fitted Values", ylab="Pearson Residuals",
     main = "Pearson Residuals vs Fitted")
abline(h=0, col="red")

#Deviance Residuals vs Fitted
plot(fitted(final_nb), residuals(final_nb, type="deviance"),
     xlab="Fitted Values", ylab="Deviance Residuals",
     main = "Deviance Residuals vs Fitted")
abline(h=0, col="red")

par(mfrow=c(1,1))
#QQ Plot of Raw Residuals
qqnorm(residuals(final_nb, newdata=validation, type="response"), main="QQ Plot of Raw Residuals")
qqline(residuals(final_nb), col="red")

#QQ Plot of Pearson Residuals
qqnorm(residuals(final_nb, type="pearson"), main="QQ Plot of Pearson Residuals")
qqline(residuals(final_nb), col="red")

#QQ Plot of Deviance Residuals
qqnorm(residuals(final_nb, type="deviance"), main="QQ Plot of Deviance Residuals")
qqline(residuals(final_nb), col="red")

#================================================================================================================
##Dunn Smyth Residual Plots

response_var = as.character(formula(final_nb)[[2]])

#Extract fitted means on validation set
mu = predict(final_nb, newdata = validation, type = "response")

#Extract theta
theta = final_nb$theta

#Observed responses
y = validation[[response_var]]

#Dunn–Smyth residuals
set.seed(100)
n = length(y)
residuals_ds = numeric(n)

for (i in 1:n) {
  lower = pnbinom(y[i] - 1, size = theta, mu = mu[i])
  upper = pnbinom(y[i],     size = theta, mu = mu[i])
  
  #Randomized quantile
  u = runif(1, lower, upper)
  
  #Dunn–Smyth residual
  residuals_ds[i] = qnorm(u)
}

plot(mu, residuals_ds,
     pch = 19, col = rgb(0, 0, 0, 0.5),
     xlab = "Fitted values",
     ylab = "Dunn-Smyth Residuals",
     main = "Dunn-Smyth Residuals vs Fitted (Validation Set)")

abline(h = 0, col = "red", lwd = 2)

hist(residuals_ds,
     breaks = 100,
     border = "white",
     main = "Histogram of Dunn-Smyth residuals",
     xlab = "Dunn-Smyth Residuals",
     ylab = "Frequency")
dev.off()

qqnorm(residuals_ds,
       main = "QQ Plot of Dunn-Smyth Residuals",
       xlab = "Theoretical Quantiles",
       ylab = "Sample Quantiles",
       pch = 19, col = rgb(0, 0, 0, 0.5))

qqline(residuals_ds, col = "red", lwd = 2)

