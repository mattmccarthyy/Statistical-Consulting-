#########################################################################################################
## 1). Comparing GBDT with GLM (Assuming log-link)
#########################################################################################################

gamma_glm_test <- best_model_severity

# Observed severities on test set
y_obs <- test$gross_amount

# GBDT predictions on original scale (already computed)
mu_gbdt <- gross_pred_test

# Gamma GLM predictions (original scale, mean of gross_amount)
mu_glm <- predict(gamma_glm_test, newdata = test, type = "response")

# Helper metrics
rmse_fun <- function(y, yhat) sqrt(mean((y - yhat)^2))
mae_fun  <- function(y, yhat) mean(abs(y - yhat))

rmse_glm  <- rmse_fun(y_obs, mu_glm)
mae_glm   <- mae_fun(y_obs, mu_glm)

rmse_gbdt <- rmse_fun(y_obs, mu_gbdt)
mae_gbdt  <- mae_fun(y_obs, mu_gbdt)

# Gamma deviance 
gamma_deviance <- function(y, mu) {
  eps <- 1e-8
  y  <- pmax(y, eps)
  mu <- pmax(mu, eps)
  2 * sum((y - mu) / mu - log(y / mu))
}

dev_glm  <- gamma_deviance(y_obs, mu_glm)
dev_gbdt <- gamma_deviance(y_obs, mu_gbdt)

model_comp <- data.frame(
  model         = c("Gamma GLM", "GBDT"),
  RMSE_original = c(rmse_glm, rmse_gbdt),
  MAE_original  = c(mae_glm, mae_gbdt),
  GammaDeviance = c(dev_glm, dev_gbdt)
)

model_comp
