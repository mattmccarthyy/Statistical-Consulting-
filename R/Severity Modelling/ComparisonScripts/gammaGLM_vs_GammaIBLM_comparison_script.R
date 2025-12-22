###############################################################################
# Model Comparison: Gamma IBLM vs Gamma GLM (Same Test Set, slighlty diff. cols)
###############################################################################
rm(list = ls())
options(timeout = 600)
set.seed(100)

library(splines)

###############################################################################
# Load test set + fitted models
###############################################################################
test_glm <- readRDS(url("https://github.com/mattmccarthyy/Statistical-Consulting-/raw/refs/heads/main/R/Severity%20Modelling/Gamma/test.rds"))

glm_final <- readRDS(url("https://github.com/mattmccarthyy/Statistical-Consulting-/raw/refs/heads/main/R/Severity%20Modelling/Gamma/gamma_GLM.rds"))

iblm_obj  <- readRDS(url("https://github.com/mattmccarthyy/Statistical-Consulting-/raw/refs/heads/main/R/Severity%20Modelling/GammaIBLM/iblm_gamma_final.rds"))
iblm_final <- iblm_obj$model
preds <- iblm_obj$preds

# Need data we trained on just for the age splines. 
claims_severity <- readRDS(url("https://github.com/mattmccarthyy/Statistical-Consulting-/raw/refs/heads/main/data/processed/claims_severity.rds"))

###############################################################################
# Rebuilding IBLM test set (factors etc changing minorly, same test set, just reformatting)
###############################################################################
test_iblm_base <- test_glm
test_iblm_base$security_device <- NULL

test_iblm_base$occasional_commercial <- factor(test_iblm_base$occasional_commercial)
test_iblm_base$ncd_level <- factor(test_iblm_base$ncd_level)
test_iblm_base$reported_mileage_missing <- factor(test_iblm_base$reported_mileage_missing)
test_iblm_base$engine_cc_missing <- factor(test_iblm_base$engine_cc_missing)

age_train_ns <- ns(claims_severity$age, df = 6)
knots <- attr(age_train_ns, "knots")
bknots <- attr(age_train_ns, "Boundary.knots")

age_test_ns <- ns(test_iblm_base$age, knots = knots, Boundary.knots = bknots)
colnames(age_test_ns) <- paste0("age_ns", 1:6)

tmp <- cbind(test_iblm_base, age_test_ns)

X_test <- model.matrix(
  ~ vehicle_power + usage + area + fuel + overnight_parking +
    age_ns1 + age_ns2 + age_ns3 + age_ns4 + age_ns5 + age_ns6 +
    engine_cc + occasional_commercial +
    ncd_level + vehicle_age + body_type +
    reported_mileage_missing + engine_cc_missing +
    vehicle_power:usage,
  data = tmp
)

X_test <- X_test[, colnames(X_test) != "(Intercept)", drop = FALSE]

test_iblm <- data.frame(gross_amount = test_iblm_base$gross_amount, X_test, check.names = FALSE)
test_iblm <- test_iblm[, c("gross_amount", preds), drop = FALSE] ; y_iblm <- test_iblm$gross_amount

###############################################################################
# Response and Predictors
###############################################################################
y_glm <- test_glm$gross_amount

mu_glm<- as.numeric(predict(glm_final, newdata = test_glm, type = "response"))
mu_iblm <- as.numeric(predict(iblm_final, newdata = test_iblm, type = "response"))

###############################################################################
# Same Fit Metrics for Report (Gamma deviance and RMSE) 
###############################################################################
gamma_dev <- function(y, mu){
  y <- pmax(y,  1e-12)
  mu <- pmax(mu, 1e-12)
  2 * sum((y - mu) / mu - log(y / mu))
}

rmse <- function(a, b) sqrt(mean((a - b)^2))

dev_glm <- gamma_dev(y_glm, mu_glm)
dev_iblm <- gamma_dev(y_iblm, mu_iblm)
rmse_glm <- rmse(y_glm, mu_glm)
rmse_iblm<- rmse(y_iblm, mu_iblm)

fit_tab <- c(
  Deviance_GLM = dev_glm,
  Deviance_IBLM = dev_iblm,
  RMSE_GLM = rmse_glm,
  RMSE_IBLM = rmse_iblm,
  Decrease_in_Deviance = dev_glm - dev_iblm,
  Decrease_in_RMSE = rmse_glm - rmse_iblm
)
fit_tab

# Common calibration frame (y is identical across both now)
calib <- data.frame(
  y = y_glm, # == y_iblm
  mu_glm  = mu_glm,
  mu_iblm = mu_iblm
)

###############################################################################
# 1). Calibration by GLM deciles
###############################################################################
calib$decile_glm <- cut(
  calib$mu_glm,
  breaks = quantile(calib$mu_glm, probs = seq(0, 1, 0.1), na.rm = TRUE),
  include.lowest = TRUE
)

calib_tab_glm <- aggregate(cbind(y, mu_glm, mu_iblm) ~ decile_glm, data = calib, FUN = mean)
calib_tab_glm


###############################################################################
# Calibration Plot by GLM deciles
###############################################################################
{
  tab <- calib_tab_glm
  
  par(mfrow = c(1, 1),
      xaxs = "i", 
      yaxs = "i",
      mar = c(5.5, 5.5, 3, 1),
      tcl = -0.25,
      cex.main = 1.5,
      cex.lab = 1.3,
      cex.axis = 1.2,
      mgp = c(3.5, 0.7, 0))
  
  d <- seq_len(nrow(tab))
  
  plot(d, tab$y,
       type = "n",
       xlab = "GLM predicted severity decile (Low to High)",
       ylab = "Mean observed / predicted severity",
       xaxt = "n",
       yaxt = "n",
       ylim = c(8000, 22000))
  
  axis(1, at = d, labels = d)
  
  y_all <- c(tab$y, tab$mu_glm, tab$mu_iblm)
  ticks <- pretty(y_all)
  axis(2, at = ticks, labels = format(ticks, big.mark = ",", scientific = FALSE))
  
  usr <- par("usr")
  abline(h = pretty(usr[3:4]), v = d, col = "grey90", lwd = 1, lty = "dotted")
  
  lines(d, tab$y, type = "b", pch = 16, lty = 1, col = "black",   lwd = 2)
  lines(d, tab$mu_glm, type = "b", pch = 16, lty = 1, col = "#1f77b4", lwd = 2)
  lines(d, tab$mu_iblm, type = "b", pch = 16, lty = 1, col = "#8d17f1", lwd = 2)
  
  legend("topleft",
         legend = c("Observed", "Gamma GLM predicted", "Gamma IBLM predicted"),
         col = c("black", "#1f77b4", "#8d17f1"),
         lty = 1, lwd = 2, pch = 16, bty = "n")
  box()
}











