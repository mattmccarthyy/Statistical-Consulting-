###############################################################################
# Fit Comparison for IBLM vs Negative Binomial (Using common test set)
###############################################################################
rm(list = ls())

options(timeout=600) # My wifi is not fast enough for this.

test1 <- readRDS(
  url("https://github.com/mattmccarthyy/Statistical-Consulting-/raw/refs/heads/main/R/Frequency%20Modelling/PoissonIBLM/test_iblm.rds")
)

test <- readRDS(
  url("https://github.com/mattmccarthyy/Statistical-Consulting-/raw/refs/heads/main/R/Frequency%20Modelling/NegBin/test_negbin.rds")
)

final_nb <- readRDS(
  url("https://github.com/mattmccarthyy/Statistical-Consulting-/raw/refs/heads/main/R/Frequency%20Modelling/NegBin/negbin_model.rds")
)

# Not loading in the IBLM object for now. 300mb file difficult to get to GitHub. Keeping local. 
iblm_poisson <- iblm_poisson_model$model

###############################################################################
# Ensuring both test sets are identical. 
# NOTE: It is the same test set, just slightly different columns for both. 
# Ensuring that the response is always identical. 
###############################################################################
# Test sets, same rows, different columns
test_iblm <- test1
test_nb <- test

stopifnot("n_claims" %in% names(test_iblm), "n_claims" %in% names(test_nb))
y <- test_nb$n_claims
stopifnot(identical(y, test_iblm$n_claims)) # Rows aligned



###############################################################################
# Getting Predictions (mean counts)
###############################################################################
mu_iblm <- as.numeric(predict(iblm_poisson, newdata = test_iblm, type = "response"))
mu_nb <- as.numeric(predict(final_nb, newdata = test_nb, type = "response"))



###############################################################################
# Getting Poisson Deviance for Report (On Count Scale)
###############################################################################
poisson_dev <- function(y, mu){ # Function to make comparison easier.
  mu <- pmax(mu, 1e-10)
  2 * sum(ifelse(y == 0, mu,
                 y * log(y / mu) - (y - mu)))
}

# Table for Report.
c(
  Deviance_IBLM = poisson_dev(y, mu_iblm),
  Deviance_NB = poisson_dev(y, mu_nb))


###############################################################################
# Calculating Rate-Scale RMSE 
###############################################################################
rate_obs <- y / test_nb$exposure
rate_iblm <- mu_iblm / test_nb$exposure
rate_nb <- mu_nb   / test_nb$exposure
  
rmse <- function(a, b) {
  sqrt(mean((a - b)^2))
}
# Table to compare RMSE's in report. 
c(
  RMSE_rate_IBLM = rmse(rate_obs, rate_iblm),
  RMSE_rate_NB = rmse(rate_obs, rate_nb)
)


###############################################################################
# Calibration by predicted risk decile (NB as reference ranking)
###############################################################################
calib <- data.frame(
  y = y,
  mu_iblm = mu_iblm,
  mu_nb = mu_nb
)

calib$decile <- cut(
  calib$mu_nb,
  breaks = quantile(calib$mu_nb, probs = seq(0, 1, 0.1), na.rm = TRUE),
  include.lowest = TRUE
)

calib_tab <- aggregate(cbind(y, mu_iblm, mu_nb) ~ decile, data = calib, FUN = mean)
calib_tab


###############################################################################
# Visualisation for Report
###############################################################################
{
  par(mfrow = c(1, 1),
      xaxs  = "i", yaxs = "i",
      mar = c(5.5, 5.5, 3, 1),
      tcl = -0.25,
      cex.main = 1.5,
      cex.lab = 1.3,
      cex.axis = 1.2,
      mgp = c(3.5, 0.7, 0))
  
  d <- seq_len(nrow(calib_tab)) # Decile index
  
  plot(d, calib_tab$y,
       type = "n",
       xlab = "Frequency risk decile (ranked by NegBin GLM predicted mean)",
       ylab = "Decile mean frequency (Observed and Predicted)",
       xaxt = "n",
       ylim = c(0, 0.6))
  
  axis(1, at = d, labels = d)
  
  usr <- par("usr")
  abline(h = pretty(usr[3:4]), v = d, col = "grey90", lwd = 1, lty = "dotted") # Adding a light grid. Looks better in Report. 
  
  lines(d, calib_tab$y, type = "b", pch = 16, lty = 1, col = "black", lwd = 2)
  lines(d, calib_tab$mu_nb, type = "b", pch = 16, lty = 1, col = "#1f77b4", lwd = 2)
  lines(d, calib_tab$mu_iblm, type = "b", pch = 16, lty = 1, col = "#8d17f1", lwd = 2)
  
  legend("topleft",
         legend = c("Observed", "NB predicted", "IBLM predicted"),
         col = c("black", "#1f77b4", "#8d17f1"),
         lty = 1,
         lwd = 2,
         pch = 16,
         bty = "n",
         cex = 1.35)
  box() # Looks rough compared to other plots in report without the box. 
}
