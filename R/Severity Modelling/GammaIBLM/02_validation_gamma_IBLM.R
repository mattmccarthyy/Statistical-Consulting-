###############################################################################
# Gamma IBLM validation (train / validation / test)
###############################################################################
rm(list = ls())
set.seed(100)

###############################################################################
# Load fitted model + splits
###############################################################################
obj <- readRDS(url("https://github.com/mattmccarthyy/Statistical-Consulting-/raw/refs/heads/main/R/Severity%20Modelling/GammaIBLM/iblm_gamma_final.rds"))
m <- obj$model

train <- readRDS(url("https://github.com/mattmccarthyy/Statistical-Consulting-/raw/refs/heads/main/R/Severity%20Modelling/GammaIBLM/train_gamma_iblm.rds"))
validation <- readRDS(url("https://github.com/mattmccarthyy/Statistical-Consulting-/raw/refs/heads/main/R/Severity%20Modelling/GammaIBLM/validation_gamma_iblm.rds"))
test <- readRDS(url("https://github.com/mattmccarthyy/Statistical-Consulting-/raw/refs/heads/main/R/Severity%20Modelling/GammaIBLM/test_gamma_iblm.rds"))

###############################################################################
# Mean gamma deviance
###############################################################################
gamma_dev_mean <- function(y, mu){
  y <- pmax(y,  1e-12)
  mu <- pmax(mu, 1e-12)
  mean(2 * ((y - mu) / mu - log(y / mu)))
}

###############################################################################
# Predictions
###############################################################################
y_train <- train$gross_amount
y_val <- validation$gross_amount
y_test  <- test$gross_amount

mu_train <- pmax(as.numeric(predict(m, newdata = train)), 1e-12)
mu_val <- pmax(as.numeric(predict(m, newdata = validation)), 1e-12)
mu_test <- pmax(as.numeric(predict(m, newdata = test)), 1e-12)

###############################################################################
# Metrics table (train vs validation vs test) and a simple baseline
###############################################################################
base_mean <- mean(y_train)

metrics <- function(y, mu){
  c(
    dev_mean = gamma_dev_mean(y, mu),
    rmse = sqrt(mean((y - mu)^2)),
    mae  = mean(abs(y - mu)),
    mape = mean(abs(y - mu) / pmax(y, 1e-12)),
    mean_y = mean(y),
    mean_mu = mean(mu),
    ratio = mean(mu) / mean(y),
    cor_y_mu = suppressWarnings(cor(y, mu))
  )
}

m_tr <- metrics(y_train, mu_train)
m_va <- metrics(y_val, mu_val)
m_te <- metrics(y_test, mu_test)

rmse0 <- function(y) sqrt(mean((y - base_mean)^2))
mae0 <- function(y) mean(abs(y - base_mean))

tab <- rbind(
  c(split = "train", m_tr, rmse_base = rmse0(y_train), mae_base = mae0(y_train),
    rmse_impr = 1 - m_tr["rmse"]/rmse0(y_train), mae_impr = 1 - m_tr["mae"]/mae0(y_train)),
  c(split = "validation", m_va, rmse_base = rmse0(y_val),   mae_base = mae0(y_val),
    rmse_impr = 1 - m_va["rmse"]/rmse0(y_val),   mae_impr = 1 - m_va["mae"]/mae0(y_val)),
  c(split = "test", m_te, rmse_base = rmse0(y_test),  mae_base = mae0(y_test),
    rmse_impr = 1 - m_te["rmse"]/rmse0(y_test),  mae_impr = 1 - m_te["mae"]/mae0(y_test))
)

tab_num <- apply(tab[, -1, drop = FALSE], 2, as.numeric)
tab_out <- data.frame(split = tab[, 1], round(tab_num, 6), check.names = FALSE)
print(tab_out, row.names = FALSE)

###############################################################################
# Tail sensitivity (test, top 1 percent removed)
###############################################################################
keep <- y_test <= quantile(y_test, 0.99, na.rm = TRUE)

tail_tab <- c(
  rmse_99 = sqrt(mean((y_test[keep] - mu_test[keep])^2)),
  mae_99 = mean(abs(y_test[keep] - mu_test[keep])),
  dev_mean_99 = gamma_dev_mean(y_test[keep], mu_test[keep])
)
tail_tab

###############################################################################
# Ranking checks (Lift and Capture again) on test
###############################################################################
o <- order(mu_test, decreasing = TRUE)
y_o <- y_test[o]
n <- length(y_o)

p_grid <- seq(0.01, 1, by = 0.01)
lift <- numeric(length(p_grid))
capture <- numeric(length(p_grid))

base_y_mean <- mean(y_o)
base_y_sum  <- sum(y_o)

for (i in seq_along(p_grid)){
  k <- max(1, floor(p_grid[i] * n))
  lift[i] <- mean(y_o[1:k]) / base_y_mean
  capture[i] <- sum(y_o[1:k]) / base_y_sum
}


###############################################################################
# Calibration by predicted deciles (test)
###############################################################################
br <- quantile(mu_test, probs = seq(0, 1, by = 0.1), na.rm = TRUE)
br[1] <- br[1] - 1e-12
grp <- cut(mu_test, breaks = unique(br), include.lowest = TRUE)

cal <- data.frame(
  decile = levels(grp),
  n = as.integer(tapply(y_test, grp, length)),
  mean_y = as.numeric(tapply(y_test, grp, mean)),
  mean_mu = as.numeric(tapply(mu_test, grp, mean))
)
cal$ratio_y_mu <- cal$mean_y / pmax(cal$mean_mu, 1e-12)

cal



###############################################################################
# Residual checks (on test, deviance residuals only)
###############################################################################
y_safe  <- pmax(y_test, 1e-12)
mu_safe <- pmax(mu_test, 1e-12)

d <- 2 * ((y_safe - mu_safe) / mu_safe - log(y_safe / mu_safe))
rdev <- sign(y_safe - mu_safe) * sqrt(pmax(d, 0))



###############################################################################
# Actual Plots for the Report
###############################################################################
{
  par(mfrow = c(1, 1),
      xaxs = "i", yaxs = "i",
      mar = c(5.5, 5.5, 3, 3.5),
      tcl = -0.25,
      cex.main = 1.5,
      cex.lab = 1.3,
      cex.axis = 1.2,
      col = "black",
      mgp = c(3.5, 0.7, 0))
  
  plot(cal$mean_mu, cal$mean_y,
       xlab = "Mean predicted severity by decile",
       ylab = "Mean observed severity by decile",
       xlim = c(8000, 22000),
       ylim = c(8000, 22000),
       pch = 16,
       col = "#8d17f1",
       xaxt = "n")
  
  grid()
  
  xt <- axTicks(1)
  axis(1, at = xt, labels = format(xt, big.mark = ",", trim = TRUE), las = 1)
  
  abline(0, 1, lty = 2, lwd = 1.5)
  
  box()
}

{
  par(mfrow = c(1, 1),
      xaxs = "i", yaxs = "i",
      mar = c(5.5, 5.5, 3, 1),
      tcl = -0.25,
      cex.main = 1.5,
      cex.lab = 1.3,
      cex.axis = 1.2,
      col = "black",
      mgp = c(3.5, 0.7, 0))
  
  plot(mu_test, rdev,
       type = "n",
       xlab = "Predicted severity (€)",
       ylab = "Deviance residuals",
       ylim = c(-4, 4),
       xaxt = "n")
  
  grid(col = "grey85", lty = 1)
  
  xt <- axTicks(1)
  axis(1, at = xt, labels = format(xt, big.mark = ",", trim = TRUE), las = 1)
  
  points(mu_test, rdev, pch = 16, cex = 0.4)
  
  abline(h = 0, lty = 2, col = "#8d17f1", lwd = 2)
  box()
}

# Including this table
c(
  lift_10 = lift[which.min(abs(p_grid - 0.10))],
  cap_10 = capture[which.min(abs(p_grid - 0.10))],
  lift_20 = lift[which.min(abs(p_grid - 0.20))],
  cap_20 = capture[which.min(abs(p_grid - 0.20))]
)

{
  par(mfrow = c(1, 1),
      xaxs = "i", yaxs = "i",
      mar = c(5.5, 5.5, 3, 1),
      tcl = -0.25,
      cex.main = 1.5,
      cex.lab = 1.3,
      cex.axis = 1.2,
      col = "black",
      mgp = c(3.5, 0.7, 0))
  
  qqnorm(rdev,
         pch = 16,
         col = "#8d17f1",
         xaxt = "n",
         main = "",
         ylim = c(-4.5, 4.5))
  
  grid(col = "grey85", lty = 1)
  
  xt <- axTicks(1)
  axis(1, at = xt, labels = format(xt, big.mark = ",", trim = TRUE), las = 1)
  
  qqline(rdev, lty = 2, lwd = 1.5)
  
  box()
}

