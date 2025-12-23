###############################################################################
# Gamma IBLM Interpretability for Report
###############################################################################
rm(list = ls())
options(timeout = 600)
set.seed(100)

library(SHAPforxgboost)
library(xgboost)
library(shapviz)
library(splines)

test_iblm_shap <- readRDS(
  url("https://github.com/mattmccarthyy/Statistical-Consulting-/raw/refs/heads/main/R/Severity%20Modelling/GammaIBLM/test_gamma_iblm.rds")
)

iblm_obj <- readRDS(
  url("https://github.com/mattmccarthyy/Statistical-Consulting-/raw/refs/heads/main/R/Severity%20Modelling/GammaIBLM/iblm_gamma_final.rds")
)
iblm_final <- iblm_obj$model
preds <- iblm_obj$preds
booster <- iblm_final$booster_model

claims_severity <- readRDS(
  url("https://github.com/mattmccarthyy/Statistical-Consulting-/raw/refs/heads/main/data/processed/claims_severity.rds")
) # Need to rebuild raw age in test set for SHAP. Can't interpret spline components alone. 
age_cols <- paste0("age_ns", 1:6) # Done here. 



###############################################################################
# SHAP Contributions on Test Set
###############################################################################
X_shap <- as.matrix(test_iblm_shap[, preds, drop = FALSE])

shap_contrib <- predict(booster, newdata = X_shap, predcontrib = TRUE)
colnames(shap_contrib) <- c(colnames(X_shap), "BIAS")

shap_contrib_nobias <- shap_contrib[, colnames(shap_contrib) != "BIAS", drop = FALSE]

shap_long <- shap.prep(
  shap_contrib = as.data.frame(shap_contrib_nobias),
  X_train = as.data.frame(X_shap)
)


###############################################################################
# 1). Global Interpretability for Report, SHAP Beeswarm and Ranked Importance 
###############################################################################
# Beeswarm Plot, I tried to make this for only the top 8 predictors.
# One hour later and it didn't work, proceeding with this for the sake of time. 
shap.plot.summary(shap_long)

# Global importance table
mean_abs <- sort(colMeans(abs(shap_contrib_nobias)), decreasing = TRUE)
mean_abs_grp <- mean_abs
mean_abs_grp <- mean_abs_grp[setdiff(names(mean_abs_grp), age_cols)]
mean_abs_grp["Age (spline)"] <- sum(mean_abs[age_cols])

top_imp <- head(sort(mean_abs_grp, decreasing = TRUE), 8)
top_imp



###############################################################################
# 2). How Key Drivers Act for Report, Dependence Plot
###############################################################################
p1 <- shap.plot.dependence(shap_long, x = "vehicle_age", y = "vehicle_age")
print(p1)



###############################################################################
# 3). Local Interpretability for Report, Typical (Median) and High-Risk (p95) case
###############################################################################
mu_iblm <- as.numeric(predict(iblm_final, newdata = test_iblm_shap, type = "response"))

i_typ <- which.min(abs(mu_iblm - median(mu_iblm)))
i_hi <- which.min(abs(mu_iblm - quantile(mu_iblm, 0.95)))

local_tab <- function(i, k = 10){
  s <- shap_contrib[i, colnames(X_shap)]
  idx <- head(order(abs(s), decreasing = TRUE), k)
  idx <- idx[order(s[idx], decreasing = TRUE)]
  data.frame(feature = names(s)[idx], value = X_shap[i, idx], shap = s[idx])
}

local_info <- data.frame(
  case = c("Typical (median mu)", "High-risk (p95 mu)"),
  row_index = c(i_typ, i_hi),
  pred_severity = c(mu_iblm[i_typ], mu_iblm[i_hi]),
  row.names = NULL
)

local_info
local_tab_typ <- local_tab(i_typ, 10); local_tab_typ
local_tab_hi <- local_tab(i_hi,  10); local_tab_hi



###############################################################################
# 4). Waterfall Plots for Typical, High-Risk and Moderately-High cases
###############################################################################
sp  <- ns(claims_severity$age, df = 6)
k   <- attr(sp, "knots")
bk  <- attr(sp, "Boundary.knots")

# Helper: recover age from spline basis row
recover_age <- function(basis_row){
  obj <- function(a){
    sum((as.numeric(ns(a, knots = k, Boundary.knots = bk)) - as.numeric(basis_row))^2)
  }
  optimize(obj, interval = range(bk))$minimum
}

i_mid <- which.min(abs(mu_iblm - unname(quantile(mu_iblm, 0.75))))

# SHAP without bias and baseline
S  <- shap_contrib[, colnames(X_shap), drop = FALSE]
b0 <- as.numeric(shap_contrib[1, "BIAS"])

# Grouping age spline SHAPs into one "Age (years)"
S_grp <- cbind(
  S[, setdiff(colnames(S), age_cols), drop = FALSE],
  "Age (years)" = rowSums(S[, age_cols, drop = FALSE])
)

# Display Age (years) only for plotted rows
X_df <- as.data.frame(X_shap)
age_years <- rep(NA_real_, nrow(X_df))
age_years[i_typ] <- recover_age(X_df[i_typ, age_cols, drop = FALSE])
age_years[i_mid] <- recover_age(X_df[i_mid, age_cols, drop = FALSE])
age_years[i_hi] <- recover_age(X_df[i_hi,  age_cols, drop = FALSE])

X_grp <- data.frame(
  X_df[, setdiff(colnames(X_df), age_cols), drop = FALSE],
  "Age (years)" = age_years,
  check.names = FALSE
)[, colnames(S_grp), drop = FALSE]

sv <- shapviz(as.matrix(S_grp), X = X_grp, baseline = b0)
print(sv_waterfall(sv, row_id = i_typ, max_display = 10))
print(sv_waterfall(sv, row_id = i_mid, max_display = 10))
print(sv_waterfall(sv, row_id = i_hi, max_display = 10))

# Final Plot for Report (Late addition)
age_years <- apply(X_df[, age_cols, drop = FALSE], 1, recover_age)
shap_age <- rowSums(shap_contrib[, age_cols, drop = FALSE])

plot(age_years, shap_age,
     xlab = "Age (years)",
     ylab = "Grouped SHAP contribution for age (sum over spline bases)",
     pch = 16,
     col = adjustcolor(col = "black", alpha = 0.6))
lines(lowess(age_years, shap_age), lwd = 2, lty = 2, col = "#8d17f1")


