rm(list = ls())

library(tidyverse)
library(splines)

policy_frequency <- readRDS(url("https://github.com/mattmccarthyy/Statistical-Consulting-/raw/refs/heads/main/data/processed/policy_frequency.rds")) 
attach(policy_frequency)

glimpse(policy_frequency)

##############################################################################
# Vehicle age: cap + EDA (Similiar to EDA for age)
##############################################################################

table(policy_frequency$vehicle_age)  # check sparsity in upper tail

# Cap very old vehicles to stabilise tail relativities
policy_frequency$vehicle_age_cap <- pmin(policy_frequency$vehicle_age, 19)
table(policy_frequency$vehicle_age_cap)

## Aggregate claims & exposure by capped vehicle age
vage_tab <- aggregate(
  cbind(claims = n_claims, exposure = exposure) ~ vehicle_age_cap,
  data = policy_frequency,
  FUN = sum
)

# Claim frequency per vehicle age
vage_tab$freq <- with(vage_tab, claims / exposure)

## Plot: observed frequency vs capped vehicle age with smooth
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
  
  plot(vage_tab$vehicle_age_cap, vage_tab$freq,
       xlab = "Vehicle age (years, capped at 19+)",
       ylab = "Observed claim frequency",
       pch = 16,
       ylim = c(0.2, 0.35),
       xlim = c(0, 20))
  lines(lowess(vage_tab$vehicle_age_cap, vage_tab$freq, f = 0.33),
        col = "#8d17f1", lwd = 2.2)
  grid()
}




##############################################################################
# Vehicle age as covariate, factor or spline (using capped age)
##############################################################################

## Linear term
m_lin_va <- glm(
  n_claims ~ vehicle_age_cap + offset(log(exposure)),
  family = poisson,
  data = policy_frequency
)

## Vehicle age bands on capped age
policy_frequency$vehicle_age_band <- cut(
  policy_frequency$vehicle_age_cap,
  breaks = c(0, 5, 10, 15, 20),   # 0–5, 5–10, 10–15, 15–19+
  right = FALSE,
  include.lowest = TRUE
)

m_fac_va <- glm(
  n_claims ~ vehicle_age_band + offset(log(exposure)),
  family = poisson,
  data = policy_frequency
)

## Natural spline (initial df, e.g. 6)
m_spl_va <- glm(
  n_claims ~ ns(vehicle_age_cap, df = 6) + offset(log(exposure)),
  family = poisson,
  data   = policy_frequency
)

## AIC comparison
AIC(m_lin_va, m_fac_va, m_spl_va)

## LR tests (nested pairs)
anova(m_lin_va, m_fac_va, test = "Chisq")
anova(m_lin_va, m_spl_va, test = "Chisq")
anova(m_fac_va, m_spl_va, test = "Chisq")

##############################################################################
# Vehicle age – spline df selection via AIC
##############################################################################

dfs_va  <- 2:8
mods_va <- vector("list", length(dfs_va))
aic_va <- numeric(length(dfs_va))

for (i in seq_along(dfs_va)) {
  k <- dfs_va[i]
  mods_va[[i]] <- glm(
    n_claims ~ ns(vehicle_age_cap, df = k) + offset(log(exposure)),
    family = poisson,
    data   = policy_frequency
  )
  aic_va[i] <- AIC(mods_va[[i]])
}

cbind(df = dfs_va, AIC = aic_va)

## Choose smallest df within (say) 2 AIC units of the minimum
min_aic_va <- min(aic_va)
tol <- 2
candidates <- dfs_va[aic_va <= min_aic_va + tol]
chosen_df_va <- min(candidates)
chosen_df_va

best_model_va <- mods_va[[which(dfs_va == chosen_df_va)]]

## Plot AIC vs df for report
{
  par(mfrow = c(1, 1),
      xaxs  = "i", yaxs = "i",
      mar   = c(5.5, 5.5, 3, 1),
      tcl   = -0.25,
      cex.lab  = 1.3,
      cex.axis = 1.2,
      col  = "black",
      mgp = c(3.5, 0.7, 0))
  
  plot(dfs_va, aic_va,
       type = "l",
       xlab = "Spline Degrees of Freedom (vehicle age, capped)",
       ylab = "AIC",
       pch  = 16,
       col  = "black",
       lwd  = 2,
       yaxt = "n",
       ylim = c(404956, 404966))
  grid()
  
  points(dfs_va, aic_va,
         pch = 19,
         col = "#8d17f1",
         cex = 1.35)
  
  abline(v = chosen_df_va, lty = 2, lwd = 2)
  text(chosen_df_va,
       min(aic_va) - 0.025 * diff(range(aic_va)),
       labels = paste("Chosen df =", chosen_df_va),
       pos = 4)
  
  yticks <- axTicks(2)
  axis(2, at = yticks,
       labels = formatC(yticks, format = "d", big.mark = ","))
}




##############################################################################
# Final Check to Decide Whether or Not to Use Spline
##############################################################################
mod_spline <- glm(n_claims ~ ns(vehicle_age_cap, df = 3) + offset(log(exposure)),
  family = poisson,
  data   = policy_frequency
)

mod_covariate <- glm(n_claims ~ vehicle_age_cap + offset(log(exposure)),
                     family = poisson,
                     data = policy_frequency)

anova(mod_spline, mod_covariate, test = "Chisq")


