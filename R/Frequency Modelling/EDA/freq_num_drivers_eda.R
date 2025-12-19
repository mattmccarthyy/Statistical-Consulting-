rm(list = ls())

library(tidyverse)

policy_frequency <- readRDS(url("https://github.com/mattmccarthyy/Statistical-Consulting-/raw/refs/heads/main/data/processed/policy_frequency.rds")) 
glimpse(policy_frequency)



###############################################################################
# 1). Exposure and claims by number of drivers
###############################################################################
table(policy_frequency$num_drivers)

drv_tab <- aggregate(
  cbind(claims = n_claims, exposure = exposure) ~ num_drivers,
  data = policy_frequency,
  FUN = sum
)

# Portfolio claim frequency
overall_freq <- with(policy_frequency, sum(n_claims) / sum(exposure))

# Frequency and relativities
drv_tab$claim_freq <- drv_tab$claims / drv_tab$exposure
drv_tab$relativity <- drv_tab$claim_freq / overall_freq

drv_tab <- drv_tab[order(drv_tab$num_drivers), ]
drv_tab



###############################################################################
# 2). Barplot of claim frequency by number of drivers
###############################################################################
# Finally a usable barplot!!!
{
  par(mfrow = c(1, 1),
      xaxs = "i", 
      yaxs = "i",
      mar = c(6.5, 5.5, 3, 1),
      tcl = -0.25,
      cex.lab = 1.3,
      cex.axis = 1.1,
      col = "black",
      mgp = c(3.5, 0.7, 0))
  
  nb <- nrow(drv_tab)
  col_grad <- colorRampPalette(c("#d0aeee", "#8d17f1"))(nb) # Using cool gradient again.
  
  bar_centres <- barplot(
    drv_tab$claim_freq,
    names.arg = drv_tab$num_drivers,
    xlab = "Number of named drivers on policy",
    ylab = "Observed claim frequency",
    ylim = c(0, max(drv_tab$claim_freq) * 1.1),
    col = col_grad
  )
  
  # Portfolio average frequency (reference line)
  abline(h = overall_freq, lty = 2, lwd = 2.2)
}



###############################################################################
# 3). Any-claim incidence by number of drivers (looking for evidence of gaming)
###############################################################################
any_claim <- as.integer(policy_frequency$n_claims > 0)

incidence_tab <- table(
  num_drivers = policy_frequency$num_drivers,
  any_claim = any_claim
)
incidence_tab
prop.table(incidence_tab, margin = 1)  # within–num_drivers proportions

# Chi-squared test of association
chisq.test(incidence_tab)



###############################################################################
# 4). Covariate vs. Factor (assuming Poisson)
###############################################################################
# Numeric covariate form
m_drv_lin <- glm(
  n_claims ~ num_drivers + offset(log(exposure)),
  family = poisson,
  data = policy_frequency
)

# Factor form
policy_frequency$num_drivers_f <- factor(policy_frequency$num_drivers)
m_drv_fac <- glm(n_claims ~ num_drivers_f + offset(log(exposure)),
  family = poisson,
  data = policy_frequency
)

# Compare information criteria (models not nested, using AIC/BIC, not LRT)
AIC(m_drv_lin, m_drv_fac)
BIC(m_drv_lin, m_drv_fac) # Even the BIC is lower => Definitely include as a factor

summary(m_drv_lin)
summary(m_drv_fac)
