rm(list = ls())
library(tidyverse)

policy_frequency <- readRDS(url("https://github.com/mattmccarthyy/Statistical-Consulting-/raw/refs/heads/main/data/processed/policy_frequency.rds"))

###############################################################################
# 1). Exposure and claims by vehicle power
###############################################################################
# Raw policy counts and proportions (sparsity check)
table(policy_frequency$vehicle_power) # No sparsity
prop.table(table(policy_frequency$vehicle_power))

policy_frequency$vehicle_power <- factor(
  policy_frequency$vehicle_power,
  levels = c("Low", "Med", "High")
)

power_tab <- aggregate(
  cbind(claims = n_claims,
        exposure = exposure) ~ vehicle_power,
  data = policy_frequency,
  FUN = sum
)

# Portfolio mean claim frequency
overall_freq <- with(policy_frequency, sum(n_claims) / sum(exposure))

# Frequency and relativities by band
power_tab$freq <- with(power_tab, claims / exposure)
power_tab$relativity <- power_tab$freq / overall_freq

power_tab # summary table for the report


###############################################################################
# 2). 95% confidence intervals for relativities (Poisson approximation)
###############################################################################
# var(freq) approx claims / exposure^2  under a Poisson assumption
se_freq_power <- sqrt(power_tab$claims) / power_tab$exposure

# Standard error on relativity scale
se_rel_power <- se_freq_power / overall_freq

# 95% CI bounds for relativities
power_tab$lower_rel <- power_tab$relativity - 1.96 * se_rel_power
power_tab$upper_rel <- power_tab$relativity + 1.96 * se_rel_power

power_tab[, c("vehicle_power", "relativity", "lower_rel", "upper_rel")]


###############################################################################
# 3). Plots
###############################################################################
## (a) Barplot of relativities with values on top
{
  par(mfrow = c(1,1),
      xaxs  = "i", yaxs = "i",
      mar = c(5.5, 5.5, 3, 1),
      tcl = -0.25,
      cex.lab  = 1.3,
      cex.axis = 1.2,
      mgp = c(3.5, 0.7, 0))
  
  bp <- barplot(power_tab$relativity,
                names.arg = power_tab$vehicle_power,
                ylab = "Relative claim frequency",
                col = "gray",
                ylim = c(0.9 * min(power_tab$relativity),
                         1.1 * max(power_tab$relativity)))
  
  abline(h = 1, lty = 2, lwd = 2, col = "#8d17f1")
  
  text(x = bp,
       y = power_tab$relativity + 0.01,
       labels = sprintf("%.3f", power_tab$relativity),
       cex = 1.1)
}


# Using form used in "Occupation" EDA. Much clearer. 
{
  par(mfrow = c(1, 1),
      xaxs = "i", yaxs = "i",
      mar = c(5.5, 5.5, 3, 1),
      tcl = -0.25,
      cex.lab = 1.3,
      cex.axis = 1.2,
      col = "black",
      mgp = c(3.5, 0.7, 0))
  
  y_min <- min(power_tab$freq) * 0.98
  y_max <- max(power_tab$freq) * 1.02
  
  plot(power_tab$vehicle_power, power_tab$freq,
       xlab = "Vehicle power band",
       ylab = "Observed claim frequency",
       pch = 16,
       ylim = c(y_min, y_max))
  
  grid()
  
  lines(1:nrow(power_tab), power_tab$freq, type = "b")
  abline(h = overall_freq, lty = 2, col = "#8d17f1", lwd = 2.2)
}



###############################################################################
# 4). Statistical evidence for differences across power bands
###############################################################################
#Chi-square test on claim counts by band
power_chi <- xtabs(n_claims ~ vehicle_power, data = policy_frequency)
chisq.test(power_chi) # test of independence between claims and power band

mod0      <- glm(n_claims ~ offset(log(exposure)),
                 family = poisson,
                 data = policy_frequency)

mod_power <- glm(n_claims ~ vehicle_power + offset(log(exposure)),
                 family = poisson,
                 data = policy_frequency)

anova(mod0, mod_power, test = "Chisq")

