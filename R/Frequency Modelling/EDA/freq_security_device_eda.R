rm(list = ls())

library(tidyverse)

policy_frequency <- readRDS(url("https://github.com/mattmccarthyy/Statistical-Consulting-/raw/refs/heads/main/data/processed/policy_frequency.rds")) 
glimpse(policy_frequency)

# No instruction on what 0, and 1 are.
# From online, 1 generally means feature is present, so working under this assumption.
# Outlining our assumptions in report. 
policy_frequency$security_device <- factor(policy_frequency$security_device, levels = c(0, 1), labels = c("No Security Device", "Security Device"))



###############################################################################
# 1). Exposure and claims by security_device
###############################################################################
sec_tab <- aggregate(
  cbind(claims   = n_claims,
        exposure = exposure) ~ security_device,
  data = policy_frequency,
  FUN  = sum
)

# Portfolio claim frequency
overall_freq <- sum(policy_frequency$n_claims) / sum(policy_frequency$exposure)

# Frequency and relativities
sec_tab$claim_freq <- sec_tab$claims / sec_tab$exposure
sec_tab$relativity <- sec_tab$claim_freq / overall_freq

sec_tab # Relativities are within 1%, weak signal.



###############################################################################
# 2). Barplot of claim frequency by security_device
###############################################################################
{
  par(mfrow = c(1, 1),
      xaxs  = "i", yaxs = "i",
      mar = c(7.5, 5.5, 3, 1),
      tcl = -0.25,
      cex.lab = 1.3,
      cex.axis = 1.3,
      col = "black",
      mgp = c(3.5, 0.7, 0))
  
  # Not a very informative plot. 
  bar_centres <- barplot(
    sec_tab$claim_freq,
    names.arg = sec_tab$security_device,
    xlab = "Security device",
    ylab = "Observed claim frequency",
    ylim = c(0, max(sec_tab$claim_freq) * 1.1)
  )
  abline(h = overall_freq, lty = 2, lwd = 2.2) # line overlaying portfolio average frequency
}



###############################################################################
# 3). Any-claim incidence by security_device (for brief text comment)
###############################################################################
any_claim <- as.integer(policy_frequency$n_claims > 0)

incidence_tab <- table(
  security_device = policy_frequency$security_device,
  any_claim = any_claim
)
incidence_tab

# Chi-squared test
chisq.test(incidence_tab)
# No strong evidence of a material effect, but still p < 0.1.
# Discussing implications in report. 
