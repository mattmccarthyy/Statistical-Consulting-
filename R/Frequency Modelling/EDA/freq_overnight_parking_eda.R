rm(list = ls())

library(tidyverse)

policy_frequency <- readRDS(url("https://github.com/mattmccarthyy/Statistical-Consulting-/raw/refs/heads/main/data/processed/policy_frequency.rds")) 
glimpse(policy_frequency)



###############################################################################
# 1). Exposure and claims by overnight_parking
###############################################################################
table(policy_frequency$overnight_parking)
# overnight_parking already a labelled factor (Garage, Street, Driveway)
park_tab <- aggregate(
  cbind(claims = n_claims, exposure = exposure) ~ overnight_parking,
  data = policy_frequency,
  FUN = sum
)

# Portfolio claim frequency
overall_freq <- sum(policy_frequency$n_claims) / sum(policy_frequency$exposure)

# Frequency and relativities
park_tab$claim_freq <- park_tab$claims / park_tab$exposure
park_tab$relativity <- park_tab$claim_freq / overall_freq

park_tab <- park_tab[order(park_tab$claim_freq), ] # Order by increasing frequency for easier reading
park_tab



###############################################################################
# 2). Barplot of claim frequency by overnight_parking
###############################################################################
# God I hate barplots
# This one is useless. Excluding from final report. 
{
  par(mfrow = c(1, 1),
      xaxs = "i", yaxs = "i",
      mar = c(8, 5.5, 3, 1),
      tcl = -0.25,
      cex.lab = 1.3,
      cex.axis = 1.1,
      col = "black",
      mgp = c(3.5, 0.7, 0))
  
  bar_centres <- barplot(
    park_tab$claim_freq,
    names.arg = park_tab$overnight_parking,
    xlab = "Overnight parking",
    ylab = "Observed claim frequency",
    ylim = c(0, max(park_tab$claim_freq) * 1.1),
    las  = 2  # rotate labels for readability
  )
  abline(h = overall_freq, lty = 2, lwd = 2.2)  # portfolio average frequency
}



###############################################################################
# 3). Any-claim incidence by overnight_parking
###############################################################################
any_claim <- as.integer(policy_frequency$n_claims > 0)

incidence_tab <- table(
  overnight_parking = policy_frequency$overnight_parking,
  any_claim = any_claim
)
incidence_tab
prop.table(incidence_tab, margin = 1)  # within–parking-location proportions

# Chi-squared test of association
chisq.test(incidence_tab)
