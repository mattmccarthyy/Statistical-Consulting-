rm(list = ls())

library(tidyverse)

policy_frequency <- readRDS(url("https://github.com/mattmccarthyy/Statistical-Consulting-/raw/refs/heads/main/data/processed/policy_frequency.rds")) 
attach(policy_frequency)

glimpse(policy_frequency)
glimpse(policy_frequency$fuel)



###############################################################################
# 1). Basic Dist. of Fuel Types
###############################################################################
# Raw counts and proportions of policies
fuel_counts <- table(fuel)
fuel_props <- prop.table(fuel_counts)

fuel_counts
round(100 * fuel_props, 1)  # % of portfolio, include in report. Need to account for future proofing as more EV's go on risk. 

###############################################################################
#) 2. Exposure-Weighted Claim Frequency by Fuel
###############################################################################
fuel_tab <- aggregate(
  cbind(claims = n_claims,
        exposure = exposure) ~ fuel,
  data = policy_frequency,
  FUN  = sum
)

fuel_tab$freq <- with(fuel_tab, claims / exposure)

# Overall portfolio claim frequency
overall_freq <- sum(n_claims) / sum(exposure)

# Relativities: frequency vs portfolio mean
fuel_tab$relativity <- fuel_tab$freq / overall_freq

# Order by relativity (highest risk first)
fuel_tab <- fuel_tab[order(-fuel_tab$relativity), ]
fuel_tab



###############################################################################
# 3). Plotting Exposure-Weighted Relativities by Fuel
###############################################################################
par(mfrow = c(1, 1),
    xaxs = "i", yaxs = "i",
    mar = c(7, 5.5, 3, 1),
    tcl = -0.25,
    cex.lab = 1.3,
    cex.axis = 1.2,
    col = "black",
    mgp = c(3.5, 0.7, 0))

bp <- barplot(fuel_tab$relativity,
              names.arg = fuel_tab$fuel,
              ylab = "Relative claim frequency",
              main = "Fuel-type relativities (claims / exposure)",
              col = "gray",
              ylim = range(c(0.9, max(fuel_tab$relativity) * 1.05)))

abline(h = 1, lty = 2, lwd = 2, col = "#8d17f1")  # portfolio mean
text(bp, fuel_tab$relativity,
     labels = round(fuel_tab$relativity, 3),
     pos = 3, cex = 1.1)



###############################################################################
# 3). Chisq to see if Number of Claims Varies Across Fuel Types
###############################################################################
# Build a claim/no-claim indicator
policy_frequency$made_claim <- as.integer(policy_frequency$n_claims > 0)

# Contingency table
fuel_tab_chi <- table(policy_frequency$fuel, policy_frequency$made_claim)

fuel_tab_chi

# Chi-square test of independence
chisq.test(fuel_tab_chi) # No evidence of differentiation across the levels 



###############################################################################
# 5). LRT for Fuel Type
###############################################################################
mod_no_fuel <- glm(n_claims ~ offset(log(exposure)),
                   family = poisson,
                   data = policy_frequency)

mod_fuel <- glm(n_claims ~ fuel + offset(log(exposure)),
                family = poisson,
                data = policy_frequency)

AIC(mod_no_fuel, mod_fuel)
anova(mod_no_fuel, mod_fuel, test = "Chisq") # Not statistically significant.
# We will likely include in our model, and we most definitely should keep collecting this data even 
# if it is not a key risk factor due to changing fleet composition. 
