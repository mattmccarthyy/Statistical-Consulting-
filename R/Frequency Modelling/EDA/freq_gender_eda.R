rm(list = ls())

library(tidyverse)

policy_frequency <- readRDS(url("https://github.com/mattmccarthyy/Statistical-Consulting-/raw/refs/heads/main/data/processed/policy_frequency.rds")) 
attach(policy_frequency)

glimpse(policy_frequency)
table(policy_frequency$gender)



###############################################################################
# 1). Gender vs. Number of Claims
###############################################################################
gender_tab <- aggregate(
  cbind(claims = n_claims,
        exposure = exposure) ~ gender,
  data = policy_frequency,
  FUN = sum
)

# 2) Frequency and relativities
overall_freq <- sum(policy_frequency$n_claims) / sum(policy_frequency$exposure)

gender_tab$freq <- with(gender_tab, claims / exposure)
gender_tab$relativity <- gender_tab$freq / overall_freq

gender_tab

# 3) Chi-square test on total claim counts by gender
gender_chi <- xtabs(n_claims ~ gender, data = policy_frequency)
chisq.test(gender_chi)
# Tests whether claim counts differ systematically between male and female policyholders.


###############################################################################
# 2). Plotting Relative Claim Frequency by Gender
###############################################################################
{
  par(mfrow = c(1, 1),
      xaxs  = "i", yaxs = "i",
      mar   = c(5.5, 5.5, 3, 1),
      tcl   = -0.25,
      cex.lab  = 1.3,
      cex.axis = 1.2,
      mgp   = c(3.5, 0.7, 0))
  
  bp <- barplot(gender_tab$relativity,
                names.arg = gender_tab$gender,
                ylab = "Relative claim frequency",
                col  = "gray",
                ylim = c(min(gender_tab$relativity) - 0.03,
                         max(gender_tab$relativity) + 0.03))
  
  abline(h = 1, lty = 2, lwd = 2.2, col = "#8d17f1")
  
  text(x = bp,
       y = gender_tab$relativity + 0.01,  # just above bar
       labels = sprintf("%.3f", gender_tab$relativity),
       cex = 1.1)
}


###############################################################################
# 3). LRT to Check Inclusion of Gender
###############################################################################
mod0 <- glm(n_claims ~ offset(log(exposure)),
            family = poisson,
            data = policy_frequency)

mod_gender <- glm(n_claims ~ gender + offset(log(exposure)),
                  family = poisson,
                  data = policy_frequency)

anova(mod0, mod_gender, test = "Chisq")



###############################################################################
# 4). 95% CIs for gender relativities (for report, no plot)
###############################################################################
# Approximate Poisson SE for claim frequency:
# var(claims) approx claims  =>  var(freq) approx claims / exposure^2

se_freq <- sqrt(gender_tab$claims) / gender_tab$exposure

# Convert to SE on relativities (divide by overall_freq)
se_rel <- se_freq / overall_freq

# 95% Wald confidence intervals
gender_tab$lower_rel <- gender_tab$relativity - 1.96 * se_rel
gender_tab$upper_rel <- gender_tab$relativity + 1.96 * se_rel

gender_tab

