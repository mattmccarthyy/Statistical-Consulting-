rm(list = ls())

library(tidyverse)

policy_frequency <- readRDS(url("https://github.com/mattmccarthyy/Statistical-Consulting-/raw/refs/heads/main/data/processed/policy_frequency.rds")) 
attach(policy_frequency)

glimpse(policy_frequency)
table(ncd_level)


###############################################################################
# 1). Claims and Exposure by NCD Level
###############################################################################
table(policy_frequency$ncd_level)
prop.table(table(policy_frequency$ncd_level))

# Aggregate claims and exposure by NCD level
ncd_tab <- aggregate(
  cbind(claims = n_claims,
        exposure = exposure) ~ ncd_level,
  data = policy_frequency,
  FUN = sum
)

## Claim frequency and relativities
ncd_tab$freq <- with(ncd_tab, claims / exposure)

overall_freq <- sum(policy_frequency$n_claims) / sum(policy_frequency$exposure)

ncd_tab$relativity <- ncd_tab$freq / overall_freq
ncd_tab



###############################################################################
# 2). Visualisation for Report
###############################################################################
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
  
  barplot(ncd_tab$relativity,
          names.arg = ncd_tab$ncd_level,
          xlab = "NCD level",
          ylab = "Relative claim frequency",
          col = "gray")
  
  abline(h = 1, lty = 2, lwd = 2, col = "#8d17f1")
}



###############################################################################
# 3). Deciding on Form to Include in Model
###############################################################################
m_ncd_lin <- glm(n_claims ~ ncd_level + offset(log(exposure)),
                 family = poisson, data = policy_frequency)

m_ncd_fac <- glm(n_claims ~ factor(ncd_level) + offset(log(exposure)),
                 family = poisson, data = policy_frequency)

anova(m_ncd_lin, m_ncd_fac, test = "Chisq")
AIC(m_ncd_lin, m_ncd_fac)
# No justification of a spline here, only 6 discrete levels, a factor will perform as well if not better.
# Also no interpolation between discrete levels, no actuarial meaning of this.
# Factor perfectly sufficient. 