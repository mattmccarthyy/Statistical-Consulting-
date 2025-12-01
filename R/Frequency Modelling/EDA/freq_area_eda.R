rm(list = ls())

library(tidyverse)

policy_frequency <- readRDS(url("https://github.com/mattmccarthyy/Statistical-Consulting-/raw/refs/heads/main/data/processed/policy_frequency.rds")) 
attach(policy_frequency)

glimpse(policy_frequency)
table(policy_frequency$area)


###############################################################################
# 1). Area vs. Number of Claims
###############################################################################
# 1) Exposure and claims by area
area_tab <- aggregate(
  cbind(claims = n_claims,
        exposure = exposure) ~ area,
  data = policy_frequency,
  FUN = sum
)

# 2) Frequency and relativities
overall_freq <- sum(policy_frequency$n_claims) / sum(policy_frequency$exposure)
area_tab$freq <- with(area_tab, claims / exposure)
area_tab$relativity <- area_tab$freq / overall_freq

area_tab

area_chi <- xtabs(cbind(n_claims) ~ area, data = policy_frequency)
chisq.test(area_chi)
# Tests independence between "had a claim occur" and area. 
# Highly statistically significant, going to include an LRT in report, so excluding for now.



###############################################################################
# 2). Plotting Relative Claim Frequency by Area
###############################################################################
{
  par(mfrow = c(1, 1),
      xaxs = "i", yaxs = "i",
      mar = c(5.5, 5.5, 3, 1),
      tcl = -0.25,
      cex.lab = 1.3,
      cex.axis = 1.2,
      mgp   = c(3.5, 0.7, 0))
  
  bp <- barplot(area_tab$relativity,
                names.arg = area_tab$area,
                ylab = "Relative claim frequency",
                col = "gray",
                ylim = c(0, 1.06))
  
  abline(h = 1, lty = 2, lwd = 2.2, col = "#8d17f1")
  
  text(x = bp,
       y = area_tab$relativity + 0.01, # just above bar
       labels = sprintf("%.3f", area_tab$relativity), # formatting as strings, with fixed-point number with 3 d.p's.
       cex = 1.1)
}
# This plot is mildly informative, but doesn't display the difference well, trying a different method.
# Approximate standard error for the frequency under Poisson ~ Yes we mighn't stick this 
# model, but worth testing. Time constraint make testing everything implausible.
# var(claims) is approximately claims  =>  var(freq) approx claims / exposure^2
se_freq  <- sqrt(area_tab$claims) / area_tab$exposure

# Convert to relativities (divide by overall_freq)
se_rel <- se_freq / overall_freq
lower_rel <- area_tab$relativity - 1.96 * se_rel
upper_rel <- area_tab$relativity + 1.96 * se_rel

x <- 1:nrow(area_tab)

y_min <- min(lower_rel) - 0.002 # small padding below
y_max <- max(upper_rel) + 0.002 # small padding above

# Much better visualisation 
{
  par(mfrow = c(1,1),
      xaxs  = "r", yaxs = "i",
      mar = c(5.5, 5.5, 3, 1),
      tcl = -0.25,
      cex.lab  = 1.3,
      cex.axis = 1.2,
      mgp = c(3.5, 0.7, 0))
  
  plot(x, area_tab$relativity,
       ylim = c(y_min, y_max),
       xlab = "",
       ylab = "Relative claim frequency",
       xaxt = "n",
       pch = 16)
  
  grid()
  
  arrows(x0 = x, y0 = lower_rel,
         x1 = x, y1 = upper_rel,
         angle = 90, code = 3, length = 0.06, lwd = 2)
  
  axis(1, at = x, labels = area_tab$area)
  abline(h = 1, lty = 2, lwd = 2, col = "#8d17f1")
}



###############################################################################
# 3). LRT to Check Inclusion of Area
###############################################################################
mod0 <- glm(n_claims ~ offset(log(exposure)),
               family = poisson,
               data = policy_frequency)

mod_area <- glm(n_claims ~ area + offset(log(exposure)),
                family = poisson,
                data = policy_frequency)

anova(mod0, mod_area, test = "Chisq")

