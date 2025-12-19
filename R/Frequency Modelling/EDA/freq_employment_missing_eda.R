rm(list = ls())

library(tidyverse)

policy_frequency <- readRDS(
  url("https://github.com/mattmccarthyy/Statistical-Consulting-/raw/refs/heads/main/data/processed/policy_frequency.rds")
)
glimpse(policy_frequency)



###############################################################################
# 1). Exposure and claims by employment_missing
###############################################################################
# 0 = employment recorded, 1 = employment missing
table(policy_frequency$employment_missing)

# Label for readability
policy_frequency$employment_missing_f <- factor(
  policy_frequency$employment_missing,
  levels = c(0, 1),
  labels = c("Recorded", "Missing")
)

emp_tab <- aggregate(
  cbind(claims = n_claims, exposure = exposure) ~ employment_missing_f,
  data = policy_frequency,
  FUN = sum
)

# Portfolio claim frequency
overall_freq <- with(policy_frequency, sum(n_claims) / sum(exposure))

# Frequency and relativities
emp_tab$claim_freq <- emp_tab$claims / emp_tab$exposure
emp_tab$relativity <- emp_tab$claim_freq / overall_freq

emp_tab 



###############################################################################
# 2). Barplot of claim frequency by employment_missing
###############################################################################
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
  
  nb <- nrow(emp_tab)
  col_grad <- colorRampPalette(c("#d0aeee", "#8d17f1"))(nb)
  
  bar_centres <- barplot(
    emp_tab$claim_freq,
    names.arg = emp_tab$employment_missing_f,
    xlab = "Employment data availability",
    ylab = "Observed claim frequency",
    ylim = c(0, max(emp_tab$claim_freq) * 1.1),
    col = col_grad,
    las = 1
  )
  
  abline(h = overall_freq, lty = 2, lwd = 2.2)  # portfolio average
}



###############################################################################
# 3). Any-claim incidence by employment_missing
###############################################################################
any_claim <- as.integer(policy_frequency$n_claims > 0)

incidence_tab <- table(
  employment_missing = policy_frequency$employment_missing_f,
  any_claim = any_claim
)
incidence_tab
prop.table(incidence_tab, margin = 1)  # within-group incidence

chisq.test(incidence_tab)  # association between missingness and claim incidence



###############################################################################
# 4). Checking if the flag adds signal (again assuming Poisson)
###############################################################################
m_emp_flag <- glm(n_claims ~ employment_missing_f + offset(log(exposure)),
  family = poisson,
  data   = policy_frequency
)
summary(m_emp_flag)

exp(coef(m_emp_flag)["employment_missing_fEmployment missing"])
