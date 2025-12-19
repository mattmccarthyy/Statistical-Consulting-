rm(list = ls())
library(tidyverse)
library(splines)

policy_frequency <- readRDS(url("https://github.com/mattmccarthyy/Statistical-Consulting-/raw/refs/heads/main/data/processed/policy_frequency.rds")) 
glimpse(policy_frequency)

##############################################################################
# Examining Relationship between Age and Number of Claims
##############################################################################
age_tab <- aggregate(cbind(claims = n_claims,
                           exposure = exposure) ~ age,
                     data = policy_frequency,
                     FUN = sum)

# Claim frequency per age
age_tab$freq <- age_tab$claims / age_tab$exposure

par(mfrow = c(1, 1),
    xaxs = "i", yaxs = "i", 
    mar = c(5.5, 5.5, 3, 1),
    tcl = -0.25, 
    cex.main = 1.5,
    cex.lab = 1.3,
    cex.axis = 1.2,
    col = "black",
    mgp = c(3.5, 0.7, 0))

plot(age_tab$age, age_tab$claims,
     type = "h",  # vertical lines
     xlab = "Age",
     ylab = "Total number of claims",
     main = "Total claims by age")
grid()

## Plot: claim frequency vs age with smooth
{
  par(mfrow = c(1, 1),
      xaxs  = "i", yaxs = "i", 
      mar = c(5.5, 5.5, 3, 1),
      tcl = -0.25, 
      cex.main = 1.5,
      cex.lab  = 1.3,
      cex.axis = 1.2,
      col = "black",
      mgp = c(3.5, 0.7, 0))
  
  plot(age_tab$age, age_tab$freq,
       xlab = "Age",
       ylab = "Observed claim frequency",
       pch = 16,
       xlim = c(15, 95),
       ylim = c(0, 0.8))
  lines(lowess(age_tab$age, age_tab$freq, f = 0.6), col = "#8d17f1", lwd = 2.2)
  grid()
}
dev.off()



##############################################################################
# Examining if Age should be a Covariate, Factor or Spline (Fitting GLM's)
##############################################################################
# Age as Covariate
m_lin <- glm(n_claims ~ age + offset(log(exposure)),
             family = poisson,
             data = policy_frequency)

# Age as a factor
policy_frequency$age_band <- cut(policy_frequency$age,
                                 breaks = c(17, 25, 35, 45, 55, 65, Inf),
                                 right  = FALSE,
                                 include.lowest = TRUE)

m_fac <- glm(n_claims ~ age_band + offset(log(exposure)),
             family = poisson,
             data = policy_frequency)

# Age as a spline
m_spl <- glm(n_claims ~ ns(age, df = 6) + offset(log(exposure)),
             family = poisson,
             data = policy_frequency)
summary(m_spl)


##############################################################################
# Comparing Model Fits
##############################################################################
## AIC comparison
AIC(m_lin, m_fac, m_spl)

## Linear vs age-band
anova(m_lin, m_fac, test = "Chisq")

## Linear vs spline
anova(m_lin, m_spl, test = "Chisq")

## Age-band vs spline
anova(m_fac, m_spl, test = "Chisq")

## (For non-nested models, also compare AIC as a sanity check)
AIC(m_lin, m_fac, m_spl)

# Spline is undoubtedly the best choice, now to decide how many degrees of freedom



############################################################
## Spline testing for Age – AIC vs degrees of freedom
############################################################
dfs <- 2:16
mods <- vector("list", length(dfs))
aic <- numeric(length(dfs))

for (i in seq_along(dfs)) {
  k <- dfs[i]
  mods[[i]] <- glm(
    n_claims ~ splines::ns(age, df = k) + offset(log(exposure)),
    family = poisson,
    data = policy_frequency
  )
  aic[i] <- AIC(mods[[i]])
}

## AIC table (optional to print in console)
cbind(df = dfs, AIC = aic)

## Plot for report
{
  par(mfrow = c(1, 1),
      xaxs  = "i", yaxs = "i", 
      mar   = c(5.5, 5.5, 3, 1),
      tcl = -0.25, 
      cex.main = 1.5,
      cex.lab  = 1.3,
      cex.axis = 1.2,
      col = "black",
      mgp = c(3.5, 0.7, 0))
  plot(dfs, aic,
       type  = "l",
       xlab = "Spline Degrees of Freedom",
       ylab = "AIC",
       ylim = c(394345, 394500),
       pch = 16,
       col = "black",
       lwd = 2,
       yaxt = "n")
  grid()
  
  points(dfs, aic,
         pch = 19,                
         col = "#8d17f1",
         cex = 1.35)   
  
  abline(v = 8, lty = 2, lwd = 2)
  text(8, min(aic) + 40, labels = "Initial chosen df = 8", pos = 4)
  
  yticks <- axTicks(2)  # default tick positions on y-axis
  axis(2, at = yticks,
       labels = formatC(yticks, format = "d", big.mark = ","))  # 394,480
}



## Checking which minimised AIC
min_aic <- min(aic)
best_df <- dfs[which.min(aic)]
best_df
best_model <- mods[[which.min(aic)]]
# 15 and we would just overfit, plateau at 8 is reasonable. 