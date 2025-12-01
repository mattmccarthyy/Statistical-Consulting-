rm(list = ls())

library(tidyverse)

claims_severity <- readRDS(url("https://github.com/mattmccarthyy/Statistical-Consulting-/raw/refs/heads/main/data/processed/claims_severity.rds")) 
glimpse(claims_severity)



###############################################################################
# 1). Quick Summaries of age and gross_amount
###############################################################################
summary(claims_severity$age)
summary(claims_severity$gross_amount)

# Checking tail (helps interpret later plots)
quantile(claims_severity$gross_amount, probs = c(0.5, 0.75, 0.9, 0.95, 0.99, 0.9999))



###############################################################################
# 2). Aggregating severity by exact age
###############################################################################
# Mean severity by age
age_tab <- aggregate(gross_amount ~ age,
                     data = claims_severity,
                     FUN = mean)
names(age_tab)[2] <- "mean_sev"

# Median severity by age
age_tab$median_sev <- aggregate(gross_amount ~ age,
                                data = claims_severity,
                                FUN = median)$gross_amount

# Number of claims at each age
age_tab$n_claims <- aggregate(gross_amount ~ age,
                              data = claims_severity,
                              FUN = length)$gross_amount



###############################################################################
# 3). Plotting number of claims by age (checks data support per age)
###############################################################################
{
  par(mfrow = c(1, 1),
      xaxs  = "i", yaxs = "i",
      mar = c(5.5, 5.5, 3, 1),
      tcl = -0.25,
      cex.main = 1.5,
      cex.lab = 1.3,
      cex.axis = 1.2,
      col = "black",
      mgp = c(3.5, 0.7, 0))
  
  plot(age_tab$age, age_tab$n_claims,
       type = "h",
       xlab = "Age",
       ylab = "Number of claims",
       ylim = c(0, 2500))
  
  grid()
}

# Ages with very few claims => noisy mean/median severity.



###############################################################################
# 4). Plot: mean severity vs age with smooth (non-linearity check)
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
  
  plot(age_tab$age, age_tab$mean_sev,
       xlab = "Age",
       ylab = "Mean gross claim amount",
       pch = 16,
       xlim = c(15, 95))
  
  grid()
  
  lines(lowess(age_tab$age, age_tab$mean_sev, f = 0.6),
        lwd = 2)
}
# Lowess is relatively straight, it may make sense to leave as a linear term.
# Will consider respective AIC's (and BIC in this case) as overfitting to the sparse
# higher ages is a real concern. 



###############################################################################
# 5). Plot: mean vs median severity by age (skew / outliers by age)
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
  
  plot(age_tab$age, age_tab$median_sev,
       xlab = "Age",
       ylab = "Severity",
       pch = 16)
  
  grid()
  
  points(age_tab$age, age_tab$mean_sev, pch = 1)
  
  legend("topright",
         legend = c("Median", "Mean"),
         pch = c(16, 1),
         bty = "n",
         cex = 1.3)
}
# Mean >> Median across ages => heavy right tail (few large claims).
# If the gap between mean and median widens for some ages =>
# tail heaviness varies with age (age interacts with tail risk).
# Not really the case here. Sparsity in upper tail may cause issues.



###############################################################################
# 6). Boxplots of severity by age band (distributional differences)
###############################################################################
claims_severity$age_band <- cut(claims_severity$age,
                                breaks = c(17, 25, 35, 45, 55, 65, Inf),
                                right = FALSE,
                                include.lowest = TRUE)
{
  par(mfrow = c(1, 1),
      xaxs  = "i", yaxs = "i",
      mar = c(6.5, 6.5, 3, 1),
      tcl = -0.25,
      cex.main = 1.5,
      cex.lab = 1.3,
      cex.axis = 1.2,
      col = "black",
      mgp = c(3.5, 0.7, 0))
  
  nb <- length(levels(claims_severity$age_band))
  col_grad <- colorRampPalette(c("#d0aeee", "#8d17f1"))(nb) # Making these look professional for report. Too monotone for me at the moment.
  
  boxplot(gross_amount ~ age_band,
          data = claims_severity,
          outline = TRUE,
          xlab = "Age band",
          ylab = "Gross claim amount",
          ylim = c(0, 5e4),
          col = col_grad) # see comment below for justification of this.
}

# Large amounts are ruining any informative aspect of this plot. 
# Reducing ylim, but noting that these higher amounts do exist.
# Median and IQR both increase with age. Again effect of sparsity in later ages.
# Decent monotone trend => considering a linear / smooth age effect.
# Not much supporting spline inclusion at this point.



###############################################################################
# 7). Modelling age as covariate, factor, and spline (Gamma GLM's)
###############################################################################
library(splines) 

# Age as linear covariate
m_lin <- glm(gross_amount ~ age,
             family = Gamma(link = "log"),
             data = claims_severity)
summary(m_lin)

# Age as factor via age_band
m_fac <- glm(gross_amount ~ age_band,
             family = Gamma(link = "log"),
             data = claims_severity)
summary(m_fac)

# Age as spline (initial df choice, used df = 6, ended up being best choice anyway ahah)
m_spl <- glm(gross_amount ~ ns(age, df = 6),
             family = Gamma(link = "log"),
             data = claims_severity)
summary(m_spl)

###############################################################################
# 8). Model comparison, covariate vs factor vs spline
###############################################################################
# AIC comparison (lower is better)
AIC(m_lin, m_fac, m_spl)
BIC(m_lin, m_fac, m_spl) # Penalising for the extra df's, not for report, we are basing off AIC's.



###############################################################################
# 9). Choosing spline degrees of freedom for age
###############################################################################
dfs  <- 2:10
mods <- vector("list", length(dfs))
aic  <- numeric(length(dfs))

for (i in seq_along(dfs)) {
  k <- dfs[i]
  mods[[i]] <- glm(gross_amount ~ ns(age, df = k),
                   family = Gamma(link = "log"),
                   data = claims_severity)
  aic[i] <- AIC(mods[[i]])
}

cbind(df = dfs, AIC = aic)

# Identify df with minimum AIC
min_aic  <- min(aic)

tol <- 2 # Tolerance, as df minimising AIC is just biggest df ow.
# We are avoiding overfitting here, staying conservative. 

# AIC differences relative to min
delta_aic <- aic - min_aic

# Among models within tolerance, choose smallest df
ok_idx  <- which(delta_aic <= tol)
best_df <- dfs[min(ok_idx)]
best_mod <- mods[[min(ok_idx)]]

best_df
AIC(m_lin, best_mod)
# Can't do regular LRT, models are not nested. 
# AIC and visual comparison will suffice.
BIC(m_lin, best_mod) # Just out of interest, not too informative here. 



###############################################################################
# 10). Plotting AIC vs spline degrees of freedom (overfitting check)
###############################################################################
{
  par(mfrow = c(1, 1),
      xaxs  = "i", yaxs = "i",
      mar = c(5.5, 5.5, 3, 1),
      tcl = -0.25,
      cex.main = 1.5,
      cex.lab = 1.3,
      cex.axis = 1.2,
      col = "black",
      mgp = c(3.5, 0.7, 0))
  
  plot(dfs, aic,
       type = "l",
       xlab = "Spline degrees of freedom for age",
       ylab = "AIC",
       lwd = 2)
  
  grid()
  
  points(dfs, aic, pch = 19, col = "#8d17f1")
  
  abline(v = best_df, lty = 2, lwd = 2.2)
  text(best_df, min_aic + 9, labels = paste("Chosen df =", best_df),
       pos = 4, offset = 0.5)
}



###############################################################################
# 11). Residuals vs. Age Check
###############################################################################
{
  par(mfrow = c(1, 1),
      xaxs = "i", yaxs = "i",
      mar  = c(5.5, 5.5, 3, 1),
      tcl  = -0.25,
      cex.main = 1.5,
      cex.lab  = 1.3,
      cex.axis = 1.2,
      col  = "black",
      mgp  = c(3.5, 0.7, 0))
  
  plot(claims_severity$age,
       residuals(best_mod, type = "pearson"),
       xlab = "Age",
       ylab = "Pearson residuals",
       pch  = 16,
       ylim = c(-2, 35))
  
  grid()
  
  abline(h = 0, lty = 2)
  
  
}
# No clear pattern => age as a spline with 6 df is adequate. 
# The large residuals are from large claims. Fromt the literature review
# we know that this is expected for Gamma.
