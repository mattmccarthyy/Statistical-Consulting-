rm(list = ls())
library(tidyverse)

policy_frequency <- readRDS(url("https://github.com/mattmccarthyy/Statistical-Consulting-/raw/refs/heads/main/data/processed/policy_frequency.rds")) 
glimpse(policy_frequency)
attach(policy_frequency)

str(policy_frequency$occupation)
length(unique(occupation))


###############################################################################
# 1). EDA: claim frequency by occupation
###############################################################################
occ_tab <- aggregate(
  cbind(claims = n_claims,
        exposure = exposure) ~ occupation,
  data = policy_frequency,
  FUN  = sum
)

occ_tab$freq <- with(occ_tab, claims / exposure)

overall_freq <- sum(policy_frequency$n_claims) / sum(policy_frequency$exposure)

occ_tab$relativity <- occ_tab$freq / overall_freq

# Sort by relativity (high risk first)
occ_tab <- occ_tab[order(-occ_tab$relativity), ]

head(occ_tab, 10)

{
  par(mfrow = c(1, 1),
      xaxs  = "i", yaxs = "i",
      mar = c(7.5, 5.5, 3, 1),
      tcl = -0.25,
      cex.lab  = 1.3,
      cex.axis = 1.3,
      col  = "black",
      mgp = c(3.5, 0.7, 0))
  
  bp <- barplot(occ_tab$relativity,
                names.arg = FALSE,
                las = 2, 
                ylab = "Relative claim frequency",
                col = "gray")
  
  ymin <- par("usr")[3]
  
  text(x = bp,
       y = ymin - 0.02 * diff(par("usr")[3:4]),
       labels = occ_tab$occupation,
       srt = 45,       # rotation angle (45° = diagonal)
       adj = 1,        # right-justify on the tick
       xpd = TRUE,     # allow drawing outside plot region
       cex = 1.3)
  
  abline(h = 1, lty = 2, lwd = 2.2, col = "#8d17f1")
}


###############################################################################
# 2). Cluster relativities into 5 risk groups (k-means)
###############################################################################
set.seed(100)

log_rel <- log(occ_tab$relativity)

km_occ <- kmeans(log_rel, centers = 5)

occ_tab$cluster <- km_occ$cluster

clust_means <- tapply(occ_tab$relativity, occ_tab$cluster, mean)
clust_rank  <- rank(clust_means, ties.method = "first")  # 1..5

occ_tab$risk_bucket <- clust_rank[occ_tab$cluster]

risk_labels <- c("Very Low", "Low", "Medium", "High", "Very High")
occ_tab$risk_label <- factor(occ_tab$risk_bucket,
                             levels = 1:5,
                             labels = risk_labels)

aggregate(relativity ~ risk_label, data = occ_tab, mean)



###############################################################################
# 3). Map 5-level occupation risk factor back to policies
###############################################################################
occupation_risk5 <- occ_tab$risk_label[
  match(policy_frequency$occupation, occ_tab$occupation)
]

policy_frequency$occupation_risk5 <- occupation_risk5

table(policy_frequency$occupation_risk5)



###############################################################################
# 4). Checking frequency by 5 occupation risk buckets
###############################################################################

occ_risk_tab <- aggregate(
  cbind(claims = n_claims,
        exposure = exposure) ~ occupation_risk5,
  data = policy_frequency,
  FUN  = sum
)

occ_risk_tab$freq <- with(occ_risk_tab, claims / exposure)
occ_risk_tab

{
  par(mfrow = c(1, 1),
      xaxs  = "i", yaxs = "i",
      mar   = c(5.5, 5.5, 3, 1),
      tcl   = -0.25,
      cex.lab  = 1.3,
      cex.axis = 1.2,
      col  = "black",
      mgp = c(3.5, 0.7, 0))
  
  plot(occ_risk_tab$occupation_risk5, occ_risk_tab$freq,
       xlab = "Occupation risk bucket (1 = Very Low, 5 = Very High)",
       ylab = "Observed claim frequency",
       pch = 16,
       ylim = c(0.25, 0.32))
  
  grid()
  
  lines(1:5, occ_risk_tab$freq[order(occ_risk_tab$occupation_risk5)], type = "b")
  abline(h = overall_freq, lty = 2, col = "#8d17f1", lwd = 2.2) 
}




###############################################################################
# 5). Checking frequency by 5 occupation risk buckets
###############################################################################
mod_orig <- glm(n_claims ~ occupation,
                family = poisson,
                data = policy_frequency)

mod_occ5 <- glm(n_claims ~ occupation_risk5,
                family = poisson,
                data = policy_frequency)

# For comparison - not a formal test just figures to quote and compare
1 - (mod_occ5$deviance / mod_occ5$null.deviance)
1 - (mod_orig$deviance / mod_orig$null.deviance)




###############################################################################
# 6). Saving Key to Include in Original Dataset
###############################################################################
dir.create("data/derived", recursive = TRUE, showWarnings = FALSE)

occ_lookup <- occ_tab[, c("occupation", "risk_label")]
names(occ_lookup)[2] <- "occupation_risk5"

write.csv(
  occ_lookup,
  file = "data/derived/occupation_risk5_lookup.csv",
  row.names = FALSE
)
