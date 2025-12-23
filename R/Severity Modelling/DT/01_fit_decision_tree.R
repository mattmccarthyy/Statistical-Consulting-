################################################################################
# Interpretable Decision Tree
################################################################################
rm(list = ls())

library(tidyverse)
library(rpart)
library(rpart.plot)

set.seed(123)

claims_severity <- readRDS(
  url("https://github.com/mattmccarthyy/Statistical-Consulting-/raw/refs/heads/main/data/processed/claims_severity.rds")
)

glimpse(claims_severity)

################################################################################
## 1). Occupation aggregation into 5 risk bands
################################################################################
# a). Exposure / severity by original occupation
occ_summary <- claims_severity |>
  group_by(occupation) |>
  summarise(
    n_claims = n(),
    tot_exposure = sum(exposure),
    mean_sev = mean(gross_amount),
    .groups = "drop"
  ) |>
  arrange(n_claims)
occ_summary

# b). Occupation-only Gamma GLM to get relativities
fit_occ <- glm(
  gross_amount ~ occupation,
  family = Gamma(link = "log"),
  data = claims_severity
)

coefs <- summary(fit_occ)$coefficients
occ_rows <- grep("^occupation", rownames(coefs))
occ_coef <- coefs[occ_rows, , drop = FALSE]

beta  <- occ_coef[, "Estimate"]
se <- occ_coef[, "Std. Error"]
rel <- exp(beta)
low95 <- exp(beta - 1.96 * se)
upp95 <- exp(beta + 1.96 * se)

occ_names <- sub("^occupation", "", rownames(occ_coef))

occ_glm <- data.frame(
  occupation = occ_names,
  estimate = beta,
  std_error = se,
  rel = rel,
  lower95 = low95,
  upper95 = upp95
)

base_row <- data.frame(
  occupation = "Accountant",
  estimate = 0,
  std_error = 0,
  rel = 1,
  lower95 = 1,
  upper95 = 1
)

occ_glm_full <- rbind(base_row, occ_glm[order(occ_glm$rel), ])
rownames(occ_glm_full) <- NULL
occ_glm_full

# c). Manual grouping into 5 bands based on relativities + judgement
occ_group <- as.character(claims_severity$occupation)

very_low <- c("Secondary Teacher", "Primary Teacher", "Unemployed")
low <- c("Other", "Nurse")
below <- c("Lecturer", "Retail", "Garda")
avg <- c("Farmer", "Accountant", "Actuary")
high <- c("Skilled Trades", "Doctor", "Driver/Delivery")

occ_group[occ_group %in% very_low] <- "Very Low"
occ_group[occ_group %in% low] <- "Low"
occ_group[occ_group %in% below] <- "Below Average"
occ_group[occ_group %in% avg] <- "Average"
occ_group[occ_group %in% high] <- "High"

claims_severity$occ_group5 <- factor(
  occ_group,
  levels = c("Very Low", "Low", "Below Average", "Average", "High")
)

# Quick checks
table(claims_severity$occ_group5)
tapply(claims_severity$exposure, claims_severity$occ_group5, sum)
tapply(claims_severity$gross_amount, claims_severity$occ_group5, mean)

# Compare original vs grouped occupation-only Gamma GLM
fit5 <- glm(
  gross_amount ~ occ_group5,
  family = Gamma(link = "log"),
  data = claims_severity
)
summary(fit5)
anova(fit5, fit_occ, test = "Chisq")


################################################################################
# 2). Train / test split
################################################################################
set.seed(100)
n <- nrow(claims_severity)
train_idx <- sample(seq_len(n), size = floor(0.7 * n))

train <- claims_severity[train_idx, ]
test <- claims_severity[-train_idx, ]


################################################################################
# 3). Interpretable decision tree on log(severity)
################################################################################

fit_tree <- rpart(
  log(gross_amount) ~ occ_group5 + age + ncd_level + vehicle_value + engine_cc +
    area + province + overnight_parking + usage + marital + employment + fuel +
    vehicle_age + reported_mileage + employment_missing +
    reported_mileage_missing + engine_cc_missing,
  data = train,
  method = "anova",
  control = rpart.control(
    maxdepth = 3, # shallow, interpretable
    minbucket = 800, # stable leaves
    minsplit = 1600,
    cp = 0.001 # base cp, will prune with 1-SE
  )
)

# Cross-validated cp table and 1-SE pruning
printcp(fit_tree)

cp_tab <- fit_tree$cptable
min_row <- which.min(cp_tab[, "xerror"])
x_min <- cp_tab[min_row, "xerror"]
x_std <- cp_tab[min_row, "xstd"]

cp_1se <- cp_tab[cp_tab[, "xerror"] <= x_min + x_std, "CP"][1]

fit_tree_pruned <- prune(fit_tree, cp = cp_1se)

par(mfrow = c(1, 2))
rpart.plot(fit_tree, main = "Unpruned tree")
rpart.plot(fit_tree_pruned, main = "Pruned tree (final)")

par(mfrow = c(1, 1))


################################################################################
# 4). Compare pruned tree vs Gamma GLM on test set
################################################################################
fit_glm <- readRDS("https://github.com/mattmccarthyy/Statistical-Consulting-/raw/refs/heads/main/R/Severity%20Modelling/Gamma/gamma_GLM.rds")
# Gamma GLM from severity Modelling Script

pred_glm <- predict(fit_glm, newdata = test, type = "response")

rmse_glm <- sqrt(mean((test$gross_amount - pred_glm)^2))
mae_glm <- mean(abs(test$gross_amount - pred_glm))

pred_tree <- exp(predict(fit_tree_pruned, newdata = test))

rmse_tree <- sqrt(mean((test$gross_amount - pred_tree)^2))
mae_tree<- mean(abs(test$gross_amount - pred_tree))

cbind(
  rmse_glm = rmse_glm,
  rmse_tree = rmse_tree,
  mae_glm = mae_glm,
  mae_tree = mae_tree
)

