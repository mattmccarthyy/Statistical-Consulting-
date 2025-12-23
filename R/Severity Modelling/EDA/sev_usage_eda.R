#Usage EDA
library(dplyr)
claims_severity <- readRDS(url("https://github.com/mattmccarthyy/Statistical-Consulting-/raw/refs/heads/main/data/processed/claims_severity.rds"))
data<-claims_severity %>% select(vehicle_power,engine_cc,usage,gross_amount)
str(data)
summary(data)
# 1. Univariate summary
usage_counts <- table(data$usage)
print(usage_counts)
print(prop.table(usage_counts))

# 2. Severity by usage summary
usage_summary <- data.frame(
  Usage = levels(data$usage),
  Claims = as.numeric(usage_counts),
  Proportion = as.numeric(prop.table(usage_counts)),
  Mean = tapply(data$gross_amount, data$usage, mean),
  Median = tapply(data$gross_amount, data$usage, median)
)
usage_summary$Relativity <- usage_summary$Mean / mean(data$gross_amount)
print(usage_summary)

# Calculate exact summary statistics
usage_stats <- data %>%
  group_by(usage) %>%
  summarise(
    Claims = n(),
    Proportion = n() / nrow(data),
    Mean = mean(gross_amount),
    Median = median(gross_amount),
    Relativity = Mean / mean(data$gross_amount),
    SE = sd(gross_amount) / sqrt(Claims),
    CI_lower = Mean - 1.96 * SE,
    CI_upper = Mean + 1.96 * SE,
    Rel_CI_lower = CI_lower / mean(data$gross_amount),
    Rel_CI_upper = CI_upper / mean(data$gross_amount)
  )

print(usage_stats)
# Simple Plot 1: Bar plot of claim counts
par(mfrow = c(1, 1),
    xaxs  = "i", yaxs = "i", 
    mar   = c(5.5, 5.5, 3, 1),
    tcl = -0.25, 
    cex.main = 1.5,
    cex.lab  = 1.3,
    cex.axis = 1.2,
    col = "black",
    mgp = c(3.5, 0.7, 0))


# Simple Plot 2: Boxplots of gross_amount by usage
boxplot(log10(gross_amount) ~ usage, data = data,
        main = "Claim Severity Distribution by Usage",
        xlab = "Usage", 
        ylab = "Gross Amount (EUR)",
        col = "skyblue")
abline(h = mean(data$gross_amount), col = "red", lty = 2, lwd = 2)

# 3. Statistical testing

null_model <- glm(gross_amount ~ 1, family = Gamma(link = "log"), data = data)
usage_model <- glm(gross_amount ~ usage, family = Gamma(link = "log"), data = data)

# Likelihood ratio test
lrt <- anova(null_model, usage_model, test = "LRT")
print(lrt)

# Model comparison
#using AIC
cat("Null model AIC:", AIC(null_model), "\n")
cat("Usage model AIC:", AIC(usage_model), "\n")
cat("ΔAIC:", AIC(null_model) - AIC(usage_model), "\n")

coefs <- coef(usage_model)
print(coefs)

#relativities
relativities <- exp(coefs)
print(relativities)

