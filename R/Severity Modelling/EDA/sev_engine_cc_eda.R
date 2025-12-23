claims_severity <- readRDS(url("https://github.com/mattmccarthyy/Statistical-Consulting-/raw/refs/heads/main/data/processed/claims_severity.rds"))
data<-claims_severity %>% select(vehicle_power,engine_cc,usage,gross_amount)


summary(data$engine_cc)
plot(density(data$engine_cc))
boxplot(engine_cc~vehicle_power,data=data)


par(mfrow = c(1, 1),
    xaxs  = "i", yaxs = "i", 
    mar   = c(5.5, 5.5, 3, 1),
    tcl = -0.25, 
    cex.main = 1.5,
    cex.lab  = 1.3,
    cex.axis = 1.2,
    col = "black",
    mgp = c(3.5, 0.7, 0))


# 1. Basic distribution plot
library(ggplot2)
#install.packages("patchwork")
library(patchwork)  # For arranging multiple plots


# Calculate summary statistics per bin
bin_summary <- data %>%
  group_by(engine_cc_bin) %>%
  summarise(
    n_claims = n(),
    mean_severity = mean(gross_amount),
    median_severity = median(gross_amount),
    sd_severity = sd(gross_amount),
    cv = sd_severity / mean_severity  # Coefficient of variation
  ) %>%
  mutate(relativity = mean_severity / mean(data$gross_amount))

print(bin_summary)


library(splines)
library(MASS)  # For Gamma GLM

# Create a sequence of df to test (1 = linear, up to reasonable max)
df_values <- 1:10

# Initialize storage for results
results <- data.frame(
  df = df_values,
  AIC = NA,
  BIC = NA,
  Deviance = NA,
  GCV = NA
)

# Fit Gamma GLMs with different df
for (i in seq_along(df_values)) {
  df <- df_values[i]
  
  # Fit the model with natural spline
  if (df == 1) {
    # Linear model (no spline)
    model <- glm(gross_amount ~ engine_cc,
                 family = Gamma(link = "log"),
                 data = data)
  } else {
    # Natural spline with specified df
    model <- glm(gross_amount ~ ns(engine_cc, df = df),
                 family = Gamma(link = "log"),
                 data = data)
  }
  
  # Store results
  results$AIC[i] <- AIC(model)
  results$BIC[i] <- BIC(model)
  results$Deviance[i] <- deviance(model)
  results$GCV[i] <- (deviance(model) / (nobs(model) - df))  # Generalized Cross-Validation approximation
}

# View results
print(results)

plot(results$df, results$BIC,
     pch = 19,  # Solid for optimal, hollow for others
     cex=2,
     col = "purple",
     xlab = "Degrees of Freedom (df)",
     ylab = "BIC",
     main = "",)
lines(results$df,results$BIC,lwd=3,col="black")
grid()
# Add vertical line at optimal df
abline(v = 3, col = "red", lty = "aa",lwd=2)




cc_spline_model1<-glm(gross_amount~ns(engine_cc,df=3),data = data,family = Gamma(link="log"))
cc_spline_model2<-glm(gross_amount~ns(engine_cc,df=10),data = data,family = Gamma(link="log"))
cc_linear<-glm(gross_amount~engine_cc,data = data,family = Gamma(link="log"))
cc_binned<-glm(gross_amount~engine_cc_bin,data = data,family = Gamma(link="log"))

# Create comparison data frame without binned model
simple_comparison <- data.frame(
  Model = c("Linear", "Spline (df=3)", "Spline (df=10)"),
  DOF = sapply(list(cc_linear, cc_spline_model1, cc_spline_model2), 
               function(m) length(coef(m))),
  AIC = round(sapply(list(cc_linear, cc_spline_model1, cc_spline_model2), 
                     AIC), 1),
  BIC = round(sapply(list(cc_linear, cc_spline_model1, cc_spline_model2), 
                     BIC), 1),
  ΔAIC_vs_Best = NA,
  ΔBIC_vs_Best = NA
)

# Calculate differences from best model
best_aic <- min(simple_comparison$AIC)
best_bic <- min(simple_comparison$BIC)
simple_comparison$ΔAIC_vs_Best <- simple_comparison$AIC - best_aic
simple_comparison$ΔBIC_vs_Best <- simple_comparison$BIC - best_bic

# Print the table
print(simple_comparison, row.names = FALSE)

bin_severity <- data %>%
  group_by(engine_cc_bin) %>%
  summarise(
    n = n(),
    proportion = n() / nrow(data),
    mean_severity = mean(gross_amount),
    median_severity = median(gross_amount),
    cv = sd(gross_amount) / mean(gross_amount)
  ) %>%
  arrange(mean_severity)

print(bin_severity)



# Compute density of log-transformed data
d <- density(log10(data$engine_cc))

# Create custom x-axis labels
x_breaks <- c(600, 800, 1000, 1500, 2000, 3000, 5000, 6000)
x_labels <- ifelse(x_breaks < 1000, 
                   paste0(x_breaks, "cc"), 
                   paste0(x_breaks/1000, "L"))
log10(600)
# Plot
plot(d, 
     main = "",
     xlab = "Engine Capacity (Log Scale)",
     ylab = "Density",
     xlim = c(log10(600),log10(max(data$engine_cc))),
     col = NA,  # No border initially
     xaxt = "n",  # No default x-axis
     yaxt = "n")  # No default y-axis
grid()
# Fill area
polygon(d, col = "#3B7EA1", border = NA)

# Add line
lines(d, col = "#003262", lwd = 1.5)

# Custom axes
axis(1, at = log10(x_breaks), labels = x_labels)
axis(2, las = 1)


