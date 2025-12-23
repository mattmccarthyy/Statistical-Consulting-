#===================================================================================================================================
# security device EDA
#===================================================================================================================================

# exporting a jpeg image
jpeg("Security_Device_plot.jpg", width = 1200, height = 900, res = 150)

# Preparing the data
df_sec <- subset(claims_severity, !is.na(security_device) & gross_amount > 0)
df_sec$security_device <- as.factor(df_sec$security_device)

# Split amounts by security device
grp <- split(df_sec$gross_amount, df_sec$security_device)

# Compute statistics
n  <- sapply(grp, length)
m  <- sapply(grp, mean)
se <- sapply(grp, sd) / sqrt(n)

lwr <- m - 1.96 * se
upr <- m + 1.96 * se

# plotting the means and 95% confidence intervals
plot(
  x = m,
  y = seq_along(m),
  pch = 16,
  yaxt = "n",
  xlab = "Mean Claim Severity (€)",
  ylab = "Security Device",
  main = "Mean Claim Severity by Security Device",
  xlim = range(c(lwr,upr))
)

axis(2, at = y, labels = names(m), las = 1)

# 95% CI bars
segments(lwr, seq_along(m), upr, seq_along(m), lwd = 2)
segments(lwr, seq_along(m) - 0.05, lwr, seq_along(m) + 0.05, lwd = 2) 
segments(upr, seq_along(m) - 0.05, upr, seq_along(m) + 0.05, lwd = 2)

grid(nx = NA, ny = NULL)

dev.off()

# null model for the Gamma glm
null_model_s = glm(net_amount ~1, family=Gamma(link="log"), data=claims_severity)
summary(null_model_s) #1645224

# Gamma glm model including secuirty device
model_2 = glm(net_amount ~ security_device, family=Gamma(link="log"), data=claims_severity)
summary(model_2) #1645144
