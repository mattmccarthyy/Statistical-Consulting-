#==================================================================================================================
# Overnight Parking EDA
#==================================================================================================================

# Preparing the data
df_pk = subset(claims_severity, !is.na(overnight_parking) & gross_amount > 0)
df_pk$overnight_parking = as.factor(df_pk$overnight_parking)

# Summary statistics
grp = split(df_pk$gross_amount, df_pk$overnight_parking)

summary_tab = data.frame(
  overnight_parking = names(grp),
  n      = sapply(grp, length),
  mean   = sapply(grp, mean),
  median = sapply(grp, median),
  sd     = sapply(grp, sd),
  q25    = sapply(grp, quantile, probs = 0.25),
  q75    = sapply(grp, quantile, probs = 0.75),
  row.names = NULL
)

summary_tab <- summary_tab[order(-summary_tab$mean), ]
print(summary_tab)

# Exporting a jpeg image
jpeg("Overnight_Parking_Boxplot.jpg", width = 1200, height = 900, res = 150)

# Log-scaled boxplot
par(mar = c(5, 6, 4, 2) + 0.1)
boxplot(
  gross_amount ~ overnight_parking,
  data = df_pk,
  log = "y",
  outline = FALSE,
  col = c("lightpink", "#abd9e9", "violetred2"),
  border = "grey30",
  ylab = "Claim Severity (€, log scale)",
  xlab = "Overnight Parking",
  main = "Claim Severity by Overnight Parking"
)
grid(nx = NA, ny = NULL)

dev.off()

# Gamma glm for the null model
null_model_1 = glm(net_amount ~1, family=Gamma(link="log"), data=claims_severity)
summary(null_model_1) #1645224

# Gamma glm for a model including overnight parking
model_op = glm(net_amount ~ overnight_parking, family=Gamma(link="log"), data=claims_severity)
summary(model_op) #AIC 1644737 
