#================================================================================================================================================
# Area EDA
#================================================================================================================================================

claims_severity <- readRDS(url("https://github.com/mattmccarthyy/Statistical-Consulting-/raw/refs/heads/main/data/processed/claims_severity.rds")) 
view(claims_severity)

# Preparing the data
df_area = claims_severity[!is.na(claims_severity$area) & claims_severity$gross_amount > 0, ]

# Export a jpeg image
jpeg("Area_Boxplot.jpg", width = 1200, height = 900, res = 150)

# Log-scale boxplot
boxplot(
  gross_amount ~ area,
  data = df_area,
  log = "y",
  col = c("lightpink", "#abd9e9", "violetred2"),
  border = "grey30",
  outline = FALSE,
  ylab = "Claim Severity (€ , log scale)",
  xlab = "Area",
  main = "Claim Severity by Area"
)

# Add grid
grid(nx = NA, ny = NULL)

dev.off()

#creating null model for Gamma glm
null_model = glm(net_amount ~1, family=Gamma(link="log"), data=claims_severity)
summary(null_model_s) #1,645,224

# Gamma glm model including area
model_a = glm(net_amount ~ area, family=Gamma(link="log"), data=claims_severity)
summary(model_a) #1,644,525        




# Turn area into a factor
df_area$area = factor(df_area$area)

# Split severity by area
sev_by_area = split(df_area$gross_amount, df_area$area)

# Basic statistics
stats_area = data.frame(
  area   = names(sev_by_area),
  n      = sapply(sev_by_area, length),
  mean   = sapply(sev_by_area, mean),
  median = sapply(sev_by_area, median),
  sd     = sapply(sev_by_area, sd)
)

# Show results
print(stats_area)
