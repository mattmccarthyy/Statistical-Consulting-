claims_severity <- readRDS(url("https://github.com/mattmccarthyy/Statistical-Consulting-/raw/refs/heads/main/data/processed/claims_severity.rds"))
data<-claims_severity %>% select(vehicle_power,engine_cc,usage,gross_amount)
boxplot(log(gross_amount) ~ vehicle_power,data = data)

library(dplyr)

severity_summary <- data %>%
  group_by(vehicle_power) %>%
  summarise(
    Claims = n(),  # Number of claims (exposure)
    Mean_Severity = mean(gross_amount),
    Median_Severity = median(gross_amount),
    Std_Dev = sd(gross_amount),
    Relativity = Mean_Severity / mean(data$gross_amount)
  ) %>%
  arrange(desc(Mean_Severity))

print(severity_summary)

library(ggplot2)
library(scales)

# Create custom color palette
data$vehicle_power <- factor(data$vehicle_power, levels = c("Low", "Med", "High"))
custom_colors<-  c("High" = "#08306B", "Med" = "#2171B5", "Low" = "#6BAED6")
ggplot(data, aes(x = vehicle_power, y = gross_amount, fill = vehicle_power)) +
  geom_violin(alpha = 0.8, 
              scale = "width",           # Makes violins same width
              trim = TRUE,                # Trims tails to data range
              adjust = 1.5) +             # Controls smoothness (1.5 = smoother)
  geom_boxplot(width = 0.15, 
               fill = "white", 
               color = "black",
               outlier.shape = NA,        # Remove outliers (already in violin)
               alpha = 0.7) +
  stat_summary(fun = "mean", 
               geom = "point", 
               shape = 18, 
               size = 4, 
               color = "red") +
  scale_y_log10(labels = scales::dollar,
                breaks = c(1000, 5000, 10000, 50000, 100000)) +
  scale_fill_manual(values = custom_colors) +  # Apply custom colors
  labs(y = "Gross Claim Amount (log scale)",
       x = "Vehicle Power Category",
       fill = "Power Level") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank())

summary(claims_severity$vehicle_power)


vehicle_power_claims<-c(8690,30465,40490)
vehicle_power_claims_prop<-vehicle_power_claims/79645
vehicle_power_claims_prop

null_model<-glm(formula=gross_amount~1,data=data,family = Gamma(link = "log"))
power_model<-glm(formula=gross_amount~vehicle_power,data=data,family = Gamma(link = "log"))
summary(null_model)
summary(power_model)
