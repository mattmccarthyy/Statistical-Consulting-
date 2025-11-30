policy_frequency <- readRDS(url("https://github.com/mattmccarthyy/Statistical-Consulting-/raw/refs/heads/main/data/processed/policy_frequency.rds")) 

attach(policy_frequency)

table(province)
tab <- table(province, n_claims)

# number of claims contributed by each cell = n_claims * number of policies
claims_per_province <- rowSums(
  sweep(tab, 2, as.numeric(colnames(tab)), `*`)
)

# total number of claims in the portfolio
total_claims <- sum(claims_per_province)

# proportion of all claims from each province
prop_claims_province <- claims_per_province / total_claims
prop_claims_province

# number of counties
n_counties <- c(
  Connacht = 5,
  Leinster = 12,
  Munster  = 6,
  Ulster   = 9
)

# per-county proportion
prop_per_county <- prop_claims_province / n_counties
prop_per_county

## 1. Vector of counties, ordered by province
counties <- c(
  # Connacht (5)
  "Galway", "Leitrim", "Mayo", "Roscommon", "Sligo",
  # Leinster (12)
  "Carlow","Dublin","Kildare","Kilkenny","Laois","Longford",
  "Louth","Meath","Offaly","Westmeath","Wexford","Wicklow",
  # Munster (6)
  "Clare","Cork","Kerry","Limerick","Tipperary","Waterford",
  # Ulster (9: ROI + NI)
  "Antrim","Armagh","Cavan","Donegal","Down","Fermanagh",
  "Derry","Monaghan","Tyrone"
)

## 2. Matching province for each county
province_for_county <- c(
  rep("Connacht", 5),
  rep("Leinster", 12),
  rep("Munster",  6),
  rep("Ulster",   9)
)

## 3. Look up per-county proportion for each province
##    (prop_per_county must be a named vector with those 4 province names)
prop_claims_county <- prop_per_county[province_for_county]

## 4. Final Datawrapper table
ireland_map_df <- data.frame(
  county        = counties,
  province      = province_for_county,
  prop_claims   = prop_claims_county,
  stringsAsFactors = FALSE
)

ireland_map_df$prop_claims <- ireland_map_df$prop_claims*100 

## 5. Write CSV for Datawrapper upload
write.csv(ireland_map_df,
          "ireland_province_claim_proportions.csv",
          row.names = FALSE)

# Checking if number of claims varies with province for report.
fit_prov  <- glm(n_claims ~ province + offset(log(exposure)),
                 family = poisson, data = policy_frequency)

# Null model (no province effect)
fit_null  <- glm(n_claims ~ 1 + offset(log(exposure)),
                 family = poisson, data = policy_frequency)

# Likelihood ratio test: H0 = frequency does NOT depend on province
anova(fit_null, fit_prov, test = "Chisq")

