# Motor Insurance Dataset — Initial Cut

This drop contains two CSV files suitable for initial exploratory data analysis (EDA).
Rows cover calendar years 2018–2022 for a subset of policies. There is a small amount of missing data.

## Files
- `motor_policy_year_initial_2025-10-08.csv` — policy-year level table
- `motor_claims_initial_2025-10-08.csv` — per-claim table

## Granularity
- **Policy-year table**: one row per policy_id × calendar year, including exposure, claim counts, and key attributes.
- **Claims table**: one row per claim with occurrence date (if present) and claim amount.

## Column dictionary — policy-year

| Column | Description |
|---|---|
| policy_id | Anonymised policy identifier |
| cal_year | Calendar year |
| exposure | Earned exposure in year (years, 0–1) |
| n_claims | Number of claims occurring in the year |
| sum_net | Sum of net claim amounts in the year (if any) |
| avg_net | Average net claim amount in the year (if any) |
| age | Driver age at year |
| years_licensed | Years since licence at year |
| gender | Driver gender (M/F) |
| marital | Marital status |
| employment | Employment status |
| occupation | Occupation group |
| area | Area type (Rural/Suburban/Urban) |
| usage | Vehicle use (Private/Commuting/Commercial) |
| transmission | Transmission (M/A) |
| fuel | Fuel type |
| vehicle_power | Vehicle power band (Low/Med/High) |
| vehicle_age | Vehicle age (years) |
| engine_cc | Engine displacement (cc, rounded) |
| vehicle_value | Estimated vehicle value (rounded) |
| reported_mileage | Self-reported annual mileage (may be missing/rounded) |
| num_drivers | Number of named drivers on policy |

## Column dictionary — claims

| Column | Description |
|---|---|
| policy_id | Anonymised policy identifier |
| cal_year | Calendar year of occurrence |
| occurrence_date | Claim occurrence date (if present) |
| usage | Vehicle use (at claim) |
| area | Area type (at claim) |
| vehicle_power | Vehicle power band (at claim) |
| gender | Driver gender |
| age | Driver age (at claim) |
| net_amount | Net claim amount |

## Notes
- Exposure should be used as an offset in frequency models: `offset(log_exposure)`.
- `sum_net`/`avg_net` are provided for convenience; they will be zero/NA when there are no claims in that year.
- Some fields include missing values.
- Dates fall within the period 2018–2022 for this initial cut.
