library(tidyverse)
library(fixest)

load('Updated Working Files/Clean Data/cleanMigrationData.RData')
# Loads: df (county-pair migration panel, 2011–2021 labels)

# ---- Year convention note ----
# The migration data labels each observation by the STARTING year of the IRS file.
# So `year == t` means the person was in y1_fips on their year-t return and in
# y2_fips on their year-(t+1) return, i.e., they physically moved during year t+1.
#
# NY raised its top marginal rate effective tax year 2021.
# We chose pre = year <= 2019, post = year %in% c(2020, 2021)
# This keeps the pre-period as untreated as possible and captures 
# both the immediate (2021) and lagged (2022) responses.

# ---- Build regression sample ----

# Treatment group: origin in NY state (FIPS prefix 36)
# Control group: origin outside NY
# Exclude: destinations within NY, only want out-migration flow

df_reg = df |>
  filter(floor(y2_fips / 1000) != 36) |>
  mutate(
    treat = ifelse(floor(y1_fips / 1000) == 36, 1, 0),
    post  = ifelse(year %in% c(2020, 2021), 1, 0)
  )

# ---- Model 1: Effect on migration volume ----
# This looks at the log number of returns (n1) moving from y1 to y2,
# treat*post is the DiD coefficient of interest.

model1 = feols(
  log(n1) ~ treat * post,
  data = df_reg
)

etable(model1)

# ---- Model 2: Effect on income of migrants ----
# This looks at the log per-capita AGI of people making the move
# Asks whether the COMPOSITION of migrants (not just the count) changed for NY

model2 = feols(
  log(agi / n1) ~ treat * post,
  data = df_reg
)

# Nice output table
etable(model2)
# Loads: df (county-pair migration panel, 2011b

save(model1, model2, file='Output/Quantitative Estimates/NY Tax Hike DD Regression Results.RData')
