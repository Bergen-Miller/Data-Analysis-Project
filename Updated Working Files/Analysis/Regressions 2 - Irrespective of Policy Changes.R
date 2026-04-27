library(tidyverse)
library(fixest)

load('Updated Working Files/Clean Data/cleanMigrationData.RData')

# The purpose of this script is to, without considering statutory policy changes,
#   considering the (non-causal) relationship between the historical mean agi of
#   people moving between a pair of counties and the increase in that flow following
#   a 1 percentage point 

# Create column for highTaxRate in y1 county in year t-1


# Create a lookup table of y1_highTaxRate by y1_fips and year
tax_rate_lookup = df |>
  select(y1_fips, year, y1_highTaxRate, y2_fips, highTaxDiff, agi, n1) |>
  distinct() |>
  mutate(year = year + 1) |>  # shift year forward by 1
  rename(y1_highTaxRate_lag = y1_highTaxRate,
         highTaxDiff_lag = highTaxDiff,
         agi_lag = agi,
         n1_lag = n1)

# Join back to df to get the lagged value
df1 = df |>
  left_join(tax_rate_lookup, by = c("y1_fips", 'y2_fips', "year"))

# Add column for the tax shock applied to y1 counties, as well as the
#   divergence in a pair of counties' tax rates.

df2 = df1 |> mutate(y1_HTS = y1_highTaxRate - y1_highTaxRate_lag,
                    highTaxDiff_shock = highTaxDiff - highTaxDiff_lag) |>
  relocate(key, y1_fips, y2_fips, year, y2_state, y2_countyname, y1_HTS, highTaxDiff_shock)

ggplot(df2, aes(x=y1_HTS, y=n1)) + geom_point(aes(color=as.factor(year==2018)))
ggplot(df2, aes(x=highTaxDiff_shock, y=n1)) + geom_point(aes(color=as.factor(year==2018)))


#nrow(df2 |>filter(abs(highTaxDiff_shock)>.03)) / nrow(df2)
# The difference in high-income households' realized tax rates between counties in year 1 and
#   year 2, for any pair of counties, changes by more than 3 percentage points
#   in 4.9% of observations.

# ---- Model 1 ----

# For a pair of counties with people migrating between them, is there a correlation between
#   a change in the counties' difference in tax rates and the number of people moving.

model1 = feols(
  log(n1) ~ as.factor(highTaxDiff_shock>.05) * log(agi_lag/n1_lag) |
    y1_fips + year,
  data=df2
)

etable(model1)

# ---- Model 2 ----

# Repeat model 1 for agi per capita of the migrants in the county pair
model2 = feols(
  log(agi/n1) ~ as.factor(highTaxDiff_shock>.05) * log(agi_lag/n1_lag) |
    y1_fips + year,
  data=df2
)

etable(model2)

# Visualize

ggplot(df2, aes(x=log(agi_lag/n1_lag), y=log(agi/n1))) +
  geom_point(data = filter(df2, highTaxDiff_shock <= .05), color='steelblue', alpha=.05) +
  geom_point(data = filter(df2, highTaxDiff_shock>.05), color='red', alpha=.05) +
  geom_smooth(data = filter(df2, highTaxDiff_shock <= .05), color='steelblue') +
  geom_smooth(data = filter(df2, highTaxDiff_shock>.05), color='red')

ggplot(df2, aes(x=log(agi_lag/n1_lag), y=log(n1))) +
  geom_point(data = filter(df2, highTaxDiff_shock <= .05), color='steelblue', alpha=.05) +
  geom_point(data = filter(df2, highTaxDiff_shock>.05), color='red', alpha=.05) +
  geom_smooth(data = filter(df2, highTaxDiff_shock <= .05), color='steelblue') +
  geom_smooth(data = filter(df2, highTaxDiff_shock>.05), color='red')
