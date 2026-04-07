library(tidyverse)
library(skimr)
library(summarytools)

load('Temp Data/temp_countyIncomePanel.RData')
load('Temp Data/temp_countyImmigrationData.RData')


# ---- Widen the income panel --------------------------------------------------
#
# The raw income panel data has one row per county-year-AGI bin (agi_stub). We need
# to collapse it into one row per county-year with separate columns for low and
# high-income households, because our research question is specifically about
# whether high earners respond differently to tax changes than everyone else.
#
# The IRS changed its income bin structure in 2012: before 2012 the top bin was
# agi_stub==7 (>$200k), after 2012 it became agi_stub==8 (>$200k). We created a
# single "rich" var that handles both cases.

wideIncomes = countyIncomePanel |>
  mutate(rich = ifelse((agi_stub == 7 & year == 2011) | (agi_stub == 8 & year > 2011), 1, 0)) |>
  group_by(fips, year, rich) |>
  summarize(
    across(numReturns:incTax, sum, na.rm = T),
    across(c(state, county, sup), first),
    .groups = 'drop'
  ) |>
  pivot_wider(names_from = 'rich', values_from = c('numReturns':'incTax'))
# _0 columns = households with AGI <= $200k
# _1 columns = households with AGI > $200k

# Redefine suppression to notify only counties where the high-income group is
# suppressed, since that's the group we care most about for our analysis.
wideIncomes1 = wideIncomes |> mutate(sup = ifelse(numReturns_1 == 0, T, F))

save(wideIncomes1, file = 'Temp Data/temp_wideIncomePanel.RData')


# ---- Clean and combine the migration data ------------------------------------
#
# The migration data came in as a list of 11 raw data frames (one per year pair).
# Each row in a migration file represents the county-to-county migration: the number of
# returns, individuals, and total AGI that reported county A as their address in
# year t and county B in year t+1.
#
# y1_fips = origin county (where they lived in year t)
# y2_fips = destination county (where they lived in year t+1)
# n1 = number of tax returns making that move
# n2 = number of individuals
# agi = total AGI of those returns (reported on the year t+1 return)
#
# Year label convention: the 2020-2021 file is labeled year=2020. This means
# year=2020 captures people who physically moved during 2021.

# Converts raw FIPS columns to numeric and builds a single fips code for each county
reworkCols = function(df){
  df |>
    mutate(
      across(c(y1_statefips:y2_countyfips), as.numeric),
      y1_fips = 1000 * y1_statefips + y1_countyfips,
      y2_fips = 1000 * y2_statefips + y2_countyfips
    ) |>
    select(-c(y1_statefips:y2_countyfips)) |>
    relocate(c(y1_fips, y2_fips))
}

newList = list()
for(i in 1:length(inflows)){
  newList[[i]] = reworkCols(inflows[[i]])
}
names(newList) = 2011:2021

# Stack all years into one data frame and keep only the columns we need
inflows_combined = imap(newList, ~ .x |> mutate(year = .y)) |>
  bind_rows() |>
  select(y1_fips, y2_fips, y2_state, y2_countyname, n1, n2, agi, year) |>
  relocate(y1_fips, y2_fips, year)

inflows_combined = inflows_combined |>
  mutate(key = 1:nrow(inflows_combined)) |>
  relocate(key)

# Check for duplicate county-pair-year rows. Some exist because the IRS
# sometimes includes both a suppressed and unsuppressed version of one magration pair.
# We keep the first occurrence (lower key) to drop the duplicates.
dups = inflows_combined |> group_by(y1_fips, y2_fips, year) |> count() |> filter(n > 1)

newInflows = inflows_combined |>
  group_by(y1_fips, y2_fips, year) |>
  mutate(lowerKey = min(key)) |>
  ungroup() |>
  filter(key == lowerKey)

migration = newInflows |> select(-lowerKey)

# The IRS includes aggregate rows (e.g. total out-of-state, total foreign) using
# FIPS codes >= 57000. Split those off from real county-to-county flows.
aggMig = migration |> filter(y2_fips >= 57000) |> mutate(year = as.numeric(year))
countyMig = migration |> filter(y2_fips < 57000)  |> mutate(year = as.numeric(year))

save(aggMig, countyMig, file = 'Temp Data/temp_mergedImmigrationData.RData')


# ---- Compute effective tax rates and merge onto migration data ----------------
#
# Effective tax rate = (federal income tax + total SALT paid) / AGI
# Computed separately for low- and high-income households in each county-year.
# We also pull employment rate and income composition variables.

selectCols = wideIncomes1 |>
  mutate(
    lowTaxRate = (incTax_0 + taxesPaid_0) / agi_0,
    highTaxRate = (incTax_1 + taxesPaid_1) / agi_1,
    lowEmploymentRate = numSalaries_0 / numReturns_0,
    highEmploymentRate = numSalaries_1 / numReturns_1,
    lowEarned = totSalaries_0,
    highEarned = totSalaries_1,
    # Capital income = interest + dividends + business income + capital gains
    lowCapital = interest_0 + ordDividends_0 + qualDividends_0 + busAndProf_0 + capGains_0,
    highCapital = interest_1 + ordDividends_1 + qualDividends_1 + busAndProf_1 + capGains_1
  ) |>
  select(fips, year, lowTaxRate:highCapital)

# Join origin county tax info, then destination county tax info, onto each
# migration flow. The tax rate columns reflect the county's rates in year t
# (the year the person was still living there, before the move).
newCountyData = countyMig |>
  left_join(
    selectCols |> rename(
      y1_lowTaxRate = lowTaxRate,  y1_highTaxRate = highTaxRate,
      y1_lowER = lowEmploymentRate, y1_highER = highEmploymentRate,
      y1_lowEarned = lowEarned,   y1_highEarned  = highEarned,
      y1_lowCapital = lowCapital,  y1_highCapital = highCapital
    ),
    by = c('y1_fips' = 'fips', 'year' = 'year')
  ) |>
  left_join(
    selectCols |> rename(
      y2_lowTaxRate = lowTaxRate,  y2_highTaxRate = highTaxRate,
      y2_lowER = lowEmploymentRate, y2_highER = highEmploymentRate,
      y2_lowEarned = lowEarned,   y2_highEarned  = highEarned,
      y2_lowCapital = lowCapital,  y2_highCapital = highCapital
    ),
    by = c('y2_fips' = 'fips', 'year' = 'year')
  ) |>
  mutate(
    # Positive diff means the destination county has a higher tax rate
    lowTaxDiff = y2_lowTaxRate  - y1_lowTaxRate,
    highTaxDiff = y2_highTaxRate - y1_highTaxRate
  )


# ---- Final cleaning ----------------------------------------------------------

# The IRS codes flows with fewer than 10 filers as n1 = n2 = agi = -1 to protect
# privacy. We drop these since we're studying migration volumes, not whether a
# given county pair has any migration at all.
newCountyData2 = newCountyData |> filter(n1 != -1)

# Counties where the high-income group is suppressed end up with agi_1 = 0,
# which produces Inf tax rates. These are small counties and make up about 5%
# of observations. We drop any row where either county has an Inf tax rate or
# an Inf tax differential (NaN cases from Inf - Inf also evaluate to FALSE in
# the comparison and get dropped).
newCountyData3 = newCountyData2 |>
  filter(
    lowTaxDiff < Inf & highTaxDiff < Inf &
    y1_lowTaxRate != Inf & y1_highTaxRate != Inf
  )

# Separate people who stayed in the same county from those who moved.
# dfNonMig is retained for completeness but not used in the main analysis.
df = newCountyData3 |> filter(y1_fips != y2_fips)
dfNonMig = newCountyData3 |> filter(y1_fips == y2_fips)

save(df, dfNonMig, file = 'Updated Working Files/Clean Data/cleanMigrationData.RData')
