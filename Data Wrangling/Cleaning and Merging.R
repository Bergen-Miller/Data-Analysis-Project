library(tidyverse)
library(skimr)
library(summarytools)

load('Temp Data/temp_countyIncomePanel.RData')
load('Temp Data/temp_countyImmigrationData.RData')

# ---- First Widen Income Data ----

wideIncomes = countyIncomePanel |>
  mutate(rich=ifelse((agi_stub==7 & year==2011) | (agi_stub==8 & year>2011), 1, 0)) |>
  group_by(fips, year, rich) |>
  summarize(across(numReturns:incTax, sum, na.rm=T),
            across(c(state, county, sup), first), .groups='drop') |>
  pivot_wider(names_from='rich', values_from=c('numReturns':'incTax'))
# _0 is for agi under $200k, _1 is for households with agi above $200k

# Redefine the sup column to indicate only counties with the top income group suppressed
wideIncomes1 = wideIncomes |> mutate(sup=ifelse(numReturns_1==0, T, F))

#
# ---- Save wide Incomes ----
save(wideIncomes1, file='Temp Data/temp_wideIncomePanel.RData')
#
# ---- Merge migration data from list into single df ----

# inflows[[1]] represent changes from 2011 to 2012.

# Data is to be identified by pairs of counties and year.

# Need columns for each of the To and From counties:
#   fips, county, flow (population), salaries, investment income, st & loc inc tax,
#   st & loc sales and property tax, and federal income tax.



# Rename and redefine columns in data.
reworkCols = function(df){ # df is the tibble object given to the function
  newDf = df |> mutate(across(c(y1_statefips:y2_countyfips), as.numeric), # coerce fips codes
                       y1_fips = 1000*y1_statefips + y1_countyfips, # define new fips columns
                       y2_fips = 1000*y2_statefips + y2_countyfips) |>
                select(-c(y1_statefips:y2_countyfips)) |> # remove redundant columns
                relocate(c(y1_fips, y2_fips)) # move county fips identifier codes to front
}

excludeSuperfluousRows = function(df){
  # The migration data has some selected aggregates for each
}

newList=list()
for(i in 1:length(inflows)){
  newList[[i]] = reworkCols(inflows[[i]])
}

# rename tibbles in the list by their starting year:
names(newList) = 2011:2021

# Combine immigration data from list into single tibble
inflows_combined = imap(newList, ~ .x |> mutate(year = .y)) |> 
                   bind_rows() |>
                   select(y1_fips, y2_fips, y2_state, y2_countyname, n1, n2, agi, year) |>
                   relocate(y1_fips, y2_fips, year)

# Add key column
inflows_combined = inflows_combined |> mutate(key=1:nrow(inflows_combined)) |>
                                       relocate(key)

# make sure combinations of y1_fips, y2_fips, year are unique
# If the following line prints 'FALSE', there are duplicates
nrow(inflows_combined |> group_by(y1_fips, y2_fips, year) |> count() |> filter(n>1))==0

dups = inflows_combined |> group_by(y1_fips, y2_fips, year) |> count() |> filter(n>1)

# See if duplicate rows are exactly identical
inflows_combined |> filter(paste(y1_fips, y2_fips, year) %in%
                             paste(dups$y1_fips, dups$y2_fips, dups$year)) |> View()
# Some duplicates are proper duplicates, others have suppressed data values. I don't know why
#   I would have one county pair-year with two observations (one with data, one with unsuppressed data)

# Remove duplicates: It will work to keep the instance of duplicate (y1_fips, y2_fips, year) with the smaller key
newInflows = inflows_combined |> group_by(y1_fips, y2_fips, year) |>
                                 mutate(lowerKey=min(key)) |>
                                 ungroup() |>
                                 filter(key==lowerKey)

newInflows |> group_by(y1_fips, y2_fips, year) |> count() |> filter(n>1) |> View()
# No duplicate rows

length(newInflows$key) - length(unique(newInflows$key))
# all keys are unique

# Final Adjustment to migration tibble:

migration = newInflows |> select(-lowerKey)

#
# ---- Save combined migration tibble ----

# The migration tibble shows the number of returns (n1), number of individuals (n2), and those
#   tax returns' cumulative AGI (agi) in year t+1, which reported residence in county y1_fips in 
#   the year specified and residence in county y2_fips in year t+1

# Separate tibble into aggregates and non-aggregates
aggMig = migration |> filter(y2_fips >=57000) |> mutate(year=as.numeric(year))
countyMig = migration |> filter(y2_fips < 57000) |> mutate(year=as.numeric(year))

# Only need some columns from income data:
selectCols = wideIncomes1 |> mutate(
  lowTaxRate = (incTax_0+taxesPaid_0)/agi_0,
  highTaxRate = (incTax_1 + taxesPaid_1)/agi_1,
  lowEmploymentRate = numSalaries_0 / numReturns_0,
  highEmploymentRate = numSalaries_1/ numReturns_1,
  lowEarned = totSalaries_0,
  highEarned = totSalaries_1,
  lowCapital = interest_0+ordDividends_0+qualDividends_0+busAndProf_0+capGains_0,
  highCapital = interest_1+ordDividends_1+qualDividends_1+busAndProf_1+capGains_1
) |> select(
  fips, year, lowTaxRate:highCapital
)

# tibble selectCols has data on low and high-income households' :
#   tax rates, employment rates, salaries and wages, and investment income

newCountyData = countyMig |> left_join( # add all columns from selectCols for first-year county
  selectCols |> rename(y1_lowTaxRate=lowTaxRate, y1_highTaxRate=highTaxRate,
                       y1_lowER = lowEmploymentRate, y1_highER=highEmploymentRate,
                       y1_lowEarned = lowEarned, y1_highEarned = highEarned,
                       y1_lowCapital = lowCapital, y1_highCapital=highCapital),
  by = c('y1_fips'='fips', 'year'='year')
) |> left_join( # add all columns from selectCols for second-year county
  selectCols |> rename(y2_lowTaxRate=lowTaxRate, y2_highTaxRate=highTaxRate,
                       y2_lowER = lowEmploymentRate, y2_highER=highEmploymentRate,
                       y2_lowEarned = lowEarned, y2_highEarned = highEarned,
                       y2_lowCapital = lowCapital, y2_highCapital=highCapital),
  by= c('y2_fips'='fips', 'year'='year')
) |> mutate( # add columns for tax differentials
  lowTaxDiff = y2_lowTaxRate-y1_lowTaxRate,
  highTaxDiff = y2_highTaxRate-y1_highTaxRate # If second-year county has higher tax rate, these columns are positive 
)

save(aggMig, countyMig, newCountyData, file='Temp Data/temp_mergedImmigrationData.RData')

# Note that the y2 tax rate columns represent that county's effective tax rates in the first year

# ---- More cleaning ----

# Some observations have fewer than 10 people, so the IRS gives the values n1, n2, and agi
#   as -1 to protect their privacy. Since our analysis does not concern the extensive margin,
#   (whether or not a county pair has any migration), and we only consider the increases or
#   and decreases in migration (intensive margin) and the incomes of these individuals, we will
#   exclude these observation from our data.

newCountyData2 = newCountyData |> filter(n1 != -1)

# Counties in the income data with too few households over $200k have that data suppressed.
#   Those counties, having low populations and being only 5% of observations, are okay to drop from the analysis.
#   They also pose problems, having tax rates in the newCountyData data frame of Inf.

newCountyData3 = newCountyData2 |> filter(lowTaxDiff < Inf & highTaxDiff < Inf &
                                              y1_lowTaxRate!=Inf & y1_highTaxRate != Inf)

# Only 0.8% of observations had Infinity values

#   The data frame contains non-migrants, people who stay in the same county

df = newCountyData3 |> filter(y1_fips!=y2_fips)
dfNonMig = newCountyData3 |> filter(y1_fips==y2_fips)

# Many counties only appear in the data set once, but that shouldn't be a problem.

# The data should satisfy everything in the data-cleaning checklist.

save(df, dfNonMig, file='Updated Working Files/Clean Data/cleanMigrationData.RData')

# tibble df has all the data about counties involved in inter-county migration, and
# tibble dfNonMig has all the data about counties involved in intra-county migration.

# I expect tibble dfNonMig to be of little use.

