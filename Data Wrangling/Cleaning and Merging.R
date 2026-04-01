library(tidyverse)
library(skimr)
library(summarytools)

load('Temp Data/temp_countyIncomePanel.RData')
load('Temp Data/temp_countyImmigrationData.RData')

# ---- First Widen Income Data ----

wideIncomes = countyIncomePanel |>
  mutate(rich=ifelse((agi_stub==7 & year==2011) | (agi_stub==8 & year>2011), 1, 0)) |>
  group_by(fips, year, rich) |> summarize(across(numReturns:incTax, sum, na.rm=T),
                                          across(c(state, county, sup), first)) |>
  pivot_wider(names_from='rich', values_from=c('numReturns':'incTax'))
# _0 is for agi under $200k

# Redefine the sup column to indicate only counties with the top income group suppressed
wideIncomes1 = wideIncomes |> mutate(sup=ifelse(numReturns_1==0, T, F))

# Exploring Data
freq(wideIncomes1$sup) # 4.8% of observations suppressed.

# What cut-off to use for 'rich' households

df = countyIncomePanel |> mutate(maybeRich = agi_stub>=7 & year==2011 | agi_stub >=8 & year>2011) |>
  group_by(year, maybeRich) |> summarize(pop = sum(numReturns))

newDf = df |> pivot_wider(
  names_from = 'maybeRich',
  values_from= 'pop'
)
newDf = newDf |> mutate(totPop=`FALSE` + `TRUE`,
                        fracHi = `TRUE`/(`TRUE` + `FALSE`))

# Even in 2022, only 24% of households report AGI greater than $100k.
# In 2022, only 8% report AGI greater than $200k.

# For this project, it might be better not to drop these observations.


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

save(migration, file='Temp Data/temp_mergedImmigrationData.RData')

#
# ---- Continue Cleaning migration data (address aggregates) ----
skim(migration)
# No NA values, some data is suppressed (set to -1 for privacy)

# Separate tibble into aggregates and non-aggregates





