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



# ---- Merge migration with incomes data ----

# inflows[[1]] and outflows[[1]] represent changes from 2011 to 2012.



