library(tidyverse)
library(fixest)

load('Temp Data/temp_mergedImmigrationData.RData')

# In 2021, New York State increased top marginal income tax rate

df = countyWithTaxes |> filter(floor(y2_fips/1000)!=36) |>
  mutate(treat = ifelse(floor(y1_fips/1000)==36, 1, 0),
                        post  = ifelse(year==2021, 1, 0))

model1 = feols(
  log(n1) ~ treat*post + lowTaxDiff + highTaxDiff,
  data=df
)

etable(model1)

model2 = feols(
  log(agi/n1) ~ treat*post,
  data=df
)

etable(model2)
