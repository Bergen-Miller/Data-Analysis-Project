library(tidyverse)
library(rvest)
library(httr)
library(jsonlite)
library(skimr)

#setwd('C:/Coding/R/ECNS 432 Working')

url = 'https://apps.bea.gov/regional/zip/CAGDP2.zip' # URL to the zip file download

temp_zip = tempfile(fileext = ".zip")
temp_dir = tempdir()

GET(url, write_disk(temp_zip, overwrite = TRUE)) # download zip file

unzip(temp_zip, exdir = temp_dir)

gdpDf = read_csv(file.path(temp_dir, "CAGDP2__ALL_AREAS_2001_2024.csv")) # read csv

# remove rows of notes at bottom and columns of years I don't need
gdpDf = gdpDf |> filter(!is.na(GeoName) & !is.na(Region) & !is.na(TableName)) |>
  select(-(`2001`:`2010`))

gdpDf = gdpDf |> filter(Description=='All industry total') |> select(-Description)
gdpDf = gdpDf |> mutate(fips = as.numeric(GeoFIPS)) |> select(-Unit)
gdpDf = gdpDf |> filter(fips>0) |> select(-GeoFIPS)
gdpDf = gdpDf |> relocate(fips)

gdpDf = gdpDf |> separate(col='GeoName',
                          into=c('county', 'state'),
                          sep=', ',
                          extra='merge',
                          fill='right')

# Convert to long format
longGdp = gdpDf |> pivot_longer(
  cols = `2011`:`2023`,
  names_to = 'year',
  values_to= 'gdp'
) |> select(-c(`2024`, IndustryClassification, LineCode, TableName, Region))

longGdp = longGdp |> mutate(year = as.numeric(year),
                            gdp = as.numeric(gdp))

#---- Merging Data ----

load('countyIncomePanel.RData')


merged = countyIncomePanel |>
  left_join(longGdp, by=c('fips', 'year'))

# Examine Data
merged = merged |> select(-c(county.y, state.y))
merged = merged |> mutate(state=state.x, county=county.x)
merged = merged |> select(-c(state.x, county.x))
merged = merged |> relocate(fips, state, county, year, sup)

save(merged, file='mergedIncomesGdp.RData')









