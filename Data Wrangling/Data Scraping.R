# Load the needed libraries
library(tidyverse)

# For web scaping
library(rvest)
library(httr)

# To handle JSON files
library(jsonlite)

#
# ---- Scrape Incomes ----

# Main IRS county data page that contains links to yearly county income files
url='https://www.irs.gov/statistics/soi-tax-stats-county-data'

# Read the HTML from the IRS web page
page= read_html(url)

# The years of data you wish to select
nums=11:22

# Build CSS selectors dynamically for each of the relevant list items
# inside the collapsible IRS page section
selectors = lapply(nums, function(x){
  paste0('#collapseCollapsible1742324652757 > div > div > ul > li:nth-child(', x-10, ')')
})

# Give each selector a name
names(selectors) = paste0('inc', nums)

# Get the urls these hyperlinks take the browser to
#   1. find that element on the main page
#   2. find the <a> tag inside it
#   3. extract the href
#   4. convert relative links into full IRS URLs
secondaryURLs = map_chr(selectors, ~{
  page |> html_element(.x) |> html_element('a') |>
    html_attr('href') |>
    url_absolute('https://www.irs.gov')
})

# These are the CSS selectors for the actual CSV download links on each page
# The exact selector differs by year because the IRS page structure changes
secondarySelectors = c('body > div.dialog-off-canvas-main-canvas > div.pup-main-container.container > div > div.pup-header-content-rt.col-sm-12.col-md-9 > div > article > div > div > p:nth-child(8) > a:nth-child(3)',
                       'body > div.dialog-off-canvas-main-canvas > div.pup-main-container.container > div > div.pup-header-content-rt.col-sm-12.col-md-9 > div > article > div > div > p:nth-child(8) > a:nth-child(3)',
                       'body > div.dialog-off-canvas-main-canvas > div.pup-main-container.container > div > div.pup-header-content-rt.col-sm-12.col-md-9 > div > article > div > div > p:nth-child(8) > a:nth-child(3)',
                       'body > div.dialog-off-canvas-main-canvas > div.pup-main-container.container > div > div.pup-header-content-rt.col-sm-12.col-md-9 > div > article > div > div > p:nth-child(8) > a:nth-child(3)',
                       'body > div.dialog-off-canvas-main-canvas > div.pup-main-container.container > div > div.pup-header-content-rt.col-sm-12.col-md-9 > div > article > div > div > p:nth-child(8) > a:nth-child(3)',
                       'body > div.dialog-off-canvas-main-canvas > div.pup-main-container.container > div > div.pup-header-content-rt.col-sm-12.col-md-9 > div > article > div > div > p:nth-child(8) > a:nth-child(3)',
                       'body > div.dialog-off-canvas-main-canvas > div.pup-main-container.container > div > div.pup-header-content-rt.col-sm-12.col-md-9 > div > article > div > div > p:nth-child(9) > a:nth-child(1)',
                       'body > div.dialog-off-canvas-main-canvas > div.pup-main-container.container > div > div.pup-header-content-rt.col-sm-12.col-md-9 > div > article > div > div > p:nth-child(8) > a:nth-child(3)',
                       'body > div.dialog-off-canvas-main-canvas > div.pup-main-container.container > div > div.pup-header-content-rt.col-sm-12.col-md-9 > div > article > div > div > p:nth-child(8) > a:nth-child(3)',
                       'body > div.dialog-off-canvas-main-canvas > div.pup-main-container.container > div > div.pup-header-content-rt.col-sm-12.col-md-9 > div > article > div > div > ul:nth-child(7) > li:nth-child(1) > a',
                       'body > div.dialog-off-canvas-main-canvas > div.pup-main-container.container > div > div.pup-header-content-rt.col-sm-12.col-md-9 > div > article > div > div > ul:nth-child(7) > li:nth-child(1) > a',
                       'body > div.dialog-off-canvas-main-canvas > div.pup-main-container.container > div > div.pup-header-content-rt.col-sm-12.col-md-9 > div > article > div > div > ul:nth-child(7) > li:nth-child(1) > a')

# Write function to get data frame from each csv.
# Inputs:
#   url = the yearly IRS page
#   sel = CSS selector pointing to the CSV link on that page
# Output:
#   a data frame read directly from the CSV file
loadFromPage = function(url, sel){
  localPage = read_html(url) # Load secondary page

  # Find the CSV link on the second page and convert to a URL path
  incomeData = localPage |> html_elements(sel) |>
    html_attr('href') |> url_absolute('https://www.irs.gov') # This series of pipes returns the csv I'm looking for
  
  data = read_csv(incomeData) # Read the csv into R
  return(data)
}

# Load the 2011-2012 county-income CSV files to test it works
inc11 = loadFromPage(secondaryURLs[1], secondarySelectors[1])
inc12 = loadFromPage(secondaryURLs[2], secondarySelectors[2])

# Give each data frame a column indicating its year,
# select only certain columns, rename them, relocate them as needed.

# I have read the relevant documentation to know which columns I am selecting and what they mean.

# Manually wrangle the 2011 data first
incomeData = inc11 |> mutate(year=2011) |>
  select(STATEFIPS, STATE, COUNTYFIPS, COUNTYNAME, agi_stub, # Agi_stub bins households by adjusted gross income
         numReturns=N1, # Number of returns filed in each AGI bin
         agi=A00100, # Total adjusted gross income of households in AGI bin
         numSalaries=N00200, # Number reporting salary or wage income
         totSalaries=A00200, # Total salaries and wages
         interest=A00300, # Taxable interest
         ordDividends=A00600, # Ordinary dividends
         qualDividends=A00650, # Specially low-tax qualified dividends
         busAndProf=A00900, # Business and professional income (like sole-proprietorships)
         capGains=A01000, # Net capital gains
         numUnemp=N02300, # Number of returns reporting unemployment benefits
         totUnemp=A02300, # Total unemployment benefits received
         stLocIncTax=A18425, # Total state and local income tax paid
         stLocSalTax=A18450, # Total state and local sales tax paid
         numRealEstTax=N18500, # Number of returns paying real estate taxes
         realEstTax=A18500, # Total $ of real estate tax
         taxesPaid=A18300, # Sum of state and local taxes paid (SALT)
         numAMT=N09600, # Number of returns paying alternative minimum tax
         totAMT=A09600, # Total AMT paid
         incTax=A06500, year) # Federal income tax after credits, and year

# Create a combined county FIPS code, move key identifying variables forward,
# and remove the separate state and county FIPS columns
updatedData = incomeData |> mutate(fips = 1000*as.numeric(STATEFIPS) + as.numeric(COUNTYFIPS)) |>
  relocate(fips, state=STATE, county = COUNTYNAME, year, agi_stub) |>
  select(-c(STATEFIPS, COUNTYFIPS)) # Cleaning up columns some.

# All $ values are in thousands

# Having written function to retrieve data from IRS (loadFromPage()),
# write functions to select columns, make them neater, and bind rows into 1 data frame

# Abstracting process I already did for 2011 income data
# This function takes a raw IRS county income file and a year,
# then returns the cleaned and standardized version of that data
wrangleDf = function(df, yr){ # Takes in the df spit out by the loadFromPage() function
  newData = df |> mutate(year=yr) |>
    select(STATEFIPS, STATE, COUNTYFIPS, COUNTYNAME, agi_stub,
           numReturns=N1,
           agi=A00100,
           numSalaries=N00200,
           totSalaries=A00200,
           interest=A00300,
           ordDividends=A00600,
           qualDividends=A00650,
           busAndProf=A00900,
           capGains=A01000,
           numUnemp=N02300,
           totUnemp=A02300,
           stLocIncTax=A18425,
           stLocSalTax=A18450,
           numRealEstTax=N18500,
           realEstTax=A18500,
           taxesPaid=A18300,
           numAMT=N09600,
           totAMT=A09600,
           incTax=A06500, year)

  # Build county FIPS, reorder columns, and remove redundant FIPS
  updated = newData |> mutate(fips = 1000*as.numeric(STATEFIPS) + as.numeric(COUNTYFIPS)) |>
    relocate(fips, state=STATE, county = COUNTYNAME, year, agi_stub) |>
    select(-c(STATEFIPS, COUNTYFIPS))
  
  return(updated)
}

# With functions written, now execute them

# Create a list to store each raw yearly income data frame
incomesList = list()

# Store the two test years already loaded
incomesList[[1]] = inc11
incomesList[[2]] = inc12

# Load the remaining yearly income files and store them
for(i in 3:12){ # Iterating through the remaining values in the vector secondaryURLs
  incomesList[[i]] = loadFromPage(secondaryURLs[i], secondarySelectors[i]) # Storing these data frames in a list
  Sys.sleep(.5) # Don't ask for too much at the same time
}

# Create another list that will hold the cleaned yearly panels
panelList = list()

# Apply wrangleDf() to every yearly raw data frame
for(i in 1:12){
  year = 2010+i
  panelList[[i]] = wrangleDf(incomesList[[i]], year)
}

# Name each list element by its corresponding year
names(panelList) = paste0('incomeData', 2011:2022)

# Stack all yearly county-income files together into one long panel,
# then sort by county, year, and AGI bin
panel = bind_rows(panelList) |> arrange(fips, year, agi_stub)

# Remove any duplicate county-year-agi_stub rows, keeping the first version found.
panel = panel |> distinct(fips, year, agi_stub, .keep_all=T) # Dropping repeat

# Now check for tidiness
stopifnot(nrow(distinct(panel, fips, year, agi_stub))==nrow(panel)) # Perfect!

# Define primary key
panel$key = 1:nrow(panel)

# Move the key column to the front
panel = panel |> relocate(key)

# Unit of observation is the county-year-agi_stub combination

# Drop counties missing all of their data
newPanel = panel |> filter(!fips %in% c(48261, 48301)) # Dropping counties missing all their data
stopifnot(sum(rowSums(is.na(newPanel)))==0) # NewPanel is free of NA values

# Round all numeric values down to the nearest integer
newPanelRounded = newPanel |> mutate(across(where(is.numeric), floor)) # Take floor of numerical values

# Mark suppressed observations as those with zero tax returns
suppressed = newPanelRounded |> mutate(sup = numReturns==0) # Indicate which observations are suppressed

# If any AGI bin in a county-year is suppressed, mark the whole county-year as suppressed
newSupMark = suppressed |> group_by(fips, year) |> mutate(sup=any(sup)) |> ungroup()

# How large are the suppressed counties?

# For suppressed county-years, compute the total number of returns across AGI bins
supCounties = newSupMark |> filter(sup) |> group_by(fips, year) |>
  summarize(pop = sum(numReturns), .groups='drop') # Total tax returns filed in each county-year

# For each year, summarize the average population, largest population,
# and number of suppressed counties
supCountyPops = supCounties |> group_by(year) |> # Mean and max population of suppressed counties, and number of suppressed counties in each year
  summarize(meanPop = mean(pop), maxPop = max(pop), numCounties=n(), .groups='drop')

# Final descriptive name for the cleaned county-income panel
countyIncomePanel = newSupMark |> relocate(key, fips, state, county, year, agi_stub, sup) # Descriptive name

# Save the cleaned county income panel to an RData file
save(countyIncomePanel, file='temp_countyIncomePanel.RData')

#
# ---- Scrape Immigration Flows ----

# IRS migration data pages for each year-to-year county migration flow release
urls = c('https://www.irs.gov/statistics/soi-tax-stats-migration-data-2011-2012',
         'https://www.irs.gov/statistics/soi-tax-stats-migration-data-2012-2013',
         'https://www.irs.gov/statistics/soi-tax-stats-migration-data-2013-2014',
         'https://www.irs.gov/statistics/soi-tax-stats-migration-data-2014-2015',
         'https://www.irs.gov/statistics/soi-tax-stats-migration-data-2015-2016',
         'https://www.irs.gov/statistics/soi-tax-stats-migration-data-2016-2017',
         'https://www.irs.gov/statistics/soi-tax-stats-migration-data-2017-2018',
         'https://www.irs.gov/statistics/soi-tax-stats-migration-data-2018-2019',
         'https://www.irs.gov/statistics/soi-tax-stats-migration-data-2019-2020',
         'https://www.irs.gov/statistics/soi-tax-stats-migration-data-2020-2021',
         'https://www.irs.gov/statistics/soi-tax-stats-migration-data-2021-2022')

# CSS selectors for the inflow migration data links on each yearly IRS migration page
# As with the county income pages, the layout differs slightly by year
inflowSelectors=c('body > div.dialog-off-canvas-main-canvas > div.pup-main-container.container > div > div.pup-header-content-rt.col-sm-12.col-md-9 > div > article > div > div > p:nth-child(8) > a:nth-child(6)',
                  'body > div.dialog-off-canvas-main-canvas > div.pup-main-container.container > div > div.pup-header-content-rt.col-sm-12.col-md-9 > div > article > div > div > ul:nth-child(10) > li:nth-child(4)',
                  'body > div.dialog-off-canvas-main-canvas > div.pup-main-container.container > div > div.pup-header-content-rt.col-sm-12.col-md-9 > div > article > div > div > ul:nth-child(9) > li:nth-child(4)',
                  'body > div.dialog-off-canvas-main-canvas > div.pup-main-container.container > div > div.pup-header-content-rt.col-sm-12.col-md-9 > div > article > div > div > ul:nth-child(9) > li:nth-child(4) > a',
                  'body > div.dialog-off-canvas-main-canvas > div.pup-main-container.container > div > div.pup-header-content-rt.col-sm-12.col-md-9 > div > article > div > div > ul:nth-child(9) > li:nth-child(4) > a',
                  'body > div.dialog-off-canvas-main-canvas > div.pup-main-container.container > div > div.pup-header-content-rt.col-sm-12.col-md-9 > div > article > div > div > ul:nth-child(9) > li:nth-child(4) > a',
                  'body > div.dialog-off-canvas-main-canvas > div.pup-main-container.container > div > div.pup-header-content-rt.col-sm-12.col-md-9 > div > article > div > div > ul:nth-child(9) > li:nth-child(4) > a',
                  'body > div.dialog-off-canvas-main-canvas > div.pup-main-container.container > div > div.pup-header-content-rt.col-sm-12.col-md-9 > div > article > div > div > ul:nth-child(8) > li:nth-child(4) > a',
                  'body > div.dialog-off-canvas-main-canvas > div.pup-main-container.container > div > div.pup-header-content-rt.col-sm-12.col-md-9 > div > article > div > div > ul:nth-child(9) > li:nth-child(4) > a',
                  'body > div.dialog-off-canvas-main-canvas > div.pup-main-container.container > div > div.pup-header-content-rt.col-sm-12.col-md-9 > div > article > div > div > p:nth-child(8) > a:nth-child(4)',
                  'body > div.dialog-off-canvas-main-canvas > div.pup-main-container.container > div > div.pup-header-content-rt.col-sm-12.col-md-9 > div > article > div > div > p:nth-child(7) > a:nth-child(4)')

# Create a list to store the migration inflow data
inflows = list()

# Loop across all migration years specified, scrape from each page
# store in the list
for(i in 1:11){
  inflows[[i]] = loadFromPage(urls[i], outflowSelectors[i])
  Sys.sleep(.5)
}

# Save data
save(inflows, file='Temp Data/temp_countyImmigrationData.RData')




