library(tidyverse)
library(rvest)
library(httr)
library(jsonlite)

# ---- Scrape County Income Data -----------------------------------------------
#
# The IRS publishes county-level income data at:
# https://www.irs.gov/statistics/soi-tax-stats-county-data
#
# Each year's data is a separate CSV linked from that page. The page uses a
# collapsible section, so we first scrape the links to each yearly sub-page,
# then scrape the CSV download link from each sub-page.
#
# All dollar amounts in the IRS files are reported in thousands.

url = 'https://www.irs.gov/statistics/soi-tax-stats-county-data'
page = read_html(url)

# Years 11 through 22 correspond to 2011-2022 in the IRS file naming
nums = 11:22

# Each year's sub-page link sits in a numbered list item inside the collapsible
# section. The CSS selector is the same structure for each, just a different
# nth-child index.
selectors = lapply(nums, function(x){
  paste0('#collapseCollapsible1742324652757 > div > div > ul > li:nth-child(', x-10, ')')
})
names(selectors) = paste0('inc', nums)

# Pull the href from each list item and convert to a full URL
secondaryURLs = map_chr(selectors, ~{
  page |> html_element(.x) |> html_element('a') |>
    html_attr('href') |>
    url_absolute('https://www.irs.gov')
})

# The IRS redesigned their sub-pages several times between 2011 and 2022, so the
# CSS selector pointing to the CSV download link is different for some years.
# These were found by manually inspecting each page in a browser.
secondarySelectors = c(
  'body > div.dialog-off-canvas-main-canvas > div.pup-main-container.container > div > div.pup-header-content-rt.col-sm-12.col-md-9 > div > article > div > div > p:nth-child(8) > a:nth-child(3)',
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
  'body > div.dialog-off-canvas-main-canvas > div.pup-main-container.container > div > div.pup-header-content-rt.col-sm-12.col-md-9 > div > article > div > div > ul:nth-child(7) > li:nth-child(1) > a'
)

# Navigates to a yearly IRS page, finds the CSV link using the given selector,
# and reads it directly into a data frame.
loadFromPage = function(url, sel){
  localPage = read_html(url)
  csvUrl = localPage |> html_elements(sel) |>
    html_attr('href') |> url_absolute('https://www.irs.gov')
  return(read_csv(csvUrl))
}

# Test on 2011 and 2012 before running the full loop
inc11 = loadFromPage(secondaryURLs[1], secondarySelectors[1])
inc12 = loadFromPage(secondaryURLs[2], secondarySelectors[2])

# Standardizes a raw IRS income file: selects the columns we need, renames them
# to something readable, and builds a single county FIPS code from the separate
# state and county FIPS columns. Column definitions come from the IRS documentation
# in the Documentation/ folder.
wrangleDf = function(df, yr){
  newData = df |> mutate(year = yr) |>
    select(
      STATEFIPS, STATE, COUNTYFIPS, COUNTYNAME,
      agi_stub, # income bin (1-7 pre-2012, 1-8 after; bins households by AGI)
      numReturns = N1, # returns filed in this AGI bin
      agi = A00100, # total AGI ($000s)
      numSalaries= N00200, # returns reporting wage/salary income
      totSalaries= A00200, # total wages and salaries ($000s)
      interest = A00300, # taxable interest ($000s)
      ordDividends = A00600, # ordinary dividends ($000s)
      qualDividends = A00650, # qualified dividends ($000s) - taxed at lower rate
      busAndProf = A00900, # business/professional net income ($000s)
      capGains = A01000, # net capital gains ($000s)
      numUnemp = N02300, # returns reporting unemployment benefits
      totUnemp = A02300, # total unemployment benefits ($000s)
      stLocIncTax = A18425, # state and local income taxes paid ($000s)
      stLocSalTax = A18450, # state and local sales taxes paid ($000s)
      numRealEstTax = N18500, # returns paying real estate taxes
      realEstTax = A18500, # total real estate taxes ($000s)
      taxesPaid  = A18300, # total SALT deduction (income + sales + property, $000s)
      numAMT = N09600, # returns paying alternative minimum tax
      totAMT = A09600, # total AMT paid ($000s)
      incTax = A06500, # federal income tax after credits ($000s)
      year
    )

  updated = newData |>
    mutate(fips = 1000 * as.numeric(STATEFIPS) + as.numeric(COUNTYFIPS)) |>
    relocate(fips, state = STATE, county = COUNTYNAME, year, agi_stub) |>
    select(-c(STATEFIPS, COUNTYFIPS))

  return(updated)
}

# Load all 12 years into a list, then clean each one
incomesList = list()
incomesList[[1]] = inc11
incomesList[[2]] = inc12

for(i in 3:12){
  incomesList[[i]] = loadFromPage(secondaryURLs[i], secondarySelectors[i])
  Sys.sleep(.5) # Small pause to be nice
}

panelList = list()
for(i in 1:12){
  panelList[[i]] = wrangleDf(incomesList[[i]], 2010 + i)
}
names(panelList) = paste0('incomeData', 2011:2022)

# Stack all years into one long panel, sorted by county, year, and AGI bin
panel = bind_rows(panelList) |> arrange(fips, year, agi_stub)

# Remove duplicate rows
panel = panel |> distinct(fips, year, agi_stub, .keep_all = T)
stopifnot(nrow(distinct(panel, fips, year, agi_stub)) == nrow(panel))

panel$key = 1:nrow(panel)
panel = panel |> relocate(key)

# Counties 48261 (Kenedy, TX) and 48301 (Loving, TX) are missing across all years
newPanel = panel |> filter(!fips %in% c(48261, 48301))
stopifnot(sum(rowSums(is.na(newPanel))) == 0)

# IRS reports some values as decimals; add a floor them to keep everything as integers
newPanelRounded = newPanel |> mutate(across(where(is.numeric), floor))

# The IRS suppresses data for small populations by reporting zero returns.
# Notify any county-year where at least one AGI bin has zero returns.
suppressed = newPanelRounded |> mutate(sup = numReturns == 0)
newSupMark = suppressed |> group_by(fips, year) |> mutate(sup = any(sup)) |> ungroup()

countyIncomePanel = newSupMark |> relocate(key, fips, state, county, year, agi_stub, sup)

save(countyIncomePanel, file = 'Temp Data/temp_countyIncomePanel.RData')


# ---- Scrape Migration Data ---------------------------------------------------
#
# The IRS also publishes year-over-year county migration files. Each file covers
# one pair of consecutive years and records how many tax returns (and their
# cumulative AGI) reported a different county address from one year to the next.
#
# We only need the inflow files (where people moved TO each county).
# year label in the data = starting year of the file, e.g. the 2020-2021 file
# gets labeled year=2020 and records people who physically moved during 2021.

urls = c(
  'https://www.irs.gov/statistics/soi-tax-stats-migration-data-2011-2012',
  'https://www.irs.gov/statistics/soi-tax-stats-migration-data-2012-2013',
  'https://www.irs.gov/statistics/soi-tax-stats-migration-data-2013-2014',
  'https://www.irs.gov/statistics/soi-tax-stats-migration-data-2014-2015',
  'https://www.irs.gov/statistics/soi-tax-stats-migration-data-2015-2016',
  'https://www.irs.gov/statistics/soi-tax-stats-migration-data-2016-2017',
  'https://www.irs.gov/statistics/soi-tax-stats-migration-data-2017-2018',
  'https://www.irs.gov/statistics/soi-tax-stats-migration-data-2018-2019',
  'https://www.irs.gov/statistics/soi-tax-stats-migration-data-2019-2020',
  'https://www.irs.gov/statistics/soi-tax-stats-migration-data-2020-2021',
  'https://www.irs.gov/statistics/soi-tax-stats-migration-data-2021-2022'
)

# As with the income pages, the IRS changed their migration page layout over time
inflowSelectors = c(
  'body > div.dialog-off-canvas-main-canvas > div.pup-main-container.container > div > div.pup-header-content-rt.col-sm-12.col-md-9 > div > article > div > div > p:nth-child(8) > a:nth-child(6)',
  'body > div.dialog-off-canvas-main-canvas > div.pup-main-container.container > div > div.pup-header-content-rt.col-sm-12.col-md-9 > div > article > div > div > ul:nth-child(10) > li:nth-child(4)',
  'body > div.dialog-off-canvas-main-canvas > div.pup-main-container.container > div > div.pup-header-content-rt.col-sm-12.col-md-9 > div > article > div > div > ul:nth-child(9) > li:nth-child(4)',
  'body > div.dialog-off-canvas-main-canvas > div.pup-main-container.container > div > div.pup-header-content-rt.col-sm-12.col-md-9 > div > article > div > div > ul:nth-child(9) > li:nth-child(4) > a',
  'body > div.dialog-off-canvas-main-canvas > div.pup-main-container.container > div > div.pup-header-content-rt.col-sm-12.col-md-9 > div > article > div > div > ul:nth-child(9) > li:nth-child(4) > a',
  'body > div.dialog-off-canvas-main-canvas > div.pup-main-container.container > div > div.pup-header-content-rt.col-sm-12.col-md-9 > div > article > div > div > ul:nth-child(9) > li:nth-child(4) > a',
  'body > div.dialog-off-canvas-main-canvas > div.pup-main-container.container > div > div.pup-header-content-rt.col-sm-12.col-md-9 > div > article > div > div > ul:nth-child(9) > li:nth-child(4) > a',
  'body > div.dialog-off-canvas-main-canvas > div.pup-main-container.container > div > div.pup-header-content-rt.col-sm-12.col-md-9 > div > article > div > div > ul:nth-child(8) > li:nth-child(4) > a',
  'body > div.dialog-off-canvas-main-canvas > div.pup-main-container.container > div > div.pup-header-content-rt.col-sm-12.col-md-9 > div > article > div > div > ul:nth-child(9) > li:nth-child(4) > a',
  'body > div.dialog-off-canvas-main-canvas > div.pup-main-container.container > div > div.pup-header-content-rt.col-sm-12.col-md-9 > div > article > div > div > p:nth-child(8) > a:nth-child(4)',
  'body > div.dialog-off-canvas-main-canvas > div.pup-main-container.container > div > div.pup-header-content-rt.col-sm-12.col-md-9 > div > article > div > div > p:nth-child(7) > a:nth-child(4)'
)

inflows = list()
for(i in 1:11){
  inflows[[i]] = loadFromPage(urls[i], inflowSelectors[i])
  Sys.sleep(.5)
}

save(inflows, file = 'Temp Data/temp_countyImmigrationData.RData')
