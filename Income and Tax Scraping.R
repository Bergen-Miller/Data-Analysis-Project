library(tidyverse)
library(rvest)
library(httr)
library(jsonlite)

url='https://www.irs.gov/statistics/soi-tax-stats-county-data'
page= read_html(url)

nums=11:22
selectors = lapply(nums, function(x){
  paste0('#collapseCollapsible1742324652757 > div > div > ul > li:nth-child(', x-10, ')')
})

names(selectors) = paste0('inc', nums)

# Get the urls these hyperlinks take the browser to.
secondaryURLs = map_chr(selectors, ~{
  page |> html_element(.x) |> html_element('a') |>
    html_attr('href') |>
    url_absolute('https://www.irs.gov')
})

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
loadFromPage = function(url, sel){
  localPage = read_html(url) # load secondary page.
  incomeData = localPage |> html_elements(sel) |>
    html_attr('href') |> url_absolute('https://www.irs.gov') # this series of pipes returns the csv I'm looking for
  
  data = read_csv(incomeData) # read it
  return(data)
}

# Try it out
inc11 = loadFromPage(secondaryURLs[1], secondarySelectors[1])
inc12 = loadFromPage(secondaryURLs[2], secondarySelectors[2])

# Give each data frame a column indicating its year,
#   select only certain columns, rename them, relocate them as needed.

# I have read the relevant documentation to know which columns I am selecting and what they mean.

incomeData = inc11 |> mutate(year=2011) |>
  select(STATEFIPS, STATE, COUNTYFIPS, COUNTYNAME, agi_stub, # agi_stub bins households by adjusted gross income
         numReturns=N1, # number of returns filed in each AGI bin
         agi=A00100, # total adjusted gross income of households in AGI bin
         numSalaries=N00200, # number reporting salary or wage income
         totSalaries=A00200, # total salaries and wages
         interest=A00300, # taxable interest
         ordDividends=A00600, # ordinary dividends
         qualDividends=A00650, # specially low-tax qualified dividends
         busAndProf=A00900, # business and professional income (like sole-proprietorships)
         capGains=A01000, # net capital gains
         numUnemp=N02300, # number of returns reporting unemployment benefits
         totUnemp=A02300, # total unemployment benefits received
         stLocIncTax=A18425, # total state and local income tax paid
         stLocSalTax=A18450, # total state and local sales tax paid
         numRealEstTax=N18500, # number of returns paying real estate taxes
         realEstTax=A18500, # total $ of real estate tax
         taxesPaid=A18300, # sum of state and local taxes paid (SALT)
         numAMT=N09600, # number of returns paying alternative minimum tax
         totAMT=A09600, # total AMT paid
         incTax=A06500, year) # federal income tax after credits, and year

updatedData = incomeData |> mutate(fips = 1000*as.numeric(STATEFIPS) + as.numeric(COUNTYFIPS)) |>
  relocate(fips, state=STATE, county = COUNTYNAME, year, agi_stub) |>
  select(-c(STATEFIPS, COUNTYFIPS)) # cleaning up columns some.

# All $ values are in thousands

# Having written function to retrieve data from IRS (loadFromPage()),
#   write functions to select columns, make them neater, and bind rows into 1 data frame

# Abstracting process I already did for 2011 income data
wrangleDf = function(df, yr){ # takes in the df spit out by the loadFromPage() function
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
  updated = newData |> mutate(fips = 1000*as.numeric(STATEFIPS) + as.numeric(COUNTYFIPS)) |>
    relocate(fips, state=STATE, county = COUNTYNAME, year, agi_stub) |>
    select(-c(STATEFIPS, COUNTYFIPS))
  
  return(updated)
}

# With functions written, now execute them

incomesList = list()
incomesList[[1]] = inc11
incomesList[[2]] = inc12

for(i in 3:12){ # iterating through the remaining values in the vector secondaryURLs
  incomesList[[i]] = loadFromPage(secondaryURLs[i], secondarySelectors[i]) # storing these data frames in a list
  Sys.sleep(.5) # don't ask for too much at the same time
}

panelList = list()
for(i in 1:12){
  year = 2010+i
  panelList[[i]] = wrangleDf(incomesList[[i]], year)
}
names(panelList) = paste0('incomeData', 2011:2022)

panel = bind_rows(panelList) |> arrange(fips, year, agi_stub)

panel = panel |> distinct(fips, year, agi_stub, .keep_all=T) #dropping repeat

# Now check for tidiness
stopifnot(nrow(distinct(panel, fips, year, agi_stub))==nrow(panel)) # Perfect!

# Define primary key
panel$key = 1:nrow(panel)
panel = panel |> relocate(key)

# unit of observation is the county-year-agi_stub combination

newPanel = panel |> filter(!fips %in% c(48261, 48301)) # dropping counties missing all their data
stopifnot(sum(rowSums(is.na(newPanel)))==0) # newPanel is free of NA values

newPanelRounded = newPanel |> mutate(across(where(is.numeric), floor)) # take floor of numerical values
suppressed = newPanelRounded |> mutate(sup = numReturns==0) # Indicate which observations are suppressed
suppressed |> group_by(sup) |> count() # How many county-year-agi_stubs are suppressed?

newSupMark = suppressed |> group_by(fips, year) |> mutate(sup=any(sup)) |> ungroup()

# How large are the suppressed counties?

supCounties = newSupMark |> filter(sup) |> group_by(fips, year) |>
  summarize(pop = sum(numReturns), .groups='drop') # total tax returns filed in each county-year

supCountyPops = supCounties |> group_by(year) |> # mean and max population of suppressed counties, and number of suppressed counties in each year
  summarize(meanPop = mean(pop), maxPop = max(pop), numCounties=n(), .groups='drop')

countyIncomePanel = newSupMark |> relocate(key, fips, state, county, year, agi_stub, sup) # descriptive name


save(countyIncomePanel, file='countyIncomePanel.RData')




