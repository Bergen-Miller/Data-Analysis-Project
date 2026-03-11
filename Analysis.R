library(tidyverse)
library(fixest)
library(skimr)

#setwd('C:/coding/R/ECNS 432 Working')

load('mergedIncomesGdp.RData')

# ---- Considering Dropping all NA values ----

mergedExMissing = merged |> group_by(fips, year) |>
  filter(!any(is.na(gdp))) |> ungroup()

# I see no reason why the Census Bureau deciding to rename counties, or to change their classifications such that the IRS
# and BEA would have different fips codes in some years would be correlated with tax shocks OR gdp shocks. To
# bias my estimate, this condition must be correlated with both.

# I will continue to use mergedExMissing for all future analysis.

# Get these agi-stubs into binary (rich or poor)

# a household is 'rich' if it is in an unsuppressed county, and its agi_stub is 7 in 2011 or 8 afterwards.
# It is also rich if it is in a suppressed county and its agi_stub is 6 in 2011 or 7 afterwards.
# Otherwise, it is a poor household.
mergedExMissing2 = mergedExMissing |> group_by(fips, year) |>
  mutate(sup=any(numReturns==0 & agi_stub>6)) |> ungroup() # redefine suppressed only based on high-income suppression

exMissingAndSuppressed = mergedExMissing2 |> filter(!sup)

myRichTest = function(a, y){
  value = (y==2011 & a==7) | (y>2011 & a==8)
  return(value)
}

binaryAgi = exMissingAndSuppressed |> mutate(rich=myRichTest(agi_stub, year))

# keep only counties with data for every year.

binaryAgi2 = binaryAgi |> group_by(fips) |> summarize(n=n()) |> arrange(n)

cleaned = binaryAgi |> group_by(fips) |>
  filter(min(year)==2011 & max(year)==2022) |> ungroup()

# cleaned is data for every unsuppressed county with no NAs and a row for every year.

# ---- More exploration ----

# What fraction of population and gdp were cut out by excluding NA's and suppressed counties?
# Total population and GDP each year in merged vs. cleaned data frames

df1 = merged |> group_by(year) |>
  summarize(pop = sum(numReturns, na.rm=T),
            gdp = sum(gdp, na.rm=T), .groups='drop')

df2 = cleaned |> group_by(year) |>
  summarize(pop = sum(numReturns, na.rm=T),
            gdp = sum(gdp, na.rm=T), .groups='drop')

fractionPopDropped = (df1$pop-df2$pop)/df1$pop
fractionGdpDropped = (df1$gdp-df2$gdp)/df1$gdp

# There is never a year in my sample where these dropped counties have more than 1.4% of the population,
# And there is never a year where these dropped counties have more than 1% of GDP

# I also see no reason why dropping these counties would bias my estimates.

# Proceed with cleaned data frame

# ---- Widen and consolidate data to perform regressions ----

wide = cleaned |> group_by(fips, year, rich) |> mutate(rich=ifelse(rich, 'high', 'low')) |>
  summarize(state=first(state), county=first(county),
            across(numReturns:incTax, sum), 
            gdp=first(gdp), .groups='drop') |> pivot_wider(
  names_from = 'rich',
  values_from= c(numReturns:incTax)
) # information for households with agi over $200k stored in columns ending in _high

# Renaming, adjusting, selecting vars
wide2 = wide |> mutate(lowPop=numReturns_low, highPop=numReturns_high,
                       lowAgi=agi_low, highAgi=agi_high,
                       lowNumEmp=numSalaries_low, highNumEmp=numSalaries_high,
                       lowEarn=totSalaries_low, highEarn=totSalaries_high,
                       lowInv =interest_low+ordDividends_low+qualDividends_low+busAndProf_low+capGains_low,
                       highInv=interest_high+ordDividends_high+qualDividends_high+busAndProf_high+capGains_high,
                       lowNumUnemp=totUnemp_low, highNumUnemp=totUnemp_high,
                       lowTotTax=stLocIncTax_low+stLocSalTax_low+realEstTax_low+incTax_low,
                       highTotTax=stLocIncTax_high+stLocSalTax_high+realEstTax_high+incTax_high) |>
  select(fips, year, state, county, gdp, lowPop:highTotTax) # More tractable data frame

wide2ExStates = wide2 |> filter(fips%%1000 !=0)

wide3 = wide2ExStates |> mutate(lowTaxRate = lowTotTax/lowAgi, highTaxRate = highTotTax/highAgi)

wideStates = wide2 |> filter(fips%%1000 ==0) |> mutate(lowTaxRate = lowTotTax/lowAgi, highTaxRate = highTotTax/highAgi) |>
  select(-c(lowAgi, highAgi, lowTotTax, highTotTax))

#
# ---- Save Wide data ----
save(wide3, file='cleanWideData.RData')
#
# ---- Considering a population floor on high-income cohort ----

popFloor = wide3 |> group_by(fips) |> mutate(minPop=min(highPop)) |> ungroup() |>
  filter(minPop>100)

popFloor2 = popFloor |> group_by(year) |> summarize(
  manyHighPop = sum(lowPop + highPop),
  manyHighGdp = sum(gdp))

popFloor3 = wide3 |> group_by(year) |> summarize(
  totPop = sum(lowPop + highPop),
  totGdp = sum(gdp)
)

popFloor4 = popFloor2 |> left_join(popFloor3, by='year')
# fraction of population and gdp coming from counties with over 100 tax returns reporting agi>$200k
ggplot(popFloor4, aes(x=year)) +
  geom_point(aes(y=manyHighPop/totPop, color='fraction of population in many-high-income counties')) +
  geom_point(aes(y=manyHighGdp/totGdp, color='fraction of gdp in many-high-income counties'))

# Since in every year, at least 95% of the U.S. lives in a county which in every year has at least 100 tax
#   returns reporting agi over $200k. These counties also account for at least 96.5% of GDP.
#   This is depsite the fact these counties are only 2/3 of all counties

# ---- Save wide data with only counties with highPop>100 each year ----
save(popFloor, file='countiesWithAtLeast100HighAgiReturns.RData')
#

# ---- Here is a Trend! ----
# Using filtered data frame
ggplot(popFloor, aes(x=(lowTotTax+highTotTax)/(lowEarn+lowInv+highEarn+highInv),
                     y=(log(gdp)))) + geom_point()
#
# ---- Everything below Includes all counties (except suppressed or missing ones) ----
# ---- Low-income cohort share of incomes, population ----

agiStubIncomeShare = wide3 |> group_by(year) |>
  summarize(lowAgiShare = sum(lowAgi)/sum(lowAgi+highAgi),
            lowEarnShare= sum(lowEarn)/sum(lowEarn + highEarn),
            lowInvShare = sum(lowInv)/sum(lowInv+highInv),
            popShare = sum(lowPop)/sum(lowPop+highPop))

ggplot(agiStubIncomeShare, aes(x=year)) +
  geom_point(aes(y=lowAgiShare, color='agi share')) +
  geom_point(aes(y=lowEarnShare, color='salaries share'))+
  geom_point(aes(y=lowInvShare, color='capital share'))+
  geom_point(aes(y=popShare, color='population share'))

#
# ---- Visualizing Tax Shocks and GDP ----

# Establish variation in independent vars, a few percentage points is sufficient.
# Recall that independent vars are tax SHOCKS --> change in TotTax/agi from year t-1 to year t

# years 2011 and 2022 are not changes in same county from one year to next,
#   they are changes from 2022 in one county to 2011 in the next county.

vis1 = wide3 |> select(fips, year, state, gdp, lowTaxRate, highTaxRate) |>
  mutate(gdpShock = lead(gdp)/gdp - 1, # % growth in gdp from year t to t+1
         lowTaxShock = lowTaxRate-lag(lowTaxRate), # pp change in effective tax rate from year t-1 to t
         highTaxShock = highTaxRate-lag(highTaxRate)) |> filter(year!=2011 & year!=2022)


# Simple plots
ggplot(vis1, aes(lowTaxShock)) + geom_histogram(boundary=0,binwidth=.01)
ggplot(vis1, aes(highTaxShock)) + geom_histogram(boundary=0, binwidth=.01)
ggplot(vis1, aes(gdpShock)) + geom_histogram(boundary=0, binwidth=.01)


# National averages each year
natAvgs = vis1 |> group_by(year) |>
  summarize(avgGdpGrowth = weighted.mean(gdpShock, gdp),
            avgLowTaxShock=weighted.mean(lowTaxShock, gdp),
            avgHighTaxShock=weighted.mean(highTaxShock, gdp))

# Good contender for ECNS 432 Figure
ggplot(natAvgs, aes(x=year)) +
  geom_line(aes(y=avgGdpGrowth, color='National Next-Year Gdp Growth')) +
  geom_line(aes(y=avgLowTaxShock, color='National Previous-Year Low-Income Tax Shock')) +
  geom_line(aes(y=avgHighTaxShock, color='National Previous-Year High-Income Tax Shock'))


ggplot(vis1, aes(x=lowTaxShock, y=gdpShock,
                 color=ifelse(year==2018, '2018', 'other'))) + geom_point()
ggplot(vis1, aes(x=highTaxShock, y=gdpShock,
                 color=ifelse(year==2018, '2018', 'other'))) + geom_point()

# How can I aggregate to make the effect I'm studying visible?
# Perhaps only use large counties?

vis2 = wide3 |> select(fips, year, state, gdp, lowPop, highPop, lowTaxRate, highTaxRate) |>
  mutate(gdpShock = lead(gdp)/gdp - 1, # % growth in gdp from year t to t+1
         lowTaxShock = lowTaxRate-lag(lowTaxRate), # pp change in effective tax rate from year t-1 to t
         highTaxShock = highTaxRate-lag(highTaxRate)) |> filter(year!=2011 & year!=2022)

vis2States = wideStates |> select(fips, year, state, gdp, lowPop, highPop, lowTaxRate, highTaxRate) |>
  mutate(gdpShock = lead(gdp)/gdp - 1, # % growth in gdp from year t to t+1
         lowTaxShock = lowTaxRate-lag(lowTaxRate), # pp change in effective tax rate from year t-1 to t
         highTaxShock = highTaxRate-lag(highTaxRate)) |> filter(year!=2011 & year!=2022)

# Plotting with states
ggplot(vis2States, aes(x=lowTaxShock, y=gdpShock,
                       color=ifelse(year==2018, '2018', 'else'))) + geom_point()

ggplot(vis2States, aes(x=highTaxShock, y=gdpShock,
                       color=ifelse(year==2018, '2018', 'else'))) + geom_point()

ggplot(vis2States, aes(x=year)) +
  geom_point(aes(y=lowTaxShock, color='lowTaxShock'))+
  geom_point(aes(y=highTaxShock, color='highTaxShock'))+
  geom_point(aes(y=gdpShock, color='forward gdp change'))

ggplot(vis2States, aes(x=lowTaxShock, y=highTaxShock,
                       color=ifelse(year==2018,'2018','other'))) + geom_point()
#
# ---- Visualizing Tax Shocks and GDP over 2 or 3-year averages ----

yearBlocks2 = wide3 |> mutate(yearBlock=ifelse(year<=2012, 1,
                                       ifelse(year<=2014, 2,
                                       ifelse(year<=2016, 3,
                                       ifelse(year<=2018, 4,
                                       ifelse(year<=2020, 5, 6)))))) |>
  group_by(fips, yearBlock) |> summarize(gdp=mean(gdp),
                                         lowPop=mean(lowPop),
                                         highPop=mean(highPop),
                                         lowTaxRate=mean(lowTaxRate),
                                         highTaxRate=mean(highTaxRate),
                                         lowEarn=mean(lowEarn),
                                         highEarn=mean(highEarn),
                                         lowInv=mean(lowInv),
                                         highInv=mean(highInv), .groups='drop') |>
  select(fips, yearBlock, gdp:highInv)

yearBlocks3 = wide3 |> mutate(yearBlock=ifelse(year<=2013, 1,
                                        ifelse(year<=2016, 2,
                                        ifelse(year<=2019, 3, 4)))) |>
  group_by(fips, yearBlock) |> summarize(gdp=mean(gdp),
                                         lowPop=mean(lowPop),
                                         highPop=mean(highPop),
                                         lowTaxRate=mean(lowTaxRate),
                                         highTaxRate=mean(highTaxRate),
                                         lowEarn=mean(lowEarn),
                                         highEarn=mean(highEarn),
                                         lowInv=mean(lowInv),
                                         highInv=mean(highInv), .groups='drop') |>
  select(fips, yearBlock, gdp:highInv)

# Relationship between average tax rates over two years and change in average gdp to next two years?

df2 = yearBlocks2 |> mutate(gdpChg=lead(gdp)/gdp-1, # growth from block t to t+1
                          lowTaxShock=lowTaxRate-lag(lowTaxRate), # tax shock from block t-1 to t
                          highTaxShock=highTaxRate-lag(highTaxRate),
                          lowPopChg=lead(lowPop)/lowPop-1,
                          highPopChg=lead(highPop)/highPop-1,
                          lowPopShock=lowPop/lag(lowPop)-1,
                          highPopShock=highPop/lag(highPop)-1,
                          lowEarnChg=lead(lowEarn)/lowEarn -1,
                          lowEarnShock=lowEarn/lag(lowEarn)-1,
                          highEarnChg=ifelse(highEarn!=0,lead(highEarn)/highEarn-1,NA), # assigning NA to growth off a zero basis
                          highEarnShock=ifelse(lag(highEarn)!=0,highEarn/lag(highEarn)-1,NA),
                          lowInvChg=lead(lowInv)/lowInv-1,
                          lowInvShock=lowInv/lag(lowInv)-1,
                          highInvChg=ifelse(highInv!=0,lead(highInv)/highInv-1,NA), # assigning NA to growth off a zero basis
                          highInvShock=ifelse(lag(highInv)!=0,highInv/lag(highInv-1),NA)) |> 
  filter(yearBlock<max(yearBlock) & yearBlock>min(yearBlock))

df3 = yearBlocks3 |> mutate(gdpChg=lead(gdp)/gdp-1,
                            lowTaxShock=lowTaxRate-lag(lowTaxRate),
                            highTaxShock=highTaxRate-lag(highTaxRate),
                            lowPopChg=lead(lowPop)/lowPop-1,
                            highPopChg=lead(highPop)/highPop-1,
                            lowPopShock=lowPop/lag(lowPop)-1,
                            highPopShock=highPop/lag(highPop)-1,
                            lowEarnChg=lead(lowEarn)/lowEarn -1,
                            lowEarnShock=lowEarn/lag(lowEarn)-1,
                            highEarnChg=ifelse(highEarn!=0, lead(highEarn)/highEarn-1, NA), # assigning NA to growth off a zero basis
                            highEarnShock=ifelse(lag(highEarn)!=0, highEarn/lag(highEarn)-1, NA),
                            lowInvChg=lead(lowInv)/lowInv-1,
                            lowInvShock=lowInv/lag(lowInv)-1,
                            highInvChg=ifelse(highInv!=0,lead(highInv)/highInv-1, NA),# assigning NA to growth off a zero basis
                            highInvShock=ifelse(lag(highInv)!=0,highInv/lag(highInv-1),NA)) |>
  filter(yearBlock<max(yearBlock) & yearBlock>min(yearBlock))

# ---- Variation in independent vars ----
ggplot(df2,aes(x=lowTaxShock, y=highTaxShock,
               color=ifelse(yearBlock==4, 'tcja occurs between years', 'other'))) +
  geom_point() + labs(color=NULL)

ggplot(df3, aes(x=lowTaxShock, y=highTaxShock,
                color=ifelse(yearBlock==3, 'tcja occurs after first year', 'others'))) +
  geom_point() + labs(color=NULL)

#
# ---- Variation in incomes ----

ggplot(df2, aes(highInv/highPop)) + geom_histogram(boundary=0,binwidth=50)
ggplot(df2, aes(lowInv/lowPop)) + geom_histogram(boundary=0, binwidth=1)

ggplot(df2, aes(x=lowEarnChg, y=lowInvChg)) + geom_point()
ggplot(df2, aes(x=highEarnChg, y=highInvChg)) + geom_point()

#
# ---- Population changes ----
ggplot(df2, aes(x=lowPopChg, y=highPopChg)) + geom_point()

ggplot(df2, aes(x=lowPopShock, y=lowPopChg)) + geom_point()
ggplot(df2, aes(x=lowPopShock, y=highPopChg)) + geom_point()
ggplot(df2, aes(x=highPopShock, y=lowPopChg)) + geom_point()

ggplot(df2, aes(x=lowPopShock, y=gdpChg)) + geom_point()
ggplot(df2, aes(x=highPopShock, y=gdpChg)) + geom_point()

#
# ---- Visualizing Tax Shocks and Labor Market Outcomes ----

empRateChg = wide3 |> mutate(lts=lowTaxRate-lag(lowTaxRate), # change in low-income effective tax rate from previous year
                         hts=highTaxRate-lag(highTaxRate),
                         lers=lowNumEmp/lowPop-lag(lowNumEmp/lowPop), # change in low-income employment rate from previous year
                         hers=highNumEmp/highPop-lag(highNumEmp/highPop)) |> filter(year!=2011)



#
# ---- Regression 1: %change in following-year gdp = low-income Tax shock + high-income Tax shock + population growth ----

justTaxGdp = cleaned |> select(fips, year, rich, numReturns, agi, gdp, totStLocTax, incTax)

justTaxGdp2 = justTaxGdp |> group_by(fips, year, rich) |>
  summarize(numReturns=sum(numReturns),
            gdp=first(gdp),
            agi=sum(agi),
            totTax = sum(totStLocTax + incTax), .groups='drop')

wideTaxGdp = justTaxGdp2 |> pivot_wider(
  names_from = 'rich',
  values_from= c('numReturns', 'agi', 'totTax')
)

wideTaxGdp2 = wideTaxGdp |> mutate(lowPop=numReturns_FALSE,
                                   highPop=numReturns_TRUE,
                                   lowTaxRate=totTax_FALSE/agi_FALSE,
                                   highTaxRate=totTax_TRUE/agi_TRUE,
                                   ) |>
  select(fips, year, gdp, lowPop, highPop, lowTaxRate, highTaxRate)

# don't need to keep rows for state-level
wideTaxGdp3 = wideTaxGdp2 |> filter(fips%%1000 !=0)

# Construct regression vars.
# lead(gdp)/gdp - 1 would give what I want, but the value in the year==2022 rows would be the next county's 2011 value in the numerator
regReady = wideTaxGdp3 |> mutate(forwardGdpGrowth = lead(gdp)/gdp - 1,
                                 lowTaxShock = lowTaxRate - lag(lowTaxRate),
                                 highTaxShock= highTaxRate -lag(highTaxRate),
                                 lowPopChg = lowPop/lag(lowPop)-1,
                                 highPopChg = highPop/lag(highPop)-1) |> filter(year!=2011 & year!=2022) |>
  select(fips, year, `forwardGdpGrowth`:`highPopChg`)

# Examine regression-ready data

skim(regReady)
ggplot(regReady, aes(highTaxShock)) + geom_density()
ggplot(regReady, aes(lowTaxShock)) + geom_density()

# Conduct the following regression:
# gdpGrowth = lowTaxShock + highTaxShock + lowPopChg + highPopChg + county, year fixed effects

regression1 = feols(forwardGdpGrowth ~ lowTaxShock + highTaxShock + lowPopChg + highPopChg |
                      fips + year, data=regReady)
etable(regression1)


































