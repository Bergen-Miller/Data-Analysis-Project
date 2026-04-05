library(tidyverse)
library(fixest)
library(binsreg)

load('Updated Working Files/Clean Data/cleanMigrationData.RData')

# Is there a relationship, making no claim about causation, between the difference in effective
#   tax rates for high-income households and the average income of people changing counties?

# ---- Migrants' agi per capita vs. high-inc tax differential ----

binsreg(df$agi / df$n1, df$highTaxDiff)

#

# ---- Same y, new x is low-inc tax diff ----

binsreg(df$agi / df$n1, df$lowTaxDiff)

#

# ---- Reframing tax differential's effect on migration ----

# Instead of just examining correlation between tax differential
#   and per capita agi, consider the following. Of all the people
#   leaving a county, regress their agi per capita (to each destination county)
#   against the destination county's high-income tax rate

# For simplicity, consider Manhattan, NY.
# manhattan exodus
manExodus = df |> filter(y1_fips==36061)

# bin scattering per capita agi vs. high-income tax differential
binsreg(manExodus$agi / manExodus$n1, manExodus$highTaxDiff)

# Not binning (some extreme values, having small populations)
ggplot(manExodus, aes(x=highTaxDiff, y=agi/n1)) +
  geom_point() + geom_smooth()

# Taking only positive AGIs, so I can log them
positiveAgiPC = manExodus |> filter(agi>0)

ggplot(positiveAgiPC, aes(x=highTaxDiff, y=log(agi/n1))) +
  geom_point() + geom_smooth() + labs(color=NULL)

# Peculiarly, this plot slopes upwards. Given that an individual is emmigrating
#   from Manhattan to anywhere else, the destination county having higher taxes
#   on high-income individuals is correlated with higher per capita agi in people going there

# Maybe these are highly skilled people heading to San Francisco or something.

# ---- high-income tax shock and changes in flows ----

# In 2021, the top marginal income tax rate in New York State increased.
#   When this happened, is there a relationship between the change in number
#   of people leaving NY to each destination and the per capita agi of people
#   going to the same place the previous year.

# I am thinking of counties which were attracting wealthy people in 2020. Did those
#   counties attract even more people in 2021 relative to counties that attract 
#   non-wealthy people?

# People who moved from a New York county to a non-New York county
nyExodus = df |> filter(floor(y1_fips/1000)==36 & floor(y2_fips/1000)!=36)

nyExodus2 = nyExodus |> left_join(
  nyExodus |> filter(year==2020) |>
    mutate(pcAgi2020=agi/n1,
           n1_2020 = n1) |> select(y1_fips, y2_fips, pcAgi2020, n1_2020),
  
  by=c('y1_fips', 'y2_fips')
)

# Only need the last year of the data frame

finalNyExodus = nyExodus2 |> filter(year==2021)

ggplot(finalNyExodus, aes(x=log(pcAgi2020), y=n1 / n1_2020 -1)) +
  geom_point() + geom_smooth()

binsreg(finalNyExodus$n1 / finalNyExodus$n1_2020 -1, log(finalNyExodus$pcAgi2020))
# From the last plot, it looks like a 1% increase in the per capita AGI
#   of people who moved from New York to a county was associated with a less
#   positive increase in migration from New York to that county from 2020 to 2021.

