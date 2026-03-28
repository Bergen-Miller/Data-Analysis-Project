### Topic
We examine the relationships among cross-county migration, household income, county GDP, and effective income taxes in the United States from 2011 to 2022.

### Motivation
This project asks whether tax changes affect where people choose to live and potentially, how those migration responses may shape local economic outcomes. A tax change can alter behavior within a county, such as labor supply, investment, and consumption, but it may also change who lives there. If households respond by moving across counties, then tax policy may reallocate economic activity across space rather than simply increase or decrease it overall.

Understanding this distinction is important for both economics and public policy. Policy makers care not only about total population, but also about the composition of that population. If higher-income households are more mobile than others, tax shocks may change the local tax base, the distribution of income, consumer demand, and the productivity mix of residents. A county that loses high-income households may see lower GDP not just because fewer people live there, but because the people who leave account for a disproportionate share of earnings, spending, and tax revenue.

This project is also motivated by the broader question of whether observed county GDP effects reflect true production changes or sorting effects. For example, a county’s GDP could fall because remaining residents work or invest less, or because high-income households relocate elsewhere because of taxes. Distinguishing between these mechanisms helps clarify what tax policy is actually doing and whether county-level economic and policy changes reflect a change in individual behavior, migration, or both.

More broadly, this analysis helps connect household mobility to regional economic performance. By combining county income, tax, migration, and GDP data, we can study whether mobility differs systematically by income group, whether tax shocks affect migration decisions differently across groups, and whether those migration patterns have measurable consequences for local output. These questions are relevant for understanding the geographic incidence of taxation and why similar tax changes may have different effects across places.

### Research Questions
1. (Descriptive) Is there a relationship between household AGI and cross-county mobility?
2. (Causal) How do tax shocks affect cross-county migration among households with different economic and geographic characteristics?
3. (Causal) What is the effect of cross-county migration on county GDP, and through what mechanisms does that effect operate?

Questions are listed roughly in order of expected completion.

For question 3, some examples of potential mechanisms which could be observed are population change, highly productive (high income) individuals selecting into counties, or non-migration changes in the income distribution changing consumption patterns.

### Data
The IRS Statistics of Income (SOI) offers not only county-by-year aggregate household income and tax statistics, but they bin these households by agi_stub (data is county-year-agi_stub). The cutoffs for the adjusted gross income (AGI) bins change from 2011 to 2012, but are constant afterwards. In 2011, agi_stub==2 represents households with AGI between $1 and $25k. This bin is split in two from 2012 forth. In all cases, agi_stub==1 represents households with less than $1 in AGI. In 2011 (afterwards), agi_stub==7 (8) represents households reporting AGI over $200k. From [this webpage](https://www.irs.gov/statistics/soi-tax-stats-county-data), this data can easily be scraped for the years 2011-2022. This data is cleaned, saved as countyIncomePanel.RData, and includes information such as number of tax returns in each county-year-agi_stub, their aggregate salaries and wages, investment income (interest + dividends + net capital gains + business and professional income), unemployment benefits, state and local taxes, and federal income tax.

[This IRS SOI webpage](https://www.irs.gov/statistics/soi-tax-stats-migration-data) has links to cross-county migration data for the years 2011-2022. For each year pair (from 2011 to 2012, from 2012 to 2013, etc.), there are two csv files for the number of tax returns filled in a different county from where they were the year before and the AGI of these households in the latter year. One csv describes gross population outflows from counties, and the other describes gross inflows. This data is stored in countyImmigrationData.RData.

[This link](https://apps.bea.gov/regional/zip/CAGDP2.zip) downloads a csv from the Bureau of Economic Analysis (BEA) containing nominal GDP estimates for each county by industry for the years 2001 to 2024. To match our IRS data, we only keep the years 2011 to 2022. We also ignore industry categorizations, using only aggregates.

In the IRS data as well as BEA, all dollar values are in thousands. In cases where an agi group has fewer than 20 tax returns, all the values of that row are set to zero. Also, if there is a single tax return one of whose values exceeds some unspecified percentage of that value in that county-year-agi_stub, that value is set to zero. In the immigration data, if there are fewer than 20 tax returns in gross flows between any pair of counties, values are set to -1. These data 'suppressions' are for privacy reasons and described in the relevant documentation.

The income and GDP data are cleaned and merged from Bergen's ECNS 432 project. The migration data are specific to this project and have not yet been cleaned. Since the migration data contain flows between pairs of counties instead of a single statistic for each individually, it is not obvious how they should be merged. One idea would be to take the mergedIncomesGdp.RData tibble, consolidate agi_stub information in some way (such as counting all agi_stubs less than 7 in 2011 or 8 in future years as a single group), pivot this wider so that each row is a county-year and new columns are made for the lowAgi and highAgi groups, then add columns for net immigration flows from every county-year to every other county-same year.
