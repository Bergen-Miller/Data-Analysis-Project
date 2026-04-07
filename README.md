# Tax Shocks and County-Level Migration

## Research Question

Do people "vote with their feet" when their state raises taxes, moving to lower-tax counties, and is this effect stronger for high-income households?

All data come from the IRS Statistics of Income (SOI) county-level income and migration files. Our sample covers 2011-2022. The unit of observation is a county-pair x year: for each origin county, we observe how many people moved to each destination county in a given year, along with income and tax characteristics of both counties.

To identify causal effects we exploit three state-level policy shocks:

1. **New Jersey (2018):** top marginal rate raised from 8.97% to 10.75% on income above $5M; threshold later lowered to $1M effective 2021.
2. **New York State (2021):** top marginal rate raised from 6.85% to 9.85% on income above $1M.
3. **Delaware (2018):** estate tax repealed (previously 16% on inheritances above $5.49M).

Sources: [state millionaire surtaxes](https://www.davemanuel.com/state-millionaire-surtaxes.php), [Delaware estate tax repeal](https://www.forbes.com/sites/ashleaebeling/2017/07/05/latest-state-to-repeal-estate-tax-delaware/).

---

## Project Structure

```
Data Wrangling/
  Data Scraping.R - scrapes raw IRS income and migration CSVs
  Cleaning and Merging.R - cleans, merges, and saves the analysis dataset

Updated Working Files/
  Clean Data/
    cleanMigrationData.RData - fully cleaned dataset (committed to repo)
  Analysis/
    Visualizations.R - descriptive binscatter plots
    Map Visualization.R - choropleth map and animated GIF of NY out-migration
    Regression 1 - DD with only migration and policy change.R - DiD models

Project Write-Ups/
  Stage_Two.Rmd - Stage 2 submission document

Temp Data/ - intermediate .RData files from the scraping pipeline
Documentation/ - IRS data documentation
```

---

## Peer Review Reproduction

You need R (4.1 or later) and RStudio. An internet connection is required for package installation and the map section on the first knit.

### Step 1: Clone and open the project

```bash
git clone <repo-url>
```

Open the project by double-clicking **`Data-Analysis-Project.Rproj`** in RStudio. Don't open the .Rmd or .R files directly from your file browser - the .Rproj file is what sets the working directory correctly so the paths in the scripts work.

### Step 2: Delete temp and output files

| Delete | Keep |
|---|---|
| `Temp Data/*.RData` | `Updated Working Files/Clean Data/cleanMigrationData.RData` |
| Any PDFs in `Project Write-Ups/` | All .R and .Rmd scripts |

`cleanMigrationData.RData` is the final cleaned dataset, not a temp file, it lives in `Clean Data/`, not `Temp Data/`. The scraping step that regenerates the `Temp Data/` files pulls from the IRS website directly and can take 10-15 minutes, so keeping the clean data is the intended starting point for peer review.

### Step 3: Knit the report

Open `Project Write-Ups/Stage_Two.Rmd` in RStudio and click **Knit**. Required packages install automatically on the first knit. The map section will download county shapefiles from the Census Bureau the first time. The PDF will appear in `Project Write-Ups/`.

**If you also deleted `cleanMigrationData.RData`**, run these two scripts from the RStudio console first:

```r
source("Data Wrangling/Data Scraping.R") # scrapes IRS website, takes a while
source("Data Wrangling/Cleaning and Merging.R") # builds cleanMigrationData.RData
```

Note: `Data Scraping.R` uses CSS selectors on IRS web pages and could break if the IRS updates their site layout. If it fails, re-clone the repo to get the committed `Temp Data/` files back and run only `Cleaning and Merging.R`.

---

## Running Individual Analysis Scripts

All scripts in `Updated Working Files/Analysis/` read from `Updated Working Files/Clean Data/cleanMigrationData.RData` and can be run on their own once that file exists. Make sure the project is open before running them.

| Script | What it does |
|---|---|
| `Visualizations.R` | Descriptive binscatter plots (Figures 1-5) |
| `Map Visualization.R` | Static choropleth and animated GIF |
| `Regression 1 - DD with only migration and policy change.R` | Difference-in-differences models |

---

## Year Convention in the Migration Data

Each `year` value in the dataset is the starting year of the IRS migration file, not the year people physically moved:

| `year` in data | IRS file | People physically moved during |
|---|---|---|
| 2019 | 2019-2020 | 2020 |
| 2020 | 2020-2021 | 2021 (NY tax shock year) |
| 2021 | 2021-2022 | 2022 |

The NY 2021 rate increase is modeled with `post = year %in% c(2020, 2021)` (moves during 2021 and 2022) and `pre = year <= 2019` (moves through 2020).
