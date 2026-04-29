library(tidyverse)
library(fixest)
library(binsreg)

load('Updated Working Files/Clean Data/cleanMigrationData.RData')

# ============================================================
# VISUALIZATIONS
# Do people "vote with their feet" when taxes
# rise, and is this effect stronger for high-income households?
#
# Data: IRS county-level income and migration flows, 2011–2022.
# Unit of observation: county-pair × year.
# ============================================================


# ---- Figure 1: Migrants' Wealth vs. High-Income Tax Differential ----
#
# Across all county-pairs and years, is there a correlation between
# the gap in effective high-income tax rates (destination minus origin)
# and the per-capita income of people who made that move?
#
# A downward slope would indicate that, unconditionally, wealthier migrants
# tend to move toward lower-tax counties, which is consistent with tax avoidance.

p1 = binsreg(df$agi / df$n1, df$highTaxDiff)

p1Plot = p1$bins_plot +
  labs(
    title = "Migrants' Income vs. High-Income Tax Premium at Destination",
    subtitle = "All U.S. county-pairs, 2011–2022",
    x = "High-Income Tax Rate Differential (Destination − Origin)",
    y = "Migrants' Per-Capita AGI ($000s)",
    caption = paste(
      "Binscatter (equal-count bins). Tax differential > 0 means the destination county",
      "has a higher effective tax rate on high-income households than the origin county.",
      sep = "\n"
    )
  )  + coord_cartesian(xlim=c(-.15, .15)) +
  theme(text=element_text(family='serif'))

ggsave("Output/Visualizations/Migrant_per-capita_AGI_vs_High-Income_Tax_Differential.pdf",
       plot=p1Plot, width=6,height=6)

# ---- Figure 2: Migrants' Wealth vs. Low-Income Tax Differential (All Counties) ----
#
# Repeating Figure 1 using the effective tax rate for low-income households.
# Comparing the two slopes shows whether tax-sensitivity differs by income group:
#   if Figure 1 has a steeper negative slope than Figure 2, high-income movers
#   are more tax-responsive than low-income movers.

p2 = binsreg(df$agi / df$n1, df$lowTaxDiff)

p2Plot = p2$bins_plot +
  labs(
    title = "Migrants' Income vs. Low-Income Tax Premium at Destination",
    subtitle = "All U.S. county-pairs, 2011–2022",
    x = "Low-Income Tax Rate Differential (Destination − Origin)",
    y = "Migrants' Per-Capita AGI ($000s)",
    caption = paste(
      "Binscatter (equal-count bins). Tax differential > 0 means the destination county",
      "has a higher effective tax rate on low-income households than the origin county.",
      sep = "\n"
    )
  ) + coord_cartesian(xlim=c(-.15, .15)) +
  theme(text=element_text(family='serif'))

ggsave("Output/Visualizations/Migrant_per-capita_AGI_vs_Low-Income_Tax_Differential.pdf",
       plot=p2Plot, width=6, height=6)

# ---- Figures 3 & 4: Manhattan Case Study ----
#
# Looking at just Manhattan to make less noisy.
# Among people leaving Manhattan, does the destination county's high-income tax rate
# predict the wealth of people heading there?
#
# Counterintuitive finding: Manhattan leavers headed to *higher*-tax counties
# tend to be wealthier on average. A possible explanation is that high earners
# are relocating to other expensive, high-tax metros (San Francisco, Boston, etc.)
# rather than fleeing taxes per se.

manExodus = df |> filter(y1_fips == 36061)
positiveManExodus = manExodus |> filter(agi > 0) # keep only positive AGI for log transform

# ---- Figure 3: Binscatter for Manhattan ----
p3 = binsreg(manExodus$agi / manExodus$n1, manExodus$highTaxDiff)

p3Plot = p3$bins_plot +
  labs(
    title = "Manhattan Emigrants: Wealth vs. Destination Tax Rate",
    subtitle = "People leaving Manhattan (FIPS 36061), all years",
    x = "High-Income Tax Rate Differential (Destination − Origin)",
    y = "Migrants' Per-Capita AGI ($000s)",
    caption = "Binscatter of Manhattan-origin county-pair observations."
  ) + coord_cartesian(xlim=c(-.15, 0)) + theme(text=element_text(family='serif'))

ggsave("Output/Visualizations/Manhattan_Emigrants_mean_AGI_vs_Destination_High-Income_Tax_Differential.pdf",
       plot=p3Plot, width=6, height=6)

# ---- Figure 4: Scatter with smoother on log scale----
# Logging per-capita AGI compresses the heavy right skew in income.
# The dashed vertical line marks destinations with the same tax rate as Manhattan.

# ggplot(positiveManExodus, aes(x = highTaxDiff, y = log(agi / n1))) +
#   geom_point(alpha = 0.35, size = 1.2, color = "gray40") +
#   geom_smooth(method = "loess", color = "steelblue", se = TRUE) +
#   geom_vline(xintercept = 0, linetype = "dashed", color = "firebrick", linewidth = 0.6) +
#   labs(
#     title = "Manhattan Emigrants: Log Income vs. Destination Tax Rate",
#     subtitle = "Positive-AGI observations only; all years",
#     x = "High-Income Tax Rate Differential (Destination − Origin)",
#     y = "Log Migrants' Per-Capita AGI",
#     caption = paste(
#       "Smoothed scatter with 95% confidence band. Red dashed line: destination tax rate = Manhattan's.",
#       "Rightward observations represent moves to higher-tax destinations.",
#       sep = "\n"
#     )
#   )


# ---- Figure 5: NY Tax Shock — Change in Out-Migration by Previous Migrants' Income ----
#
# In 2021, New York State raised its top marginal income tax rate from 6.85% to 9.85%
# on income exceeding $1M. This is our cleanest policy shock.
#
# Strategy: for each NY county to non-NY county pair, compare migration flows in 2021
# to the 2020 baseline. The x-axis shows how wealthy the migrants to that destination
# were in 2020 (a proxy for the destination county attracting high-income New Yorkers).
#
# Hypothesis: if the tax increase caused high earners to leave, destinations that were
# already attracting wealthy New Yorkers in 2020 should see a larger *increase* in flows
# in 2021. A positive slope would support this hypothesis.

nyExodus = df |> filter(floor(y1_fips / 1000) == 36 & floor(y2_fips / 1000) != 36)

# Year convention: year == t means the person physically moved during year t+1.
#
# year == 2019: moves during 2020  (last pre-shock year)
# year == 2020: moves during 2021  (immediate response)
# year == 2021: moves during 2022  (lagged response, if it exists)
#
# Baseline: year == 2019 (pre-shock)
# Post: average of year %in% c(2020, 2021) (captures both response years)

# Pre-shock: per-capita AGI and flow count from year 2019
baseline = nyExodus |>
  filter(year == 2019) |>
  mutate(pcAgiPre = agi / n1, n1_pre = n1) |>
  select(y1_fips, y2_fips, pcAgiPre, n1_pre)

# Post-shock: average annual flow across the two post years (2020 and 2021 labels)
postFlows = nyExodus |>
  filter(year %in% c(2020, 2021)) |>
  group_by(y1_fips, y2_fips) |>
  summarize(n1_post = mean(n1, na.rm = TRUE), .groups = "drop")

finalNyExodus = baseline |>
  inner_join(postFlows, by = c("y1_fips", "y2_fips")) |>
  filter(pcAgiPre > 0 & n1_pre > 0) # Need a good pre-shock baseline

# Figure 5: Binscatter
p5 = binsreg(
  finalNyExodus$n1_post / finalNyExodus$n1_pre - 1,
  log(finalNyExodus$pcAgiPre)
)

p5Plot = p5$bins_plot +
  geom_hline(yintercept = 0, linetype = "dashed", color = "firebrick", linewidth = 0.6) +
  labs(
    title = "NY Tax Shock: Change in Out-Migration by Destination's Previous Migrants' Income",
    subtitle = "NY-origin county-pairs; avg. post-shock flows (2021–2022) vs. pre-shock baseline (2020)",
    x = "Log Per-Capita AGI of Migrants to Destination (2019 pre-shock baseline)",
    y = "Proportional Change in Migration Flow During and After NY Tax Hike",
    caption = paste(
      "Binscatter. Y-axis: (avg. 2020–2021 label flow / 2019 label flow) − 1; zero = no change (red dashed).",
      "X-axis: wealth of people moving from NY to each destination in the pre-shock year.",
      "Destinations attracting wealthier NY migrants before the shock are on the right.",
      sep = "\n"
    )
  ) + coord_cartesian(xlim=c(3, 6)) +
  theme(text=element_text(family='serif'))

ggsave("Output/Visualizations/Change_in_flow_after_NY_tax_hike_vs_pre-tax-hike_migrant_mean_agi.pdf",
       plot=p5Plot, width=8, height=6, units='in')


# ---- Figure 6: NJ Tax Shock — Change in Out-Migration by Previous Migrants' Income ----
#
# NJ raised its top marginal rate effective tax year 2018.
# Pre-shock baseline: year == 2016 (moves during 2017, last pre-shock year).
# Post-shock: year %in% c(2017, 2018) (moves during 2018-2019).

njExodus = df |> filter(floor(y1_fips / 1000) == 34 & floor(y2_fips / 1000) != 34)

baseline_nj = njExodus |>
  filter(year == 2016) |>
  mutate(pcAgiPre = agi / n1, n1_pre = n1) |>
  select(y1_fips, y2_fips, pcAgiPre, n1_pre)

postFlows_nj = njExodus |>
  filter(year %in% c(2017, 2018)) |>
  group_by(y1_fips, y2_fips) |>
  summarize(n1_post = mean(n1, na.rm = TRUE), .groups = "drop")

finalNjExodus = baseline_nj |>
  inner_join(postFlows_nj, by = c("y1_fips", "y2_fips")) |>
  filter(pcAgiPre > 0 & n1_pre > 0)

p6 = binsreg(
  finalNjExodus$n1_post / finalNjExodus$n1_pre - 1,
  log(finalNjExodus$pcAgiPre)
)

p6Plot = p6$bins_plot +
  geom_hline(yintercept = 0, linetype = "dashed", color = "firebrick", linewidth = 0.6) +
  labs(
    title = "NJ Tax Shock: Change in Out-Migration by Destination's Previous Migrants' Income",
    subtitle = "NJ-origin county-pairs; avg. post-shock flows (2018-2019) vs. pre-shock baseline (2017)",
    x = "Log Per-Capita AGI of Migrants to Destination (2016 pre-shock baseline)",
    y = "Proportional Change in Migration Flow After NJ Tax Hike",
    caption = paste(
      "Binscatter. Y-axis: (avg. 2017-2018 label flow / 2016 label flow) - 1; zero = no change (red dashed).",
      "X-axis: wealth of people moving from NJ to each destination in the pre-shock year.",
      "Destinations attracting wealthier NJ migrants before the shock are on the right.",
      sep = "\n"
    )
  ) + coord_cartesian(xlim = c(3, 6)) +
  theme(text = element_text(family = "serif"))

ggsave("Output/Visualizations/NJ_semicausal_binscatter.pdf",
       plot = p6Plot, width = 8, height = 6, units = "in")


# ---- Figure 7: DE Estate Tax Repeal — Change in In-Migration by Previous Migrants' Income ----
#
# Delaware repealed its estate tax effective 2018. Treatment is on the DESTINATION side:
# we ask whether flows *into* Delaware rose after the repeal, and whether the income
# composition of those in-migrants changed. Origins within Delaware are excluded.
# Pre-shock baseline: year == 2016. Post-shock: year %in% c(2017, 2018).

deInflux = df |> filter(floor(y2_fips / 1000) == 10 & floor(y1_fips / 1000) != 10)

baseline_de = deInflux |>
  filter(year == 2016) |>
  mutate(pcAgiPre = agi / n1, n1_pre = n1) |>
  select(y1_fips, y2_fips, pcAgiPre, n1_pre)

postFlows_de = deInflux |>
  filter(year %in% c(2017, 2018)) |>
  group_by(y1_fips, y2_fips) |>
  summarize(n1_post = mean(n1, na.rm = TRUE), .groups = "drop")

finalDeInflux = baseline_de |>
  inner_join(postFlows_de, by = c("y1_fips", "y2_fips")) |>
  filter(pcAgiPre > 0 & n1_pre > 0)

p7 = binsreg(
  finalDeInflux$n1_post / finalDeInflux$n1_pre - 1,
  log(finalDeInflux$pcAgiPre)
)

p7Plot = p7$bins_plot +
  geom_hline(yintercept = 0, linetype = "dashed", color = "firebrick", linewidth = 0.6) +
  labs(
    title = "DE Estate Tax Repeal: Change in In-Migration by Origin's Previous Migrants' Income",
    subtitle = "DE-destination county-pairs; avg. post-shock flows (2018-2019) vs. pre-shock baseline (2017)",
    x = "Log Per-Capita AGI of Migrants Arriving in Delaware (2016 pre-shock baseline)",
    y = "Proportional Change in In-Migration Flow After DE Estate Tax Repeal",
    caption = paste(
      "Binscatter. Y-axis: (avg. 2017-2018 label flow / 2016 label flow) - 1; zero = no change (red dashed).",
      "X-axis: wealth of people moving to Delaware from each origin in the pre-shock year.",
      "Origins sending wealthier migrants to Delaware before the repeal are on the right.",
      sep = "\n"
    )
  ) + coord_cartesian(xlim = c(3, 6)) +
  theme(text = element_text(family = "serif"))

ggsave("Output/Visualizations/DE_semicausal_binscatter.pdf",
       plot = p7Plot, width = 8, height = 6, units = "in")

