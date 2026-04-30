library(tidyverse)
library(fixest)

load("Updated Working Files/Clean Data/cleanMigrationData.RData")
# Loads: df (county-pair migration panel, 2011-2021 labels)

# ---- Year convention note ----
# The migration data labels each observation by the STARTING year of
# the IRS file. So `year == t` means the person was in y1_fips on
# their year-t return and in y2_fips on their year-(t+1) return,
# i.e., they physically moved during calendar year t+1.
#
# Post period definitions are correct: e.g., for Delaware (repealed
# estate tax effective calendar 2018), post = year %in% c(2017, 2018)
# because IRS label 2017 captures moves during calendar 2018 -- the
# first year the repeal was in effect. The post period IS after the
# policy change; the IRS label is just one year behind the calendar.
#
# All parallel trends plots use CALENDAR YEAR OF MOVE (IRS label + 1)
# on the x-axis so dashed lines align with the actual policy year.
#
# NY: rate hike effective tax year 2021. post = year %in% c(2020, 2021)
#   captures moves during calendar 2021 (immediate) and 2022 (lagged).
# NJ: rate hike effective tax year 2018. post = year %in% c(2017, 2018)
#   captures moves during calendar 2018 (immediate) and 2019 (lagged).
# DE: estate tax repealed effective calendar 2018.
#   post = year %in% c(2017, 2018) captures same calendar years.

# ---- Build regression sample ----

# Treatment group: origin in NY state (FIPS prefix 36)
# Control group: origin outside NY
# Exclude: destinations within NY, only want out-migration flow

df_reg = df |>
  filter(floor(y2_fips / 1000) != 36) |>
  mutate(
    pcAgi = agi / n1,
    treat = ifelse(floor(y1_fips / 1000) == 36, 1, 0),
    post = ifelse(year %in% c(2020, 2021), 1, 0)
  )

# ---- Model 1: Effect on migration volume ----
# This looks at the log number of returns (n1) moving from y1 to y2,
# treat*post is the DiD coefficient of interest.

model1 = feols(
  log(n1) ~ treat * post,
  data = df_reg
)

etable(model1)

# ---- Model 2: Effect on income of migrants ----
# This looks at the log per-capita AGI of people making the move.
# Asks whether the COMPOSITION of migrants (not just the count) changed.

model2 = feols(
  log(pcAgi) ~ treat * post,
  data = filter(df_reg, pcAgi > 0)
)

etable(model2)

save(
  model1, model2,
  file = "Output/Quantitative Estimates/NY Tax Hike DD Regression Results.RData"
)


# ---- New Jersey 2018 DiD ----
#
# NJ raised its top marginal rate from 8.97% to 10.75% effective tax
# year 2018. IRS label 2017 = moves during calendar 2018 (immediate).
# IRS label 2018 = moves during calendar 2019 (lagged response).
# pre = year <= 2016, post = year %in% c(2017, 2018)

# Treatment group: origin in NJ state (FIPS prefix 34)
# Control group: origin outside NJ
# Exclude: destinations within NJ, only want out-migration flow

df_reg_nj = df |>
  filter(floor(y2_fips / 1000) != 34) |>
  mutate(
    pcAgi = agi / n1,
    treat = ifelse(floor(y1_fips / 1000) == 34, 1, 0),
    post = ifelse(year %in% c(2017, 2018), 1, 0)
  )

# ---- Model 3: NJ - Effect on migration volume ----
# treat*post asks whether NJ out-migration volume rose relative to
# other states after the 2018 rate hike.

model3 = feols(
  log(n1) ~ treat * post,
  data = df_reg_nj
)

etable(model3)

# ---- Model 4: NJ - Effect on income of migrants ----
# Asks whether the income composition of people leaving NJ changed
# after the hike.

model4 = feols(
  log(pcAgi) ~ treat * post,
  data = filter(df_reg_nj, pcAgi > 0)
)

etable(model4)

save(
  model3, model4,
  file = "Output/Quantitative Estimates/NJ Tax Hike DD Regression Results.RData"
)


# ---- Delaware 2018 Estate Tax Repeal DiD ----
#
# Delaware repealed its estate tax effective calendar 2018 (previously
# 16% on estates above $5.49M). Unlike the NJ and NY hikes, this is a
# tax cut, so the incentive pulls high-wealth households toward Delaware
# rather than away from it. Treatment is on the DESTINATION side: we ask
# whether Delaware attracted more (and wealthier) migrants after the repeal.
#
# IRS label 2017 = moves during calendar 2018 (immediate response)
# IRS label 2018 = moves during calendar 2019 (lagged response)
# pre = year <= 2016, post = year %in% c(2017, 2018)

# Treatment group: destination in Delaware (FIPS prefix 10)
# Control group: destination outside Delaware
# Exclude: origins within Delaware, only want in-migration flow

df_reg_de = df |>
  filter(floor(y1_fips / 1000) != 10) |>
  mutate(
    pcAgi = agi / n1,
    treat = ifelse(floor(y2_fips / 1000) == 10, 1, 0),
    post = ifelse(year %in% c(2017, 2018), 1, 0)
  )

# ---- Model 5: Delaware - Effect on migration volume ----
# treat*post asks whether flows into Delaware rose relative to other
# destinations after the estate tax repeal.

model5 = feols(
  log(n1) ~ treat * post,
  data = df_reg_de
)

etable(model5)

# ---- Model 6: Delaware - Effect on income of migrants ----
# Asks whether the income composition of people moving to Delaware
# changed after the repeal.

model6 = feols(
  log(pcAgi) ~ treat * post,
  data = filter(df_reg_de, pcAgi > 0)
)

etable(model6)

save(
  model5, model6,
  file = paste0(
    "Output/Quantitative Estimates/",
    "DE Estate Tax Repeal DD Regression Results.RData"
  )
)


# ---- Visualizations ----
#
# For each DiD, plot mean log outcome by year for treated vs. control.
# X-axis is CALENDAR YEAR OF MOVE (= IRS file label + 1) so the dashed
# vertical line aligns with the actual effective year of each policy.


# NY: rate hike effective tax year 2021. First affected calendar year
# of moves is 2021 (IRS label 2020). Dashed line at calendar 2021.

ny_trends = df_reg |>
  mutate(
    log_pcAgi = NA_real_
  ) |>
  mutate(log_pcAgi = replace(log_pcAgi, pcAgi > 0, log(pcAgi[pcAgi > 0]))) |>
  group_by(year, treat) |>
  summarize(
    mean_log_n1 = mean(log(n1), na.rm = TRUE),
    mean_log_pcAgi = mean(log_pcAgi, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(group = ifelse(treat == 1, "New York", "Other States"))

p_ny_vol = ggplot(
  ny_trends, aes(x = year + 1, y = mean_log_n1, color = group)
) +
  geom_vline(xintercept = 2021, linetype = "dashed", color = "gray50") +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2.5) +
  scale_color_manual(
    values = c("New York" = "firebrick", "Other States" = "steelblue")
  ) +
  labs(
    x = "Calendar Year of Move",
    y = "Mean Log Out-Migration Volume",
    color = NULL,
    caption = paste(
      "Dashed line: New York top-rate increase effective tax year 2021.",
      "Moves in calendar 2021 and later are post-shock.",
      sep = "\n"
    )
  ) +
  scale_x_continuous(breaks = sort(unique(ny_trends$year + 1))) +
  theme_bw() +
  theme(text = element_text(family = "serif"))

p_ny_inc = ggplot(
  ny_trends, aes(x = year + 1, y = mean_log_pcAgi, color = group)
) +
  geom_vline(xintercept = 2021, linetype = "dashed", color = "gray50") +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2.5) +
  scale_color_manual(
    values = c("New York" = "firebrick", "Other States" = "steelblue")
  ) +
  labs(
    x = "Calendar Year of Move",
    y = "Mean Log Migrant Per-Capita AGI",
    color = NULL,
    caption = paste(
      "Dashed line: New York top-rate increase effective tax year 2021.",
      "Moves in calendar 2021 and later are post-shock.",
      "Non-positive migrant AGI observations are dropped before logging to",
      "avoid spurious dips from log(0) or log(negative AGI).",
      sep = "\n"
    )
  ) +
  scale_x_continuous(breaks = sort(unique(ny_trends$year + 1))) +
  theme_bw() +
  theme(text = element_text(family = "serif"))


# NJ: rate hike effective tax year 2018. First affected calendar year
# of moves is 2018 (IRS label 2017). Dashed line at calendar 2018.

nj_trends = df_reg_nj |>
  mutate(
    log_pcAgi = NA_real_
  ) |>
  mutate(log_pcAgi = replace(log_pcAgi, pcAgi > 0, log(pcAgi[pcAgi > 0]))) |>
  group_by(year, treat) |>
  summarize(
    mean_log_n1 = mean(log(n1), na.rm = TRUE),
    mean_log_pcAgi = mean(log_pcAgi, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(group = ifelse(treat == 1, "New Jersey", "Other States"))

p_nj_vol = ggplot(
  nj_trends, aes(x = year + 1, y = mean_log_n1, color = group)
) +
  geom_vline(xintercept = 2018, linetype = "dashed", color = "gray50") +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2.5) +
  scale_color_manual(
    values = c("New Jersey" = "firebrick", "Other States" = "steelblue")
  ) +
  labs(
    x = "Calendar Year of Move",
    y = "Mean Log Out-Migration Volume",
    color = NULL,
    caption = paste(
      "Dashed line: New Jersey top-rate increase effective tax year 2018.",
      "Moves in calendar 2018 and later are post-shock.",
      sep = "\n"
    )
  ) +
  scale_x_continuous(breaks = sort(unique(nj_trends$year + 1))) +
  theme_bw() +
  theme(text = element_text(family = "serif"))

p_nj_inc = ggplot(
  nj_trends, aes(x = year + 1, y = mean_log_pcAgi, color = group)
) +
  geom_vline(xintercept = 2018, linetype = "dashed", color = "gray50") +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2.5) +
  scale_color_manual(
    values = c("New Jersey" = "firebrick", "Other States" = "steelblue")
  ) +
  labs(
    x = "Calendar Year of Move",
    y = "Mean Log Migrant Per-Capita AGI",
    color = NULL,
    caption = paste(
      "Dashed line: New Jersey top-rate increase effective tax year 2018.",
      "Moves in calendar 2018 and later are post-shock.",
      sep = "\n"
    )
  ) +
  scale_x_continuous(breaks = sort(unique(nj_trends$year + 1))) +
  theme_bw() +
  theme(text = element_text(family = "serif"))


# Delaware: estate tax repealed effective calendar 2018. First affected
# calendar year of moves is 2018 (IRS label 2017). Dashed line at 2018.
# Note: treat = destination in DE, so groups label destinations not origins.

de_trends = df_reg_de |>
  mutate(
    log_pcAgi = NA_real_
  ) |>
  mutate(log_pcAgi = replace(log_pcAgi, pcAgi > 0, log(pcAgi[pcAgi > 0]))) |>
  group_by(year, treat) |>
  summarize(
    mean_log_n1 = mean(log(n1), na.rm = TRUE),
    mean_log_pcAgi = mean(log_pcAgi, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(group = ifelse(treat == 1, "Delaware", "Other Destinations"))

p_de_vol = ggplot(
  de_trends, aes(x = year + 1, y = mean_log_n1, color = group)
) +
  geom_vline(xintercept = 2018, linetype = "dashed", color = "gray50") +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2.5) +
  scale_color_manual(
    values = c("Delaware" = "firebrick", "Other Destinations" = "steelblue")
  ) +
  labs(
    x = "Calendar Year of Move",
    y = "Mean Log In-Migration Volume",
    color = NULL,
    caption = paste(
      "Dashed line: Delaware estate tax repealed effective calendar 2018.",
      "Moves in calendar 2018 and later are post-shock.",
      sep = "\n"
    )
  ) +
  scale_x_continuous(breaks = sort(unique(de_trends$year + 1))) +
  theme_bw() +
  theme(text = element_text(family = "serif"))

p_de_inc = ggplot(
  de_trends, aes(x = year + 1, y = mean_log_pcAgi, color = group)
) +
  geom_vline(xintercept = 2018, linetype = "dashed", color = "gray50") +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2.5) +
  scale_color_manual(
    values = c("Delaware" = "firebrick", "Other Destinations" = "steelblue")
  ) +
  labs(
    x = "Calendar Year of Move",
    y = "Mean Log Migrant Per-Capita AGI",
    color = NULL,
    caption = paste(
      "Dashed line: Delaware estate tax repealed effective calendar 2018.",
      "Moves in calendar 2018 and later are post-shock.",
      sep = "\n"
    )
  ) +
  scale_x_continuous(breaks = sort(unique(de_trends$year + 1))) +
  theme_bw() +
  theme(text = element_text(family = "serif"))


ggsave(
  "Output/Visualizations/Stage 3/NY_DiD_volume_trends.pdf",
  plot = p_ny_vol
)
ggsave(
  "Output/Visualizations/Stage 3/NY_DiD_income_trends.pdf",
  plot = p_ny_inc
)
ggsave(
  "Output/Visualizations/Stage 3/NJ_DiD_volume_trends.pdf",
  plot = p_nj_vol
)
ggsave(
  "Output/Visualizations/Stage 3/NJ_DiD_income_trends.pdf",
  plot = p_nj_inc
)
ggsave(
  "Output/Visualizations/Stage 3/DE_DiD_volume_trends.pdf",
  plot = p_de_vol
)
ggsave(
  "Output/Visualizations/Stage 3/DE_DiD_income_trends.pdf",
  plot = p_de_inc
)


# ---- Diagnostic: Per-capita AGI by state for the NY DiD control group ----
#
# This plot checks whether an apparent post-2020 control-group dip comes
# from a broad shock or from a handful of outlier states. Non-positive
# per-capita AGI observations are dropped before logging so that zero or
# negative AGI rows do not create -Inf artifacts in the state means.

fips_to_abb = c(
  "1"  = "AL", "2"  = "AK", "4"  = "AZ", "5"  = "AR", "6"  = "CA",
  "8"  = "CO", "9"  = "CT", "10" = "DE", "11" = "DC", "12" = "FL",
  "13" = "GA", "15" = "HI", "16" = "ID", "17" = "IL", "18" = "IN",
  "19" = "IA", "20" = "KS", "21" = "KY", "22" = "LA", "23" = "ME",
  "24" = "MD", "25" = "MA", "26" = "MI", "27" = "MN", "28" = "MS",
  "29" = "MO", "30" = "MT", "31" = "NE", "32" = "NV", "33" = "NH",
  "34" = "NJ", "35" = "NM", "37" = "NC", "38" = "ND", "39" = "OH",
  "40" = "OK", "41" = "OR", "42" = "PA", "44" = "RI", "45" = "SC",
  "46" = "SD", "47" = "TN", "48" = "TX", "49" = "UT", "50" = "VT",
  "51" = "VA", "53" = "WA", "54" = "WV", "55" = "WI", "56" = "WY"
)

state_income_trends = df_reg |>
  filter(treat == 0) |>
  mutate(
    origin_state = floor(y1_fips / 1000),
    log_pcAgi = NA_real_
  ) |>
  mutate(log_pcAgi = replace(log_pcAgi, pcAgi > 0, log(pcAgi[pcAgi > 0]))) |>
  group_by(year, origin_state) |>
  summarize(
    mean_log_pcAgi = mean(log_pcAgi, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    calendar_year = year + 1,
    state_abb = fips_to_abb[as.character(origin_state)]
  )

# Identify states with the largest AGI drop from calendar 2020 to 2021
# (IRS labels 2019 -> 2020). Top 8 dippers are highlighted in color.
dip_states = state_income_trends |>
  filter(year %in% c(2019, 2020)) |>
  pivot_wider(
    names_from = year,
    values_from = mean_log_pcAgi,
    names_prefix = "yr_"
  ) |>
  filter(!is.na(yr_2019) & !is.na(yr_2020)) |>
  mutate(dip = yr_2020 - yr_2019) |>
  arrange(dip) |>
  slice_head(n = 8) |>
  pull(origin_state)

state_income_trends = state_income_trends |>
  mutate(is_dip_state = origin_state %in% dip_states)

p_state_dip = ggplot(
  state_income_trends,
  aes(x = calendar_year, y = mean_log_pcAgi, group = origin_state)
) +
  geom_vline(xintercept = 2021, linetype = "dashed", color = "gray50") +
  geom_line(
    data = filter(state_income_trends, !is_dip_state),
    color = "gray80", linewidth = 0.35, alpha = 0.7
  ) +
  geom_line(
    data = filter(state_income_trends, is_dip_state),
    aes(color = state_abb), linewidth = 0.9
  ) +
  geom_point(
    data = filter(state_income_trends, is_dip_state),
    aes(color = state_abb), size = 2
  ) +
  labs(
    x = "Calendar Year of Move",
    y = "Mean Log Migrant Per-Capita AGI",
    color = "State",
    caption = paste(
      "NY DiD control group (non-NY origin counties) only.",
      "Gray lines: all other origin states.",
      "Colored lines: the 8 states with the steepest AGI drop from",
      "calendar 2020 to 2021 (IRS labels 2019 to 2020).",
      "Non-positive migrant AGI observations are dropped before logging.",
      "Dashed line: New York 2021 rate increase.",
      sep = "\n"
    )
  ) +
  scale_x_continuous(breaks = sort(unique(state_income_trends$calendar_year))) +
  theme_bw() +
  theme(text = element_text(family = "serif"))

ggsave(
  "Output/Visualizations/Stage 3/Control_States_Income_Trends_2020_Dip.pdf",
  plot = p_state_dip,
  width = 10, height = 6
)
