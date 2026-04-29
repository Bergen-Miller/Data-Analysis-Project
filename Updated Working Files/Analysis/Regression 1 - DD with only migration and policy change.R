library(tidyverse)
library(fixest)

load("Updated Working Files/Clean Data/cleanMigrationData.RData")
# Loads: df (county-pair migration panel, 2011-2021 labels)

# ---- Year convention note ----
# The migration data labels each observation by the STARTING year of
# the IRS file. So `year == t` means the person was in y1_fips on
# their year-t return and in y2_fips on their year-(t+1) return,
# i.e., they physically moved during year t+1.
#
# NY raised its top marginal rate effective tax year 2021.
# We chose pre = year <= 2019, post = year %in% c(2020, 2021)
# This keeps the pre-period as untreated as possible and captures
# both the immediate (2021) and lagged (2022) responses.

# ---- Build regression sample ----

# Treatment group: origin in NY state (FIPS prefix 36)
# Control group: origin outside NY
# Exclude: destinations within NY, only want out-migration flow

df_reg <- df |>
  filter(floor(y2_fips / 1000) != 36) |>
  mutate(
    treat = ifelse(floor(y1_fips / 1000) == 36, 1, 0),
    post  = ifelse(year %in% c(2020, 2021), 1, 0)
  )

# ---- Model 1: Effect on migration volume ----
# This looks at the log number of returns (n1) moving from y1 to y2,
# treat*post is the DiD coefficient of interest.

model1 <- feols(
  log(n1) ~ treat * post,
  data = df_reg
)

etable(model1)

# ---- Model 2: Effect on income of migrants ----
# This looks at the log per-capita AGI of people making the move.
# Asks whether the COMPOSITION of migrants (not just the count) changed.

model2 <- feols(
  log(agi / n1) ~ treat * post,
  data = df_reg
)

etable(model2)

save(
  model1, model2,
  file = "Output/Quantitative Estimates/NY Tax Hike DD Regression Results.RData"
)


# ---- New Jersey 2018 DiD ----
#
# NJ raised its top marginal rate from 8.97% to 10.75% effective tax
# year 2018. year == 2017: moves during 2018 (immediate response).
# year == 2018: moves during 2019 (lagged response).
# We chose pre = year <= 2016, post = year %in% c(2017, 2018)

# Treatment group: origin in NJ state (FIPS prefix 34)
# Control group: origin outside NJ
# Exclude: destinations within NJ, only want out-migration flow

df_reg_nj <- df |>
  filter(floor(y2_fips / 1000) != 34) |>
  mutate(
    treat = ifelse(floor(y1_fips / 1000) == 34, 1, 0),
    post  = ifelse(year %in% c(2017, 2018), 1, 0)
  )

# ---- Model 3: NJ - Effect on migration volume ----
# treat*post asks whether NJ out-migration volume rose relative to
# other states after the 2018 rate hike.

model3 <- feols(
  log(n1) ~ treat * post,
  data = df_reg_nj
)

etable(model3)

# ---- Model 4: NJ - Effect on income of migrants ----
# Asks whether the income composition of people leaving NJ changed
# after the hike.

model4 <- feols(
  log(agi / n1) ~ treat * post,
  data = df_reg_nj
)

etable(model4)

save(
  model3, model4,
  file = "Output/Quantitative Estimates/NJ Tax Hike DD Regression Results.RData"
)


# ---- Delaware 2018 Estate Tax Repeal DiD ----
#
# Delaware repealed its estate tax effective 2018 (previously 16% on
# estates above $5.49M). Unlike the NJ and NY hikes, this is a tax
# cut, so the incentive pulls high-wealth households toward Delaware
# rather than away from it. We flip the treatment assignment
# accordingly: treatment = destination is Delaware, and we ask
# whether Delaware attracted more (and wealthier) migrants after
# the repeal.
#
# year == 2017: moves during 2018 (immediate response)
# year == 2018: moves during 2019 (lagged response)
# We chose pre = year <= 2016, post = year %in% c(2017, 2018)

# Treatment group: destination in Delaware (FIPS prefix 10)
# Control group: destination outside Delaware
# Exclude: origins within Delaware, only want in-migration flow

df_reg_de <- df |>
  filter(floor(y1_fips / 1000) != 10) |>
  mutate(
    treat = ifelse(floor(y2_fips / 1000) == 10, 1, 0),
    post  = ifelse(year %in% c(2017, 2018), 1, 0)
  )

# ---- Model 5: Delaware - Effect on migration volume ----
# treat*post asks whether flows into Delaware rose relative to other
# destinations after the estate tax repeal.

model5 <- feols(
  log(n1) ~ treat * post,
  data = df_reg_de
)

etable(model5)

# ---- Model 6: Delaware - Effect on income of migrants ----
# Asks whether the income composition of people moving to Delaware
# changed after the repeal.

model6 <- feols(
  log(agi / n1) ~ treat * post,
  data = df_reg_de
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
# Dashed vertical line marks the start of the post period.


# NY: post starts at year == 2020

ny_trends <- df_reg |>
  group_by(year, treat) |>
  summarize(
    mean_log_n1    = mean(log(n1), na.rm = TRUE),
    mean_log_pcAgi = mean(log(agi / n1), na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(group = ifelse(treat == 1, "New York", "Other States"))

p_ny_vol <- ggplot(ny_trends, aes(x = year, y = mean_log_n1, color = group)) +
  geom_vline(xintercept = 2019.5, linetype = "dashed", color = "gray50") +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2.5) +
  scale_color_manual(
    values = c("New York" = "firebrick", "Other States" = "steelblue")
  ) +
  labs(
    x = "Year (IRS file label)",
    y = "Mean Log Out-Migration Volume",
    color = NULL,
    caption = "Dashed line: NY 2021 rate increase. Post period: year >= 2020."
  ) +
  theme_bw() +
  theme(text = element_text(family = "serif"))

p_ny_inc <- ggplot(
  ny_trends, aes(x = year, y = mean_log_pcAgi, color = group)
) +
  geom_vline(xintercept = 2019.5, linetype = "dashed", color = "gray50") +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2.5) +
  scale_color_manual(
    values = c("New York" = "firebrick", "Other States" = "steelblue")
  ) +
  labs(
    x = "Year (IRS file label)",
    y = "Mean Log Migrant Per-Capita AGI",
    color = NULL,
    caption = "Dashed line: NY 2021 rate increase. Post period: year >= 2020."
  ) +
  theme_bw() +
  theme(text = element_text(family = "serif"))


# NJ: post starts at year == 2017

nj_trends <- df_reg_nj |>
  group_by(year, treat) |>
  summarize(
    mean_log_n1    = mean(log(n1), na.rm = TRUE),
    mean_log_pcAgi = mean(log(agi / n1), na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(group = ifelse(treat == 1, "New Jersey", "Other States"))

p_nj_vol <- ggplot(nj_trends, aes(x = year, y = mean_log_n1, color = group)) +
  geom_vline(xintercept = 2016.5, linetype = "dashed", color = "gray50") +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2.5) +
  scale_color_manual(
    values = c("New Jersey" = "firebrick", "Other States" = "steelblue")
  ) +
  labs(
    x = "Year (IRS file label)",
    y = "Mean Log Out-Migration Volume",
    color = NULL,
    caption = "Dashed line: NJ 2018 rate increase. Post period: year >= 2017."
  ) +
  theme_bw() +
  theme(text = element_text(family = "serif"))

p_nj_inc <- ggplot(
  nj_trends, aes(x = year, y = mean_log_pcAgi, color = group)
) +
  geom_vline(xintercept = 2016.5, linetype = "dashed", color = "gray50") +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2.5) +
  scale_color_manual(
    values = c("New Jersey" = "firebrick", "Other States" = "steelblue")
  ) +
  labs(
    x = "Year (IRS file label)",
    y = "Mean Log Migrant Per-Capita AGI",
    color = NULL,
    caption = "Dashed line: NJ 2018 rate increase. Post period: year >= 2017."
  ) +
  theme_bw() +
  theme(text = element_text(family = "serif"))


# Delaware: post starts at year == 2017
# Note: treat = destination in DE, so groups label destinations not origins.

de_trends <- df_reg_de |>
  group_by(year, treat) |>
  summarize(
    mean_log_n1    = mean(log(n1), na.rm = TRUE),
    mean_log_pcAgi = mean(log(agi / n1), na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(group = ifelse(treat == 1, "Delaware", "Other Destinations"))

p_de_vol <- ggplot(de_trends, aes(x = year, y = mean_log_n1, color = group)) +
  geom_vline(xintercept = 2016.5, linetype = "dashed", color = "gray50") +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2.5) +
  scale_color_manual(
    values = c("Delaware" = "firebrick", "Other Destinations" = "steelblue")
  ) +
  labs(
    x = "Year (IRS file label)",
    y = "Mean Log In-Migration Volume",
    color = NULL,
    caption = paste(
      "Dashed line: Delaware 2018 estate tax repeal.",
      "Post period: year >= 2017."
    )
  ) +
  theme_bw() +
  theme(text = element_text(family = "serif"))

p_de_inc <- ggplot(
  de_trends, aes(x = year, y = mean_log_pcAgi, color = group)
) +
  geom_vline(xintercept = 2016.5, linetype = "dashed", color = "gray50") +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2.5) +
  scale_color_manual(
    values = c("Delaware" = "firebrick", "Other Destinations" = "steelblue")
  ) +
  labs(
    x = "Year (IRS file label)",
    y = "Mean Log Migrant Per-Capita AGI",
    color = NULL,
    caption = paste(
      "Dashed line: Delaware 2018 estate tax repeal.",
      "Post period: year >= 2017."
    )
  ) +
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

