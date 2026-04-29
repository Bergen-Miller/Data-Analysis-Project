library(tidyverse)
library(tigris)
library(sf)
library(gganimate)
library(gifski)
library(scales)

# Cache county shapefiles so this only downloads once
options(tigris_use_cache = TRUE)

load("Updated Working Files/Clean Data/cleanMigrationData.RData")

# ============================================================
# MAP VISUALIZATION
# Geographically, where do people leave to
# when a major tax shock hits their city?
#
# Our Primary Case: Manhattan (FIPS 36061), using the NY State 2021 top
# marginal rate increase (6.85% to 9.85% on income > $1M) as
# the policy shock. The animation runs 2011–2021 so you can
# watch destination patterns shift in the shock year.
# ============================================================


# ---- Build county shapefile (continental US) ----

counties_sf = counties(cb = TRUE, resolution = "20m", year = 2020) |>
  filter(!STATEFP %in% c("02", "15", "72")) |> # drop AK, HI, PR
  mutate(fips = as.numeric(paste0(STATEFP, COUNTYFP))) |>
  select(fips, geometry) |>
  st_transform(5070) # Albers equal-area, better for national choropleth

# ---- Aggregate Manhattan out-flows by destination county and year ----

# Exclude intra-NY moves so the map focuses on people truly leaving the state
manFlows = df |>
  filter(y1_fips == 36061,
         floor(y2_fips / 1000) != 36) |>
  group_by(y2_fips, year) |>
  summarize(n1 = sum(n1, na.rm = TRUE), .groups = "drop") |>
  mutate(year = as.integer(year))

# Get all years in the data
all_years = sort(unique(manFlows$year))

# Join migration counts onto the spatial object.
# Counties with no observed Manhattan out-migration stay NA and render
# as the gray base layer rather than being forced to zero.
mapData = counties_sf |>
  left_join(manFlows, by = c("fips" = "y2_fips")) |>
  filter(!is.na(year))

# ---- Color scale: log(n1 + 1) so zeros stay visible ----
# We know +1 is not best practice, primarily for readability.
# Make the labels plain return counts, easier to read.

fill_labels = function(x) {
  vals = round(expm1(x))
  ifelse(vals == 0, "0", comma(vals))
}


# White plot background, no panel gridlines, no axis text.
# County borders are drawn via geom_sf color in each plot.
map_theme = theme_minimal(base_size = 11) +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "gray30", size = 9),
    legend.position = "bottom",
    legend.key.width = unit(2.2, "cm"),
    legend.key.height = unit(0.35, "cm")
  )


# ---- Animated choropleth ----
# Layer 1: all counties in light gray (the "country background").
# Layer 2: only counties with observed Manhattan out-migration, colored
#           white-to-blue by log count. NA counties show through as gray.
# White borders on layer 2 give the county line effect.

anim = ggplot(mapData) +
  geom_sf(data = counties_sf, fill = "gray88", color = "white",
          linewidth = 0.15) +
  geom_sf(aes(fill = log1p(n1)), color = "white", linewidth = 0.15) +
  scale_fill_gradient(
    low = "#cfe2f3",
    high = "#0339f8",
    name = "IRS Returns\n(out of Manhattan)",
    labels = fill_labels,
    breaks = log1p(c(0, 10, 50, 200, 500, 1500)),
    na.value = "gray88"
  ) +
  labs(
    title    = "Where Do Manhattan Residents Move Each Year?",
    subtitle = "Year: {current_frame}  |  NY top marginal rate raised in 2021 (6.85% to 9.85%)"
  ) +
  map_theme +
  transition_manual(year)


# ---- Render and save ----
# Renders one frame per year (length(all_years) = 11).
# fps = 1.5 means each year is visible for ~0.67 seconds; adjust as needed.

animate(
  anim,
  nframes = length(all_years),
  fps = 1.5,
  width = 1000,
  height = 620,
  res = 110,
  renderer = gifski_renderer()
)

anim_save("Output/Visualizations/manhattan_exodus_animated.gif")

message("Animation saved to: Output/Visualizations/manhattan_exodus_animated.gif")


# ============================================================
# Non-Animated: Before vs. After the 2021 NY Tax Shock
#
# For each destination county outside NY, compare average
# inflows from all NY counties in 2019–2020 (pre-shock) to
# 2021 (post-shock). Shows which destinations gained or lost
# NY migrants after the rate increase.
# ============================================================

# Pre-shock: average annual inflows from any NY county, years <= 2019
# (moves through 2020, before the NY rate increase took effect)
nyFlowsPre = df |>
  filter(floor(y1_fips / 1000) == 36, # origin in NY state
         floor(y2_fips / 1000) != 36, # destination outside NY
         year <= 2019) |>
  group_by(y2_fips, year) |>
  summarize(n1 = sum(n1, na.rm = TRUE), .groups = "drop") |>
  group_by(y2_fips) |>
  summarize(n1_pre = mean(n1), .groups = "drop")

# Post-shock: average annual inflows across years 2020 and 2021
# (moves during 2021 and 2022)
nyFlowsPost = df |>
  filter(floor(y1_fips / 1000) == 36,
         floor(y2_fips / 1000) != 36,
         year %in% c(2020, 2021)) |>
  group_by(y2_fips, year) |>
  summarize(n1 = sum(n1, na.rm = TRUE), .groups = "drop") |>
  group_by(y2_fips) |>
  summarize(n1_post = mean(n1), .groups = "drop")

# Compute percent change; keep only counties with a meaningful baseline
# to remove clutter.
nyChange = nyFlowsPre |>
  inner_join(nyFlowsPost, by = "y2_fips") |>
  filter(n1_pre >= 5) |>  # drop near-zero baseline counties (noise)
  mutate(pct_change = (n1_post / n1_pre - 1) * 100,
         # Winsorise at ±100% for color scale legibility
         pct_change_capped = pmax(pmin(pct_change, 100), -100))

# Join to shapefile
staticMapData = counties_sf |>
  left_join(nyChange, by = c("fips" = "y2_fips"))

# Diverging color scale: blue = gained NY migrants, red = lost.
# Layer 1: all counties in light gray for the country background.
# Layer 2: only counties in nyChange colored; the rest stay gray.
nyMigrationChangeMap = 
  ggplot(staticMapData) +
  geom_sf(data = counties_sf, fill = "gray88", color = "white",
          linewidth = 0.15) +
  geom_sf(aes(fill = pct_change_capped), color = "white", linewidth = 0.15) +
  scale_fill_gradient2(
    low = "#FF0000",
    mid = "#e0e0e0",
    high = "#0339f8",
    midpoint = 0,
    name = "Change in NY\nIn-Migration (%)",
    labels = function(x) paste0(ifelse(x > 0, "+", ""), x, "%"),
    na.value = "gray88",
    limits = c(-100, 100)
  ) +
  labs(
    title    = "Where Did NY Migration Shift After the 2021 Tax Increase?",
    subtitle = "Percent change in arrivals from NY State: avg. 2021–2022 vs. avg. through 2020"
  ) +
  map_theme

ggsave('Output/Visualizations/NY_Emigration_Change_at_Tax_Policy-Map.pdf', 
       plot=nyMigrationChangeMap)

