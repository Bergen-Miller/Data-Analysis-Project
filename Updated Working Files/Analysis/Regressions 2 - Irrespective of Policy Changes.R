library(tidyverse)
library(fixest)

load('Updated Working Files/Clean Data/cleanMigrationData.RData')

#
# ---- Building Tibble for Regression ----

# The purpose of this script is to, without considering statutory policy changes,
#   considering the (non-causal) relationship between the historical mean agi of
#   people moving between a pair of counties and the increase in that flow following
#   a 1 percentage point 

# Create column for highTaxRate in y1 county in year t-1


# Create a tibble containing lagged values of a county-pair's difference
#   in taxes to low-income households, high-income households, the number
#   of returns and the agi of people moving

tax_rate_lookup = df |>
  select(y1_fips, year, y2_fips, lowTaxDiff, highTaxDiff, agi, n1) |>
  distinct() |>
  mutate(year = year + 1) |>  # shift year forward by 1
  rename(highTaxDiff_lag = highTaxDiff,
         lowTaxDiff_lag = lowTaxDiff,
         agi_lag = agi,
         n1_lag = n1)

# Join back to df
df1 = df |>
  left_join(tax_rate_lookup, by = c("y1_fips", 'y2_fips', "year"))

# Add columns for the 'shock' (not necessarily exogenous), applied to the
#   RELATIVE tax rates of a sending and receiving county.

df2 = df1 |> mutate(highTaxDiff_shock = highTaxDiff - highTaxDiff_lag,
                    lowTaxDiff_shock = lowTaxDiff - lowTaxDiff_lag,
                    pcAgi_lag = agi_lag / n1_lag) |>
  relocate(key, y1_fips, y2_fips, year, y2_state, y2_countyname, highTaxDiff_shock, lowTaxDiff_shock)


# For the following regressions, it is necessary to understand what each variable
#   created below means. highTaxDiff_shock is centered around zero and
#   is on the scale of 0.01. Consider two counties, such as Gallatin and Missoula counties
#   in Montana. Every year we observe over one hundred tax returns filed in Gallatin County
#   which are filed in Missoula County the next year. I'll refer to Gallatin as the 'sending'
#   county and Missoula as the 'receiving' county. 

# Missoula and Bozeman have historically had about the same tax rates paid by their households
#   with AGI above $200k, but from 2016 to 2017 those households in Missoula went from paying
#   an average rate of 28% to an average rate of 31%. At the same time, the rate paid by
#   Gallatin's high-income households did not increase. From 2017 to 2018, the rate paid by
#   Gallatin's high-income households fell by 9 percentage points, but the rate paid by Missoula's
#   high-income households fell by only 6 percentage points.

# Both from 2016 to 2017 and from 2017 to 2018, Missoula county experienced a positive tax shock
#   of 3 percentage points RELATIVE to Gallatin county. Did these tax shocks affect people's behavior?
#   Something we might expect is that high-income people are sensitive to their tax rates, and upon
#   seeing the increase in Missoula's relative tax burden, they might elect to stay in Gallatin county,
#   and the people who move to Missoula anyway would have a lower average income.

# These two instances of an increase in Missoula's relative tax rate to high-income households appear
#   as highTaxDiff_shock ~= 0.03 in year==2017 and in year==2018, because Missoula is the 'receiving' county.

# The values in our year, highTaxDiff_shock, and n1 columns are the year for which the increase
#   in the 'receiving' county's tax rate was highTaxDiff_shock from the previous, and n1 is the number of
#   households moving from the 'sending' county to the 'receiving' county over the next year.

# In addition to a series of years of people (n1) with agi (agi) moving from Gallatin county to Missoula
#   county, there are also people moving from Missoula county to Gallatin county. In this second set of
#   observations, the values of highTaxDiff_shock in year==2017 and 2018 are ~= -0.03, because now it is
#   the 'sending' county experience the relative tax increase.

# Notably, in both of these years, the relative tax rates paid by households with AGI under $200k
#   changes (in magnitude) by only 0.002. These shifts in relative tax burdens are specific to households
#   with AGI above $200k.


# ---- Model 1 ----

# For a pair of counties with people migrating between them, is there a correlation between
#   a change in the counties' difference in tax rates and the number of people moving.

model1 = feols(
  log(n1) ~ as.factor(highTaxDiff_shock>.05) * log(pcAgi_lag) |
    y1_fips + year,
  data=df2
)

etable(model1)


# Interpretation of Estimates:

#   highTaxDiff_shock > .05 dummy coefficient: 
#     The percentage point increase in the tax rate paid by high-income households
#     in the receiving county relative to the sending county from year t-1 to year t
#     being greater than 5 is associated with a statistically insignificant 0.11% increase
#     in the number of people moving from the sending county in year t to the receiving county
#     in year t+1.

#   log(agi_lag/n1_lag) coefficient:
#     Of people moving from the sending county in year t-1 to the receiving county
#     in year t, a 1% increase in their per capita AGI is associated with
#     a statistically extremely significant 0.07% increase in the number of people
#     moving from the sending county in year t to the receiving county in year t+1.

#   Interaction coefficient:
#     Given that the receiving county experienced a 5 percentage point or greater
#     increase in their tax rate applied to high-income households relative to the
#     sending county, the associated increase in number of people moving from the
#     sending county in year t to the receiving county in year t+1 from a 1% increase
#     in the average income of people making the same move the previous year is
#     a statistically significant (95%) -0.057%.

#   In other words, a county receiving a tax hike relative to another of 5 pp or greater
#     decreases the expected number of people making that move by .057% when the average
#     income of people who made the same move the previous year in increased by 1%.


# Is there something significant about this 5 percentage point cutoff? We chose it
#   somewhat arbitrarily, but it is not completely arbitrary. There is variation in
#   relative tax rates all the time for a multitude of reasons, but that variation
#   is not always caused by an exogenous shock. For example, Missoula might experience
#   a positive tax shock relative to Gallatin because wealthy types (who pay higher taxes)
#   are already moving to Missoula. Some of this reverse causality is mitigated by
#   our using the tax shock preceding the measurement in migration (or pc agi of migrants),
#   but there is probably non-trivial correlation between the previously measured outcomes
#   and the variation in tax rates. We use the 5 percentage point cutoff to
#   separate variation caused by literally anything from a massive jump in relative
#   tax rates of the receiving county most likely caused by a policy change.


# The fundamental relationship we're trying to capture is this:
# For runtime and clarity of picture, using only 5% sample
set.seed(1)

# These points out to the far left and right are so few but, cause the image to zoom so far out,
#   I'm electing to omit them.
sample = df2 |> filter(log(pcAgi_lag)>2.5 & log(pcAgi_lag)<7) |> slice_sample(n=floor(nrow(df2)*.05))

p1 = ggplot(sample, aes(x=pcAgi_lag, y=n1)) +
  geom_point(data = filter(sample,highTaxDiff_shock <= .05), color='blue', alpha=.1) +
  geom_point(data = filter(sample,highTaxDiff_shock > .05), color='red', alpha=.3) +
  geom_smooth(data = filter(sample,highTaxDiff_shock <= .05), aes(color='No Tax Shock')) +
  geom_smooth(data = filter(sample,highTaxDiff_shock > .05), aes(color='Large Tax Shock')) +
  scale_x_log10() +
  scale_y_log10() +
  scale_color_manual(
    values = c('Large Tax Shock'='red', 'No Tax Shock'= 'steelblue')
  ) +
  labs(color=NULL,
       x='Previous Year Per Capita AGI ($000s)',
       y='Current Year Migrating Population') +
  theme_bw() +
  theme(text=element_text(family='serif'))
  
p1

# The results from model1 indicate that county pairs for which the receiving county
#   is attracting higher average income households attract fewer households after
#   it receives an increase in its tax rate to high-income households relative to the
#   sending county of 5 pp or greater. Is there something significant about the 5 pp cutoff?

letters = c('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j')
models1 = list()
for(i in 1:10){
  models1[[paste0('model1', letters[i])]] = feols(
    log(n1) ~ as.factor(highTaxDiff_shock > (i-1)/100) * log(pcAgi_lag) |
      y1_fips + year,
    data=df2
  )
}

# models1[[model1a]] is the TWFE with the cutoff for relative tax hike in the receiving
#   county only only has to be positive.
# models1[[model1f]] is the TWFE conducted above, called model1

# Visualize coefficients of the interaction terms and pc income terms:
#   does the threshold to distinguish between spurious and substantial
#   relative tax increases of the receiving county change the
#   correlation between pc Agi and subsequent migration flows?

model1Results = lapply(names(models1), function(model_name) {
  m = models1[[model_name]]
  
  ct = coeftable(m)
  cn = rownames(ct)
  
  
  main_name = cn[grep('log\\(pcAgi_lag\\)$', cn)] # coefficient of last year's pc agi
  main_name = main_name[!grepl(":", main_name)]
  int_name  = cn[grep(':', cn)]     # coefficient of interaction term
  
  vc = vcov(m)
  
  rbind(
    data.frame(
      model    =model_name,
      term     = 'main',
      estimate = ct[main_name, 'Estimate'],
      se       = ct[main_name, 'Std. Error'],
      threshold = which(names(models1)==model_name)-1),
      
    data.frame(
      model    =model_name,
      term     = 'interaction',
      estimate =ct[int_name, 'Estimate'],
      se       =ct[int_name, 'Std. Error'],
      threshold = which(names(models1) ==model_name)-1
    )
  )
})

model1Results_df = do.call(rbind, model1Results)
model1Results_df$ci_lo = model1Results_df$estimate - 1.96*model1Results_df$se
model1Results_df$ci_hi = model1Results_df$estimate + 1.96*model1Results_df$se

# Plotting:
p2 = ggplot(model1Results_df,
       aes(x=threshold, y=estimate, color=term, fill=term)) +
  geom_hline(yintercept=0, linetype='dashed', color='gray50') +
  geom_ribbon(aes(ymin=ci_lo, ymax=ci_hi), alpha=.2, color=NA) +
  geom_line(linewidth=.8) +
  geom_point(size=2.5) +
  scale_x_continuous(
    breaks = 0:(length(letters)-1),
    labels = paste0((0:(length(letters)-1)), '%')
  ) +
  scale_color_manual(
    values = c('main'='steelblue', 'interaction'='firebrick'),
    labels = c('main'='Previous Year pc AGI', 'interaction'='Interaction')
  ) +
  scale_fill_manual(
    values = c('main'='steelblue', 'interaction'='firebrick'),
    labels = c('main'='Previous Year pc AGI', 'interaction'='Interaction')
  ) +
  labs(x='Threshold for Substantial Relative Tax Hike in Receiving County',
       y='Coefficients for Population',
       color = NULL,
       fill  = NULL) +
  theme_minimal() + theme(text=element_text(family='serif'))

p2


######## What about a relative tax hike in the SENDING county?

newModels1 = list()
for(i in 1:10){
  newModels1[[paste0('newModel1', letters[i])]] = feols(
    log(n1) ~ as.factor(highTaxDiff_shock < -(i-1)/100) * log(pcAgi_lag) |
      y1_fips + year,
    data=df2
  )
}

newModel1Results = lapply(names(newModels1), function(model_name) {
  m = newModels1[[model_name]]
  
  ct = coeftable(m)
  cn = rownames(ct)
  
  
  main_name = cn[grep('log\\(pcAgi_lag\\)$', cn)] # coefficient of last year's pc agi
  main_name = main_name[!grepl(":", main_name)]
  int_name  = cn[grep(':', cn)]     # coefficient of interaction term
  
  vc = vcov(m)
  
  rbind(
    data.frame(
      model    =model_name,
      term     = 'main',
      estimate = ct[main_name, 'Estimate'],
      se       = ct[main_name, 'Std. Error'],
      threshold = which(names(newModels1)==model_name)-1),
    
    data.frame(
      model    =model_name,
      term     = 'interaction',
      estimate =ct[int_name, 'Estimate'],
      se       =ct[int_name, 'Std. Error'],
      threshold = which(names(newModels1) ==model_name)-1
    )
  )
})

newModel1Results_df = do.call(rbind, newModel1Results)
newModel1Results_df$ci_lo = newModel1Results_df$estimate - 1.96*newModel1Results_df$se
newModel1Results_df$ci_hi = newModel1Results_df$estimate + 1.96*newModel1Results_df$se

# Plotting:
p3 = ggplot(newModel1Results_df,
       aes(x=threshold, y=estimate, color=term, fill=term)) +
  geom_hline(yintercept=0, linetype='dashed', color='gray50') +
  geom_ribbon(aes(ymin=ci_lo, ymax=ci_hi), alpha=.2, color=NA) +
  geom_line(linewidth=.8) +
  geom_point(size=2.5) +
  scale_x_continuous(
    breaks = 0:(length(letters)-1),
    labels = paste0((0:(length(letters)-1)), '%')
  ) +
  scale_color_manual(
    values = c('main'='steelblue', 'interaction'='firebrick'),
    labels = c('main'='Previous Year pc AGI', 'interaction'='Interaction')
  ) +
  scale_fill_manual(
    values = c('main'='steelblue', 'interaction'='firebrick'),
    labels = c('main'='Previous Year pc AGI', 'interaction'='Interaction')
  ) +
  labs(x='Threshold for Substantial Relative Tax Hike in Receiving County',
       y="Coefficients for Population",
       color = NULL,
       fill  = NULL) +
  theme_minimal() + theme(text=element_text(family='serif'))

p3

# ---- Model 2 ----

# Repeat model 1 for agi per capita of the migrants in the county pair

# The fundamental relationship we're trying to capture is this:

df2 = df2 |> mutate(pcAgi = agi/n1)

# Main trend attempting to identify

sample = df2 |> slice_sample(n=floor(nrow(df2)*.05))

p4 = ggplot(sample, aes(x=pcAgi_lag, y=pcAgi)) +
  geom_point(data = filter(sample,highTaxDiff_shock <= .05), color='blue', alpha=.1) +
  geom_point(data = filter(sample,highTaxDiff_shock > .05), color='red', alpha=.3) +
  geom_smooth(data = filter(sample,highTaxDiff_shock <= .05), aes(color='No Tax Shock')) +
  geom_smooth(data = filter(sample,highTaxDiff_shock > .05), aes(color='Large Tax Shock')) +
  scale_x_log10() +
  scale_y_log10() +
  coord_cartesian(xlim=c(10, 1000),
                  ylim=c(10, 1000)) +
  scale_color_manual(
    values = c('Large Tax Shock'='red', 'No Tax Shock'= 'steelblue')
  ) +
  labs(color=NULL,
       x='Previous Year Per Capita AGI ($000s)',
       y='Current Year Migrant Per Capita AGI ($000s)') +
  theme_bw() +
  theme(text=element_text(family='serif'))

p4

models2 = list()
for(i in 1:10){
  models2[[paste0('model2', letters[i])]] = feols(
    log(pcAgi) ~ as.factor(highTaxDiff_shock > (i-1)/100) * log(pcAgi_lag) |
      y1_fips + year,
    data=df2
  )
}

# models2[[model2a]] is the TWFE with the cutoff for relative tax hike in the sending
#   county only only has to be positive.

# Visualize coefficients of the interaction and lag pc Agi terms:
#   does the threshold to distinguish between spurious and
#   substantial relative tax increases of the receiving county
#   change the correlation between pc Agi and subsequent migrants incomes?

model2Results = lapply(names(models2), function(model_name) {
  m = models2[[model_name]]
  
  ct = coeftable(m)
  cn = rownames(ct)
  
  
  main_name = cn[grep('log\\(pcAgi_lag\\)$', cn)] # coefficient of last year's pc agi
  main_name = main_name[!grepl(":", main_name)]
  int_name  = cn[grep(':', cn)]     # coefficient of interaction term
  
  vc = vcov(m)
  
  rbind(
    data.frame(
      model    =model_name,
      term     = 'main',
      estimate = ct[main_name, 'Estimate'],
      se       = ct[main_name, 'Std. Error'],
      threshold = which(names(models2)==model_name)-1),
    
    data.frame(
      model    =model_name,
      term     = 'interaction',
      estimate =ct[int_name, 'Estimate'],
      se       =ct[int_name, 'Std. Error'],
      threshold = which(names(models2) ==model_name)-1
    )
  )
})

model2Results_df = do.call(rbind, model2Results)
model2Results_df$ci_lo = model2Results_df$estimate - 1.96*model2Results_df$se
model2Results_df$ci_hi = model2Results_df$estimate + 1.96*model2Results_df$se

# In this model, the interaction coefficients' estimates are an order of magnitude
#   smaller than estimates of the coefficients of just log(pcAgi_lag).
#   Let's rescale these estimates down to make the graph easier to interpret:

model2Results_df_scaled = model2Results_df
model2Results_df_scaled[model2Results_df_scaled$term == 'main', c('estimate', 'ci_lo', 'ci_hi')] =
  model2Results_df_scaled[model2Results_df_scaled$term == 'main', c('estimate', 'ci_lo', 'ci_hi')] / 10

# Plotting:
p5 = ggplot(model2Results_df_scaled,
       aes(x=threshold, y=estimate, color=term, fill=term)) +
  geom_hline(yintercept=0, linetype='dashed', color='gray50') +
  geom_ribbon(aes(ymin=ci_lo, ymax=ci_hi), alpha=.2, color=NA) +
  geom_line(linewidth=.8) +
  geom_point(size=2.5) +
  scale_x_continuous(
    breaks = 0:(length(letters)-1),
    labels = paste0((0:(length(letters)-1)), '%')
  ) +
  scale_color_manual(
    values = c('main'='steelblue', 'interaction'='firebrick'),
    labels = c('main'='Previous Year pc AGI (/10)', 'interaction'='Interaction')
  ) +
  scale_fill_manual(
    values = c('main'='steelblue', 'interaction'='firebrick'),
    labels = c('main'='Previous Year pc AGI (/10)', 'interaction'='Interaction')
  ) +
  labs(x='Threshold for Substantial Relative Tax Hike in Receiving County',
       y='Coefficients for Population',
       color = NULL,
       fill  = NULL) +
  theme_minimal() + theme(text=element_text(family='serif'))

p5

######## What about a relative tax hike in the sending county?

newModels2 = list()
for(i in 1:10){
  newModels2[[paste0('newModel2', letters[i])]] = feols(
    log(pcAgi) ~ as.factor(highTaxDiff_shock < -(i-1)/100) * log(pcAgi_lag) |
      y1_fips + year,
    data=df2
  )
}

newModel2Results = lapply(names(newModels2), function(model_name) {
  m = newModels2[[model_name]]
  
  ct = coeftable(m)
  cn = rownames(ct)
  
  
  main_name = cn[grep('log\\(pcAgi_lag\\)$', cn)] # coefficient of last year's pc agi
  main_name = main_name[!grepl(":", main_name)]
  int_name  = cn[grep(':', cn)]     # coefficient of interaction term
  
  vc = vcov(m)
  
  rbind(
    data.frame(
      model    =model_name,
      term     = 'main',
      estimate = ct[main_name, 'Estimate'],
      se       = ct[main_name, 'Std. Error'],
      threshold = which(names(newModels2)==model_name)-1),
    
    data.frame(
      model    =model_name,
      term     = 'interaction',
      estimate =ct[int_name, 'Estimate'],
      se       =ct[int_name, 'Std. Error'],
      threshold = which(names(newModels2) ==model_name)-1
    )
  )
})

newModel2Results_df = do.call(rbind, newModel2Results)
newModel2Results_df$ci_lo = newModel2Results_df$estimate - 1.96*newModel2Results_df$se
newModel2Results_df$ci_hi = newModel2Results_df$estimate + 1.96*newModel2Results_df$se

# Like in model2Results, the coefficient estimates for log(pcAgi_lag) are
#   an order of magnitude larger than the estimates for the interaction.
#   Rescale for the graph.

newModel2Results_df_scaled = newModel2Results_df
newModel2Results_df_scaled[newModel2Results_df_scaled$term == 'main', c('estimate', 'ci_lo', 'ci_hi')] =
  newModel2Results_df_scaled[newModel2Results_df_scaled$term == 'main', c('estimate', 'ci_lo', 'ci_hi')] / 10


# Plotting:
p6 = ggplot(newModel2Results_df_scaled,
       aes(x=threshold, y=estimate, color=term, fill=term)) +
  geom_hline(yintercept=0, linetype='dashed', color='gray50') +
  geom_ribbon(aes(ymin=ci_lo, ymax=ci_hi), alpha=.2, color=NA) +
  geom_line(linewidth=.8) +
  geom_point(size=2.5) +
  scale_x_continuous(
    breaks = 0:(length(letters)-1),
    labels = paste0((0:(length(letters)-1)), '%')
  ) +
  scale_color_manual(
    values = c('main'='steelblue', 'interaction'='firebrick'),
    labels = c('main'='Previous Year pc AGI (/10)', 'interaction'='Interaction')
  ) +
  scale_fill_manual(
    values = c('main'='steelblue', 'interaction'='firebrick'),
    labels = c('main'='Previous Year pc AGI (/10)', 'interaction'='Interaction')
  ) +
  labs(x='Threshold for Substantial Relative Tax Hike in Receiving County',
       y="Coefficients for Population",
       color = NULL,
       fill  = NULL) +
  theme_minimal() + theme(text=element_text(family='serif'))

p6

# ---- Saving Results to Output Folder ----

save(models1, newModels1, models2, newModels2,
     file='Output/Quantitative Estimates/Regressions_2_TWFEs.RData')

ggsave('Output/Visualizations/Stage 3/current_year_pop_vs_previous_year_agi.pdf',
       plot = p1)
ggsave('Output/Visualizations/Stage 3/model1_coefficients_receiving_thresholds.pdf',
       plot = p2)
ggsave('Output/Visualizations/Stage 3/model1_coefficients_sending_thresholds.pdf',
       plot = p3)
ggsave('Output/Visualizations/Stage 3/current_year_agi_vs_previous_year_agi.pdf',
       plot = p4)
ggsave('Output/Visualizations/Stage 3/model2_coefficients_receiving_thresholds.pdf',
       plot = p5)
ggsave('Output/Visualizations/Stage 3/model2_coefficients_sending_thresholds.pdf',
       plot = p6)