#--------------------------------------------------------------------------------

# Ch 6: Time-to-Event Analysis
# S2: Log-Rank Test for Comparison of Two Survival Functions

# Joey Gadbois


# libraries
library(tidyverse)
library(survival)
library(survminer)

#--------------------------------------------------------------------------------

# Example 6.2

# data
strep_throat = tibble(
  group = c('Tx', 'Tx', 'Tx', 'Tx', 'Tx', 'Tx', 'Tx', 
            'Cx', 'Cx',  'Cx', 'Cx', 'Cx', 'Cx', 'Cx'), 
  days = c(3, 3, 4, 4, 4, 5, 5, 4, 5, 5, 6, 6, 6, 10), 
  censored = c(0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1)
)
strep_throat %>% print()

# fit KM estimator
fit = survfit(Surv(days, censored == 0) ~ group, data = strep_throat)
fit %>% summary()

# log-rank test
survdiff(Surv(days, censored == 0) ~ group, data = strep_throat)

# plot survival curves
# play around with hyperparams: cumevents, cumcensor are tables
# NOTE: pval = T >> p-value from log-rank test
ggsurvplot(
  fit, 
  data = strep_throat, 
  censor.shape = '|', 
  censor.size = 5,
  palette = c('blue', 'red'), 
  linetype = 1, 
  conf.int = F, 
  pval = T, 
  risk.table = T, 
  tables.height = 0.25, 
  cumevents = F, 
  cumcensor = F, 
  xlab = 'Days', 
  break.time.by = 2, 
  risk.table.y.text = F, 
  legend.title = 'Group', 
  legend.labs = c('Cx', 'Tx'), 
  ggtheme = theme_survminer()
)

#--------------------------------------------------------------------------------


