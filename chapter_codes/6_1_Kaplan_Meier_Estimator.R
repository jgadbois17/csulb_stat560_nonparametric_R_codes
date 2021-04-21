#--------------------------------------------------------------------------------

# Ch 6: Time-to-Event Analysis
# S1: Kaplan-Meier Estimator of Survival Function

# Joey Gadbois


# libraries
library(tidyverse)
library(survival)
library(survminer)

#--------------------------------------------------------------------------------

# Example 6.1

# data
pilot_cert = tibble(
  hours = c(2, 5, 8, 8, 10, 16, 22, 36, 40, 42, 47, 50, 50, 52, 52, 52, 
            59, 67, 84), 
  censored = c(1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 1)
)
pilot_cert %>% print()

# fit KM estimator
fit = survfit(Surv(hours, censored == 0) ~ 1, data = pilot_cert)
fit %>% summary()

# plot survival curve
# play around with hyperparams: cumevents, cumcensor are tables
ggsurvplot(
  fit, 
  data = pilot_cert, 
  censor.shape = '|', 
  censor.size = 5, 
  palette = 'blue', 
  linetype = 1, 
  conf.int = F, 
  pval = F, 
  risk.table = T, 
  cumevents = F, 
  cumcensor = F, 
  tables.height = 0.25, 
  xlab = 'Hours', 
  ggtheme = theme_survminer()
)

#--------------------------------------------------------------------------------


