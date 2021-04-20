#--------------------------------------------------------------------------------

# Ch 1: Hypothesis Testing for Two Samples
# S2: Wilcoxon Signed-Rank Test for Location Parameter for Matched Paired Samples

# Joey Gadbois


# libraries
library(tidyverse)
library(rstatix)

#--------------------------------------------------------------------------------

# Example 1.4

# data
glaucoma = tibble(
  Tx = c(0.45, 1.95, 1.20, 0.65, 0.98, -1.98, 1.80, -0.76, 0.56), 
  Cx = c(0.38, 0.90, 0.70, -0.50, 0.47, -1.30, 1.34, 0.13, -0.40), 
  diff = Tx - Cx
)
glaucoma %>% print()

# wilcoxon signed-rank test - option 1
glaucoma %>% 
  select(diff) %>% 
  wilcox_test(diff ~ 1, 
              paired = F, 
              alternative = 'greater', 
              conf.level = 0.95, 
              detailed = T)

# wilcoxon signed-rank test - option 2
glaucoma %>% 
  gather(key = 'group', value = 'iop_reduc', Tx, Cx) %>% 
  wilcox_test(iop_reduc ~ group, 
              paired = T, 
              alternative = 'greater', 
              ref.group = 'Tx', 
              conf.level = 0.95, 
              detailed = F)

#--------------------------------------------------------------------------------

# Example 1.5

# data
algebra = tibble(
  interv = c(10, 22, 44, 23, 8, 33, 0, 8, 14, 34, 2, 10), 
  control = c(26, 40, 66, 55, 16, 33, 8, 6, 18, 14, 23, 15), 
  diff = interv - control
)
algebra %>% print()

# wilcoxon signed-rank test - option 1
algebra %>% 
  select(diff) %>% 
  wilcox_test(diff ~ 1, 
              paired = F, 
              alternative = 'less', 
              conf.level = 0.95, 
              detailed = T)

# wilcoxon signed-rank test - option 2
algebra %>% 
  gather(key = 'group', value = 'score', interv, control) %>% 
  wilcox_test(score ~ group, 
              paired = T, 
              alternative = 'less', 
              ref.group = 'interv', 
              conf.level = 0.95, 
              detailed = F)

#--------------------------------------------------------------------------------

# Example 1.6

# data
prices = tibble(
  year1 = c(572, 572, 578, 591, 601, 606, 602, 600, 602, 604, 602, 598), 
  year2 = c(593, 588, 586, 581, 576, 568, 560, 555, 553, 560, 566, 571), 
  diff = year1 - year2
)
prices %>% print()

# wilcoxon signed-rank test - option 1
prices %>% 
  select(diff) %>% 
  wilcox_test(diff ~ 1, 
              paired = F, 
              alternative = 'two.sided', 
              conf.level = 0.95, 
              detailed = T)

# wilcoxon signed-rank test - option 2
prices %>% 
  gather(key = 'year', value = 'price', year1, year2) %>% 
  wilcox_test(price ~ year, 
              paired = T, 
              alternative = 'two.sided', 
              conf.level = 0.95, 
              detailed = F)

#--------------------------------------------------------------------------------

