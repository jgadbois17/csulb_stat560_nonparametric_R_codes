#--------------------------------------------------------------------------------

# Ch 1: Hypothesis Testing for Two Samples
# S1: Sign Test for Location Parameter for Matched Paired Samples

# Joey Gadbois


# libraries
library(tidyverse)
library(rstatix)

#--------------------------------------------------------------------------------

# Example 1.1

# data
glaucoma = tibble(
  Tx = c(0.45, 1.95, 1.20, 0.65, 0.98, -1.98, 1.80, -0.76, 0.56), 
  Cx = c(0.38, 0.90, 0.70, -0.50, 0.47, -1.30, 1.34, 0.13, -0.40), 
  diff = Tx - Cx
)
glaucoma %>% print()

# sign test - option 1
glaucoma %>% 
  select(diff) %>% 
  sign_test(diff ~ 1, 
            alternative = 'greater', 
            conf.level = 0.95, 
            detailed = T)

# sign test - option 2
glaucoma %>% 
  gather(key = 'group', value = 'iop_reduc', Tx, Cx) %>% 
  sign_test(iop_reduc ~ group, 
            alternative = 'greater', 
            ref.group = 'Tx', 
            conf.level = 0.95, 
            detailed = F)

#--------------------------------------------------------------------------------

# Example 1.2

# data
algebra = tibble(
  interv = c(10, 22, 44, 23, 8, 33, 0, 8, 14, 34, 2, 10), 
  control = c(26, 40, 66, 55, 16, 33, 8, 6, 18, 14, 23, 15), 
  diff = interv - control
)
algebra %>% print()

# sign test - option 1
algebra %>% 
  select(diff) %>% 
  sign_test(diff ~ 1, 
            alternative = 'less', 
            conf.level = 0.95, 
            detailed = T)

# sign test - option 2
algebra %>% 
  gather(key = 'group', value = 'score', interv, control) %>% 
  sign_test(score ~ group, 
            alternative = 'less', 
            ref.group = 'interv', 
            conf.level = 0.95, 
            detailed = F)

#--------------------------------------------------------------------------------

# Example 1.3

# data
prices = tibble(
  year1 = c(572, 572, 578, 591, 601, 606, 602, 600, 602, 604, 602, 598), 
  year2 = c(593, 588, 586, 581, 576, 568, 560, 555, 553, 560, 566, 571), 
  diff = year1 - year2
)
prices %>% print()

# sign test - option 1
prices %>% 
  select(diff) %>% 
  sign_test(diff ~ 1, 
            alternative = 'two.sided', 
            conf.level = 0.95, 
            detailed = T)

# sign test - option 1
prices %>% 
  gather(key = 'year', value = 'price', year1, year2) %>% 
  sign_test(price ~ year, 
            alternative = 'two.sided', 
            conf.level = 0.95, 
            detailed = F)

#--------------------------------------------------------------------------------



