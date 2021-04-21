#--------------------------------------------------------------------------------

# Ch 3: Tests for Categorical Data
# S2: Fisher Exact Test

# Joey Gadbois


# libraries
library(tidyverse)
library(rstatix)

#--------------------------------------------------------------------------------

# Example 3.4

# data
fitness = list(
  actlevel = c('high', 'moderate', 'low', 'low', 'low', 'low', 'low', 'moderate', 
               'moderate', 'high', 'low', 'moderate', 'moderate', 'high', 'moderate'), 
  gym_used = c('no', 'no', 'yes', 'yes', 'yes', 'yes', 'no', 'yes', 'yes', 'no', 
               'no', 'yes', 'yes', 'yes', 'yes')
)
fitness = table(fitness$actlevel, fitness$gym_used)
fitness

# fisher exact test
fisher_test(fitness, detailed = T)

#--------------------------------------------------------------------------------

# Example 3.5

# data
# data
psoriasis = matrix(
  data = c(2, 8, 8, 2, 6, 4), nrow = 2, 
  dimnames = list(Relief = c('Yes','No'), Group=c('Placebo','Drug A','Drug B'))
)

# test
fisher_test(psoriasis, detailed = T)

#--------------------------------------------------------------------------------


