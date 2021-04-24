#--------------------------------------------------------------------------------

# Ch 1: Hypothesis Testing for Two Samples
# S5: Kolmogorov-Smirnov Test for Equality of Distributions

# Joey Gadbois


# libraries
library(tidyverse)
library(magrittr)

#--------------------------------------------------------------------------------

# Example 1.13

# data
bioimplant = list(
  Tx = c(3, 4, 6, 6, 8, 8), 
  Cx = c(4, 4, 4, 5, 6, 7, 8, 10, 12)
)

# kolmogorov-smirnov test
bioimplant %$% 
  ks.test(Tx, Cx, alternative = 'greater')

#--------------------------------------------------------------------------------

# Example 1.14

# data
leukemia = list(
  Tx = c(12.33, 10.44, 12.72, 13.13, 13.50, 16.82, 17.60, 14.37), 
  Cx = c(16.45, 18.63, 13.12, 18.94, 19.34, 22.50, 21.56, 17.63)
)

# kolmogorov-smirnov test
leukemia %$% 
  ks.test(Cx, Tx, alternative = 'less')

#--------------------------------------------------------------------------------

# Example 1.15

# data
elderly = list(
  men = c(4, 5, 4, 3, 4, 3), 
  women = c(5, 5, 4, 4, 4, 5, 4)
)

# kolmogorov-smirnov test
elderly %$% 
  ks.test(men, women, alternative = 'two.sided')

#--------------------------------------------------------------------------------


