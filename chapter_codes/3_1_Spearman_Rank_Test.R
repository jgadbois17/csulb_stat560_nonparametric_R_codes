#--------------------------------------------------------------------------------

# Ch 3: Tests for Categorical Data
# S1: Spearman Rank Correlation Coefficient Test

# Joey Gadbois


# libraries
library(tidyverse)
library(rstatix)

#--------------------------------------------------------------------------------

# Example 3.1

# data
glaucoma = tibble(
  Tx = c(0.45, 1.95, 1.20, 0.65, 0.98, -1.98, 1.80, -0.76, 0.56), 
  Cx = c(0.38, 0.90, 0.70, -0.50, 0.47, -1.30, 1.34, 0.13, -0.40)
)
glaucoma %>% print()

# spearman correlation coefficient
glaucoma %>% 
  summarise(r_spearman = cor(Tx, Cx, method = 'spearman'))

# spearman rank test
glaucoma %>% 
  cor_test(Tx, Cx, alternative = 'greater', method = 'spearman')

glaucoma %>% 
  cor_test(Tx, Cx, alternative = 'two.sided', method = 'spearman')

#--------------------------------------------------------------------------------

# Example 3.2

# data
smoke_health = tibble(
  years = c(12, 33, 5, 3, 12, 6, 3, 0, 2, 46),
  health = c(1, 2, 3, 3, 2, 2, 4, 4, 3, 1)
)
health %>% print()

# spearman correlation coefficient
smoke_health %>% 
  summarise(r_spearman = cor(years, health, method = 'spearman'))

# spearman rank test
smoke_health %>% 
  cor_test(years, health, alternative = 'less', method = 'spearman')

smoke_health %>% 
  cor_test(years, health, alternative = 'two.sided', method = 'spearman')

#--------------------------------------------------------------------------------

# Example 3.3

# data
study_exam = tibble(
  hours = c(0, 5, 9, 7, 17, 17, 5, 12, 21, 3, 7, 29, 7, 10), 
  score = c(28, 94, 84, 45, 82, 99, 67, 97, 79, 93, 62, 60, 85, 78)
)
study_exam %>% print()

# spearman correlation coefficient
study_exam %>% 
  summarise(r_spearman = cor(hours, score, method = 'spearman'))

# spearman rank test
study_exam %>% 
  cor_test(hours, score, alternative = 'two.sided', method = 'spearman')

#--------------------------------------------------------------------------------


