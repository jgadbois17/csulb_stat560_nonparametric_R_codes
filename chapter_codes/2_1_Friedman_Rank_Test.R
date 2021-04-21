#--------------------------------------------------------------------------------

# Ch 2: Hypothesis Testing for Several Samples
# S1: Friedman Rank Test for Location Parameter for Several Dependent Samples

# Joey Gadbois


# libraries
library(tidyverse)
library(rstatix)

#--------------------------------------------------------------------------------

# Example 2.1

# data
reaction_times = tibble(
  id = 1:6, 
  both = c(1.0, 0.4, 0.2, 0.3, 0.4, 0.1), 
  right = c(0.6, 0.5, 0.5, 0.2, 0.3, 0.2), 
  left = c(0.7, 0.6, 0.4, 0.5, 0.5, 0.3)
)
reaction_times %>% print()

# friedman rank test
reaction_times %>% 
  gather(key = 'hand', value = 'time', both, right, left) %>% 
  friedman_test(time ~ hand | id)

#--------------------------------------------------------------------------------

# Example 2.2

# data
beer = tibble(
  taster = 1:10, 
  a = c(4, 3, 2, 5, 1, 4, 3, 2, 2, 6), 
  b = c(5, 7, 8, 7, 3, 4, 6, 5, 7, 4), 
  c = c(8, 6, 8, 9, 6, 6, 8, 5, 9, 5)
)
beer %>% print()

# friedman rank test
beer %>% 
  gather(key = 'brand', value = 'rating', a, b, c) %>% 
  friedman_test(rating ~ brand | taster)


# wilcoxon signed-rank test for each pair
beer %>% 
  summarise(mean_a = mean(a), mean_b = mean(b), mean_c = mean(c))

# H0: a = b vs. H1: a < b
beer %>% 
  select(a, b) %>% 
  gather(key = 'brand', value = 'rating', a, b) %>% 
  wilcox_test(rating ~ brand, ref.group = 'a', paired = T, 
              alternative = 'less', conf.level = 0.95)

# H0: b = c vs. H1: b < c
beer %>% 
  select(b, c) %>% 
  gather(key = 'brand', value = 'rating', b, c) %>% 
  wilcox_test(rating ~ brand, ref.group = 'b', paired = T, 
              alternative = 'less', conf.level = 0.95)

# H0: a = c vs. H1: a < c
beer %>% 
  select(a, c) %>% 
  gather(key = 'brand', value = 'rating', a, c) %>% 
  wilcox_test(rating ~ brand, ref.group = 'a', paired = T, 
              alternative = 'less', conf.level = 0.95)

#--------------------------------------------------------------------------------






