#--------------------------------------------------------------------------------

# Ch 2: Hypothesis Testing for Several Samples
# S2: Kruskal-Wallis H-Test for Location Parameter for Several Independent Samples

# Joey Gadbois


# libraries
library(tidyverse)
library(rstatix)

#--------------------------------------------------------------------------------

# Example 2.3

# data
archery = tibble(
  Monica = c(NA, 3, 4, 4, 5, 5, 5, 10, 10), 
  Bob = c(NA, 2, 2, 3, 4, 4, 5, 10, 10), 
  Jeff = c(1, 1, 2, 4, 4, 5, 10, 10, 10)
)
archery %>% print()

# kruskal-wallis h-test
archery %>% 
  gather(key = 'person', value = 'score', Monica, Bob, Jeff) %>% 
  kruskal_test(score ~ person)

#--------------------------------------------------------------------------------

# Example 2.4

# data
weight_loss = tibble(
  aerobics = c(6.7, 7.7, 10.0, 9.4, 9.1), 
  pilates = c(10.5, 12.8, 13.1, 13.4, NA), 
  step = c(13.0, 11.2, 11.8, 11.6, NA), 
  cardio = c(19.0, 15.3, 17.5, 22.4, NA)
)
weight_loss %>% print()

# kruskal-wallis h-test
weight_loss %>% 
  gather(key = 'class', value = 'weight_loss', aerobics, pilates, step, cardio) %>% 
  kruskal_test(weight_loss ~ class)


# wilcoxon rank-sum test for each pair
weight_loss %>% 
  summarise(mean_aerobics = mean(aerobics), 
            mean_pilates = mean(pilates, na.rm = T), 
            mean_step = mean(step, na.rm = T), 
            mean_cardio = mean(cardio, na.rm = T))

# H0: aerobics = step vs. aerobics < step
weight_loss %>% 
  select(aerobics, step) %>% 
  gather(key = 'class', value = 'weight_loss', aerobics, step) %>% 
  wilcox_test(weight_loss ~ class, 
              paired = F, 
              ref.group = 'aerobics', 
              alternative = 'less', 
              conf.level = 0.95)

# H0: step = pilates vs. H1 step < pilates
weight_loss %>% 
  select(pilates, step) %>% 
  gather(key = 'class', value = 'weight_loss', pilates, step) %>% 
  wilcox_test(weight_loss ~ class, 
              paired = F, 
              ref.group = 'step', 
              alternative = 'less', 
              conf.level = 0.95)

# H0: step = cardio vs. H1 step < cardio
weight_loss %>% 
  select(cardio, step) %>% 
  gather(key = 'class', value = 'weight_loss', cardio, step) %>% 
  wilcox_test(weight_loss ~ class, 
              paired = F, 
              ref.group = 'step', 
              alternative = 'less', 
              conf.level = 0.95)

# H0: pilates = cardio vs. H1 pilates < cardio
weight_loss %>% 
  select(cardio, pilates) %>% 
  gather(key = 'class', value = 'weight_loss', cardio, pilates) %>% 
  wilcox_test(weight_loss ~ class, 
              paired = F, 
              ref.group = 'pilates', 
              alternative = 'less', 
              conf.level = 0.95)

#--------------------------------------------------------------------------------


