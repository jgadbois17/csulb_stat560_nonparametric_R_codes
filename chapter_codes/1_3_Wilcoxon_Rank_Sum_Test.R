#--------------------------------------------------------------------------------

# Ch 1: Hypothesis Testing for Two Samples
# S3: Wilcoxon Rank-Sum Test for Location Parameter for Two Independent Samples

# Joey Gadbois


# libraries
library(tidyverse)
library(rstatix)

#--------------------------------------------------------------------------------

# Example 1.7

# data
learning_program = tibble(
  yes = c(3.98, 3.45, 3.66, 3.78, 3.90, 4.00, 3.78, 3.12, 3.45, 3.97), 
  no = c(3.42, 2.56, 2.00, 3.19, 3.00, 3.56, 3.56, 4.00, 2.78, 3.44)
)
learning_program %>% print()

# wilcoxon rank-sum test
learning_program %>% 
  gather(key = 'program', value = 'gpa', yes, no) %>% 
  wilcox_test(gpa ~ program, 
              paired = F, 
              alternative = 'greater', 
              ref.group = 'yes', 
              conf.level = 0.95, 
              detailed = T)

#--------------------------------------------------------------------------------

# Example 1.8

# data
bioimplant = tibble(
  weeks = c(3, 4, 6, 6, 8, 8, 4, 4, 4, 5, 6, 7, 8, 10, 12), 
  group = c('Tx', 'Tx', 'Tx', 'Tx', 'Tx', 'Tx', 
            'Cx', 'Cx', 'Cx', 'Cx', 'Cx', 'Cx', 'Cx', 'Cx', 'Cx')
)
bioimplant %>% print()

# wilcoxon rank-sum test
bioimplant %>% 
  wilcox_test(weeks ~ group, 
              paired = F, 
              alternative = 'less', 
              ref.group = 'Tx', 
              conf.level = 0.95, 
              detailed = T)

#--------------------------------------------------------------------------------

# Example 1.9

# data
elderly = tibble(
  score = c(4, 5, 4, 3, 4, 3, 5, 5, 4, 4, 4, 5, 4), 
  gender = c('M', 'M', 'M', 'M', 'M', 'M', 'F', 'F', 'F', 'F', 'F', 'F', 'F')
)
elderly %>% print()

# wilcoxon rank-sum test
elderly %>% 
wilcox_test(score ~ gender, 
            paired = F, 
            alternative = 'two.sided', 
            conf.level = 0.95, 
            detailed = T)

#--------------------------------------------------------------------------------




