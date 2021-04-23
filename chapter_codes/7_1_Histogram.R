#--------------------------------------------------------------------------------

# Ch 7: Univariate Probability Density Estimation
# S1: Histogram

# Joey Gadbois


# libraries
library(tidyverse)

# set ggplot2 theme
theme_set(theme_classic())

#--------------------------------------------------------------------------------

# Example 7.1

# data
snack_shack = tibble(
  revenue = c(437.94, 387.51, 400.48, 403.16, 350.87, 408.43, 275.94, 470.83, 
              173.96, 423.90, 173.70, 462.77, 343.58, 425.04, 168.63, 392.00, 
              368.24, 310.25, 403.15, 177.03, 408.19, 175.33, 320.00, 185.09, 
              462.46, 197.78, 276.34, 392.71, 435.85, 283.82, 383.30, 188.31, 
              460.30, 180.14, 473.08, 177.94, 457.38, 185.24, 352.75, 400.32, 
              371.00, 372.95, 425.95, 358.55, 380.65, 377.22, 375.36, 280.11, 
              450.68, 410.33, 370.11, 380.32, 343.44, 400.26, 227.33, 440.37, 
              405.25, 425.57, 333.21, 200.34, 433.23, 293.51, 458.92, 190.42, 
              358.06, 373.27, 373.83, 182.70, 463.49, 350.00, 400.04, 367.26, 
              167.29, 460.23, 167.22, 400.34, 180.03, 442.55, 190.44, 463.85, 
              283.61, 350.64, 197.66, 428.97, 183.94, 413.37, 183.18, 465.96, 
              420.45, 393.85, 433.92, 183.60, 453.68, 203.80, 418.52, 443.48, 
              407.45, 413.35, 395.71, 410.32, 272.41, 458.21, 283.21, 450.92, 
              195.69, 223.75, 412.15, 213.03, 240.43, 287.79, 297.32, 296.89)
)
snack_shack %>% print()

# plot histogram to match SAS default
snack_shack %>% 
  ggplot(aes(x = revenue, y = ..count..)) + 
  stat_bin(breaks = seq(160, 500, by = 40), color = 'black', fill = 'blue') + 
  scale_x_continuous(breaks = seq(180, 460, by = 40)) + 
  scale_y_continuous(breaks = seq(0, 30, by = 5)) + 
  labs(title = 'Snack Shack Revenue Histogram')

# plot histogram with adjusted bins
snack_shack %>% 
  ggplot(aes(x = revenue, y = ..count..)) + 
  stat_bin(breaks = seq(165, 475, by = 10), color = 'black', fill = 'blue') + 
  scale_x_continuous(breaks = seq(150, 470, by = 20)) + 
  scale_y_continuous(breaks = seq(0, 30, by = 5)) + 
  labs(title = 'Snack Shack Revenue Histogram')

#--------------------------------------------------------------------------------





