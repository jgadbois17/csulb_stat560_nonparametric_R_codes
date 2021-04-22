#--------------------------------------------------------------------------------

# Ch 5: Nonparametric Genralized Additive Regression
# S3: Nonparametric Poisson Model

# Joey Gadbois


# libraries
library(tidyverse)
library(ggpubr)
library(plotly)
library(gam)

# set ggplot2 theme
theme_set(theme_classic())

#--------------------------------------------------------------------------------

# Example 5.3

# data
doc_visits = tibble(
  health = c('exclnt', 'exclnt', 'good', 'good', 'good','exclnt', 'good', 'good', 
             'exclnt', 'good', 'fair', 'fair', 'exclnt', 'exclnt', 'good', 'exclnt', 
             'exclnt', 'good', 'good', 'good', 'exclnt', 'fair', 'fair', 'good', 
             'good', 'exclnt', 'fair', 'fair', 'good', 'exclnt', 'fair', 'exclnt', 
             'good', 'good', 'fair', 'good', 'good', 'good', 'fair', 'fair', 'fair', 
             'fair', 'fair', 'good', 'good', 'good', 'exclnt', 'good', 'good', 
             'fair', 'good', 'fair', 'good', 'good', 'good', 'good', 'fair', 'good', 
             'fair', 'fair', 'fair', 'fair', 'good', 'good', 'exclnt', 'fair', 
             'good', 'good', 'exclnt', 'good', 'exclnt', 'exclnt', 'exclnt', 'good', 
             'good', 'fair', 'good', 'good', 'good', 'good', 'good', 'good', 'fair', 
             'fair', 'exclnt', 'good', 'exclnt', 'good', 'fair', 'fair', 'good', 
             'good', 'good', 'good', 'fair', 'good', 'fair', 'fair', 'good', 'exclnt', 
             'good', 'fair', 'good', 'poor', 'good', 'good', 'good', 'poor', 'fair', 
             'good', 'good', 'fair', 'fair', 'exclnt', 'good', 'good', 'poor', 'good', 
             'good', 'poor', 'poor', 'good', 'fair', 'poor', 'fair', 'fair', 'good', 
             'fair', 'fair', 'good', 'fair', 'good', 'poor', 'exclnt', 'good', 'good', 
             'good', 'exclnt', 'good', 'exclnt', 'good', 'poor', 'fair', 'good', 
             'exclnt', 'fair', 'fair', 'poor', 'good', 'poor'), 
  age = c(18, 53, 39, 59, 24, 39, 51, 57, 23, 42, 56, 56, 48, 43, 66, 38, 23, 52, 
          59, 38, 28, 37, 67, 41, 54, 54, 68, 40, 29, 40, 69, 36, 35, 56, 58, 42, 
          44, 67, 57, 52, 56, 47, 54, 41, 33, 41, 44, 27, 52, 41, 57, 25, 50, 39, 
          57, 44, 49, 52, 59, 50, 48, 47, 53, 32, 22, 70, 52, 53, 39, 62, 43, 56, 
          54, 50, 28, 45, 40, 52, 52, 50, 47, 68, 60, 49, 44, 57, 19, 21, 55, 58, 
          60, 44, 43, 49, 33, 55, 50, 68, 43, 36, 72, 54, 48, 77, 67, 66, 48, 74, 
          78, 33, 60, 58, 70, 30, 66, 68, 72, 62, 66, 68, 67, 60, 66, 71, 74, 64, 
          65, 55, 54, 38, 69, 58, 62, 66, 58, 49, 73, 28, 69, 24, 55, 69, 64, 44, 
          49, 69, 45, 77, 46, 75), 
  num_visits = c(0, 1, 3, 1, 0, 1, 3, 1, 0, 1, 4, 1, 0, 1, 4, 1, 0, 1, 4, 1, 0, 3, 
                 4, 1, 0, 3, 4, 1, 0, 3, 4, 1, 0, 3, 4, 1, 0, 3, 4, 1, 0, 3, 2, 1, 
                 1, 3, 2, 1, 1, 3, 2, 2, 1, 3, 2, 2, 1, 3, 2, 2, 1, 3, 2, 2, 1, 3, 
                 2, 2, 1, 3, 2, 2, 1, 3, 2, 2, 1, 3, 2, 2, 1, 3, 2, 2, 1, 3, 2, 2, 
                 1, 3, 2, 2, 1, 3, 2, 2, 1, 3, 3, 2, 4, 3, 2, 6, 4, 3, 4, 6, 5, 3, 
                 4, 6, 5, 3, 4, 7, 5, 3, 4, 7, 5, 3, 4, 8, 5, 3, 4, 2, 5, 3, 4, 2, 
                 5, 3, 4, 2, 5, 3, 4, 2, 2, 5, 4, 3, 2, 6, 2, 6, 2, 6)
) %>% 
  mutate(health = relevel(as_factor(health), ref = 'exclnt'))
doc_visits %>% print()

# fit gam
fit = gam(num_visits ~ health + s(age), family = poisson, data = doc_visits)
summary(fit)
coefficients(summary.glm(fit))

# get results
doc_visits %>% 
  mutate(p_num_visits = fit$fitted.values, 
         p_age = fit$smooth[,1]) -> results
results %>% print()

# new prediction
predict(fit, data.frame(health = 'good', age = 70), type = 'response')

# plot p_num_visits vs. age by health
results %>% 
  ggplot(aes(x = age, y = p_num_visits)) + 
  geom_line(color = 'blue', size = 1) + 
  facet_wrap(~health, nrow = 2) + 
  theme_bw()

#--------------------------------------------------------------------------------

# Example 5.4

# data
mice = tibble(
  temp = c(-10, 10, -20, -9, 14, -16, 24, -15, -15, 25, 15, 7, 28, 4, 0, 5, 7, 
           -4, 0, -8), 
  rainfall = c(36.4, 27.1, 43.3, 32.9, 25.5, 23.7, 22.0, 22.4, 17.2, 31.8, 32.3, 
               23.1, 38.0, 29.2, 28.1, 14.2, 10.4, 24.6, 27.7, 19.1), 
  num_mice = c(1, 0, 0, 1, 2, 2, 2, 0, 4, 4, 4, 5, 1, 6, 6, 9, 8, 3, 8, 2)
)
mice %>% print()

# fit gam
fit = gam(num_mice ~ s(temp) + s(rainfall), family = poisson, data = mice)
summary(fit)
coefficients(summary.glm(fit))

# get results
mice %>% 
  mutate(p_num_mice = fit$fitted.values, 
         p_temp = fit$smooth[,1], 
         p_rainfall = fit$smooth[,2]) -> results
results %>% print()

# 3D scatterplot for predicted number of mice
plot_ly(results, x=~temp, y=~rainfall, z=~p_num_mice, 
        type = 'scatter3d', mode = 'markers')

#--------------------------------------------------------------------------------



