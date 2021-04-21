#--------------------------------------------------------------------------------

# Ch 4: Nonparametric Regression
# S2: Thin-Plate Smoothing Spline Method

# Joey Gadbois


# libraries
library(tidyverse)
library(tidymodels)
library(ggpubr)
library(plotly)
library(fields)

# set ggplot2 theme
theme_set(theme_classic())

# plotting function
plot_fitted_curve = function(fit_results){
  # plots the fitted curve with confidence bands
  # NOTE: labels must be added manually
  fit_results %>%
    ggplot(aes(x = x, y = y)) + 
    geom_point(color = 'blue') + 
    geom_line(aes(y = .fitted), color = 'red', size = 1) + 
    geom_ribbon(aes(ymin = lower_cl, ymax = upper_cl), fill = 'gray', alpha = 0.5) + 
    theme_classic() -> plot
  return(plot)
}

# function to get Thin-Plate Spline results in tibble for plotting
results_tps = function(fit_tps, conf_level = 0.95){
  # get critical value for confidence interval
  alpha = 1 - conf_level
  confidence = 1 - (alpha/2)
  crit_val = qnorm(p = confidence)
  
  # create tibble with results
  tibble(x = fit_tps$x, y = fit_tps$y) %>% 
    mutate(.fitted = predict(fit_tps, x), 
           se = predictSE(fit_tps, x), 
           lower_cl = .fitted-crit_val*se, 
           upper_cl = .fitted+crit_val*se) -> results
  return(results)
}

#--------------------------------------------------------------------------------

# Example 4.3

# data
unemployment_rates = tibble(
  month = 1:120, 
  rate = c(6.0, 6.7, 4.9, 4.4, 5.8, 4.8, 5.5, 6.7, 4.7, 5.6, 6.5, 6.0, 4.7, 5.1, 
           7.2, 6.1, 7.7, 5.7, 7.1, 4.2, 5.8, 5.1, 6.3, 5.1, 3.9, 4.7, 4.4, 5.9, 
           4.1, 5.8, 4.9, 5.4, 3.9, 6.0, 4.1, 4.6, 5.7, 5.0, 4.5, 6.9, 5.6, 4.6, 
           4.4, 4.1, 3.2, 6.3, 4.2, 4.7, 4.3, 4.3, 4.5, 6.7, 3.9, 4.6, 5.8, 3.8, 
           5.5, 4.7, 5.0, 4.2, 5.0, 4.5, 3.7, 5.5, 5.4, 2.6, 5.0, 4.9, 5.7, 4.3, 
           5.3, 7.1, 7.5, 4.1, 5.1, 5.7, 4.8, 6.1, 6.3, 4.1, 5.7, 7.2, 6.0, 7.2, 
           8.0, 8.7, 8.5, 9.1, 7.5, 10.5, 8.5, 7.4, 10.5, 8.9, 8.5, 9.9, 8.3, 9.9,
           7.2, 9.5, 10.5, 11.9, 11.4, 8.0, 10.5, 11.2, 9.2, 9.5, 10.0, 10.3, 9.1, 
           8.1, 7.9, 9.5, 10.7, 8.5, 9.1, 8.7, 9.0, 8.6)
)
unemployment_rates %>% print()

# scatter plot
unemployment_rates %>% 
  ggplot(aes(x = month, y = rate)) + 
  geom_point(color = 'blue') + 
  labs(x = 'Months', y = 'Unemployment Rate') +
  ggtitle('Monthly Unemployment Rate Data')

# fit TPS with degree of smoothness: m = 2
tps_m1 = Tps(unemployment_rates$month, unemployment_rates$rate, m = 1)

# view results
tps_m1 %>% results_tps()

# plot TPS curve
pm1 = tps_m1 %>% 
  results_tps() %>% 
  plot_fitted_curve() + 
  ggtitle('Thin-Plate Spline Curve | Degree of Smoothness: M = 1') + 
  labs(x = 'Months', y = 'Unemployment Rate')
pm1

# fit TPS with degree of smoothness: m = 2
tps_m2 = Tps(unemployment_rates$month, unemployment_rates$rate, m = 2)

# view results
tps_m2 %>% results_tps()

# plot TPS curve
pm2 = tps_m2 %>% 
  results_tps() %>% 
  plot_fitted_curve() + 
  ggtitle('Thin-Plate Spline Curve | Degree of Smoothness: M = 2') + 
  labs(x = 'Months', y = 'Unemployment Rate')
pm2

# plot both curves
ggarrange(pm1, pm2, nrow = 2, ncol = 1)

#--------------------------------------------------------------------------------

# Example 4.4

# data
plaza = tibble(
  income = c(114, 79, 69, 44, 18, 32, 62, 77, 35, 66, 71, 25, 92, 44, 85, 71, 43, 
             78, 89, 116, 82, 41, 37, 83, 145, 87, 117, 86, 85, 96, 51, 59, 58, 
             46, 38, 120, 87, 114, 74, 76, 43, 37, 56, 38, 28, 75, 49, 89, 75, 
             123, 122, 111, 76, 55, 39, 46, 45, 55), 
  grocery = c(15, 23, 8, 8, 5, 7, 18, 10, 10, 10, 12, 5, 21, 12, 15, 18, 13, 18, 
              13, 12, 13, 12, 6, 13, 25, 11, 16, 12, 14, 15, 7, 13, 10, 7, 6, 22, 
              10, 17, 11, 10, 8, 5, 11, 8, 5, 11, 12, 10, 20, 22, 25, 12, 10, 7,
              6.5, 12, 8, 13), 
  entertainment = c(7, 3, 10, 3.5, 0.8, 2.4, 8, 3, 3.2, 16, 12, 0.6, 5, 4.5, 4, 
                    13, 1.3, 12, 2, 11, 2, 1, 2.4, 1, 12, 12, 8, 2, 3, 3, 1.6, 
                    4, 3.9, 2.4, 0.3, 5, 2, 5, 15, 8, 1.2, 1.2, 1.5, 5.6, 1.2, 
                    16, 1.8, 8, 12, 7, 2, 6, 17, 1, 4, 2.8, 2, 9)
)
plaza %>% print()

# plot data
plot_ly(plaza, x = ~income, y = ~grocery, z = ~entertainment)

# fit TPS surface
tps_surf = Tps(cbind(plaza$income, plaza$grocery), plaza$entertainment, 
               m = 2, method = 'GCV.model')

# data for surface prediction
plaza_surf = expand.grid(
  list(income = seq(min(plaza$income), max(plaza$income), 1), 
       grocery = seq(min(plaza$grocery), max(plaza$grocery), 1))
)

# predict TPS surface
surface_tps = predict(tps_surf, plaza_surf)

# transform predictions for 3D surface plot
plaza %>% 
  summarise(min_income = min(income), 
            max_income = max(income), 
            range_income = max_income-min_income+1,
            min_grocery = min(grocery), 
            max_grocery = max(grocery), 
            range_grocery = max_grocery-min_grocery+1)
surface_tps = matrix(surface_tps, nrow = 128, ncol = 21)

# 3D plot settings
ax = list(
  x = list(title='income', 
           showbackground=T, 
           backgroundcolor='black'), 
  y = list(title='grocery', 
           showbackground=T, 
           backgroundcolor='black'), 
  z = list(title='entertainment predictions', 
           showbackground=T, 
           backgroundcolor='black')
)

# plot TPS surface
plot_ly(z = ~surface_tps, lighting=list(ambient=0.9)) %>% 
  add_surface(showscale=F) %>% 
  layout(scene = list(xaxis = ax$x, 
                      yaxis = ax$y, 
                      zaxis = ax$z, 
                      aspectmode='cube'))

# get prediction
predict(tps_surf, data.frame(income=75, grocery=10))

#--------------------------------------------------------------------------------



