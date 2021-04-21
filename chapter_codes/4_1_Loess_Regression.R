#--------------------------------------------------------------------------------

# Ch 4: Nonparametric Regression
# S1: Loess Regression

# Joey Gadbois


# libraries
library(tidyverse)
library(tidymodels)
library(ggpubr)
library(plotly)
library(fANCOVA)

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

# function to get Loess results in tibble for plotting
results_loess = function(fit_loess, conf_level = 0.95){
  # get critical value for confidence interval
  alpha = 1 - conf_level
  confidence = 1 - (alpha/2)
  crit_val = qnorm(p = confidence)
  
  # create tibble with results from Loess
  augment(fit_loess, se_fit = T) %>% 
    mutate(x = fit_loess$x, 
           y = fit_loess$y, 
           lower_cl = .fitted-crit_val*.se.fit, 
           upper_cl = .fitted+crit_val*.se.fit) -> results
  return(results)
}

#--------------------------------------------------------------------------------

# Example 4.1

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

# fit loess curve with automatic smoothing
lo_auto = loess.as(x = unemployment_rates$month, 
                   y = unemployment_rates$rate, 
                   degree = 1, 
                   criterion = 'gcv', 
                   plot = F)
lo_auto %>% summary()

# optimal smoothing parameter
round(lo_auto$pars$span, 4)

# view results
lo_auto %>% results_loess()

# plot loess curve
lo_auto %>% 
  results_loess() %>% 
  plot_fitted_curve() + 
  ggtitle('Optimal Smoothing Parameter = 0.2101') + 
  labs(x = 'Months', y = 'Unemployment Rate')

# fit loess curves for smoothing parameters: 0.05, 0.08, 0.10, 0.12
# using loess() instead of loess.as()
lo_0.05 = loess(rate ~ month, data = unemployment_rates, degree = 1, span = 0.05)
p1 = lo_0.05 %>% 
  results_loess() %>% 
  plot_fitted_curve() + 
  labs(title = 'Smoothing Parameter = 0.05', x = 'Months', y = 'Unemployment Rate')

lo_0.08 = loess(rate ~ month, data = unemployment_rates, degree = 1, span = 0.08)
p2 = lo_0.08 %>% 
  results_loess() %>% 
  plot_fitted_curve() + 
  labs(title = 'Smoothing Parameter = 0.08', x = 'Months', y = 'Unemployment Rate')

lo_0.10 = loess(rate ~ month, data = unemployment_rates, degree = 1, span = 0.10)
p3 = lo_0.10 %>% 
  results_loess() %>% 
  plot_fitted_curve() + 
  labs(title = 'Smoothing Parameter = 0.10', x = 'Months', y = 'Unemployment Rate')

lo_0.12 = loess(rate ~ month, data = unemployment_rates, degree = 1, span = 0.12)
p4 = lo_0.12 %>% 
  results_loess() %>% 
  plot_fitted_curve() + 
  labs(title = 'Smoothing Parameter = 0.12', x = 'Months', y = 'Unemployment Rate')

# plot all 4 loess curves
ggarrange(p1, p2, p3, p4, nrow = 2, ncol = 2)

#--------------------------------------------------------------------------------

# Example 4.2

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

# fit Loess surface
lo_surf = loess(entertainment ~ income + grocery, 
                data = plaza, 
                degree = 1, 
                span = 0.47414)

# data for surface prediction
plaza_surf = expand.grid(
  list(income = seq(min(plaza$income), max(plaza$income), 1), 
       grocery = seq(min(plaza$grocery), max(plaza$grocery), 1))
)

# predict loess surface
surface_lo = predict(lo_surf, newdata = plaza_surf)

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

# plot loess surface
plot_ly(z = ~surface_lo, lighting=list(ambient=0.9)) %>% 
  add_surface(showscale=F) %>% 
  layout(scene = list(xaxis = ax$x, 
                      yaxis = ax$y, 
                      zaxis = ax$z, 
                      aspectmode='cube'))

# prediction
predict(lo_surf, data.frame(income=75, grocery=10))

#--------------------------------------------------------------------------------


