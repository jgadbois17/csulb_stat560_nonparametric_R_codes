#--------------------------------------------------------------------------------

# Ch 5: Nonparametric Genralized Additive Regression
# S2: Nonparametric Binary Logistic Model

# Joey Gadbois


# libraries
library(tidyverse)
library(ggpubr)
library(plotly)
library(gam)

# set ggplot2 theme
theme_set(theme_classic())

#--------------------------------------------------------------------------------

# Example 5.1

# data
artery_disease = tibble(
  age = c(65, 26, 46, 22, 71, 33, 28, 64, 73, 85, 43, 51, 60, 83, 26, 71, 24, 
          71, 48, 33, 52, 70, 40, 20, 85, 35, 37, 54, 55, 26, 33, 65, 21, 76, 
          83, 68, 31, 70, 67, 34, 30, 76, 26, 71, 41, 73, 29, 48, 34, 23, 82, 
          23, 61, 42, 47, 54, 75, 52, 26, 61, 51, 85, 45, 77, 59, 73, 20), 
  bmi = c(27.3, 20.3, 15.7, 28.5, 19.4, 16.2, 14.3, 20.5, 18.8, 22.4, 26.4, 26.4, 
          24.3, 18.9, 19.7, 18.7, 21.3, 18.7, 16.4, 18.8, 21.8, 19.8, 23.4, 22.1, 
          18.1, 20.6, 18.4, 22.1, 15.5, 19.5, 17.5, 26.2, 21.2, 21.1, 25.5, 20.9, 
          20.9, 24.8, 21.3, 20.6, 24.4, 19.8, 18.8, 23.2, 25.0, 19.9, 18.7, 23.6, 
          23.4, 17.5, 16.5, 24.9, 19.9, 18.7, 26.6, 20.2, 14.4, 27.5, 21.3, 21.3, 
          28.9, 26.7, 26.5, 20.6, 26.9, 22.8, 17.9), 
  pad = c(0, 0, 0, 0, 1, 1, 0, 0, 1, 1, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 0, 
          0, 1, 1, 0, 0, 1, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 1, 0, 0, 0, 1, 1, 1, 
          1, 0, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 1, 1, 0, 1, 1, 0, 1, 1, 0)
)
artery_disease %>% print()

# data for surface prediction
artery_surface_df = expand.grid(
  list(age = seq(min(artery_disease$age), max(artery_disease$age), 1), 
       bmi = seq(min(artery_disease$bmi), max(artery_disease$bmi), 0.1))
)

# 3D plotting parameters
ax = list(
  x = list(title='age', showbackground=T, backgroundcolor='black'), 
  y = list(title='bmi', showbackground=T, backgroundcolor='black'), 
  z = list(title='predicted P(pad)', showbackground=T, backgroundcolor='black')
)

# MODEL 1: gam with loess smoothers
fit1 = gam(pad ~ lo(age) + lo(bmi), family = binomial, data = artery_disease)
summary(fit1)
coefficients(summary.glm(fit1))

# get results
artery_disease %>% 
  mutate(p_pad = fit1$fitted.values, 
         p_age = fit1$smooth[,1], 
         p_bmi = fit1$smooth[,2]) -> results1
results1 %>% print()

results1 %>% 
  ggplot(aes(x = age, y = p_bmi)) + 
  geom_line(color = 'blue', size = 1) + 
  labs(title = 'Loess(age) vs. age', x = 'age', y = 'loess(age)')
results1 %>% 
  ggplot(aes(x = bmi, y = p_bmi)) + 
  geom_line(color = 'blue', size = 1) + 
  labs(title = 'Loess(bmi) vs. bmi', x = 'bmi', y = 'loess(bmi)')

# predict and plot surface
p_pad1 = predict(fit1, newdata = artery_surface_df, type = 'response')
plot_ly(z = ~p_pad1, lighting=list(ambient=0.9)) %>% 
  add_surface(showscale=T) %>% 
  layout(scene = list(xaxis=ax$x, yaxis=ax$y, zaxis=ax$z, aspectmode='cube'))

# new prediction
predict(fit1, data.frame(age = 50, bmi = 21.4), type = 'response')

# MODEL 2: gam with spline smoothers
fit2 = gam(pad ~ s(age) + s(bmi), family = binomial, data = artery_disease)
summary(fit2)
coefficients(summary.glm(fit2))

# get results
artery_disease %>% 
  mutate(p_pad = fit2$fitted.values, 
         p_age = fit2$smooth[,1], 
         p_bmi = fit2$smooth[,2]) -> results2
results2 %>% print()

results2 %>% 
  ggplot(aes(x = age, y = p_age)) + 
  geom_line(color = 'blue', size = 1) + 
  labs(title = 'Loess(age) vs. age', x = 'age', y = 'loess(age)')
results2 %>% 
  ggplot(aes(x = bmi, y = p_bmi)) + 
  geom_line(color = 'blue', size = 1) + 
  labs(title = 'Loess(bmi) vs. bmi', x = 'bmi', y = 'loess(bmi)')

# predict and plot surface
p_pad2 = predict(fit2, newdata = artery_surface_df, type = 'response')
plot_ly(z = ~p_pad2, lighting=list(ambient=0.9)) %>% 
  add_surface(showscale=T) %>% 
  layout(scene = list(xaxis=ax$x, yaxis=ax$y, zaxis=ax$z, aspectmode='cube'))

# new prediction
predict(fit2, data.frame(age = 50, bmi = 21.4), type = 'response')

# MODEL 3: gam with bivariate spline
library(mgcv)
fit3 = gam(pad ~ s(age, bmi, k=7, bs='tp'), data = artery_disease, 
           family = binomial, method = 'GCV.Cp')
gam.check(fit3)
summary(fit3)

# get results
artery_disease %>% 
  mutate(p_pad = predict(fit3, type = 'response'), 
         p_age_bmi = predict(fit3, type = 'terms')[,1]) -> results3
results3 %>% print()

# predict and plot surface
artery_disease %>% 
  summarise(age_range = length(seq(min(age), max(age), 1)), 
            bmi_range = length(seq(min(bmi), max(bmi), 0.1)))

p_pad3 = predict(fit3, artery_surface_df, type = 'response')
p_pad3 = matrix(p_pad3, nrow = 66, ncol = 147)
plot_ly(z = ~p_pad3, lighting=list(ambient=0.9)) %>% 
  add_surface(showscale=T) %>% 
  layout(scene = list(xaxis=ax$x, yaxis=ax$y, zaxis=ax$z, aspectmode='cube'))

# new prediction
predict(fit3, data.frame(age = 50, bmi = 21.4), type = 'response')
detach(package:mgcv, unload = T)

#--------------------------------------------------------------------------------

# Example 5.2

# data
movies = tibble(
  cold_opening = c('no', 'no', 'no', 'no', 'no', 'yes', 'yes', 'yes', 'yes', 
                   'no', 'no', 'no', 'no', 'no', 'yes','no', 'no', 'yes', 
                   'yes','no', 'no', 'no', 'no', 'no', 'no', 'no', 'no', 
                   'no', 'no', 'no', 'no', 'no', 'no', 'no', 'no', 'no', 
                   'no', 'no', 'no', 'no', 'yes','no', 'no', 'no', 'no', 
                   'no', 'yes', 'yes','no', 'yes'), 
  critic_rating = c('bad', 'good', 'good', 'good', 'good', 'good', 'good','bad', 
                    'bad', 'good', 'good', 'good', 'bad', 'good', 'good','bad', 
                    'good', 'good','bad', 'good', 'good', 'good', 'good', 'good', 
                    'good', 'good', 'good', 'good', 'good', 'bad', 'good', 'good',
                    'good', 'good', 'good', 'good', 'good', 'bad', 'good', 'good',
                    'bad', 'good', 'good', 'good', 'good', 'good', 'good', 'good', 
                    'good', 'good'), 
  log_prod_cost = c(13.7, 14.1, 14.0, 13.1, 13.0, 14.2, 10.7, 14.5, 10.3, 14.0, 
                    12.8, 14.4, 12.2, 11.5, 13.0, 13.2, 13.4, 12.2, 14.0, 4.3, 
                    11.6, 14.5, 9.7, 14.0, 11.1, 13.0, 14.2, 13.7, 14.1, 13.3, 
                    12.5, 14.0, 13.4, 12.1, 14.2, 14.1, 13.0, 14.2, 14.2, 13.5, 
                    13.9, 14.4, 14.2, 17.6, 14.3, 14.0, 12.5, 13.1, 13.5, 13.4), 
  success = c('no', 'yes', 'no', 'yes', 'yes','no', 'no', 'no', 'no', 'yes', 
              'no', 'yes', 'no', 'yes', 'no', 'no', 'no', 'yes', 'no', 'yes', 
              'yes', 'yes','no', 'yes', 'yes', 'yes', 'yes', 'yes','no', 'no', 
              'yes', 'yes', 'yes', 'yes','no', 'no', 'yes', 'no', 'yes', 'yes', 
              'yes','no', 'yes', 'no', 'yes', 'yes', 'yes','no', 'no', 'no')
) %>% 
  mutate(success = if_else(success=='yes', 1, 0), 
         cold_opening = relevel(as_factor(cold_opening), ref = 'yes'), 
         critic_rating = as_factor(critic_rating)) -> movies
movies %>% print()

# fit gam with spline smoothing
fit = gam(success ~ cold_opening + critic_rating + s(log_prod_cost), 
          data = movies, family = binomial)
summary(fit)
summary.glm(fit)
coefficients(summary.glm(fit))

# get results
movies %>% 
  mutate(p_success = fit$fitted.values, 
         p_log_prod_cost = fit$smooth[,1]) -> results
results %>% print()

results %>% 
  filter(cold_opening == 'no', critic_rating == 'bad') %>% 
  ggplot(aes(x = log_prod_cost, y = p_success)) + 
  geom_line(color = 'blue', size = 1) + 
  labs(title = 'cold_opening = no | critic_rating = bad') -> p1
results %>% 
  filter(cold_opening == 'no', critic_rating == 'good') %>% 
  ggplot(aes(x = log_prod_cost, y = p_success)) + 
  geom_line(color = 'blue', size = 1) + 
  labs(title = 'cold_opening = no | critic_rating = good') -> p2
results %>% 
  filter(cold_opening == 'yes', critic_rating == 'bad') %>% 
  ggplot(aes(x = log_prod_cost, y = p_success)) + 
  geom_line(color = 'blue', size = 1) + 
  labs(title = 'cold_opening = yes | critic_rating = bad') -> p3
results %>% 
  filter(cold_opening == 'yes', critic_rating == 'good') %>% 
  ggplot(aes(x = log_prod_cost, y = p_success)) + 
  geom_line(color = 'blue', size = 1) + 
  labs(title = 'cold_opening = yes | critic_rating = good') -> p4
ggarrange(p1, p2, p3, p4, nrow = 2, ncol = 2)

# prediction
predict(
  fit, 
  data.frame(cold_opening='no', critic_rating='good', log_prod_cost=10), 
  type = 'response'
)

#--------------------------------------------------------------------------------


