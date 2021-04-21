#--------------------------------------------------------------------------------

# Ch 8: Resampling Methods for Interval Estimation
# S1: Jackknife

# Joey Gadbois


# libraries
library(tidyverse)
library(bootstrap)

# function for jackknife intervals
jackknife_interval = function(jack_results, data_matrix, stat, conf_level = 0.95){
  # values for confidence intercal
  n = nrow(data_matrix)
  a = (1 - conf_level) / 2
  crit_val = qt(a, n - 1)
  bias = jack_results$jack.bias
  se = jack_results$jack.se
  
  if (stat == 'mean'){
    low = mean(data_matrix)+bias+crit_val*se
    upp = mean(data_matrix)+bias-crit_val*se
  }
  else if (stat == 'var'){
    low = var(data_matrix)+bias+crit_val*se
    upp = var(data_matrix)+bias-crit_val*se
  }
  else if (stat == 'pearson'){
    x1 = X[,1]
    x2 = X[,2]
    low = cor(x1, x2, method = 'pearson')+bias+crit_val*se
    upp = cor(x1, x2, method = 'pearson')+bias-crit_val*se
  }
  else if (stat == 'spearman'){
    x1 = X[,1]
    x2 = X[,2]
    low = cor(x1, x2, method = 'spearman')+bias+crit_val*se
    upp = cor(x1, x2, method = 'spearman')+bias-crit_val*se
  }
  else{
    message = 'stat must be mean, var, pearson, or spearman'
    return(print(message))
  }
  interval = data.frame(lower_cl = low, upper_cl = upp)
  return(interval)
}

#--------------------------------------------------------------------------------

# Example 8.1

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

# convert to matrix
X = as.matrix(snack_shack)

# jackknife estimation
samp_jack = jackknife(X, mean)

# jackknife confidence interval
jackknife_interval(samp_jack, X, stat = 'mean', conf_level = 0.95)

#--------------------------------------------------------------------------------

# Example 8.2

# data
study_exam = tibble(
  hours = c(0, 5, 9, 7, 17, 17, 5, 12, 21, 3, 7, 29, 7, 10), 
  score = c(28, 94, 84, 45, 82, 99, 67, 97, 79, 93, 62, 60, 85, 78)
)
study_exam %>% print()

# convert to matrix
X = as.matrix(study_exam)

# parameter function for estimation
theta = function(x, data){cor(data[x, 1], data[x, 2], method = 'spearman')}

# jackknife estimation
samp_jack = jackknife(1:nrow(X), theta, X)

# jackknife confidence interval
jackknife_interval(samp_jack, X, stat = 'spearman', conf_level = 0.99)

#--------------------------------------------------------------------------------


