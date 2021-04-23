# R Codes by Section

**Some issues in the code:**

* For testing, the methods cannot handle ties in ranks, so results will sometimes differ
* Ch 5 - the `gam` package does not support bivariate spline smoothers, so the `mgcv` package is used. The `mgcv` package supports bivariate spline smoothers, but it is based on Bayesion methods so the results will differ
  * When using, make sure to detach the `mgcv` package after use because the function call is the same as in the `gam` package
* the kernel density estimators do not seem to respond as much to differing bandwidths as in SAS. The function *bw_lambda* computes the relationship between standardized bandwidth and bandwidth according to the SAS documentation
* resampling methods will not match perfectly due to how each software handles random digits

---

## Chapter 1 Hypothesis Testing for Two Samples

1. Sign Test for Location Parameter for Matched Paired Samples
2. Wilcoxon Signed-Rank Test for Location Parameter for Matched Paired Samples
3. Wilcoxon Rank-Sum Test for Location Parameter for Two Independent Samples
4. Ansari-Bradley Test for Scale Parameter for Two Independent Samples
5. Kolmogorov-Smirnov Test for Equality of Distributions

## Chapter 2 Hypothesis Testing for Several Samples

1. Friedman Rank Test for Location Parameter for Several Dependent Samples
2. Kruskal-Wallis H-Test for Location Parameter for Several Independent Samples

## Chapter 3 Tests for Categorical Data

1. Spearman Rank Correlation Coefficient Test
2. Fisher Exact Test

## Chapter 4 Nonparametric Regression

1. Loess Regression
2. Thin-Plate Smoothing Spline Method

## Chapter 5 Nonparametric Generalized Additive Regression

2. Nonparametric Binary Logistic Model
3. Nonparametric Poisson Model

## Chapter 6 Time-to-Event Analysis

1. Kaplan-Meier Estimator of Survival Function
2. Log-Rank Test for Comparison of Two Survival Functions
3. Cox Proportional Hazards Model

## Chapter 7 Univariate Probability Density Estimation

1. Histogram
2. Kernel Density Estimator

## Chapter 8 Resampling Methods for Interval Estimation

1. Jackknife
2. Bootstrap

---
