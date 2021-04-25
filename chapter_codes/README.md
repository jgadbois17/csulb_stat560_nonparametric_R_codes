# R Codes by Section

**Some issues in the code:**

* For testing, the methods cannot handle ties in ranks, so results will sometimes differ
* Ch 5 - the `gam` package does not support bivariate spline smoothers, so the `mgcv` package is used. The `mgcv` package supports bivariate spline smoothers, but it is based on Bayesion methods so the results will differ
  * When using, make sure to detach the `mgcv` package after use because the function call is the same as in the `gam` package
* the kernel density estimators do not seem to respond as much to differing bandwidths as in SAS. The function *bw_lambda* computes the relationship between standardized bandwidth and bandwidth according to the SAS documentation
* resampling methods will not match perfectly due to how each software handles random digits

---

# Chapter 1 Hypothesis Testing for Two Samples

1. [Sign Test for Location Parameter for Matched Paired Samples](https://github.com/jgadbois17/csulb_stat560_nonparametric_R_codes/blob/main/chapter_codes/1_1_Sign_Test.R)
2. [Wilcoxon Signed-Rank Test for Location Parameter for Matched Paired Samples](https://github.com/jgadbois17/csulb_stat560_nonparametric_R_codes/blob/main/chapter_codes/1_2_Wilcoxon_Signed_Rank_Test.R)
3. [Wilcoxon Rank-Sum Test for Location Parameter for Two Independent Samples](https://github.com/jgadbois17/csulb_stat560_nonparametric_R_codes/blob/main/chapter_codes/1_3_Wilcoxon_Rank_Sum_Test.R)
4. [Ansari-Bradley Test for Scale Parameter for Two Independent Samples](https://github.com/jgadbois17/csulb_stat560_nonparametric_R_codes/blob/main/chapter_codes/1_4_Ansari_Bradley_Test.R)
5. [Kolmogorov-Smirnov Test for Equality of Distributions](https://github.com/jgadbois17/csulb_stat560_nonparametric_R_codes/blob/main/chapter_codes/1_5_Kolmogorov_Smirnov_Test.R)

# Chapter 2 Hypothesis Testing for Several Samples

1. [Friedman Rank Test for Location Parameter for Several Dependent Samples](https://github.com/jgadbois17/csulb_stat560_nonparametric_R_codes/blob/main/chapter_codes/2_1_Friedman_Rank_Test.R)
2. [Kruskal-Wallis H-Test for Location Parameter for Several Independent Samples](https://github.com/jgadbois17/csulb_stat560_nonparametric_R_codes/blob/main/chapter_codes/2_2_Kruskal_Wallis_H_Test.R)

# Chapter 3 Tests for Categorical Data

1. [Spearman Rank Correlation Coefficient Test](https://github.com/jgadbois17/csulb_stat560_nonparametric_R_codes/blob/main/chapter_codes/3_1_Spearman_Rank_Test.R)
2. [Fisher Exact Test](https://github.com/jgadbois17/csulb_stat560_nonparametric_R_codes/blob/main/chapter_codes/3_2_Fisher_Exact_Test.R)

# Chapter 4 Nonparametric Regression

1. [Loess Regression](https://github.com/jgadbois17/csulb_stat560_nonparametric_R_codes/blob/main/chapter_codes/4_1_Loess_Regression.R)
2. [Thin-Plate Smoothing Spline Method](https://github.com/jgadbois17/csulb_stat560_nonparametric_R_codes/blob/main/chapter_codes/4_2_Thin_Plate_Smoothing_Splines.R)

# Chapter 5 Nonparametric Generalized Additive Regression

2. [Nonparametric Binary Logistic Model](https://github.com/jgadbois17/csulb_stat560_nonparametric_R_codes/blob/main/chapter_codes/5_2_Nonparametric_Binary_Logistic_Model.R)
3. [Nonparametric Poisson Model](https://github.com/jgadbois17/csulb_stat560_nonparametric_R_codes/blob/main/chapter_codes/5_3_Nonparametric_Poisson_Model.R)

# Chapter 6 Time-to-Event Analysis

1. [Kaplan-Meier Estimator of Survival Function](https://github.com/jgadbois17/csulb_stat560_nonparametric_R_codes/blob/main/chapter_codes/6_1_Kaplan_Meier_Estimator.R)
2. [Log-Rank Test for Comparison of Two Survival Functions](https://github.com/jgadbois17/csulb_stat560_nonparametric_R_codes/blob/main/chapter_codes/6_2_Log_Rank_Test_Survival_Functions.R)
3. [Cox Proportional Hazards Model](https://github.com/jgadbois17/csulb_stat560_nonparametric_R_codes/blob/main/chapter_codes/6_3_Cox_Proportional_Hazards_Model.R)

# Chapter 7 Univariate Probability Density Estimation

1. [Histogram](https://github.com/jgadbois17/csulb_stat560_nonparametric_R_codes/blob/main/chapter_codes/7_1_Histogram.R)
2. [Kernel Density Estimator](https://github.com/jgadbois17/csulb_stat560_nonparametric_R_codes/blob/main/chapter_codes/7_2_Kernel_Density_Estimator.R)

# Chapter 8 Resampling Methods for Interval Estimation

1. [Jackknife](https://github.com/jgadbois17/csulb_stat560_nonparametric_R_codes/blob/main/chapter_codes/8_1_Jackknife.R)
2. [Bootstrap](https://github.com/jgadbois17/csulb_stat560_nonparametric_R_codes/blob/main/chapter_codes/8_2_Bootstrap.R)

---
