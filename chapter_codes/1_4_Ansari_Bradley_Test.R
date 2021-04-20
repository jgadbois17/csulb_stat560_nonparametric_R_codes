#--------------------------------------------------------------------------------

# Ch 1: Hypothesis Testing for Two Samples
# S4: Ansari-Bradley Test for Scale Parameter for Two Independent Samples

# Joey Gadbois

#--------------------------------------------------------------------------------

# Example 1.10

# data
psych_tests = list(
  old = c(72, 64, 34, 78, 87), 
  new = c(80, 72, 94, 68, 57, 78, 82)
)

# ansari-bradley test
ansari.test(psych_tests$old, psych_tests$new, 
            alternative = 'greater', 
            conf.level = 0.95)

#--------------------------------------------------------------------------------

# Example 1.11

# data
bioimplant = list(
  Tx = c(3, 4, 6, 6, 8, 8), 
  Cx = c(4, 4, 4, 5, 6, 7, 8, 10, 12)
)

# ansari-bradley test
ansari.test(bioimplant$Tx, bioimplant$Cx, 
            alternative = 'less', 
            conf.level = 0.95)

#--------------------------------------------------------------------------------

# Example 1.12

# data
elderly = list(
  men = c(4, 5, 4, 3, 4, 3), 
  women = c(5, 5, 4, 4, 4, 5, 4)
)

# ansari-bradley test
ansari.test(elderly$men, elderly$women, 
            alternative = 'two.sided', 
            conf.level = 0.95)

#--------------------------------------------------------------------------------

