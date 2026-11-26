# AI Assistance Declaration: I used Muse (Muse Spark 1.3) for drafting the
# first version of this function, for comments, and for ideas for checks.
# I verified outputs using hand calculation and before/after comparison in R.
# All final calculations are done by myself. I am responsible for the
# accuracy and originality of this work.
# Student: Maryam Alyasin
# Course: BDA400, Assignment 5
# Date: 26 November 2026
# Repository: TechnicalAnalysis / Assignment5

# Linear Regression (linreg)
# --------------------------
# Fits the best straight line y = intercept + slope * x through a window
# of the data, where x is the position inside the window (1, 2, 3, ...).
# The slope and intercept come from the least-squares formulas.
#
# Implemented in BASE R ONLY, following the template and pseudocode in the
# Assignment 5 instructions exactly. No library functions are used.
#
# NOTE ON THE WINDOW: as the pseudocode is written, start_index =
# max(1, n - regressionLength + regressionOffset) and end_index =
# min(n, n - regressionOffset). With regressionOffset = 0 the window
# holds regressionLength + 1 points, not regressionLength points
# (for example, length 5 on 9 points takes elements 4-9, six points).
# This is NOT "fixed" here: the pseudocode is followed as written, and
# the tests below show the actual subset used.

linreg <- function(regressionSource, regressionLength, regressionOffset) {
  # Calculate the total number of elements in the regressionSource
  n <- length(regressionSource)

  # Check if regressionLength is greater than the number of elements in regressionSource
  if (regressionLength > n) {
    stop("regressionLength cannot be greater than the number of elements in regressionSource")
  }

  # Check if regressionOffset is greater than or equal to regressionLength
  if (regressionOffset >= regressionLength) {
    stop("regressionOffset must be less than regressionLength")
  }

  # Calculate the starting index for the regressionSource
  start_index <- max(1, n - regressionLength + regressionOffset)

  # Calculate the ending index for the regressionSource
  end_index <- min(n, n - regressionOffset)

  # Extract the relevant portion of regressionSource
  source_subset <- regressionSource[start_index:end_index]

  # Calculate the index values for the regression points
  index_values <- 1:length(source_subset)

  # Calculate the sum of index values and the sum of source_subset
  sum_index <- sum(index_values)
  sum_source <- sum(source_subset)

  # Calculate the mean of index values and the mean of source_subset
  mean_index <- sum_index / length(index_values)
  mean_source <- sum_source / length(source_subset)

  # Calculate the numerator and denominator for the linear regression formula
  numerator <- sum((index_values - mean_index) * (source_subset - mean_source))
  denominator <- sum((index_values - mean_index)^2)

  # Calculate the slope and intercept of the linear regression line
  slope <- numerator / denominator
  intercept <- mean_source - slope * mean_index

  # Calculate the predicted values for the regressionSource
  predicted_values <- slope * index_values + intercept

  # Return the slope, intercept, and predicted values as a list
  result <- list(slope = slope,
                 intercept = intercept,
                 predicted_values = predicted_values)
  return(result)
}

# Examples (with the expected output):
#
# data <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)
# linreg(data, 5, 0)
#   subset used: c(20, 18, 22, 25, 24, 21)  (elements 4-9, six points)
#   Expected: $slope = 0.742857, $intercept = 19.066667
#   $predicted_values = 19.809524 20.552381 21.295238 22.038095 22.780952 23.523810
#   Hand check: mean of index values = 3.5, mean of subset = 130/6 = 21.666667
#   numerator = 13, denominator = 17.5, slope = 13/17.5 = 0.742857
#   intercept = 21.666667 - 0.742857 * 3.5 = 19.066667
#
# linreg(data, 5, 1)
#   subset used: c(18, 22, 25, 24)  (elements 5-8, four points)
#   Expected: $slope = 2.1, $intercept = 17
#   $predicted_values = 19.1 21.2 23.3 25.4
#   Hand check: mean of index values = 2.5, mean of subset = 89/4 = 22.25
#   numerator = 10.5, denominator = 5, slope = 10.5/5 = 2.1
#   intercept = 22.25 - 2.1 * 2.5 = 17
#
# linreg(1:10, 5, 0)
#   subset used: c(5, 6, 7, 8, 9, 10)  (elements 5-10, six points)
#   Expected: $slope = 1, $intercept = 4
#   $predicted_values = 5 6 7 8 9 10
#
# Error cases (both stop with an error, as the pseudocode requires):
# linreg(data, 20, 0)  -> "regressionLength cannot be greater than the
#                          number of elements in regressionSource"
# linreg(data, 5, 5)   -> "regressionOffset must be less than regressionLength"
