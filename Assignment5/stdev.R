# AI Assistance Declaration: I used Muse (Muse Spark 1.3) for drafting the
# first version of this function, for comments, and for ideas for checks.
# I verified outputs using hand calculation and before/after comparison in R.
# All final calculations are done by myself. I am responsible for the
# accuracy and originality of this work.
# Student: Maryam Alyasin
# Course: BDA400, Assignment 5
# Date: 26 November 2026
# Repository: TechnicalAnalysis / Assignment5

# Standard Deviation (stdev)
# ---------------------------
# Measures how far the data points spread out from their mean.
# Formula: sigma = sqrt( sum((x_i - mean)^2) / n )
# NOTE: the variance here is divided by n (the number of data points),
# NOT by n - 1. This is the POPULATION standard deviation, exactly as
# the pseudocode in the instructions says.
# R's built-in sd() divides by n - 1, so it gives a different (larger)
# answer and must NOT be used here. It is also a shortcut the
# instructions do not allow for this implementation.
#
# Implemented in BASE R ONLY, following the template and pseudocode in the
# Assignment 5 instructions. No library functions are used.

stdev <- function(data) {
  # Calculate the mean of the data
  mean_value <- sum(data) / length(data)

  # Calculate the differences between the data points and the mean
  diff_values <- data - mean_value

  # Calculate the squared differences
  squared_diff <- diff_values * diff_values

  # Calculate the variance (mean of squared differences)
  variance <- sum(squared_diff) / length(squared_diff)

  # Calculate the standard deviation (square root of the variance)
  standard_deviation <- sqrt(variance)

  return(standard_deviation)
}

# Example from the instructions:
# data <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)
# stdev_result <- stdev(data)
# print(stdev_result)
# Expected output:
# [1] 4.946629
#
# Hand check:
#   mean = (10 + 12 + 15 + 20 + 18 + 22 + 25 + 24 + 21) / 9 = 167 / 9
#        = 18.555556
#   squared differences: 73.197531, 42.975309, 12.641975, 2.086420,
#        0.308642, 11.864198, 41.530864, 29.641975, 5.975309
#   sum of squared differences = 220.222222
#   variance = 220.222222 / 9 = 24.469136
#   standard deviation = sqrt(24.469136) = 4.946629
#
# Contrast: R's sd(data) divides by n - 1 = 8 instead of 9, giving
# sd(data) = 5.246692. That is the SAMPLE standard deviation, not the
# population standard deviation the instructions ask for.
