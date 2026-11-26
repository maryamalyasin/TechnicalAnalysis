# AI Assistance Declaration: I used Muse (Muse Spark 1.3) for drafting the
# first version of this function, for comments, and for ideas for checks.
# I verified outputs using hand calculation and before/after comparison in R.
# All final calculations are done by myself. I am responsible for the
# accuracy and originality of this work.
# Student: Maryam Alyasin
# Course: BDA400, Assignment 5
# Date: 26 November 2026
# Repository: TechnicalAnalysis / Assignment5

# Simple Moving Average (SMA)
# ---------------------------
# The SMA is the average of a set of data points over a specified period,
# giving equal weight to each data point. It smooths out short-term
# fluctuations and helps identify the trend in a time series.
# Formula: SMA = (sum of all data points over the period) / (number of
# data points in the period)
#
# Implemented in BASE R ONLY, following the template and pseudocode in the
# Assignment 5 instructions. No library functions are used.

sma <- function(data, period) {
  # Check if the length of data is less than the specified period
  if (length(data) < period) {
    stop("Data length should be greater than or equal to the period")
  }

  # Initialize a vector to store the SMA values
  sma_values <- numeric(length(data) - period + 1)

  # Calculate SMA for each window of 'period' data points
  for (i in 1:(length(data) - period + 1)) {
    # Current window of 'period' data points
    current_window <- data[i:(i + period - 1)]
    # Mean of the current window: sum of the window divided by the period
    sma_values[i] <- sum(current_window) / period
  }

  return(sma_values)
}

# Example from the instructions:
# data <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)
# sma_result <- sma(data, period = 3)
# print(sma_result)
# Expected output:
# [1] 12.333333 15.666667 17.666667 20.000000 21.666667 23.666667 23.333333
#
# Hand check of the first three values:
#   window 1: (10 + 12 + 15) / 3 = 37 / 3 = 12.333333
#   window 2: (12 + 15 + 20) / 3 = 47 / 3 = 15.666667
#   window 3: (15 + 20 + 18) / 3 = 53 / 3 = 17.666667
# The result has 9 - 3 + 1 = 7 values (one per window).
