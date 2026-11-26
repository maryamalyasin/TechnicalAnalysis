# AI Assistance Declaration: I used Muse (Muse Spark 1.3) for drafting the
# first version of this function, for comments, and for ideas for checks.
# I verified outputs using hand calculation and before/after comparison in R.
# All final calculations are done by myself. I am responsible for the
# accuracy and originality of this work.
# Student: Maryam Alyasin
# Course: BDA400, Assignment 5
# Date: 26 November 2026
# Repository: TechnicalAnalysis / Assignment5

# Exponential Moving Average (EMA)
# --------------------------------
# The EMA gives greater weight to recent data points, so it reacts faster
# to recent price movements than the SMA.
# Formula: EMA(i) = (Price(i) - EMA(i-1)) * multiplier + EMA(i-1)
# where multiplier = 2 / (period + 1), and EMA(1) = Price(1).
#
# Implemented in BASE R ONLY, following the template and pseudocode in the
# Assignment 5 instructions. No library functions are used.

ema <- function(data, period) {
  # Calculate the multiplier for EMA
  multiplier <- 2 / (period + 1)

  # Initialize an empty array to store EMA values
  ema_values <- numeric(length(data))

  # Loop through the data array
  for (i in 1:length(data)) {
    # Calculate EMA for the first data point
    if (i == 1) {
      ema_values[i] <- data[i]
    } else {
      # Calculate EMA for subsequent data points
      ema_values[i] <- (data[i] - ema_values[i - 1]) * multiplier + ema_values[i - 1]
    }
  }

  return(ema_values)
}

# Example from the instructions:
# data <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)
# ema_result <- ema(data, period = 3)
# print(ema_result)
# Expected output:
# [1] 10.000000 11.000000 13.000000 16.500000 17.250000 19.625000 22.312500
# [8] 23.156250 22.078125
#
# Hand check (multiplier = 2 / (3 + 1) = 0.5):
#   EMA(1) = 10
#   EMA(2) = (12 - 10) * 0.5 + 10 = 11
#   EMA(3) = (15 - 11) * 0.5 + 11 = 13
# The result has the same length as the data (9 values).
