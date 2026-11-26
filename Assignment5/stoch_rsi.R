# AI Assistance Declaration: I used Muse (Muse Spark 1.3) for drafting the
# first version of this function, for comments, and for ideas for checks.
# I verified outputs using hand calculation and before/after comparison in R.
# All final calculations are done by myself. I am responsible for the
# accuracy and originality of this work.
# Student: Maryam Alyasin
# Course: BDA400, Assignment 5
# Date: 26 November 2026
# Repository: TechnicalAnalysis / Assignment5

# Stochastic RSI (StochRSI)
# -------------------------
# Combines RSI with the Stochastic Oscillator: it normalizes the RSI
# values to the 0-1 range using the lowest and highest RSI over the
# period, then smooths the result. The %K line is the StochRSI and the
# %D line is a simple moving average of %K. Values near 0 suggest
# oversold, values near 1 suggest overbought.
#
# Implemented in BASE R ONLY, following the template and pseudocode in the
# Assignment 5 instructions. No library functions are used.
#
# NOTE: this file uses rsi() (defined in rsi.R) and sma() (defined in
# sma.R). Source both files first:
#   source("rsi.R")
#   source("sma.R")

stoch_rsi <- function(data, period, k_period, d_period) {
  # Calculate the RSI
  rsi_values <- rsi(data, period)

  # Calculate the StochRSI: normalize the RSI values to 0-1.
  # min() and max() MUST use na.rm = TRUE here: the first 'period' RSI
  # values are NA (there is no RSI value until 'period' differences have
  # been seen), and without na.rm = TRUE, min/max of a vector containing
  # NA is NA, which would make every k_value NA.
  min_rsi <- min(rsi_values, na.rm = TRUE)
  max_rsi <- max(rsi_values, na.rm = TRUE)
  k_values <- (rsi_values - min_rsi) / (max_rsi - min_rsi)

  # Calculate the %K line (StochRSI): SMA of the normalized RSI values.
  # Windows that contain an NA stay NA (the first 'period' values).
  k_line <- sma(k_values, k_period)

  # Calculate the %D line (simple moving average of %K)
  d_line <- sma(k_line, d_period)

  # Return the %K and %D lines as a list
  result <- list(k_line = k_line,
                 d_line = d_line)
  return(result)
}

# Example (adapted from the instructions - see the note below):
# data <- c(45, 50, 48, 55, 52, 49, 58, 60, 65, 62)
# stoch_rsi_result <- stoch_rsi(data, period = 5, k_period = 3, d_period = 3)
# print(stoch_rsi_result)
# Expected output:
# $k_line
# [1]       NA       NA       NA       NA       NA 0.478104 0.811438 0.786563
# $d_line
# [1]       NA       NA       NA       NA       NA 0.692035
#
# Hand check:
#   RSI values (period 5): NA, NA, NA, NA, NA, 50.5263, 68.9256, 71.8352,
#                          78.2108, 66.8597
#   min RSI = 50.5263, max RSI = 78.2108
#   k_values (non-NA part): 0, 0.664607, 0.769705, 1, 0.589982
#     e.g. (68.9256 - 50.5263) / (78.2108 - 50.5263) = 0.664607
#   k_line = SMA of k_values with period 3:
#     (0 + 0.664607 + 0.769705) / 3 = 0.478104
#     (0.664607 + 0.769705 + 1) / 3 = 0.811438
#     (0.769705 + 1 + 0.589982) / 3 = 0.786563
#   d_line = SMA of k_line with period 3:
#     (0.478104 + 0.811438 + 0.786563) / 3 = 0.692035
#
# NOTE ON THE INSTRUCTIONS' OWN EXAMPLE: the PDF shows
# stoch_rsi(data, period = 14, k_period = 3, d_period = 3) on these same
# 10 points. That cannot produce values: rsi() needs more than 14 points
# before its first RSI value appears (there are only 9 differences in
# 10 points), so every RSI value would be NA. The test therefore uses
# period = 5 on this data, which is the same data the RSI example uses.
