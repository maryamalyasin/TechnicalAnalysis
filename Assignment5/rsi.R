# AI Assistance Declaration: I used Muse (Muse Spark 1.3) for drafting the
# first version of this function, for comments, and for ideas for checks.
# I verified outputs using hand calculation and before/after comparison in R.
# All final calculations are done by myself. I am responsible for the
# accuracy and originality of this work.
# Student: Maryam Alyasin
# Course: BDA400, Assignment 5
# Date: 26 November 2026
# Repository: TechnicalAnalysis / Assignment5

# Relative Strength Index (RSI)
# ----------------------------
# A momentum oscillator (0-100) that measures the speed and change of
# price movements. RSI = 100 - (100 / (1 + RS)), where RS = average gain
# / average loss. Above 70 is usually "overbought", below 30 "oversold".
# The averages use Wilder's smoothing: each new average keeps
# (period - 1) / period of the old average and adds 1 / period of the
# newest gain or loss.
#
# Implemented in BASE R ONLY, following the template and pseudocode in the
# Assignment 5 instructions. No library functions are used.

rsi <- function(data, period) {
  # Calculate the differences between consecutive data points
  diff_values <- diff(data)

  # Initialize two vectors to store the gains and losses
  # A gain is a positive difference, a loss is the size of a negative
  # difference; the other side is 0 in each position.
  gains <- pmax(diff_values, 0)
  losses <- pmax(-diff_values, 0)

  # Calculate the average gains and average losses
  # (mean of the first 'period' gains and losses)
  avg_gain <- mean(gains[1:period])
  avg_loss <- mean(losses[1:period])

  # Initialize the RSI vector with NA values
  # (there is no RSI value until 'period' differences have been seen)
  rsi_values <- rep(NA, length(data))

  # Calculate RSI values using the Wilder's smoothing method.
  # NOTE (R only): the pseudocode loop "for i = (period + 1) to
  # length of data" is empty when period >= length(data). In R,
  # (period + 1):length(data) would count BACKWARDS in that case, so the
  # loop is guarded: it only runs when there is at least one value to
  # calculate. This does not change any result the pseudocode defines.
  if (period < length(data)) {
    for (i in (period + 1):length(data)) {
      avg_gain <- (avg_gain * (period - 1) + gains[i - 1]) / period
      avg_loss <- (avg_loss * (period - 1) + losses[i - 1]) / period
      # If there were no losses at all, RS is infinite and RSI is 100.
      if (avg_loss == 0) {
        rsi_values[i] <- 100
      } else {
        rs <- avg_gain / avg_loss
        rsi_values[i] <- 100 - (100 / (1 + rs))
      }
    }
  }

  return(rsi_values)
}

# Example from the instructions:
# data <- c(45, 50, 48, 55, 52, 49, 58, 60, 65, 62)
# rsi_result <- rsi(data, period = 5)
# print(rsi_result)
# Expected output:
#  [1]       NA       NA       NA       NA       NA 50.5263 68.9256 71.8352
#  [9] 78.2108 66.8597
#
# Hand check:
#   differences: 5, -2, 7, -3, -3, 9, 2, 5, -3
#   gains:  5, 0, 7, 0, 0, 9, 2, 5, 0
#   losses: 0, 2, 0, 3, 3, 0, 0, 0, 3
#   average gain of the first 5 = (5 + 0 + 7 + 0 + 0) / 5 = 2.4
#   average loss of the first 5 = (0 + 2 + 0 + 3 + 3) / 5 = 1.6
#   i = 6: avg_gain = (2.4 * 4 + 0) / 5 = 1.92
#          avg_loss = (1.6 * 4 + 3) / 5 = 1.88
#          RS = 1.92 / 1.88 = 1.021277, RSI = 100 - 100 / 2.021277 = 50.5263
#   i = 7: avg_gain = (1.92 * 4 + 9) / 5 = 3.336
#          avg_loss = (1.88 * 4 + 0) / 5 = 1.504
#          RS = 3.336 / 1.504 = 2.218085, RSI = 68.9256
