# AI Assistance Declaration: I used Muse (Muse Spark 1.3) for drafting the
# first version of this function, for comments, and for ideas for checks.
# I verified outputs using hand calculation and before/after comparison in R.
# All final calculations are done by myself. I am responsible for the
# accuracy and originality of this work.
# Student: Maryam Alyasin
# Course: BDA400, Assignment 5
# Date: 26 November 2026
# Repository: TechnicalAnalysis / Assignment5

# Moving Average Convergence Divergence (MACD)
# --------------------------------------------
# MACD = short-term EMA - long-term EMA. A signal line (an EMA of the
# MACD line) and a histogram (MACD line - signal line) complete it.
# Positive MACD values suggest bullish momentum, negative values bearish.
#
# Implemented in BASE R ONLY, following the template and pseudocode in the
# Assignment 5 instructions. No library functions are used.
#
# NOTE: this file uses ema(), which is defined in ema.R.
# Source ema.R first: source("ema.R")

macd <- function(data, short_period, long_period, signal_period) {
  # Calculate the short-term and long-term exponential moving averages (EMA)
  short_ema <- ema(data, short_period)
  long_ema <- ema(data, long_period)

  # Calculate the MACD line (the difference between short_ema and long_ema)
  macd_line <- short_ema - long_ema

  # Calculate the signal line (EMA of the MACD line)
  signal_line <- ema(macd_line, signal_period)

  # Calculate the histogram (the difference between the MACD line and the signal line)
  histogram <- macd_line - signal_line

  # Return the MACD line, signal line, and histogram as a list
  result <- list(macd_line = macd_line,
                 signal_line = signal_line,
                 histogram = histogram)
  return(result)
}

# Example from the instructions:
# data <- c(100, 105, 110, 115, 120, 125, 130)
# macd_result <- macd(data, short_period = 3, long_period = 5, signal_period = 2)
# print(macd_result)
# Expected output:
# $macd_line
# [1] 0.000000 0.833333 1.805556 2.662037 3.337191 3.839378 4.200210
# $signal_line
# [1] 0.000000 0.555556 1.388889 2.237654 2.970679 3.549811 3.983410
# $histogram
# [1] 0.000000 0.277778 0.416667 0.424383 0.366512 0.289566 0.216800
#
# Hand check of the first values:
#   short EMA (period 3, multiplier 0.5): 100, 102.5, 106.25, ...
#   long EMA (period 5, multiplier 1/3): 100, 101.666667, 104.444444, ...
#   MACD line: 100 - 100 = 0; 102.5 - 101.666667 = 0.833333;
#              106.25 - 104.444444 = 1.805556
#   Signal line (EMA of the MACD line, period 2, multiplier 2/3):
#              0; (0.833333 - 0) * 2/3 + 0 = 0.555556;
#              (1.805556 - 0.555556) * 2/3 + 0.555556 = 1.388889
