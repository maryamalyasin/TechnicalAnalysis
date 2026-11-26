# AI Assistance Declaration: I used Muse (Muse Spark 1.3) for drafting the
# first version of this test file, for comments, and for ideas for checks.
# I verified outputs using hand calculation and before/after comparison in R.
# All final calculations are done by myself. I am responsible for the
# accuracy and originality of this work.
# Student: Maryam Alyasin
# Course: BDA400, Assignment 5
# Date: 26 November 2026
# Repository: TechnicalAnalysis / Assignment5

# Test file for Assignment 5 - Technical Analysis using R
# ------------------------------------------------------
# Runs every example from the instructions through all 9 indicator
# functions and compares the result with the expected value from the
# hand check (see Appendix_Hand_Check.md). Prints [PASS] or [FAIL]
# for each test, and "ALL TESTS PASSED" at the end only if every
# check passed.
#
# How to run: set the working directory to the Assignment5 folder
# (Session > Set Working Directory > To Source File Location),
# then run in the Console:
#   source("test_indicators.R")
#
# BASE R ONLY: no library() calls anywhere in this file or in the
# indicator files it sources.

# Source the indicator files in dependency order
# (macd needs ema; stoch_rsi needs rsi and sma)
source("sma.R")
source("ema.R")
source("stdev.R")
source("linreg.R")
source("rsi.R")
source("macd.R")
source("stoch_rsi.R")
source("crossover.R")
source("crossunder.R")

all_passed <- TRUE

# Print [PASS] or [FAIL] for one test and remember failures.
check <- function(name, passed) {
  if (isTRUE(passed)) {
    cat("[PASS]", name, "\n")
  } else {
    cat("[FAIL]", name, "\n")
    all_passed <<- FALSE
  }
}

# Compare two numeric vectors that may contain NA values:
# same length, NA in the same positions, and the non-NA entries
# within 'tolerance' of each other (largest absolute difference).
check_na_vector <- function(result, expected, tolerance = 1e-6) {
  if (length(result) != length(expected)) {
    return(FALSE)
  }
  if (!identical(is.na(result), is.na(expected))) {
    return(FALSE)
  }
  keep <- !is.na(expected)
  if (!any(keep)) {
    return(TRUE)
  }
  max(abs(result[keep] - expected[keep])) <= tolerance
}

# Run an expression and return its error message
# (or NA if the expression did not produce an error).
error_message <- function(expr) {
  tryCatch(
    {
      expr
      NA_character_
    },
    error = function(e) conditionMessage(e)
  )
}

cat("Assignment 5 - testing all 9 indicators\n")
cat("=======================================\n\n")

# --- 1. Simple Moving Average (sma) ---
data1 <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)
sma_expected <- c(12.333333, 15.666667, 17.666667, 20, 21.666667, 23.666667, 23.333333)
check("sma example (period 3)",
      isTRUE(all.equal(sma(data1, 3), sma_expected, tolerance = 1e-6)))
check("sma error when data is shorter than the period",
      identical(error_message(sma(c(1, 2), 3)),
                "Data length should be greater than or equal to the period"))

# --- 2. Exponential Moving Average (ema) ---
ema_expected <- c(10, 11, 13, 16.5, 17.25, 19.625, 22.3125, 23.15625, 22.078125)
check("ema example (period 3)",
      isTRUE(all.equal(ema(data1, 3), ema_expected, tolerance = 1e-6)))

# --- 3. MACD ---
data2 <- c(100, 105, 110, 115, 120, 125, 130)
macd_result <- macd(data2, short_period = 3, long_period = 5, signal_period = 2)
# Expected values are written to 7 decimals here (the files show the
# 6 decimals R prints) so the 1e-6 check below is comfortable.
check("macd line",
      isTRUE(all.equal(macd_result$macd_line,
                       c(0, 0.8333333, 1.8055556, 2.6620370, 3.3371914, 3.8393776, 4.2002100),
                       tolerance = 1e-6)))
check("macd signal line",
      isTRUE(all.equal(macd_result$signal_line,
                       c(0, 0.5555556, 1.3888889, 2.2376543, 2.9706790, 3.5498114, 3.9834105),
                       tolerance = 1e-6)))
check("macd histogram",
      isTRUE(all.equal(macd_result$histogram,
                       c(0, 0.2777778, 0.4166667, 0.4243827, 0.3665123, 0.2895662, 0.2167996),
                       tolerance = 1e-6)))

# --- 4. Standard deviation (stdev) ---
check("stdev example (population standard deviation)",
      isTRUE(all.equal(stdev(data1), 4.946629, tolerance = 1e-6)))

# --- 5. Linear regression (linreg) ---
linreg1 <- linreg(data1, 5, 0)
check("linreg slope (length 5, offset 0)",
      isTRUE(all.equal(linreg1$slope, 0.742857, tolerance = 1e-6)))
check("linreg intercept (length 5, offset 0)",
      isTRUE(all.equal(linreg1$intercept, 19.066667, tolerance = 1e-6)))
check("linreg predicted values (length 5, offset 0)",
      isTRUE(all.equal(linreg1$predicted_values,
                       c(19.809524, 20.552381, 21.295238, 22.038095, 22.780952, 23.52381),
                       tolerance = 1e-6)))
linreg2 <- linreg(data1, 5, 1)
check("linreg slope (length 5, offset 1)",
      isTRUE(all.equal(linreg2$slope, 2.1, tolerance = 1e-6)))
check("linreg intercept (length 5, offset 1)",
      isTRUE(all.equal(linreg2$intercept, 17, tolerance = 1e-6)))
check("linreg predicted values (length 5, offset 1)",
      isTRUE(all.equal(linreg2$predicted_values,
                       c(19.1, 21.2, 23.3, 25.4),
                       tolerance = 1e-6)))
linreg3 <- linreg(1:10, 5, 0)
check("linreg on 1:10 (slope 1, intercept 4)",
      isTRUE(all.equal(linreg3$slope, 1, tolerance = 1e-6)) &&
        isTRUE(all.equal(linreg3$intercept, 4, tolerance = 1e-6)) &&
        isTRUE(all.equal(linreg3$predicted_values, c(5, 6, 7, 8, 9, 10), tolerance = 1e-6)))
check("linreg error when length is too big",
      identical(error_message(linreg(data1, 20, 0)),
                "regressionLength cannot be greater than the number of elements in regressionSource"))
check("linreg error when offset is too big",
      identical(error_message(linreg(data1, 5, 5)),
                "regressionOffset must be less than regressionLength"))

# --- 6. Relative Strength Index (rsi) ---
data3 <- c(45, 50, 48, 55, 52, 49, 58, 60, 65, 62)
rsi_expected <- c(NA, NA, NA, NA, NA, 50.5263, 68.9256, 71.8352, 78.2108, 66.8597)
check("rsi example (period 5)",
      check_na_vector(rsi(data3, 5), rsi_expected, tolerance = 1e-3))

# --- 7. Stochastic RSI (stoch_rsi) ---
stoch_result <- stoch_rsi(data3, period = 5, k_period = 3, d_period = 3)
check("stoch_rsi %K line",
      check_na_vector(stoch_result$k_line,
                      c(NA, NA, NA, NA, NA, 0.478104, 0.811438, 0.786563),
                      tolerance = 1e-4))
check("stoch_rsi %D line",
      check_na_vector(stoch_result$d_line,
                      c(NA, NA, NA, NA, NA, 0.692035),
                      tolerance = 1e-4))

# --- 8. Crossover ---
arr1 <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)
arr2 <- c(18, 20, 22, 18, 15, 12, 10, 11, 13)
check("crossover example",
      identical(crossover(arr1, arr2),
                c("None", "None", "None", "Up", "None", "None", "None", "None", "None")))
check("crossover error on different lengths",
      identical(error_message(crossover(1:3, 1:4)),
                "Both arrays should have the same length"))

# --- 9. Crossunder ---
check("crossunder example",
      identical(crossunder(arr1, arr2),
                c("None", "False", "False", "False", "False", "False", "False", "False", "False")))
check("crossunder error on different lengths",
      identical(error_message(crossunder(1:3, 1:4)),
                "Both arrays should have the same length"))

# --- Summary ---
cat("\n")
if (all_passed) {
  cat("ALL TESTS PASSED\n")
} else {
  cat("SOME TESTS FAILED - see the [FAIL] lines above\n")
}
