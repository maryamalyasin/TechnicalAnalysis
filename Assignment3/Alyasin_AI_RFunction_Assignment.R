# AI Assistance Declaration: I used Muse (Muse Spark 1.3) for drafting the first
# version of this R function, for comments, and for ideas for tests.
# Prompts used: "Write an R function that removes outliers from a numeric vector.";
# "Revise the function to include comments, argument validation, and return a clean result.";
# "Explain what each line of the R code does and how you might test if it works correctly.";
# "Update the function to handle NA values.";
# "Fix this error in the R function: quantile() returns NA when the vector contains NA values.";
# "Optimize the function for efficiency and readability.";
# "How could you test this function with edge cases?";
# "Summarize how this R function changed across revisions."
# I verified outputs using hand calculation, an independent cross-check in a
# spreadsheet-style calculation, and comparison with boxplot.stats().
# All final calculations are done by myself. I am responsible for the accuracy
# and originality of this work.
# Student: Maryam Alyasin
# Course: BDA400 - Data Science Tools and Techniques, Assignment 3
# Date: 21 November 2026
# Repository: TechnicalAnalysis / Assignment3

# ---------------------------------------------------------------------------
# Original task description (one sentence, Step 1):
# "Remove outliers from a numeric vector."
#
# Method: the IQR rule. A value is an outlier when it is below
# Q1 - 1.5 * IQR or above Q3 + 1.5 * IQR, where IQR = Q3 - Q1.
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
# Version 1 - AI-generated function (as first received, before testing)
# Problem found by testing: it has no NA handling. If x contains NA,
# quantile() returns NA, so the lower and upper bounds become NA and the
# result is wrong. It also has no check that x is numeric.
# ---------------------------------------------------------------------------
remove_outliers_v1 <- function(x) {
  q1 <- quantile(x, 0.25)                # first quartile (no na.rm)
  q3 <- quantile(x, 0.75)                # third quartile (no na.rm)
  iqr_value <- q3 - q1                   # interquartile range
  lower_bound <- q1 - 1.5 * iqr_value    # lower fence
  upper_bound <- q3 + 1.5 * iqr_value    # upper fence
  x[x >= lower_bound & x <= upper_bound] # keep only values inside the fences
}

# ---------------------------------------------------------------------------
# Version 2 - Final, debugged function (human corrections added)
# Human corrections: (1) remove NA values before any calculation;
# (2) check that x is numeric and that k is a single positive number;
# (3) return numeric(0) for an empty vector instead of failing;
# (4) use names = FALSE in quantile() so the result is a plain number.
# ---------------------------------------------------------------------------
remove_outliers <- function(x, k = 1.5) {
  # Check that the input is numeric; stop with a clear message if it is not.
  if (!is.numeric(x)) {
    stop("x must be a numeric vector.")
  }
  # Check that k is one positive number (the fence multiplier, usually 1.5).
  if (!is.numeric(k) || length(k) != 1 || k <= 0) {
    stop("k must be a single positive number.")
  }
  # If the vector is empty, there is nothing to clean, so return an empty vector.
  if (length(x) == 0) {
    return(numeric(0))
  }
  # Remove missing values first, so quantile() never returns NA.
  x_no_na <- x[!is.na(x)]
  # If every value was NA, return an empty vector.
  if (length(x_no_na) == 0) {
    return(numeric(0))
  }
  # First quartile (25th percentile) of the values without NA.
  q1 <- quantile(x_no_na, 0.25, na.rm = TRUE, names = FALSE)
  # Third quartile (75th percentile) of the values without NA.
  q3 <- quantile(x_no_na, 0.75, na.rm = TRUE, names = FALSE)
  # Interquartile range: the distance between the third and first quartiles.
  iqr_value <- q3 - q1
  # Lower fence: values below this are outliers.
  lower_bound <- q1 - k * iqr_value
  # Upper fence: values above this are outliers.
  upper_bound <- q3 + k * iqr_value
  # Keep only the values inside the fences (outliers are removed).
  result <- x_no_na[x_no_na >= lower_bound & x_no_na <= upper_bound]
  # Return the cleaned vector.
  return(result)
}

# ---------------------------------------------------------------------------
# Step 2 - Testing in RStudio (run this part and take a screenshot)
# Expected results, calculated by hand before running (see hand-check file):
#   test_data = c(10, 15, 999, 20, 25)
#   Sorted: 10, 15, 20, 25, 999; Q1 = 15; median = 20; Q3 = 25; IQR = 10
#   Lower fence = 15 - 1.5 * 10 = 0; Upper fence = 25 + 1.5 * 10 = 40
#   Outlier: 999 only. Cleaned result: 10, 15, 20, 25
#   Mean before cleaning = 213.8; mean after cleaning = 17.5
#   boxplot.stats() should agree: $out = 999, $stats = 10, 15, 20, 25, 25
# ---------------------------------------------------------------------------

# Main test from the instruction sheet.
test_data <- c(10, 15, 999, 20, 25)
print(remove_outliers(test_data))

# Cross-check with the built-in R function boxplot.stats().
print(boxplot.stats(test_data))

# Test with a missing value: the first version failed here, the final one must not.
print(remove_outliers(c(10, 15, NA, 20, 25, 999)))

# Edge cases: all values equal (IQR = 0, nothing is an outlier).
print(remove_outliers(c(5, 5, 5, 5)))

# Edge case: a single value.
print(remove_outliers(c(100)))

# Edge case: no outlier at all.
print(remove_outliers(c(10, 15, 20, 25)))

# Show the effect on the mean, by hand: 213.8 before, 17.5 after.
cat("Mean before cleaning:", mean(test_data), "\n")
cat("Mean after cleaning:", mean(remove_outliers(test_data)), "\n")
