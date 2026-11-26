# AI Assistance Declaration: I used Muse (Muse Spark 1.3) for drafting the
# first version of this function, for comments, and for ideas for checks.
# I verified outputs using hand calculation and before/after comparison in R.
# All final calculations are done by myself. I am responsible for the
# accuracy and originality of this work.
# Student: Maryam Alyasin
# Course: BDA400, Assignment 5
# Date: 26 November 2026
# Repository: TechnicalAnalysis / Assignment5

# Crossunder function
# -------------------
# Compares two arrays and marks the points where the first array crosses
# UNDER the second one (from above to below) as "True", and every other
# point as "False". The first point is "None" (no previous point to
# compare with). A crossunder can signal a trend reversal.
#
# Implemented in BASE R ONLY, following the template and pseudocode in the
# Assignment 5 instructions. No library functions are used.
#
# NOTE ON THE RETURN TYPE: the pseudocode returns the strings "True"
# and "False" here (and "None" for the first point) — unlike
# crossover(), which returns "Up" / "Down" / "None". The pseudocode is
# followed as written, even though the two functions use different
# wording for their signals.

crossunder <- function(arr1, arr2) {
  # Check if the length of both arrays is the same
  if (length(arr1) != length(arr2)) {
    stop("Both arrays should have the same length")
  }

  # Initialize a vector to store the crossunder signals
  crossunder_signals <- rep("None", length(arr1))

  # The first point has no previous point to compare with
  crossunder_signals[1] <- "None"

  # Check for crossunder signals at each data point.
  # NOTE (R only): the pseudocode loop "for i = 2 to length of arr1" is
  # empty for a one-element array. In R, 2:1 would count backwards, so
  # the loop is guarded: it only runs when there are at least 2 points.
  # This does not change any result the pseudocode defines.
  if (length(arr1) >= 2) {
    for (i in 2:length(arr1)) {
      if (arr1[i] < arr2[i] && arr1[i - 1] >= arr2[i - 1]) {
        crossunder_signals[i] <- "True"
      } else {
        crossunder_signals[i] <- "False"
      }
    }
  }

  return(crossunder_signals)
}

# Example from the instructions:
# arr1 <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)
# arr2 <- c(18, 20, 22, 18, 15, 12, 10, 11, 13)
# crossunder_signals <- crossunder(arr1, arr2)
# print(crossunder_signals)
# Expected output:
# [1] "None"  "False" "False" "False" "False" "False" "False" "False" "False"
#
# Hand check:
#   i = 2: 12 < 20 but 10 >= 18? No.  -> "False"
#   i = 3: 15 < 22 but 12 >= 20? No.  -> "False"
#   i = 4: 20 < 18? No.               -> "False"
#   i = 5: 18 < 15? No.               -> "False"
#   i = 6: 22 < 12? No.               -> "False"
#   i = 7: 25 < 10? No.               -> "False"
#   i = 8: 24 < 11? No.               -> "False"
#   i = 9: 21 < 13? No.               -> "False"
# No crossunder happens in this example: arr1 never moves from above
# arr2 to below it (the only crossing is upwards, at position 4).
