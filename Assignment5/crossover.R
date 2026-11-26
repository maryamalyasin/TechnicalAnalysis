# AI Assistance Declaration: I used Muse (Muse Spark 1.3) for drafting the
# first version of this function, for comments, and for ideas for checks.
# I verified outputs using hand calculation and before/after comparison in R.
# All final calculations are done by myself. I am responsible for the
# accuracy and originality of this work.
# Student: Maryam Alyasin
# Course: BDA400, Assignment 5
# Date: 26 November 2026
# Repository: TechnicalAnalysis / Assignment5

# Crossover function
# ------------------
# Compares two arrays (for example two moving averages) and marks the
# points where the first array crosses OVER the second one (from below
# to above) as "Up", where it crosses back under as "Down", and every
# other point as "None". A crossover from below to above is an uptrend
# signal.
#
# Implemented in BASE R ONLY, following the template and pseudocode in the
# Assignment 5 instructions. No library functions are used.
#
# NOTE ON THE RETURN TYPE: the description text in the instructions says
# the output is Boolean True/False, but the pseudocode returns the
# strings "Up", "Down" and "None". The pseudocode is followed here.

crossover <- function(arr1, arr2) {
  # Check if the length of both arrays is the same
  if (length(arr1) != length(arr2)) {
    stop("Both arrays should have the same length")
  }

  # Initialize a vector to store the crossover signals
  crossover_signals <- rep("None", length(arr1))

  # The first point has no previous point to compare with
  crossover_signals[1] <- "None"

  # Check for crossovers at each data point.
  # NOTE (R only): the pseudocode loop "for i = 2 to length of arr1" is
  # empty for a one-element array. In R, 2:1 would count backwards, so
  # the loop is guarded: it only runs when there are at least 2 points.
  # This does not change any result the pseudocode defines.
  if (length(arr1) >= 2) {
    for (i in 2:length(arr1)) {
      if (arr1[i] > arr2[i] && arr1[i - 1] <= arr2[i - 1]) {
        crossover_signals[i] <- "Up"
      } else if (arr1[i] < arr2[i] && arr1[i - 1] >= arr2[i - 1]) {
        crossover_signals[i] <- "Down"
      } else {
        crossover_signals[i] <- "None"
      }
    }
  }

  return(crossover_signals)
}

# Example from the instructions:
# arr1 <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)
# arr2 <- c(18, 20, 22, 18, 15, 12, 10, 11, 13)
# crossover_signals <- crossover(arr1, arr2)
# print(crossover_signals)
# Expected output:
# [1] "None" "None" "None" "Up"   "None" "None" "None" "None" "None"
#
# Hand check:
#   i = 2: 12 > 20? No.  12 < 20 but 10 >= 18? No.  -> "None"
#   i = 3: 15 > 22? No.  15 < 22 but 12 >= 20? No.  -> "None"
#   i = 4: 20 > 18 and 15 <= 22? Yes.              -> "Up"
#   i = 5: 18 > 15 but 20 <= 18? No.  18 < 15? No.  -> "None"
#   i = 6: 22 > 12 but 18 <= 15? No.  22 < 12? No.  -> "None"
#   i = 7: 25 > 10 but 22 <= 12? No.  25 < 10? No.  -> "None"
#   i = 8: 24 > 11 but 25 <= 10? No.  24 < 11? No.  -> "None"
#   i = 9: 21 > 13 but 24 <= 11? No.  21 < 13? No.  -> "None"
# Only one crossover happens: at position 4, arr1 moves from below
# arr2 (15 < 22) to above it (20 > 18).
