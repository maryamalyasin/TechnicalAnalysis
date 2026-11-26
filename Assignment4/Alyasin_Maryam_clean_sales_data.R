# AI Assistance Declaration: I used Muse (Muse Spark 1.3) for drafting the
# first version of this dplyr cleaning pipeline, for comments, and for ideas
# for checks.
# Prompts used:
#   Step 1 seed: "I have a sales dataset in R with missing customer IDs,
#     inconsistent region names, and outliers in revenue. What are common
#     data cleaning steps I should perform using dplyr?"
#   Step 1 refinement: "Here is a sample of my data (first 10 rows).
#     Suggest R code using dplyr to fix these issues."
#   Step 1 critique: "Please explain what each line of your suggested R code
#     does, and how I can confirm it worked correctly."
#   Step 2 seed: "This AI-generated code gives an error: 'object not found'.
#     How can I debug or rewrite it in R?"
#   Step 2 refinement: "Simplify this mutate() chain for readability."
#   Step 2 critique: "Does this cleaning pipeline follow tidyverse best
#     practices? Suggest improvements."
#   (The full list of all 9 prompts, including the 3 optional Step 3
#   prompts, is in Appendix_Prompts_and_Hand_Check.md.)
# I verified outputs using hand calculation, an independent cross-check in
#   Python, and before/after comparison in R (nrow, sum(is.na()),
#   summary(), table(), and a revenue check).
# All final calculations are done by myself. I am responsible for the
#   accuracy and originality of this work.
# Student: Maryam Alyasin
# Course: BDA400 - Data Science Tools and Techniques, Assignment 4
# Date: 24 November 2026
# Repository: TechnicalAnalysis / Assignment4

# =====================================================================
# STEP 0: SETUP - load dplyr (install it first if it is missing)
# =====================================================================
if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}
library(dplyr)

# =====================================================================
# STEP 1: LOAD THE DATA
# The file sales_data_dirty.csv must be in the working directory.
# Set the working directory first:
#   Session > Set Working Directory > To Source File Location
#
# IMPORTANT - na.strings = c("", "NA"):
# R's read.csv() treats a blank field as NA only in NUMERIC columns.
# In a CHARACTER column (Product here) a blank field stays as an empty
# string "", so is.na(Product) is FALSE for those rows and
# filter(!is.na(Product)) would remove nothing. The first run of this
# script (without na.strings) gave 353 clean rows instead of 327:
# exactly 26 rows too many - the 26 rows with a missing Product but a
# valid Quantity. Adding na.strings = c("", "NA") makes blank fields NA
# in every column, so the missing values are counted correctly
# (sum(is.na(sales)) = 144) and the pipeline gives 327 rows.
# (Found by checking the R run against the hand check, 25 Nov 2026.)
# =====================================================================
sales <- read.csv("sales_data_dirty.csv", na.strings = c("", "NA"))

# =====================================================================
# STEP 1 (continued): EXPLORE - describe the data quality issues
# Run each line and compare your Console output with the expected
# values written in the comments.
#
# DATA QUALITY ISSUES FOUND (6 issues):
#   1. Missing values: 144 cells are empty (Product 49, Quantity 95).
#      6 rows are missing BOTH Product and Quantity.
#   2. Duplicates: 9 exact duplicate rows, and 10 OrderIDs appear twice
#      (53, 215, 253, 265, 333, 409, 460, 484, 521, 587). OrderID 265 is
#      a conflicting duplicate: same OrderID, different Quantity
#      (1000 in row 265 and 25 in row 608).
#   3. Invalid quantities: Quantity is -5 in 37 rows and 0 in 44 rows
#      (81 rows with Quantity <= 0). A sale cannot have 0 or -5 items.
#   4. Outliers: Quantity 100 appears 55 times and Quantity 1000 appears
#      20 times. The IQR upper fence is 58.75, so 100 and 1000 are
#      outliers. Typical Quantity values are 2-30.
#   5. Inconsistent text casing: all 8 Customer names are lowercase
#      (bob, frank, david, eve, alice, georgia, hannah, charlie).
#      Product names are already consistent Title Case, so they need
#      no fixing.
#   6. Suspicious repeated maximum price: Price 300 appears 10 times.
#      I checked it with the IQR rule (Price upper fence is 311.76),
#      and 300 is NOT an outlier, so it is kept, not removed.
# =====================================================================

nrow(sales)          # 610 rows
ncol(sales)          # 5 columns
names(sales)         # "OrderID" "Product" "Quantity" "Price" "Customer"
glimpse(sales)       # 610 rows x 5 columns; OrderID <int>, Product <chr>,
                     # Quantity <int>, Price <dbl>, Customer <chr>

summary(sales$Quantity)
# Expected: Min -5, 1st Qu 2.5, Median 15, Mean 58.93,
#           3rd Qu 25, Max 1000, NA's 95

summary(sales$Price)
# Expected: Min 5.9, 1st Qu 50.09, Median 102.23, Mean 104.90,
#           3rd Qu 154.76, Max 300, NA's 0
# NOTE: R's summary() shows only 4 significant digits, so the Console
# shows Price Q3 as 154.8 instead of 154.76. The exact value is 154.76.

table(sales$Product)
# Expected: Gadget 184, Thingamajig 183, Widget 194

table(sales$Customer)
# Expected: alice 76, bob 90, charlie 65, david 80,
#           eve 79, frank 81, georgia 72, hannah 67

sum(is.na(sales))        # 144 missing cells in total
colSums(is.na(sales))    # OrderID 0, Product 49, Quantity 95, Price 0, Customer 0
sum(is.na(sales$Product) & is.na(sales$Quantity))   # 6 rows missing both
sum(duplicated(sales))   # 9 exact duplicate rows

# OrderIDs that appear more than once (the second occurrence of each):
sales$OrderID[duplicated(sales$OrderID)]
# Expected: 333 253 215 521 460 409 53 265 484 587

# The conflicting duplicate, OrderID 265, side by side:
sales[sales$OrderID == 265, ]
# Expected: row 265 is (Gadget, Quantity 1000, Price 158.34, hannah)
#           row 608 is (Gadget, Quantity 25,   Price 158.34, hannah)

# =====================================================================
# IQR FENCE FOR QUANTITY
# Calculated on the ORIGINAL data (before any filtering), so it matches
# the exploration summary above. Q1 = 2.5, Q3 = 25, IQR = 22.5.
# (If the fence were calculated after the other filters it would be
# 67.5 instead of 58.75, but the final result would be the same,
# because both fences exclude 100 and 1000 and keep 30.)
# =====================================================================
q1_quantity <- quantile(sales$Quantity, 0.25, na.rm = TRUE)   # 2.5
q3_quantity <- quantile(sales$Quantity, 0.75, na.rm = TRUE)   # 25
iqr_quantity <- q3_quantity - q1_quantity                     # 22.5
upper_fence <- q3_quantity + 1.5 * iqr_quantity               # 58.75

# =====================================================================
# STEP 2: THE AI'S FIRST SUGGESTION (version 1) - IT FAILS. READ WHY.
# The AI assumed columns that do not exist in this dataset (CustomerID
# and Region) and used str_to_title() without loading the stringr
# package. This block is wrapped in tryCatch() so the error prints
# but the script keeps running.
# =====================================================================
ai_first_version <- tryCatch({
  sales %>%
    distinct() %>%
    filter(!is.na(CustomerID)) %>%                # ERROR: no CustomerID column
    mutate(Region = str_to_title(Region)) %>%     # ERROR: no Region column,
                                                  # and str_to_title() needs stringr
    mutate(Customer = str_to_title(Customer)) %>%
    filter(Quantity > 0)
}, error = function(e) {
  message("AI first version FAILED: ", conditionMessage(e))
  NULL
})
# Expected error message: object 'CustomerID' not found
# The real dataset has Customer names (Customer column) and no Region
# column at all, so the AI's code cannot work. The fix is in the final
# pipeline below: use the real column names, and use
# tools::toTitleCase() (base R, no extra package needed) instead of
# str_to_title().

# =====================================================================
# STEP 2 (continued): FINAL DEBUGGED PIPELINE - 610 rows -> 327 rows
# Each step says what it does and how many rows it removes.
# =====================================================================
clean <- sales %>%
  # 1. Remove exact duplicate rows: 610 -> 601 (9 rows removed)
  distinct() %>%
  # 2. Keep the first row for each OrderID: 601 -> 600 (1 row removed)
  #    This removes the second OrderID 265 row (Gadget, Quantity 25).
  #    NOTE: the kept first 265 row has Quantity 1000, so it is removed
  #    later as an outlier in step 6 - OrderID 265 is not in the
  #    clean data at all.
  distinct(OrderID, .keep_all = TRUE) %>%
  # 3. Remove rows with a missing Product: 600 -> 552 (48 rows removed)
  #    (49 rows had a missing Product, but 1 of them was already
  #    removed as an exact duplicate in step 1.)
  filter(!is.na(Product)) %>%
  # 4. Remove rows with a missing Quantity: 552 -> 464 (88 rows removed)
  #    (95 rows had a missing Quantity, but 7 of them were already
  #    removed in steps 1 and 3.)
  filter(!is.na(Quantity)) %>%
  # 5. Remove invalid quantities (0 or negative): 464 -> 397 (67 rows removed)
  #    Quantity -5 appears 37 times and 0 appears 44 times (81 rows with
  #    Quantity <= 0), but 14 of them were already removed above, so
  #    this step removes 67 rows.
  filter(Quantity > 0) %>%
  # 6. Remove Quantity outliers above the IQR fence (58.75):
  #    397 -> 327 (70 rows removed: 51 rows with Quantity 100 and
  #    19 rows with Quantity 1000. 5 more rows with Quantity 100 or
  #    1000 were already removed above.)
  filter(Quantity <= upper_fence) %>%
  # 7. Standardise Customer names to Title Case (bob -> Bob):
  #    no rows removed (327 rows)
  mutate(Customer = tools::toTitleCase(Customer))

# =====================================================================
# VERIFICATION - compare BEFORE and AFTER (run each line and check)
# =====================================================================

# Rows
nrow(sales)              # 610 before
nrow(clean)              # 327 after (283 rows removed in total)

# Missing values
sum(is.na(sales))        # 144 before
sum(is.na(clean))        # 0 after

# Quantity summary
summary(sales$Quantity)  # Min -5, 1st Qu 2.5, Median 15, Mean 58.93,
                         # 3rd Qu 25, Max 1000, NA's 95
summary(clean$Quantity)  # Min 2, 1st Qu 5, Median 15, Mean 14.20,
                         # 3rd Qu 25, Max 30, NA's 0

# Price summary
summary(sales$Price)     # Min 5.9, 1st Qu 50.09, Median 102.23, Mean 104.90,
                         # 3rd Qu 154.76, Max 300, NA's 0
                         # (Console shows Q3 as 154.8 - 4 significant digits)
summary(clean$Price)     # Min 5.9, 1st Qu 50.36, Median 113.06, Mean 107.55,
                         # 3rd Qu 157.05, Max 300, NA's 0
                         # (Console shows Q3 as 157 - 4 significant digits)

# Product and Customer tables after cleaning
table(clean$Product)     # Gadget 105, Thingamajig 105, Widget 117
table(clean$Customer)    # Alice 46, Bob 47, Charlie 38, David 38, Eve 54,
                         # Frank 35, Georgia 36, Hannah 33

# One OrderID per row, no duplicates left
length(unique(clean$OrderID))   # 327

# Price 300 is kept on purpose: it is the maximum price, repeated 10 times
# before cleaning (2 times after), but it is NOT an IQR outlier, because
# the Price upper fence is 311.76 and 300 is below it.
sum(sales$Price == 300)   # 10 before
sum(clean$Price == 300)   # 2 after

# Revenue check (sanity check)
# Revenue of the valid rows BEFORE removing duplicates:
valid_before_dedup <- sales %>%
  filter(!is.na(Product), !is.na(Quantity),
         Quantity > 0, Quantity <= upper_fence)
sum(valid_before_dedup$Quantity * valid_before_dedup$Price)   # 505088.17
# Revenue AFTER cleaning:
sum(clean$Quantity * clean$Price)                             # 493325.53
# Difference: 505088.17 - 493325.53 = 11762.64. That is exactly the
# revenue of the 6 valid duplicate rows removed in steps 1-2
# (OrderIDs 333, 215, 521, 53, 484 and 265).

# =====================================================================
# SAVE the cleaned dataset
# =====================================================================
write.csv(clean, "sales_data_clean.csv", row.names = FALSE)

# Done! The file sales_data_clean.csv has 327 rows and no missing values.
