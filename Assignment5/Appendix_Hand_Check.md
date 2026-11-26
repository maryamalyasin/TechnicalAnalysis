# Appendix — Hand Check

## BDA400 Assignment 5 — Technical Analysis using R, Development Phase

Student: Maryam Alyasin — 26 November 2026
Repository: TechnicalAnalysis / Assignment5

Every expected value below was worked out by hand from the pseudocode in
the instructions, then verified by an independent Python cross-check that
recomputed each value from the same pseudocode (not from the R code).
The R test file `test_indicators.R` compares the R output with these
values: `isTRUE(all.equal(..., tolerance = 1e-6))` for the plain numeric
results, `identical()` for the crossover/crossunder character vectors,
and an NA-aware comparison (same length, NA in the same positions,
non-NA entries within tolerance) for RSI and Stochastic RSI.

---

## 1. sma(data, period)

Example data: `c(10, 12, 15, 20, 18, 22, 25, 24, 21)`, period 3.
The result has 9 − 3 + 1 = 7 values, one per window.

| Window | Calculation | SMA |
|--------|-------------|----:|
| 1 | (10 + 12 + 15) / 3 = 37 / 3 | 12.333333 |
| 2 | (12 + 15 + 20) / 3 = 47 / 3 | 15.666667 |
| 3 | (15 + 20 + 18) / 3 = 53 / 3 | 17.666667 |
| 4 | (20 + 18 + 22) / 3 = 60 / 3 | 20 |
| 5 | (18 + 22 + 25) / 3 = 65 / 3 | 21.666667 |
| 6 | (22 + 25 + 24) / 3 = 71 / 3 | 23.666667 |
| 7 | (25 + 24 + 21) / 3 = 70 / 3 | 23.333333 |

Expected: `c(12.333333, 15.666667, 17.666667, 20, 21.666667, 23.666667, 23.333333)`.
Error if the data is shorter than the period:
"Data length should be greater than or equal to the period".

---

## 2. ema(data, period)

Same data, period 3. Multiplier = 2 / (3 + 1) = 0.5. EMA(1) = data(1).

| i | Calculation | EMA |
|---|-------------|----:|
| 1 | = 10 | 10 |
| 2 | (12 − 10) × 0.5 + 10 | 11 |
| 3 | (15 − 11) × 0.5 + 11 | 13 |
| 4 | (20 − 13) × 0.5 + 13 | 16.5 |
| 5 | (18 − 16.5) × 0.5 + 16.5 | 17.25 |
| 6 | (22 − 17.25) × 0.5 + 17.25 | 19.625 |
| 7 | (25 − 19.625) × 0.5 + 19.625 | 22.3125 |
| 8 | (24 − 22.3125) × 0.5 + 22.3125 | 23.15625 |
| 9 | (21 − 23.15625) × 0.5 + 23.15625 | 22.078125 |

Expected: `c(10, 11, 13, 16.5, 17.25, 19.625, 22.3125, 23.15625, 22.078125)`.
The result has the same length as the data (9 values).

---

## 3. macd(data, short_period, long_period, signal_period)

Example data: `c(100, 105, 110, 115, 120, 125, 130)`,
short_period = 3, long_period = 5, signal_period = 2.

Short EMA (period 3, multiplier 0.5):
100, 102.5, 106.25, 110.625, 115.3125, 120.15625, 125.078125.

Long EMA (period 5, multiplier 2/6 = 1/3):
100, 101.666667, 104.444444, 107.962963, 111.975309, 116.316872, 120.877915.

MACD line = short EMA − long EMA:
`c(0, 0.833333, 1.805556, 2.662037, 3.337191, 3.839378, 4.20021)`.
First values by hand: 100 − 100 = 0; 102.5 − 101.666667 = 0.833333;
106.25 − 104.444444 = 1.805556.

Signal line = EMA of the MACD line (period 2, multiplier 2/3):
`c(0, 0.555556, 1.388889, 2.237654, 2.970679, 3.549811, 3.98341)`.
First values by hand: 0; (0.833333 − 0) × 2/3 + 0 = 0.555556;
(1.805556 − 0.555556) × 2/3 + 0.555556 = 1.388889.

Histogram = MACD line − signal line:
`c(0, 0.277778, 0.416667, 0.424383, 0.366512, 0.289566, 0.2168)`.

---

## 4. stdev(data) — POPULATION standard deviation

Same data as sma/ema: `c(10, 12, 15, 20, 18, 22, 25, 24, 21)`.

- Mean = 167 / 9 = 18.555556.
- Squared differences: 73.197531, 42.975309, 12.641975, 2.086420,
  0.308642, 11.864198, 41.530864, 29.641975, 5.975309.
  (First value by hand: (10 − 18.555556)² = (−8.555556)² = 73.197531.)
- Sum of squared differences = 220.222222.
- Variance = 220.222222 / 9 = 24.469136 (divided by **n**, not n − 1).
- Standard deviation = √24.469136 = **4.946629**.

**Population vs sample — read this before using R's `sd()`:**
R's built-in `sd(data)` divides by n − 1 = 8 instead of 9, giving
`sd(data)` = **5.246692**. That is the *sample* standard deviation.
The instructions' formula divides by n, so the answer here is the
*population* standard deviation, 4.946629. `stdev()` therefore never
calls `sd()`; it computes the variance by hand from the pseudocode.

---

## 5. linreg(regressionSource, regressionLength, regressionOffset)

### The window — read this first

As the pseudocode is written, `start_index = max(1, n − regressionLength
+ regressionOffset)` and `end_index = min(n, n − regressionOffset)`.
With `regressionOffset = 0` the window holds **regressionLength + 1**
points, not regressionLength points. This is NOT fixed in the code: the
pseudocode is followed as written, and each test below shows the actual
subset used.

### Case 1: linreg(c(10, 12, 15, 20, 18, 22, 25, 24, 21), 5, 0)

n = 9. start = max(1, 9 − 5 + 0) = 4. end = min(9, 9 − 0) = 9.
Subset = elements 4–9 = `c(20, 18, 22, 25, 24, 21)` (six points).
Index values = 1, 2, 3, 4, 5, 6. Mean of index = 3.5.
Mean of subset = 130 / 6 = 21.666667.
Numerator = Σ(index − 3.5)(subset − 21.666667) = 13.
Denominator = Σ(index − 3.5)² = 17.5.
Slope = 13 / 17.5 = **0.742857**.
Intercept = 21.666667 − 0.742857 × 3.5 = **19.066667**.
Predicted = 0.742857 × index + 19.066667 =
`c(19.809524, 20.552381, 21.295238, 22.038095, 22.780952, 23.52381)`.
First predicted value by hand: 0.742857 × 1 + 19.066667 = 19.809524.

### Case 2: linreg(same data, 5, 1)

start = max(1, 9 − 5 + 1) = 5. end = min(9, 9 − 1) = 8.
Subset = elements 5–8 = `c(18, 22, 25, 24)` (four points).
Mean of index (1–4) = 2.5. Mean of subset = 89 / 4 = 22.25.
Numerator = 10.5. Denominator = 5.
Slope = 10.5 / 5 = **2.1**. Intercept = 22.25 − 2.1 × 2.5 = **17**.
Predicted = `c(19.1, 21.2, 23.3, 25.4)`.

### Case 3: linreg(1:10, 5, 0)

start = max(1, 10 − 5 + 0) = 5. end = min(10, 10) = 10.
Subset = `c(5, 6, 7, 8, 9, 10)` (six points).
Slope = **1**, intercept = **4**, predicted = `c(5, 6, 7, 8, 9, 10)`.

### Error cases

- `linreg(data, 20, 0)` → error: "regressionLength cannot be greater
  than the number of elements in regressionSource" (20 > 9).
- `linreg(data, 5, 5)` → error: "regressionOffset must be less than
  regressionLength" (5 >= 5).

---

## 6. rsi(data, period)

Example data: `c(45, 50, 48, 55, 52, 49, 58, 60, 65, 62)`, period 5.

- Differences: 5, −2, 7, −3, −3, 9, 2, 5, −3.
- Gains: 5, 0, 7, 0, 0, 9, 2, 5, 0.
- Losses: 0, 2, 0, 3, 3, 0, 0, 0, 3.
- Average gain of the first 5 = (5 + 0 + 7 + 0 + 0) / 5 = **2.4**.
- Average loss of the first 5 = (0 + 2 + 0 + 3 + 3) / 5 = **1.6**.

Wilder's smoothing, first two values by hand:

- i = 6: avg_gain = (2.4 × 4 + 0) / 5 = 1.92;
  avg_loss = (1.6 × 4 + 3) / 5 = 1.88;
  RS = 1.92 / 1.88 = 1.021277;
  RSI = 100 − 100 / (1 + 1.021277) = **50.5263**.
- i = 7: avg_gain = (1.92 × 4 + 9) / 5 = 3.336;
  avg_loss = (1.88 × 4 + 0) / 5 = 1.504;
  RS = 3.336 / 1.504 = 2.218085;
  RSI = 100 − 100 / (1 + 2.218085) = **68.9256**.

Expected: `c(NA, NA, NA, NA, NA, 50.5263, 68.9256, 71.8352, 78.2108, 66.8597)`.
The first 5 values are NA: there is no RSI value until 5 differences
have been seen. If the average loss ever reaches 0, RS is infinite and
the RSI is set to 100 (a guard in the code, with a comment).

---

## 7. stoch_rsi(data, period, k_period, d_period)

Same 10 points, period = 5, k_period = 3, d_period = 3.

- RSI values (period 5): NA, NA, NA, NA, NA, 50.5263, 68.9256, 71.8352,
  78.2108, 66.8597.
- min RSI = 50.5263, max RSI = 78.2108.
- k_values = (RSI − min) / (max − min), non-NA part:
  0, 0.664607, 0.769705, 1, 0.589982.
  First value by hand: (68.9256 − 50.5263) / (78.2108 − 50.5263) = 0.664607.
- k_line = SMA of k_values with period 3:
  (0 + 0.664607 + 0.769705) / 3 = **0.478104**;
  (0.664607 + 0.769705 + 1) / 3 = **0.811438**;
  (0.769705 + 1 + 0.589982) / 3 = **0.786563**.
  Full k_line: `c(NA, NA, NA, NA, NA, 0.478104, 0.811438, 0.786563)`.
- d_line = SMA of k_line with period 3:
  (0.478104 + 0.811438 + 0.786563) / 3 = **0.692035**.
  Full d_line: `c(NA, NA, NA, NA, NA, 0.692035)`.

**Why min/max need `na.rm = TRUE`:** the first 5 RSI values are NA.
In R, `min()` / `max()` of a vector containing NA returns NA, which
would make every k_value NA. With `na.rm = TRUE` the minimum and
maximum are taken over the real RSI values only.

**Why the test does NOT use the PDF's own example (period = 14):**
the PDF shows `stoch_rsi(data, period = 14, k_period = 3, d_period = 3)`
on these same 10 points. That cannot produce values: `rsi()` needs more
than 14 points before its first RSI value appears (there are only 9
differences in 10 points), so every RSI value would be NA. The test
therefore uses period = 5 on this data — the same data the RSI
example uses.

---

## 8. crossover(arr1, arr2)

arr1 = `c(10, 12, 15, 20, 18, 22, 25, 24, 21)`,
arr2 = `c(18, 20, 22, 18, 15, 12, 10, 11, 13)`.

| i | arr1[i] vs arr2[i] | arr1[i−1] vs arr2[i−1] | Signal |
|---|-------------------:|-----------------------:|--------|
| 1 | — | — (no previous point) | None |
| 2 | 12 < 20 | 10 >= 18? No | None |
| 3 | 15 < 22 | 12 >= 20? No | None |
| 4 | 20 > 18 | 15 <= 22? Yes | **Up** |
| 5 | 18 > 15 | 20 <= 18? No | None |
| 6 | 22 > 12 | 18 <= 15? No | None |
| 7 | 25 > 10 | 22 <= 12? No | None |
| 8 | 24 > 11 | 25 <= 10? No | None |
| 9 | 21 > 13 | 24 <= 11? No | None |

Expected: `c("None", "None", "None", "Up", "None", "None", "None", "None", "None")`.
Only one crossover happens: at position 4, arr1 moves from below arr2
(15 < 22) to above it (20 > 18).
Error if the arrays have different lengths: "Both arrays should have
the same length".

**Return-type note:** the description text in the instructions says the
output is Boolean True/False, but the pseudocode returns the strings
"Up", "Down" and "None". The pseudocode is followed.

---

## 9. crossunder(arr1, arr2)

Same two arrays. A crossunder needs arr1[i] < arr2[i] while
arr1[i−1] >= arr2[i−1]:

| i | arr1[i] < arr2[i]? | arr1[i−1] >= arr2[i−1]? | Signal |
|---|-------------------:|------------------------:|--------|
| 1 | — | — (no previous point) | None |
| 2 | 12 < 20: Yes | 10 >= 18? No | False |
| 3 | 15 < 22: Yes | 12 >= 20? No | False |
| 4 | 20 < 18? No | — | False |
| 5 | 18 < 15? No | — | False |
| 6 | 22 < 12? No | — | False |
| 7 | 25 < 10? No | — | False |
| 8 | 24 < 11? No | — | False |
| 9 | 21 < 13? No | — | False |

Expected: `c("None", "False", "False", "False", "False", "False", "False", "False", "False")`.
No crossunder happens in this example: arr1 never moves from above
arr2 to below it (the only crossing is upwards, at position 4).
Error if the arrays have different lengths: "Both arrays should have
the same length".

**Return-type note:** the pseudocode returns the strings "True" and
"False" here (and "None" for the first point) — unlike `crossover()`,
which returns "Up" / "Down" / "None". The pseudocode is followed as
written, even though the two functions use different wording.

---

## Verification

- Every expected value above was recomputed by an independent Python
  cross-check written directly from the pseudocode (not from the R
  code). All values matched, including the MACD line/signal/histogram
  to 6 decimals, the RSI values to 4 decimals, and the StochRSI
  %K/%D lines to 6 decimals.
- `test_indicators.R` runs all of the examples above in R and prints
  [PASS] / [FAIL] per test, and "ALL TESTS PASSED" only if every
  check passed. It also checks the error messages for sma, linreg
  (both cases), crossover and crossunder.
- Base R only: no `library()` / `require()` call exists in any file.
  `sd()` is never used (see the stdev note above).
