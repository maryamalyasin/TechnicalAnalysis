# BDA400 Assignment 2 - Technical Analysis using R, Preliminary Stage
# Student: Maryam Alyasin
# Repository: TechnicalAnalysis / Assignment2
#
# How to use (in RStudio, one step at a time):
#   1. Install the packages first:
#        install.packages(c("quantmod", "TTR"))
#   2. Put this file and portfolio.txt in the same folder, and set that
#      folder as the working directory in RStudio
#      (Session > Set Working Directory > To Source File Location).
#   3. Run the whole file (Source button), or run it line by line.
#   4. Take a screenshot of the Console output for the report.

library(quantmod)
library(TTR)

# ---------------------------------------------------------------------------
# Helper: mode (the most frequent value).
# Base R has no mode() for this, so we build it from table().
# Note: closing prices rarely repeat, so the mode is the value that
# appears most often in the data (often a price that occurred twice).
# ---------------------------------------------------------------------------
get_mode <- function(x) {
  x <- x[!is.na(x)]
  freq <- table(x)
  as.numeric(names(freq)[which.max(freq)])
}

# ---------------------------------------------------------------------------
# 1. Function to import and load stock data
#    Reads portfolio.txt (one symbol per line) and downloads each stock
#    from Yahoo Finance with quantmod::getSymbols().
#    Returns a named list, one data frame per stock, and also creates
#    one separate data frame per symbol in the global environment
#    (for example, a data frame called AAPL).
# ---------------------------------------------------------------------------
load_stock_data <- function(file = "portfolio.txt",
                            from = "2025-01-01",
                            to = Sys.Date()) {
  symbols <- readLines(file, warn = FALSE)
  symbols <- trimws(symbols)
  symbols <- symbols[symbols != ""]

  stock_list <- list()

  for (symbol in symbols) {
    stock_xts <- getSymbols(symbol,
                            src = "yahoo",
                            from = from,
                            to = to,
                            auto.assign = FALSE)
    stock_df <- as.data.frame(stock_xts)
    stock_df$Date <- as.Date(rownames(stock_df))
    rownames(stock_df) <- NULL

    # Remove rows with missing values: Yahoo data can contain gaps,
    # and the moving average cannot run over them (TTR::SMA fails with
    # "Series contains non-leading NAS"). Dropping incomplete rows is
    # the standard fix suggested by the quantmod warning itself.
    stock_df <- stock_df[complete.cases(stock_df), ]

    stock_list[[symbol]] <- stock_df
    assign(symbol, stock_df, envir = .GlobalEnv)
  }

  return(stock_list)
}

# ---------------------------------------------------------------------------
# 2. Function to compute basic statistics
#    Input: one stock's data frame (from load_stock_data()).
#    Output: a data frame with the moving average (20-day SMA),
#    mean, mode, median and standard deviation of the Close price.
# ---------------------------------------------------------------------------
calculate_statistics <- function(stock_df) {
  close_col <- grep("Close", names(stock_df), value = TRUE)[1]
  close_prices <- stock_df[[close_col]]

  # Drop any remaining missing values so SMA() does not fail.
  close_prices <- close_prices[!is.na(close_prices)]

  ma_20 <- SMA(close_prices, n = 20)

  stats <- data.frame(
    Statistic = c("Moving Average (20-day, last value)",
                  "Mean",
                  "Mode",
                  "Median",
                  "Standard Deviation"),
    Value = c(tail(ma_20, 1),
              mean(close_prices, na.rm = TRUE),
              get_mode(close_prices),
              median(close_prices, na.rm = TRUE),
              sd(close_prices, na.rm = TRUE))
  )

  return(stats)
}

# ---------------------------------------------------------------------------
# 3. Run the analysis and display the results
# ---------------------------------------------------------------------------
stocks <- load_stock_data()

for (symbol in names(stocks)) {
  cat("\n==================== ", symbol, " ====================\n")

  cat("\n--- First rows of the data ---\n")
  print(head(stocks[[symbol]]))

  cat("\n--- Statistics ---\n")
  print(calculate_statistics(stocks[[symbol]]))
}

# ---------------------------------------------------------------------------
# 4. Visualisation: Close price with its 20-day moving average,
#    one chart per stock (take a screenshot of each for the report).
# ---------------------------------------------------------------------------
for (symbol in names(stocks)) {
  stock_df <- stocks[[symbol]]
  close_col <- grep("Close", names(stock_df), value = TRUE)[1]
  close_prices <- stock_df[[close_col]]
  ma_20 <- SMA(close_prices, n = 20)

  plot(stock_df$Date, close_prices,
       type = "l",
       main = paste(symbol, "- Close Price and 20-Day Moving Average"),
       xlab = "Date",
       ylab = "Close Price (USD)")
  lines(stock_df$Date, ma_20, col = "red", lwd = 2)
  legend("topleft",
         legend = c("Close Price", "20-Day Moving Average"),
         col = c("black", "red"),
         lty = 1, lwd = 2)
}
