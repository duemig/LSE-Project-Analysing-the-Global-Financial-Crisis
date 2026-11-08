# Data loading, return construction and portfolio building.
#
# Prices are read from the cached public CSVs in data/ (see scripts/00_download_data.R).
# The study uses split-adjusted close, which reproduces the original CRSP-based
# results; returns are computed from those prices.

#' Tickers of the ten portfolio stocks, in a fixed order.
study_tickers <- function() {
  c("KO", "XOM", "GE", "JNJ", "DIS", "JPM", "INTC", "MS", "C", "GS")
}

#' Tickers of the four financial stocks used in the dependence analysis.
financial_tickers <- function() {
  c("JPM", "C", "MS", "GS")
}

#' Full company names, keyed by ticker, for labels and tables.
company_names <- function() {
  c(KO = "Coca-Cola", XOM = "ExxonMobil", GE = "General Electric",
    JNJ = "Johnson & Johnson", DIS = "Disney", JPM = "JPMorgan Chase",
    INTC = "Intel", MS = "Morgan Stanley", C = "Citigroup",
    GS = "Goldman Sachs")
}

#' Read a wide price CSV (first column Date) into an xts matrix of prices.
#'
#' @param path Path to a CSV with a Date column followed by one column per ticker.
#' @param tickers Optional subset of columns to keep, in the given order.
#' @return An xts object of prices indexed by date.
load_prices <- function(path, tickers = NULL) {
  frame <- utils::read.csv(path, check.names = FALSE)
  dates <- as.Date(frame[[1]])
  values <- as.matrix(frame[, -1, drop = FALSE])
  prices <- xts::xts(values, order.by = dates)
  if (!is.null(tickers)) {
    prices <- prices[, tickers, drop = FALSE]
  }
  prices
}

#' Restrict an xts object to a closed date range.
#'
#' @param data An xts object.
#' @param from First date to keep (character or Date), or NULL for the start.
#' @param to Last date to keep (character or Date), or NULL for the end.
#' @return The rows of `data` within the range.
window_range <- function(data, from = NULL, to = NULL) {
  lower <- if (is.null(from)) "" else as.character(as.Date(from))
  upper <- if (is.null(to)) "" else as.character(as.Date(to))
  data[paste0(lower, "/", upper)]
}

#' Simple (relative) daily returns from prices.
#'
#' @param prices An xts object of prices.
#' @return Simple returns, with the first (undefined) row dropped.
simple_returns <- function(prices) {
  returns <- prices / xts::lag.xts(prices, 1) - 1
  returns[-1, , drop = FALSE]
}

#' Log daily returns from prices.
#'
#' @param prices An xts object of prices.
#' @return Log returns, with the first (undefined) row dropped.
log_returns <- function(prices) {
  returns <- diff(log(prices))
  returns[-1, , drop = FALSE]
}

#' Equal-weight buy-and-hold portfolio daily returns.
#'
#' Weights are set equal at inception and then drift with the holdings, matching
#' the original study's \code{Return.portfolio} construction.
#'
#' @param returns Simple returns of the component stocks (xts).
#' @param weights Optional initial weights; defaults to equal weights.
#' @return A one-column xts of daily portfolio returns named "portfolio".
portfolio_returns <- function(returns, weights = NULL) {
  returns <- stats::na.omit(returns)
  if (is.null(weights)) {
    weights <- rep(1 / ncol(returns), ncol(returns))
  }
  pf <- PerformanceAnalytics::Return.portfolio(returns, weights = weights,
                                               verbose = FALSE)
  colnames(pf) <- "portfolio"
  pf
}
