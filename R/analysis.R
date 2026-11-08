# Higher-level study computations: stressed VaR and diversification.

#' Stressed VaR: the worst historical VaR over any rolling 12-month window.
#'
#' @param portfolio One-column xts of portfolio returns.
#' @param window Window length in trading days (about one year).
#' @param alpha Tail probability.
#' @return A list with the stressed VaR, the window dates that produce it, and the
#'   full series of window VaRs (right-aligned by window end date).
stressed_var <- function(portfolio, window = 250L, alpha = 0.05) {
  r <- as.numeric(portfolio)
  dates <- zoo::index(portfolio)
  n <- length(r)
  if (n < window) {
    stop(sprintf("Need at least %d observations; got %d.", window, n))
  }
  starts <- seq_len(n - window + 1)
  window_var <- vapply(starts,
                       function(s) var_historical(r[s:(s + window - 1)], alpha),
                       numeric(1))
  worst <- which.max(window_var)
  list(stressed_var = window_var[worst],
       window_start = dates[worst],
       window_end = dates[worst + window - 1],
       series = xts::xts(window_var, order.by = dates[starts + window - 1]))
}

#' Portfolio VaR against the sum of weighted stand-alone VaRs for one window.
#'
#' The gap between the two measures the diversification benefit, which shrinks as
#' correlations rise.
#'
#' @param component_returns An xts of the component stock returns.
#' @param portfolio One-column xts of portfolio returns over the same dates.
#' @param alpha Tail probability.
#' @param weights Component weights; defaults to equal weights.
#' @return A list with the portfolio VaR, the weighted sum of stand-alone VaRs,
#'   the stand-alone VaRs and the diversification benefit (1 - ratio).
diversification <- function(component_returns, portfolio, alpha = 0.05,
                            weights = NULL) {
  values <- as.matrix(component_returns)
  if (is.null(weights)) {
    weights <- rep(1 / ncol(values), ncol(values))
  }
  standalone <- vapply(seq_len(ncol(values)),
                       function(j) var_historical(values[, j], alpha), numeric(1))
  names(standalone) <- colnames(values)
  undiversified <- sum(weights * standalone)
  portfolio_var <- var_historical(portfolio, alpha)
  list(portfolio_var = portfolio_var, sum_standalone = undiversified,
       diversification_benefit = 1 - portfolio_var / undiversified,
       standalone = standalone)
}

#' Rolling portfolio VaR versus the rolling sum of weighted stand-alone VaRs.
#'
#' @param component_returns An xts of component returns.
#' @param portfolio One-column xts of portfolio returns.
#' @param window Lookback length in trading days.
#' @param alpha Tail probability.
#' @param weights Component weights; defaults to equal weights.
#' @return A list with aligned dates and the two VaR series plus the
#'   diversification benefit series.
rolling_diversification <- function(component_returns, portfolio, window = 250L,
                                    alpha = 0.05, weights = NULL) {
  values <- as.matrix(component_returns)
  pf <- as.numeric(portfolio)
  dates <- zoo::index(portfolio)
  n <- length(pf)
  if (is.null(weights)) {
    weights <- rep(1 / ncol(values), ncol(values))
  }
  eval_idx <- (window + 1):n
  portfolio_var <- numeric(length(eval_idx))
  sum_standalone <- numeric(length(eval_idx))
  for (k in seq_along(eval_idx)) {
    t <- eval_idx[k]
    rows <- (t - window):(t - 1)
    portfolio_var[k] <- var_historical(pf[rows], alpha)
    standalone <- vapply(seq_len(ncol(values)),
                         function(j) var_historical(values[rows, j], alpha),
                         numeric(1))
    sum_standalone[k] <- sum(weights * standalone)
  }
  list(dates = dates[eval_idx], portfolio_var = portfolio_var,
       sum_standalone = sum_standalone,
       diversification_benefit = 1 - portfolio_var / sum_standalone)
}
