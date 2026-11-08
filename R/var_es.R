# Value at risk and expected shortfall estimators.
#
# VaR and ES are reported as positive loss magnitudes: a VaR of 0.016 means a
# one-day loss of 1.6% of portfolio value at the stated confidence level. The
# tail probability is alpha = 1 - confidence (0.05 at 95%, 0.01 at 99%).
#
# Static estimators take a vector of returns. Rolling estimators roll a window
# over a return series and produce a one-day-ahead forecast for each date. The
# GARCH(1,1)-t model is fitted by maximum likelihood in base R so the study runs
# the same way on every platform without a compiled GARCH package.

# ---- standardised Student-t helpers (unit variance) ------------------------

#' Density of a Student t with `nu` degrees of freedom, rescaled to unit variance.
dstd_unit <- function(x, nu) {
  scale <- sqrt(nu / (nu - 2))
  stats::dt(x * scale, df = nu) * scale
}

#' Quantile of a unit-variance Student t.
qstd_unit <- function(p, nu) {
  stats::qt(p, df = nu) * sqrt((nu - 2) / nu)
}

#' Expected shortfall of a unit-variance Student t (mean below the alpha-quantile).
#'
#' @return A negative number: the conditional mean of the standardised t in its
#'   lower alpha tail.
es_std_t <- function(alpha, nu) {
  t_alpha <- stats::qt(alpha, df = nu)
  raw_mean <- -(stats::dt(t_alpha, df = nu) / alpha) * ((nu + t_alpha^2) / (nu - 1))
  raw_mean * sqrt((nu - 2) / nu)
}

# ---- static, single-window estimators --------------------------------------

#' Historical-simulation VaR: the negated empirical alpha-quantile of returns.
var_historical <- function(returns, alpha) {
  returns <- stats::na.omit(as.numeric(returns))
  -as.numeric(stats::quantile(returns, probs = alpha, type = 7))
}

#' Historical-simulation ES: the negated mean of returns at or below the quantile.
es_historical <- function(returns, alpha) {
  returns <- stats::na.omit(as.numeric(returns))
  threshold <- stats::quantile(returns, probs = alpha, type = 7)
  -mean(returns[returns <= threshold])
}

#' Gaussian (parametric normal) VaR from the sample mean and standard deviation.
var_normal <- function(returns, alpha) {
  returns <- stats::na.omit(as.numeric(returns))
  -(mean(returns) + stats::sd(returns) * stats::qnorm(alpha))
}

#' Gaussian ES: the normal expected shortfall at level alpha.
es_normal <- function(returns, alpha) {
  returns <- stats::na.omit(as.numeric(returns))
  -(mean(returns) - stats::sd(returns) * stats::dnorm(stats::qnorm(alpha)) / alpha)
}

# ---- volatility models -----------------------------------------------------

#' RiskMetrics EWMA one-day-ahead volatility forecast series.
#'
#' Returns sigma[t], the forecast of day t's volatility made at t-1, from
#' sigma2[t] = lambda * sigma2[t-1] + (1 - lambda) * r2[t-1].
#'
#' @param returns Numeric return series.
#' @param lambda Decay factor (0.94 for daily RiskMetrics).
#' @return Numeric vector of one-day-ahead volatility forecasts.
ewma_volatility <- function(returns, lambda = 0.94) {
  r <- as.numeric(returns)
  n <- length(r)
  sigma2 <- numeric(n)
  sigma2[1] <- stats::var(r)
  for (t in 2:n) {
    sigma2[t] <- lambda * sigma2[t - 1] + (1 - lambda) * r[t - 1]^2
  }
  sqrt(sigma2)
}

#' Run the GARCH(1,1) variance recursion over a return series.
#'
#' @param r Numeric return series.
#' @param mu Conditional mean.
#' @param omega,alpha1,beta1 GARCH parameters.
#' @param var_init Starting conditional variance.
#' @return A list with the conditional variance series (sigma2) and the
#'   standardised residuals (z).
garch_filter <- function(r, mu, omega, alpha1, beta1, var_init) {
  n <- length(r)
  eps <- r - mu
  sigma2 <- numeric(n)
  sigma2[1] <- var_init
  for (t in 2:n) {
    sigma2[t] <- omega + alpha1 * eps[t - 1]^2 + beta1 * sigma2[t - 1]
  }
  list(sigma2 = sigma2, z = eps / sqrt(sigma2))
}

#' Fit a GARCH(1,1) with Student-t innovations by maximum likelihood.
#'
#' The parameters are transformed so that omega > 0, alpha1 and beta1 are
#' positive with alpha1 + beta1 < 1, and nu > 2.
#'
#' @param returns Numeric return series (one estimation window).
#' @param start Optional transformed starting values (for warm starts).
#' @return A list of fitted parameters (mu, omega, alpha1, beta1, nu), the
#'   starting variance and the transformed parameter vector; NULL if the fit
#'   fails to converge.
fit_garch_t <- function(returns, start = NULL) {
  r <- as.numeric(returns)
  var0 <- stats::var(r)
  untransform <- function(par) {
    alpha1 <- 0.999 * stats::plogis(par[3])
    list(mu = par[1], omega = exp(par[2]), alpha1 = alpha1,
         beta1 = (0.999 - alpha1) * stats::plogis(par[4]), nu = 2 + exp(par[5]))
  }
  negloglik <- function(par) {
    p <- untransform(par)
    filtered <- garch_filter(r, p$mu, p$omega, p$alpha1, p$beta1, var0)
    if (any(!is.finite(filtered$sigma2)) || any(filtered$sigma2 <= 0)) {
      return(1e10)
    }
    loglik <- sum(log(dstd_unit(filtered$z, p$nu)) - 0.5 * log(filtered$sigma2))
    if (!is.finite(loglik)) 1e10 else -loglik
  }
  if (is.null(start)) {
    start <- c(mean(r), log(0.1 * var0),
               stats::qlogis(0.08 / 0.999),
               stats::qlogis(0.90 / (0.999 - 0.08)),
               log(4))
  }
  opt <- tryCatch(
    stats::optim(start, negloglik, method = "Nelder-Mead",
                 control = list(maxit = 2000, reltol = 1e-8)),
    error = function(e) NULL
  )
  if (is.null(opt) || opt$convergence != 0) {
    return(NULL)
  }
  c(untransform(opt$par), list(var_init = var0, par = opt$par))
}

#' One-day-ahead GARCH forecast given fitted parameters and the recent returns.
#'
#' @param r Numeric return series ending at the forecast origin.
#' @param params A fit from [fit_garch_t].
#' @return A list with the mean and volatility forecasts, nu, and the
#'   standardised residuals over the series.
garch_forecast <- function(r, params) {
  filtered <- garch_filter(r, params$mu, params$omega, params$alpha1,
                           params$beta1, params$var_init)
  n <- length(r)
  eps_last <- r[n] - params$mu
  sigma2_next <- params$omega + params$alpha1 * eps_last^2 +
    params$beta1 * filtered$sigma2[n]
  list(mu = params$mu, sigma = sqrt(sigma2_next), nu = params$nu,
       z = filtered$z)
}

# ---- rolling estimators ----------------------------------------------------

#' Rolling one-day-ahead VaR and ES across several methods.
#'
#' For each date from `window`+1 onward, the trailing `window` returns forecast
#' the next day's VaR and ES. The realised return on that date is retained so the
#' output can be backtested directly. GARCH parameters are re-estimated every
#' `refit_every` days and the conditional variance is filtered forward in between.
#'
#' @param portfolio One-column xts of returns (date index used for output).
#' @param window Lookback length in trading days (250).
#' @param alpha Tail probability (0.05 or 0.01).
#' @param methods Any of "historical", "normal", "ewma", "garch_t", "fhs".
#' @param ewma_lambda EWMA decay factor.
#' @param refit_every Re-estimate the GARCH every this many days.
#' @return A list with `dates`, a `realised` return vector, matrices `var` and
#'   `es` (rows = dates, columns = methods), plus `window` and `alpha`.
rolling_var_es <- function(portfolio, window = 250, alpha = 0.05,
                           methods = c("historical", "normal", "ewma",
                                       "garch_t", "fhs"),
                           ewma_lambda = 0.94, refit_every = 10L) {
  r <- as.numeric(portfolio)
  dates <- zoo::index(portfolio)
  n <- length(r)
  if (n <= window) {
    stop(sprintf("Need more than %d observations; got %d.", window, n))
  }
  eval_idx <- (window + 1):n
  ewma_sigma <- ewma_volatility(r, lambda = ewma_lambda)
  needs_garch <- any(c("garch_t", "fhs") %in% methods)

  var_out <- matrix(NA_real_, nrow = length(eval_idx), ncol = length(methods),
                    dimnames = list(NULL, methods))
  es_out <- var_out
  params <- NULL

  for (k in seq_along(eval_idx)) {
    t <- eval_idx[k]
    win <- r[(t - window):(t - 1)]
    if ("historical" %in% methods) {
      var_out[k, "historical"] <- var_historical(win, alpha)
      es_out[k, "historical"] <- es_historical(win, alpha)
    }
    if ("normal" %in% methods) {
      var_out[k, "normal"] <- var_normal(win, alpha)
      es_out[k, "normal"] <- es_normal(win, alpha)
    }
    if ("ewma" %in% methods) {
      sigma <- ewma_sigma[t]
      var_out[k, "ewma"] <- -stats::qnorm(alpha) * sigma
      es_out[k, "ewma"] <- sigma * stats::dnorm(stats::qnorm(alpha)) / alpha
    }
    if (needs_garch) {
      if (is.null(params) || ((k - 1L) %% refit_every == 0L)) {
        refitted <- fit_garch_t(win, start = if (is.null(params)) NULL else params$par)
        if (!is.null(refitted)) {
          params <- refitted
        }
      }
      if (!is.null(params)) {
        forecast <- garch_forecast(win, params)
        if ("garch_t" %in% methods) {
          q <- qstd_unit(alpha, forecast$nu)
          var_out[k, "garch_t"] <- -(forecast$mu + forecast$sigma * q)
          es_out[k, "garch_t"] <- -(forecast$mu +
                                      forecast$sigma * es_std_t(alpha, forecast$nu))
        }
        if ("fhs" %in% methods) {
          z_q <- stats::quantile(forecast$z, probs = alpha, type = 7)
          var_out[k, "fhs"] <- -(forecast$mu + forecast$sigma * z_q)
          es_out[k, "fhs"] <- -(forecast$mu +
                                  forecast$sigma * mean(forecast$z[forecast$z <= z_q]))
        }
      }
    }
  }

  list(dates = dates[eval_idx], realised = r[eval_idx], var = var_out,
       es = es_out, window = window, alpha = alpha)
}
