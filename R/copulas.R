# Copula analysis of the four financial stocks.
#
# Returns are filtered with a GARCH(1,1)-t model; the standardised residuals are
# turned into pseudo-observations and four copulas (t, Gaussian, Clayton, Gumbel)
# are fitted by maximum pseudo-likelihood. Tail-dependence coefficients come from
# the fitted parameters.

#' GARCH(1,1)-t standardised residuals of one return series.
standardized_residuals <- function(returns) {
  fit <- fit_garch_t(returns)
  if (is.null(fit)) {
    stop("GARCH fit failed for the residual filter.")
  }
  garch_filter(as.numeric(returns), fit$mu, fit$omega, fit$alpha1,
               fit$beta1, fit$var_init)$z
}

#' Matrix of GARCH-standardised residuals, one column per return series.
#'
#' @param returns An xts or matrix of returns (columns are stocks).
#' @return A numeric matrix of standardised residuals with the same columns.
garch_residual_matrix <- function(returns) {
  values <- as.matrix(returns)
  residuals <- vapply(seq_len(ncol(values)),
                      function(j) standardized_residuals(values[, j]),
                      numeric(nrow(values)))
  colnames(residuals) <- colnames(values)
  residuals
}

#' Student-t copula tail-dependence coefficient for one pair.
#'
#' @param rho Pairwise correlation parameter.
#' @param nu Degrees of freedom.
#' @return The (symmetric) lower = upper tail-dependence coefficient.
t_tail_dependence <- function(rho, nu) {
  2 * stats::pt(-sqrt((nu + 1) * (1 - rho) / (1 + rho)), df = nu + 1)
}

#' Fit the four copulas to pseudo-observations and summarise the fits.
#'
#' @param u A matrix of pseudo-observations in (0, 1) (n x d).
#' @return A list with a summary data frame (one row per copula: number of
#'   parameters, log-likelihood, AIC, lower and upper tail dependence) and the
#'   fitted copula objects.
fit_four_copulas <- function(u) {
  d <- ncol(u)
  fit_one <- function(copula_model) {
    copula::fitCopula(copula_model, u, method = "mpl")
  }
  gaussian <- fit_one(copula::normalCopula(dim = d, dispstr = "un"))
  student <- fit_one(copula::tCopula(dim = d, dispstr = "un", df.fixed = FALSE))
  clayton <- fit_one(copula::claytonCopula(dim = d))
  gumbel <- fit_one(copula::gumbelCopula(dim = d))

  student_coef <- copula::coef(student)
  df_t <- student_coef[["df"]]
  rho_t <- student_coef[setdiff(names(student_coef), "df")]
  lambda_t <- mean(t_tail_dependence(rho_t, df_t))

  theta_clayton <- as.numeric(copula::coef(clayton))
  theta_gumbel <- as.numeric(copula::coef(gumbel))

  summarise <- function(name, fit, n_par, lower, upper) {
    loglik <- as.numeric(stats::logLik(fit))
    data.frame(copula = name, n_params = n_par, loglik = loglik,
               aic = -2 * loglik + 2 * n_par,
               lambda_lower = lower, lambda_upper = upper,
               stringsAsFactors = FALSE)
  }

  summary <- rbind(
    summarise("t", student, length(student_coef), lambda_t, lambda_t),
    summarise("Gaussian", gaussian, length(copula::coef(gaussian)), 0, 0),
    summarise("Clayton", clayton, 1L, 2^(-1 / theta_clayton), 0),
    summarise("Gumbel", gumbel, 1L, 0, 2 - 2^(1 / theta_gumbel))
  )
  rownames(summary) <- NULL

  list(summary = summary,
       fits = list(t = student, Gaussian = gaussian, Clayton = clayton,
                   Gumbel = gumbel),
       df_t = df_t)
}

#' Full copula analysis for one period: filter, pseudo-observations, fit.
#'
#' @param returns An xts of the financial stocks' returns for the period.
#' @return The list from [fit_four_copulas], plus the pseudo-observations.
copula_analysis <- function(returns) {
  residuals <- garch_residual_matrix(returns)
  u <- copula::pobs(residuals)
  result <- fit_four_copulas(u)
  result$pseudo_obs <- u
  result
}

#' Average pairwise correlation across a rolling window.
#'
#' @param returns An xts of returns.
#' @param width Rolling window width in days.
#' @return A one-column xts of the mean off-diagonal correlation, right-aligned.
rolling_average_correlation <- function(returns, width = 60L) {
  values <- as.matrix(returns)
  n <- nrow(values)
  d <- ncol(values)
  out <- rep(NA_real_, n)
  for (t in width:n) {
    block <- values[(t - width + 1):t, , drop = FALSE]
    correlation <- stats::cor(block)
    out[t] <- mean(correlation[upper.tri(correlation)])
  }
  xts::xts(out, order.by = zoo::index(returns))
}
