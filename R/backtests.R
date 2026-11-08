# VaR and ES backtests.
#
# A violation (exception) occurs when the realised return is below the negative
# of the forecast VaR, i.e. the loss exceeds VaR. Hit sequences are 0/1 vectors.

#' Violation indicator sequence for a VaR forecast.
#'
#' @param realised Realised returns.
#' @param var Positive VaR forecasts aligned with `realised`.
#' @return An integer 0/1 vector; 1 where the loss exceeded VaR.
hit_sequence <- function(realised, var) {
  as.integer(as.numeric(realised) < -as.numeric(var))
}

#' Kupiec proportion-of-failures (unconditional coverage) test.
#'
#' @param violations Number of violations observed.
#' @param n Number of observations.
#' @param alpha Expected violation rate (tail probability).
#' @return A list with the observed rate, likelihood ratio and chi-square(1)
#'   p-value.
kupiec_test <- function(violations, n, alpha) {
  x <- violations
  rate <- x / n
  loglik_null <- x * log(alpha) + (n - x) * log(1 - alpha)
  loglik_alt <- if (x == 0) {
    n * log(1 - rate)
  } else if (x == n) {
    n * log(rate)
  } else {
    x * log(rate) + (n - x) * log(1 - rate)
  }
  lr <- -2 * (loglik_null - loglik_alt)
  list(violations = x, n = n, rate = rate, expected_rate = alpha,
       lr = lr, p_value = stats::pchisq(lr, df = 1, lower.tail = FALSE))
}

#' Christoffersen test of independence of violations (first-order Markov).
#'
#' @param hits A 0/1 violation sequence.
#' @return A list with the transition counts, likelihood ratio and chi-square(1)
#'   p-value.
christoffersen_independence <- function(hits) {
  hits <- as.integer(hits)
  previous <- utils::head(hits, -1)
  current <- utils::tail(hits, -1)
  n00 <- sum(previous == 0 & current == 0)
  n01 <- sum(previous == 0 & current == 1)
  n10 <- sum(previous == 1 & current == 0)
  n11 <- sum(previous == 1 & current == 1)
  pi01 <- n01 / (n00 + n01)
  pi11 <- n11 / (n10 + n11)
  pi <- (n01 + n11) / (n00 + n01 + n10 + n11)
  term <- function(count, prob) if (count == 0) 0 else count * log(prob)
  loglik_null <- term(n00 + n10, 1 - pi) + term(n01 + n11, pi)
  loglik_alt <- term(n00, 1 - pi01) + term(n01, pi01) +
    term(n10, 1 - pi11) + term(n11, pi11)
  lr <- -2 * (loglik_null - loglik_alt)
  list(n00 = n00, n01 = n01, n10 = n10, n11 = n11,
       lr = lr, p_value = stats::pchisq(lr, df = 1, lower.tail = FALSE))
}

#' Christoffersen conditional-coverage test (coverage and independence jointly).
#'
#' @param hits A 0/1 violation sequence.
#' @param alpha Expected violation rate.
#' @return A list with the combined likelihood ratio, chi-square(2) p-value and
#'   the two component tests.
christoffersen_cc <- function(hits, alpha) {
  unconditional <- kupiec_test(sum(hits), length(hits), alpha)
  independence <- christoffersen_independence(hits)
  lr <- unconditional$lr + independence$lr
  list(lr = lr, p_value = stats::pchisq(lr, df = 2, lower.tail = FALSE),
       unconditional = unconditional, independence = independence)
}

#' Basel traffic-light zones for a 99% VaR over rolling 250-day windows.
#'
#' Counts exceptions in each trailing window and classifies it green (0-4),
#' yellow (5-9) or red (10 or more), the 1996 Basel scaling for a 99% model.
#'
#' @param hits A 0/1 violation sequence for the 99% VaR.
#' @param window Window length (250 trading days).
#' @return A list with the per-window exception counts, their zones, and the
#'   final and worst zones.
basel_traffic_light <- function(hits, window = 250L) {
  hits <- as.integer(hits)
  n <- length(hits)
  if (n < window) {
    stop(sprintf("Need at least %d observations for the Basel window; got %d.",
                 window, n))
  }
  counts <- vapply(window:n, function(t) sum(hits[(t - window + 1):t]), integer(1))
  zones <- cut(counts, breaks = c(-1, 4, 9, Inf),
               labels = c("green", "yellow", "red"))
  ordered_zones <- factor(zones, levels = c("green", "yellow", "red"),
                          ordered = TRUE)
  list(counts = counts, zones = ordered_zones,
       final = ordered_zones[length(ordered_zones)],
       worst = ordered_zones[which.max(as.integer(ordered_zones))])
}

#' McNeil-Frey bootstrap test of expected-shortfall adequacy.
#'
#' On violation days the standardised exceedance residual should have mean zero
#' if the ES forecast is correct. A one-sided bootstrap tests the alternative
#' that realised tail losses exceed the ES forecast.
#'
#' @param realised Realised returns.
#' @param var Positive VaR forecasts.
#' @param es Positive ES forecasts.
#' @param n_boot Number of bootstrap resamples.
#' @param seed Random seed for reproducibility.
#' @return A list with the number of exceptions, the mean exceedance residual and
#'   the bootstrap p-value (small means ES underestimates risk).
mcneil_frey_test <- function(realised, var, es, n_boot = 10000L, seed = 1L) {
  realised <- as.numeric(realised)
  var <- as.numeric(var)
  es <- as.numeric(es)
  exceed <- realised < -var
  n_exc <- sum(exceed)
  if (n_exc < 2L) {
    return(list(n_exceptions = n_exc, mean_residual = NA_real_,
                p_value = NA_real_))
  }
  # residual = realised return + ES forecast; mean zero under a correct ES.
  residual <- realised[exceed] + es[exceed]
  observed <- mean(residual)
  centred <- residual - observed
  withr_seed <- function(code) {
    old <- if (exists(".Random.seed", envir = .GlobalEnv)) {
      get(".Random.seed", envir = .GlobalEnv)
    } else {
      NULL
    }
    set.seed(seed)
    on.exit({
      if (is.null(old)) {
        if (exists(".Random.seed", envir = .GlobalEnv)) {
          rm(".Random.seed", envir = .GlobalEnv)
        }
      } else {
        assign(".Random.seed", old, envir = .GlobalEnv)
      }
    })
    code
  }
  boot_means <- withr_seed(
    replicate(n_boot, mean(sample(centred, replace = TRUE)))
  )
  list(n_exceptions = n_exc, mean_residual = observed,
       p_value = mean(boot_means <= observed))
}
