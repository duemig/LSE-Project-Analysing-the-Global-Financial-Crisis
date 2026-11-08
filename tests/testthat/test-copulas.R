test_that("t tail dependence hits known boundary and interior values", {
  expect_equal(t_tail_dependence(1, 5), 1, tolerance = 1e-8)
  expect_gt(t_tail_dependence(0.5, 5), 0)
  expect_lt(t_tail_dependence(0.5, 5), 1)
})

test_that("the four copulas fit with sensible tail-dependence signs", {
  set.seed(7)
  model <- copula::tCopula(0.6, dim = 4, dispstr = "ex", df = 4)
  u <- copula::rCopula(600, model)
  result <- fit_four_copulas(u)
  expect_equal(nrow(result$summary), 4L)
  expect_true(all(is.finite(result$summary$aic)))

  pick <- function(name) result$summary[result$summary$copula == name, ]
  expect_equal(pick("Gaussian")$lambda_lower, 0)
  expect_gt(pick("Clayton")$lambda_lower, 0)
  expect_equal(pick("Clayton")$lambda_upper, 0)
  expect_gt(pick("Gumbel")$lambda_upper, 0)
  expect_equal(pick("Gumbel")$lambda_lower, 0)
  expect_gt(pick("t")$lambda_lower, 0)
})

test_that("a t copula fits tail-dependent data better than a Gaussian copula", {
  set.seed(8)
  model <- copula::tCopula(0.6, dim = 4, dispstr = "ex", df = 4)
  u <- copula::rCopula(600, model)
  result <- fit_four_copulas(u)
  pick <- function(name) result$summary[result$summary$copula == name, ]
  expect_lt(pick("t")$aic, pick("Gaussian")$aic)
})

test_that("rolling average correlation is in range and right-aligned", {
  set.seed(9)
  n <- 200
  factor <- rnorm(n, 0, 0.01)
  returns <- xts::xts(sapply(1:3, function(i) 0.6 * factor + 0.4 * rnorm(n, 0, 0.01)),
                      order.by = as.Date("2010-01-01") + 0:(n - 1))
  correlation <- rolling_average_correlation(returns, width = 60)
  values <- stats::na.omit(as.numeric(correlation))
  expect_true(all(values >= -1 & values <= 1))
  expect_true(is.na(as.numeric(correlation[1])))
})
