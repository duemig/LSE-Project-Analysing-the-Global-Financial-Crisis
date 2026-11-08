test_that("stressed VaR selects the most volatile window", {
  set.seed(10)
  dates <- as.Date("2010-01-01") + 0:499
  returns <- c(rnorm(250, 0, 0.005), rnorm(250, 0, 0.03))
  pf <- xts::xts(returns, order.by = dates)
  colnames(pf) <- "portfolio"
  result <- stressed_var(pf, window = 250, alpha = 0.05)
  expect_gt(result$stressed_var, var_historical(returns[1:250], 0.05))
  expect_gt(result$window_end, as.Date("2010-06-01"))
})

test_that("diversification benefit is between 0 and 1 for correlated assets", {
  set.seed(11)
  n <- 400
  factor <- rnorm(n, 0, 0.01)
  components <- xts::xts(
    sapply(1:4, function(i) 0.7 * factor + 0.3 * rnorm(n, 0, 0.01)),
    order.by = as.Date("2010-01-01") + 0:(n - 1)
  )
  colnames(components) <- paste0("S", 1:4)
  pf <- portfolio_returns(components)
  result <- diversification(components[zoo::index(pf)], pf, alpha = 0.05)
  expect_gt(result$diversification_benefit, 0)
  expect_lt(result$diversification_benefit, 1)
  expect_lt(result$portfolio_var, result$sum_standalone)
})

test_that("rolling diversification has aligned series and a sensible benefit", {
  set.seed(12)
  n <- 400
  factor <- rnorm(n, 0, 0.01)
  components <- xts::xts(
    sapply(1:4, function(i) 0.6 * factor + 0.4 * rnorm(n, 0, 0.01)),
    order.by = as.Date("2010-01-01") + 0:(n - 1)
  )
  colnames(components) <- paste0("S", 1:4)
  pf <- portfolio_returns(components)
  result <- rolling_diversification(components[zoo::index(pf)], pf, window = 250,
                                    alpha = 0.05)
  expect_equal(length(result$dates), length(result$portfolio_var))
  expect_true(all(result$portfolio_var <= result$sum_standalone + 1e-8))
})
