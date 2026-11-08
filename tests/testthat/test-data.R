test_that("simple and log returns drop one row and match the definitions", {
  dates <- as.Date("2020-01-01") + 0:4
  prices <- xts::xts(cbind(A = c(100, 101, 102, 100, 105),
                           B = c(50, 51, 52, 53, 54)), order.by = dates)
  sr <- simple_returns(prices)
  lr <- log_returns(prices)
  expect_equal(nrow(sr), 4L)
  expect_equal(nrow(lr), 4L)
  expect_equal(as.numeric(sr[1, "A"]), 0.01, tolerance = 1e-12)
  expect_equal(as.numeric(lr[1, "A"]), log(101 / 100), tolerance = 1e-12)
})

test_that("equal-weight portfolio return is a convex combination of components", {
  set.seed(1)
  dates <- as.Date("2020-01-01") + 0:119
  returns <- xts::xts(matrix(rnorm(360, 0, 0.01), ncol = 3), order.by = dates)
  colnames(returns) <- c("A", "B", "C")
  pf <- portfolio_returns(returns)
  expect_equal(ncol(pf), 1L)
  common <- zoo::index(pf)
  lo <- apply(returns[common], 1, min)
  hi <- apply(returns[common], 1, max)
  expect_true(all(as.numeric(pf) >= lo - 1e-8))
  expect_true(all(as.numeric(pf) <= hi + 1e-8))
})
