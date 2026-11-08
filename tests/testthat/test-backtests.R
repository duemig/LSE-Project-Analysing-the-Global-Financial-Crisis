test_that("Kupiec LR is near zero when the violation rate equals alpha", {
  result <- kupiec_test(violations = 50, n = 1000, alpha = 0.05)
  expect_lt(result$lr, 1e-6)
  expect_gt(result$p_value, 0.99)
})

test_that("Kupiec rejects a large excess of violations", {
  result <- kupiec_test(violations = 100, n = 500, alpha = 0.05)
  expect_lt(result$p_value, 1e-6)
})

test_that("independence test counts transitions and flags clustering", {
  hits <- c(rep(0, 90), rep(1, 10), rep(0, 100))
  result <- christoffersen_independence(hits)
  expect_equal(result$n11, 9L)
  expect_equal(result$n01, 1L)
  expect_lt(result$p_value, 0.05)
})

test_that("independence test does not flag an evenly spread pattern", {
  hits <- rep(c(rep(0, 19), 1), 10)
  result <- christoffersen_independence(hits)
  expect_gt(result$p_value, 0.05)
})

test_that("conditional coverage combines the two likelihood ratios", {
  hits <- c(rep(0, 90), rep(1, 10), rep(0, 100))
  cc <- christoffersen_cc(hits, alpha = 0.05)
  expect_equal(cc$lr, cc$unconditional$lr + cc$independence$lr, tolerance = 1e-10)
})

test_that("Basel zones map trailing-window counts correctly", {
  make_hits <- function(k) {
    hits <- rep(0L, 300)
    hits[(300 - k + 1):300] <- 1L
    hits
  }
  expect_equal(as.character(basel_traffic_light(make_hits(3))$final), "green")
  expect_equal(as.character(basel_traffic_light(make_hits(7))$final), "yellow")
  expect_equal(as.character(basel_traffic_light(make_hits(12))$final), "red")
})

test_that("McNeil-Frey flags expected-shortfall underestimation", {
  set.seed(6)
  n <- 1000
  realised <- rnorm(n, 0, 0.02)
  var <- rep(0.03, n)
  es <- rep(0.035, n)
  realised[1:20] <- -0.10
  result <- mcneil_frey_test(realised, var, es, n_boot = 2000)
  expect_lt(result$p_value, 0.05)
  expect_lt(result$mean_residual, 0)
})
