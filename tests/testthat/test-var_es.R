test_that("ES is at least as large as VaR for both estimators", {
  set.seed(2)
  returns <- rnorm(2000, 0, 0.02)
  for (alpha in c(0.05, 0.01)) {
    expect_gte(es_historical(returns, alpha), var_historical(returns, alpha))
    expect_gte(es_normal(returns, alpha), var_normal(returns, alpha))
  }
})

test_that("normal VaR and ES match their closed forms", {
  set.seed(3)
  returns <- rnorm(5000, 0.001, 0.02)
  alpha <- 0.05
  mu <- mean(returns)
  sigma <- stats::sd(returns)
  expect_equal(var_normal(returns, alpha), -(mu + sigma * qnorm(alpha)),
               tolerance = 1e-12)
  expect_equal(es_normal(returns, alpha),
               -(mu - sigma * dnorm(qnorm(alpha)) / alpha), tolerance = 1e-12)
})

test_that("standardised t quantile is symmetric and tends to the normal", {
  expect_equal(qstd_unit(0.05, 8), -qstd_unit(0.95, 8), tolerance = 1e-10)
  expect_equal(qstd_unit(0.05, 1e6), qnorm(0.05), tolerance = 1e-3)
})

test_that("standardised t ES tends to the normal ES for large nu", {
  alpha <- 0.025
  normal_es <- -dnorm(qnorm(alpha)) / alpha
  expect_equal(es_std_t(alpha, 1e5), normal_es, tolerance = 1e-2)
})

test_that("standardised t ES is deeper than the t quantile", {
  nu <- 5
  alpha <- 0.01
  expect_lt(es_std_t(alpha, nu), qstd_unit(alpha, nu))
})

test_that("GARCH filter follows the recursion exactly", {
  r <- c(0.01, -0.02, 0.015, -0.01)
  out <- garch_filter(r, mu = 0, omega = 1e-4, alpha1 = 0.1, beta1 = 0.8,
                      var_init = 4e-4)
  expected2 <- 1e-4 + 0.1 * r[1]^2 + 0.8 * 4e-4
  expect_equal(out$sigma2[2], expected2, tolerance = 1e-15)
  expect_equal(out$z[1], r[1] / sqrt(4e-4), tolerance = 1e-12)
})

test_that("GARCH(1,1)-t fit returns parameters inside the stationarity region", {
  set.seed(4)
  n <- 700
  omega <- 1e-6
  alpha1 <- 0.08
  beta1 <- 0.9
  sigma2 <- numeric(n)
  eps <- numeric(n)
  sigma2[1] <- omega / (1 - alpha1 - beta1)
  for (t in 2:n) {
    eps[t - 1] <- sqrt(sigma2[t - 1]) * stats::rt(1, df = 6) / sqrt(6 / 4)
    sigma2[t] <- omega + alpha1 * eps[t - 1]^2 + beta1 * sigma2[t - 1]
  }
  eps[n] <- sqrt(sigma2[n]) * stats::rt(1, df = 6) / sqrt(6 / 4)
  fit <- fit_garch_t(eps)
  expect_false(is.null(fit))
  expect_gt(fit$omega, 0)
  expect_gt(fit$alpha1, 0)
  expect_gt(fit$beta1, 0)
  expect_lt(fit$alpha1 + fit$beta1, 1)
  expect_gt(fit$nu, 2)
})

test_that("rolling_var_es has consistent shapes and positive VaR", {
  set.seed(5)
  dates <- as.Date("2015-01-01") + 0:399
  pf <- xts::xts(rnorm(400, 0, 0.01), order.by = dates)
  colnames(pf) <- "portfolio"
  roll <- rolling_var_es(pf, window = 250, alpha = 0.05,
                         methods = c("historical", "normal", "ewma"))
  expect_equal(length(roll$dates), 150L)
  expect_equal(nrow(roll$var), 150L)
  expect_equal(length(roll$realised), 150L)
  expect_false(anyNA(roll$var))
  expect_true(all(roll$var > 0))
})
