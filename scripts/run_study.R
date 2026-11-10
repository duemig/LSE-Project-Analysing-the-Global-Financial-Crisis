#!/usr/bin/env Rscript
# Run the whole study and write every number to results/ and every figure to
# figures/. The report and README read from results/; nothing is typed by hand.
#
# Usage (from the repository root): Rscript scripts/run_study.R

suppressMessages({
  library(xts)
  library(zoo)
  library(PerformanceAnalytics)
  library(copula)
  library(ggplot2)
  library(GGally)
  library(scales)
})

get_script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) == 1L) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg))))
  }
  getwd()
}

repo <- normalizePath(file.path(get_script_dir(), ".."))
for (file in list.files(file.path(repo, "R"), pattern = "[.]R$", full.names = TRUE)) {
  source(file)
}

results_dir <- file.path(repo, "results")
figures_dir <- file.path(repo, "figures")
dir.create(results_dir, showWarnings = FALSE)
dir.create(figures_dir, showWarnings = FALSE)

save_fig <- function(plot, name, width = 7, height = 4.3) {
  ggplot2::ggsave(file.path(figures_dir, name), plot, width = width,
                  height = height, dpi = 150)
}
write_result <- function(frame, name) {
  utils::write.csv(frame, file.path(results_dir, name), row.names = FALSE)
}

# ---- study windows ---------------------------------------------------------
study_start <- "2006-07-01"
study_end <- "2009-06-30"
precrisis_start <- "2006-07-01"
precrisis_end <- "2007-12-31"
crisis_start <- "2008-01-01"
crisis_end <- "2009-06-30"
alphas <- c("95" = 0.05, "99" = 0.01)
key <- list()

# ---- data ------------------------------------------------------------------
download_info <- readLines(file.path(repo, "data", "DOWNLOAD_INFO.txt"))
download_date <- sub("^Downloaded:\\s*", "",
                     grep("^Downloaded:", download_info, value = TRUE))
key[["data_source"]] <- "Yahoo Finance via quantmod"
key[["data_download_date"]] <- download_date

prices <- load_prices(file.path(repo, "data", "prices_public_close.csv"),
                      study_tickers())
prices <- window_range(prices, study_start, study_end)
returns <- simple_returns(prices)
log_ret <- log_returns(prices)
portfolio <- portfolio_returns(returns)

key[["study_start"]] <- format(min(zoo::index(prices)))
key[["study_end"]] <- format(max(zoo::index(prices)))
key[["n_stocks"]] <- ncol(prices)
key[["portfolio_obs"]] <- nrow(portfolio)

precrisis <- window_range(portfolio, precrisis_start, precrisis_end)
crisis <- window_range(portfolio, crisis_start, crisis_end)
key[["precrisis_obs"]] <- nrow(precrisis)
key[["crisis_obs"]] <- nrow(crisis)

# ---- static VaR and ES (the original starting point) -----------------------
static <- data.frame(
  measure = c("VaR", "VaR", "ES", "ES"),
  method = c("historical", "normal", "historical", "normal"),
  value = c(var_historical(precrisis, 0.05), var_normal(precrisis, 0.05),
            es_historical(precrisis, 0.05), es_normal(precrisis, 0.05)),
  stringsAsFactors = FALSE
)
write_result(static, "static_var_es.csv")
key[["static_var_hist"]] <- var_historical(precrisis, 0.05)
key[["static_var_norm"]] <- var_normal(precrisis, 0.05)
key[["static_es_hist"]] <- es_historical(precrisis, 0.05)
key[["static_es_norm"]] <- es_normal(precrisis, 0.05)

static_backtest <- data.frame(
  method = c("historical", "normal"),
  var = c(key[["static_var_hist"]], key[["static_var_norm"]]),
  violations = c(sum(crisis < -key[["static_var_hist"]]),
                 sum(crisis < -key[["static_var_norm"]])),
  n = nrow(crisis),
  expected = 0.05 * nrow(crisis),
  stringsAsFactors = FALSE
)
static_backtest$rate <- static_backtest$violations / static_backtest$n
write_result(static_backtest, "static_backtest.csv")
key[["static_violations_hist"]] <- static_backtest$violations[1]
key[["static_violations_norm"]] <- static_backtest$violations[2]
key[["static_backtest_n"]] <- nrow(crisis)
key[["static_kupiec_hist_p"]] <- kupiec_test(static_backtest$violations[1],
                                             nrow(crisis), 0.05)$p_value

# ---- rolling VaR and ES, backtests at 95% and 99% --------------------------
rolling_models <- c("historical", "normal", "ewma", "garch_t", "fhs")
violation_rows <- list()
es_rows <- list()
basel_rows <- list()
rollings <- list()

for (level in names(alphas)) {
  alpha <- alphas[[level]]
  roll <- rolling_var_es(portfolio, window = 250, alpha = alpha,
                         methods = rolling_models)
  rollings[[level]] <- roll

  wide <- data.frame(date = roll$dates, realised = roll$realised)
  for (method in rolling_models) {
    wide[[paste0("var_", method)]] <- roll$var[, method]
    wide[[paste0("es_", method)]] <- roll$es[, method]
  }
  write_result(wide, sprintf("rolling_var_%s.csv", level))

  for (method in rolling_models) {
    hits <- hit_sequence(roll$realised, roll$var[, method])
    uc <- kupiec_test(sum(hits), length(hits), alpha)
    cc <- christoffersen_cc(hits, alpha)
    violation_rows[[length(violation_rows) + 1]] <- data.frame(
      level = level, method = method, violations = sum(hits),
      n = length(hits), expected = alpha * length(hits),
      rate = mean(hits), kupiec_p = uc$p_value,
      christoffersen_ind_p = cc$independence$p_value,
      christoffersen_cc_p = cc$p_value, stringsAsFactors = FALSE
    )
    mf <- mcneil_frey_test(roll$realised, roll$var[, method], roll$es[, method])
    es_rows[[length(es_rows) + 1]] <- data.frame(
      level = level, method = method, n_exceptions = mf$n_exceptions,
      mean_residual = mf$mean_residual, p_value = mf$p_value,
      stringsAsFactors = FALSE
    )
    if (level == "99") {
      basel <- basel_traffic_light(hits, window = 250)
      basel_rows[[length(basel_rows) + 1]] <- data.frame(
        method = method, final_zone = as.character(basel$final),
        worst_zone = as.character(basel$worst), max_exceptions = max(basel$counts),
        stringsAsFactors = FALSE
      )
    }
  }
}

violations <- do.call(rbind, violation_rows)
write_result(violations, "rolling_violations.csv")
es_backtest <- do.call(rbind, es_rows)
write_result(es_backtest, "es_backtest.csv")
basel <- do.call(rbind, basel_rows)
write_result(basel, "basel_99.csv")

key[["roll_eval_start"]] <- format(min(rollings[["95"]]$dates))
key[["roll_eval_end"]] <- format(max(rollings[["95"]]$dates))
key[["roll_eval_n"]] <- length(rollings[["95"]]$dates)
for (method in rolling_models) {
  v95 <- violations[violations$level == "95" & violations$method == method, ]
  v99 <- violations[violations$level == "99" & violations$method == method, ]
  key[[paste0("roll95_viol_", method)]] <- v95$violations
  key[[paste0("roll99_viol_", method)]] <- v99$violations
}
key[["roll95_expected"]] <- 0.05 * key[["roll_eval_n"]]
key[["roll99_expected"]] <- 0.01 * key[["roll_eval_n"]]

# ---- stressed VaR ----------------------------------------------------------
stressed_rows <- list()
for (level in names(alphas)) {
  alpha <- alphas[[level]]
  sv <- stressed_var(portfolio, window = 250, alpha = alpha)
  precrisis_v <- var_historical(precrisis, alpha)
  stressed_rows[[length(stressed_rows) + 1]] <- data.frame(
    level = level, stressed_var = sv$stressed_var, precrisis_var = precrisis_v,
    ratio = sv$stressed_var / precrisis_v,
    window_start = format(sv$window_start), window_end = format(sv$window_end),
    stringsAsFactors = FALSE
  )
  if (level == "95") {
    key[["stressed_var_95"]] <- sv$stressed_var
    key[["stressed_ratio_95"]] <- sv$stressed_var / precrisis_v
    key[["stressed_window_start"]] <- format(sv$window_start)
    key[["stressed_window_end"]] <- format(sv$window_end)
    stressed_series_95 <- sv
  }
}
stressed <- do.call(rbind, stressed_rows)
write_result(stressed, "stressed_var.csv")

# ---- dependence: copulas before and during ---------------------------------
financials <- financial_tickers()
log_fin <- log_ret[, financials]
before <- copula_analysis(window_range(log_fin, precrisis_start, precrisis_end))
during <- copula_analysis(window_range(log_fin, crisis_start, crisis_end))
write_result(before$summary, "copulas_before.csv")
write_result(during$summary, "copulas_during.csv")
key[["copula_df_t_before"]] <- before$df_t
key[["copula_df_t_during"]] <- during$df_t
key[["copula_t_loglik_during"]] <- during$summary$loglik[during$summary$copula == "t"]
key[["copula_gauss_loglik_during"]] <- during$summary$loglik[during$summary$copula == "Gaussian"]
key[["copula_t_lambda_during"]] <- during$summary$lambda_lower[during$summary$copula == "t"]

# ---- correlations and diversification --------------------------------------
cor_before <- window_range(log_ret, precrisis_start, precrisis_end)
cor_during <- window_range(log_ret, crisis_start, crisis_end)
avg_corr <- function(returns) {
  correlation <- stats::cor(as.matrix(returns))
  mean(correlation[upper.tri(correlation)])
}
correlation_summary <- data.frame(
  period = c("before", "during"),
  avg_corr_all = c(avg_corr(cor_before), avg_corr(cor_during)),
  avg_corr_financials = c(avg_corr(cor_before[, financials]),
                          avg_corr(cor_during[, financials])),
  stringsAsFactors = FALSE
)
write_result(correlation_summary, "correlation_summary.csv")
key[["avg_corr_all_before"]] <- correlation_summary$avg_corr_all[1]
key[["avg_corr_all_during"]] <- correlation_summary$avg_corr_all[2]

div_before <- diversification(window_range(returns, precrisis_start, precrisis_end),
                              precrisis, alpha = 0.05)
div_during <- diversification(window_range(returns, crisis_start, crisis_end),
                              crisis, alpha = 0.05)
diversification_windows <- data.frame(
  period = c("before", "during"),
  portfolio_var = c(div_before$portfolio_var, div_during$portfolio_var),
  sum_standalone = c(div_before$sum_standalone, div_during$sum_standalone),
  benefit = c(div_before$diversification_benefit,
              div_during$diversification_benefit),
  stringsAsFactors = FALSE
)
write_result(diversification_windows, "diversification_windows.csv")
key[["div_benefit_before"]] <- div_before$diversification_benefit
key[["div_benefit_during"]] <- div_during$diversification_benefit

div_roll <- rolling_diversification(returns, portfolio, window = 250, alpha = 0.05)
roll_corr <- rolling_average_correlation(log_ret, width = 60)

# ---- key numbers -----------------------------------------------------------
key_frame <- data.frame(name = names(key),
                        value = vapply(key, function(x) as.character(x), character(1)),
                        stringsAsFactors = FALSE)
write_result(key_frame, "key_numbers.csv")

# ---- figures ---------------------------------------------------------------
save_fig(plot_price_panels(prices), "fig_prices.png", height = 6)
save_fig(plot_return_panels(log_ret), "fig_logreturns.png", height = 6)
save_fig(plot_return_distribution(precrisis,
                                  list(var_hist = key[["static_var_hist"]],
                                       var_norm = key[["static_var_norm"]],
                                       es_hist = key[["static_es_hist"]],
                                       es_norm = key[["static_es_norm"]])),
         "fig_return_distribution.png")
save_fig(plot_loss_vs_var(zoo::index(crisis), crisis,
                          rep(key[["static_var_hist"]], nrow(crisis))),
         "fig_loss_vs_var_static.png")
save_fig(plot_loss_vs_var(rollings[["99"]]$dates, rollings[["99"]]$realised,
                          rollings[["99"]]$var[, "historical"]),
         "fig_loss_vs_var_rolling.png")
save_fig(plot_rolling_var(rollings[["95"]]), "fig_rolling_var_95.png")
save_fig(plot_rolling_var(rollings[["99"]]), "fig_rolling_var_99.png")
save_fig(plot_rolling_correlation(roll_corr), "fig_rolling_correlation.png")
save_fig(plot_diversification(div_roll), "fig_diversification.png")
save_fig(plot_correlation_heatmap(cor_before), "fig_corr_before.png", width = 5,
         height = 4.5)
save_fig(plot_correlation_heatmap(cor_during), "fig_corr_during.png", width = 5,
         height = 4.5)
save_fig(plot_financial_pairs(during$pseudo_obs), "fig_financial_pairs.png",
         width = 6, height = 6)
save_fig(plot_stressed_var(stressed_series_95, var_historical(precrisis, 0.05)),
         "fig_stressed_var.png")
save_fig(plot_cumulative_return(portfolio), "fig_cumulative_return.png")
basel_hist_hits <- hit_sequence(rollings[["99"]]$realised,
                                rollings[["99"]]$var[, "historical"])
basel_hist <- basel_traffic_light(basel_hist_hits, window = 250)
basel_dates <- rollings[["99"]]$dates[250:length(rollings[["99"]]$dates)]
save_fig(plot_basel(basel_dates, basel_hist$counts), "fig_basel_99.png")

cat("Study complete. Results in", results_dir, "and figures in", figures_dir, "\n")
