# Figure helpers. Each function returns a ggplot object; the study script saves
# them to figures/. Colours are colour-blind friendly and consistent across the
# report.

#' Shared minimal theme for the study figures.
study_theme <- function() {
  ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(face = "bold", size = 12),
      strip.text = ggplot2::element_text(face = "bold"),
      legend.position = "bottom",
      legend.title = ggplot2::element_blank()
    )
}

#' Dates of the three crisis landmarks used as reference lines.
crisis_dates <- function() {
  as.Date(c("2008-03-14", "2008-09-15", "2008-10-15"))
}

#' Dotted vertical lines at the crisis landmarks.
crisis_lines <- function() {
  ggplot2::geom_vline(xintercept = crisis_dates(), colour = "grey55",
                      linetype = "dotted", linewidth = 0.4)
}

#' Colour palette for the five rolling VaR methods.
method_palette <- function() {
  c(historical = "#1b9e77", normal = "#d95f02", ewma = "#7570b3",
    garch_t = "#e7298a", fhs = "#66a61e")
}

#' Readable labels for the methods.
method_labels <- function() {
  c(historical = "Historical", normal = "Normal", ewma = "EWMA",
    garch_t = "GARCH(1,1)-t", fhs = "Filtered HS")
}

#' Stack a wide xts into a long data frame (date, series, value).
xts_to_long <- function(data) {
  cols <- colnames(data)
  dates <- zoo::index(data)
  do.call(rbind, lapply(cols, function(column) {
    data.frame(date = dates, series = column,
               value = as.numeric(data[, column]), stringsAsFactors = FALSE)
  }))
}

#' Price panels, one per stock (Graph A).
plot_price_panels <- function(prices) {
  long <- xts_to_long(prices)
  long$series <- factor(company_names()[long$series],
                        levels = company_names()[study_tickers()])
  ggplot2::ggplot(long, ggplot2::aes(date, value)) +
    crisis_lines() +
    ggplot2::geom_line(linewidth = 0.3) +
    ggplot2::facet_wrap(~series, ncol = 2, scales = "free_y") +
    ggplot2::labs(x = NULL, y = NULL) +
    study_theme()
}

#' Log-return panels, one per stock (Graph B).
plot_return_panels <- function(returns) {
  long <- xts_to_long(returns)
  long$series <- factor(company_names()[long$series],
                        levels = company_names()[study_tickers()])
  ggplot2::ggplot(long, ggplot2::aes(date, value)) +
    ggplot2::geom_line(linewidth = 0.2, colour = "grey30") +
    ggplot2::facet_wrap(~series, ncol = 2, scales = "free_y") +
    ggplot2::labs(x = NULL, y = NULL) +
    study_theme()
}

#' Pre-crisis return distribution with VaR and ES lines (Graph C).
#'
#' The four measures are close together, so they are drawn as coloured dashed
#' lines with a legend rather than overlapping in-line labels.
plot_return_distribution <- function(returns, measures) {
  frame <- data.frame(ret = as.numeric(returns))
  levels <- c("VaR historical", "VaR normal", "ES historical", "ES normal")
  lines <- data.frame(
    value = -c(measures$var_hist, measures$var_norm, measures$es_hist,
               measures$es_norm),
    measure = factor(levels, levels = levels)
  )
  palette <- c("VaR historical" = "#1b9e77", "VaR normal" = "#7570b3",
               "ES historical" = "#d95f02", "ES normal" = "#e7298a")
  ggplot2::ggplot(frame, ggplot2::aes(ret)) +
    ggplot2::geom_histogram(bins = 60, fill = "grey80", colour = "grey55",
                            linewidth = 0.2) +
    ggplot2::geom_vline(data = lines,
                        ggplot2::aes(xintercept = value, colour = measure),
                        linetype = "dashed", linewidth = 0.7) +
    ggplot2::scale_colour_manual(values = palette) +
    ggplot2::labs(x = "Daily portfolio return", y = "Count", colour = NULL) +
    study_theme()
}

#' Daily loss against a VaR line with violations marked.
plot_loss_vs_var <- function(dates, realised, var, title = NULL) {
  frame <- data.frame(date = dates, loss = -as.numeric(realised),
                      var = as.numeric(var))
  frame$violation <- frame$loss > frame$var
  ggplot2::ggplot(frame, ggplot2::aes(date)) +
    crisis_lines() +
    ggplot2::geom_line(ggplot2::aes(y = loss), colour = "grey60",
                       linewidth = 0.3) +
    ggplot2::geom_line(ggplot2::aes(y = var), colour = "#1f78b4",
                       linewidth = 0.6) +
    ggplot2::geom_point(data = frame[frame$violation, ],
                        ggplot2::aes(y = loss), colour = "#e31a1c", size = 1) +
    ggplot2::labs(x = NULL, y = "Daily loss", title = title) +
    study_theme()
}

#' Rolling VaR by method at one confidence level.
plot_rolling_var <- function(rolling) {
  frame <- data.frame(date = rep(rolling$dates, ncol(rolling$var)),
                      method = rep(colnames(rolling$var), each = length(rolling$dates)),
                      var = as.numeric(rolling$var))
  frame <- frame[frame$method %in% names(method_palette()), ]
  ggplot2::ggplot(frame, ggplot2::aes(date, var, colour = method)) +
    crisis_lines() +
    ggplot2::geom_line(linewidth = 0.4) +
    ggplot2::scale_colour_manual(values = method_palette(),
                                 labels = method_labels()) +
    ggplot2::labs(x = NULL, y = "One-day VaR") +
    study_theme()
}

#' Rolling average pairwise correlation.
plot_rolling_correlation <- function(correlation) {
  frame <- data.frame(date = zoo::index(correlation),
                      value = as.numeric(correlation))
  frame <- frame[!is.na(frame$value), ]
  ggplot2::ggplot(frame, ggplot2::aes(date, value)) +
    crisis_lines() +
    ggplot2::geom_line(linewidth = 0.5, colour = "#1f78b4") +
    ggplot2::labs(x = NULL, y = "Average pairwise correlation") +
    study_theme()
}

#' Rolling portfolio VaR against the summed stand-alone VaRs.
plot_diversification <- function(div) {
  frame <- rbind(
    data.frame(date = div$dates, value = div$portfolio_var,
               series = "Portfolio VaR"),
    data.frame(date = div$dates, value = div$sum_standalone,
               series = "Sum of stand-alone VaRs")
  )
  ggplot2::ggplot(frame, ggplot2::aes(date, value, colour = series)) +
    crisis_lines() +
    ggplot2::geom_line(linewidth = 0.5) +
    ggplot2::scale_colour_manual(values = c("Portfolio VaR" = "#1f78b4",
                                            "Sum of stand-alone VaRs" = "#e31a1c")) +
    ggplot2::labs(x = NULL, y = "One-day VaR") +
    study_theme()
}

#' Correlation heatmap of the stock log returns for one period.
plot_correlation_heatmap <- function(returns) {
  GGally::ggcorr(as.data.frame(returns), palette = "RdYlBu", name = "rho",
                 label = TRUE, label_size = 2.6, label_round = 2,
                 hjust = 0.8, size = 3) +
    study_theme() +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank())
}

#' Scatter-matrix of the financial stocks' pseudo-observations.
plot_financial_pairs <- function(pseudo_obs) {
  GGally::ggpairs(as.data.frame(pseudo_obs),
                  upper = list(continuous = GGally::wrap("cor", size = 3)),
                  lower = list(continuous = GGally::wrap("points", alpha = 0.3,
                                                         size = 0.4))) +
    study_theme()
}

#' Rolling one-year historical VaR with the pre-crisis level marked.
plot_stressed_var <- function(stressed, precrisis_var) {
  frame <- data.frame(date = zoo::index(stressed$series),
                      value = as.numeric(stressed$series))
  ggplot2::ggplot(frame, ggplot2::aes(date, value)) +
    crisis_lines() +
    ggplot2::geom_line(linewidth = 0.5, colour = "#1f78b4") +
    ggplot2::geom_hline(yintercept = precrisis_var, linetype = "dashed",
                        colour = "grey30") +
    ggplot2::annotate("text", x = min(frame$date), y = precrisis_var,
                      label = "Pre-crisis VaR", hjust = 0, vjust = -0.5,
                      size = 2.8, colour = "grey30") +
    ggplot2::labs(x = NULL, y = "One-year rolling 95% VaR") +
    study_theme()
}

#' Cumulative portfolio return (appendix).
plot_cumulative_return <- function(portfolio) {
  cumulative <- cumprod(1 + as.numeric(portfolio)) - 1
  frame <- data.frame(date = zoo::index(portfolio), value = cumulative)
  ggplot2::ggplot(frame, ggplot2::aes(date, value)) +
    crisis_lines() +
    ggplot2::geom_line(linewidth = 0.5) +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::labs(x = NULL, y = "Cumulative return") +
    study_theme()
}

#' Basel exception counts over rolling 250-day windows with zone bands.
plot_basel <- function(dates, counts) {
  frame <- data.frame(date = dates, counts = counts)
  ggplot2::ggplot(frame, ggplot2::aes(date, counts)) +
    ggplot2::annotate("rect", xmin = min(dates), xmax = max(dates),
                      ymin = -Inf, ymax = 4.5, fill = "#1b9e77", alpha = 0.12) +
    ggplot2::annotate("rect", xmin = min(dates), xmax = max(dates),
                      ymin = 4.5, ymax = 9.5, fill = "#e6ab02", alpha = 0.15) +
    ggplot2::annotate("rect", xmin = min(dates), xmax = max(dates),
                      ymin = 9.5, ymax = Inf, fill = "#e31a1c", alpha = 0.12) +
    ggplot2::geom_line(linewidth = 0.5) +
    ggplot2::labs(x = NULL, y = "Exceptions in trailing 250 days") +
    study_theme()
}
