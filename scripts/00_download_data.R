#!/usr/bin/env Rscript
# Download daily prices for the ten study stocks from a free public source
# (Yahoo Finance via quantmod) and cache them under data/ so the study and the
# tests run offline and reproducibly. Re-run to refresh; the download date is
# written to data/DOWNLOAD_INFO.txt.

suppressMessages({
  library(quantmod)
  library(xts)
})

tickers <- c("KO", "XOM", "GE", "JNJ", "DIS", "JPM", "INTC", "MS", "C", "GS")
from_date <- "2004-01-01"
to_date <- "2010-12-31"
source_name <- "Yahoo Finance (via quantmod::getSymbols, src = 'yahoo')"

get_script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) == 1L) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg))))
  }
  getwd()
}

repo_root <- normalizePath(file.path(get_script_dir(), ".."))
data_dir <- file.path(repo_root, "data")
dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)

fetch_one <- function(symbol, from, to, tries = 3L) {
  for (attempt in seq_len(tries)) {
    result <- tryCatch(
      quantmod::getSymbols(symbol, src = "yahoo", from = from, to = to,
                           auto.assign = FALSE),
      error = function(e) e
    )
    if (!inherits(result, "error")) {
      return(result)
    }
    message(sprintf("  attempt %d for %s failed: %s",
                    attempt, symbol, conditionMessage(result)))
    Sys.sleep(2 * attempt)
  }
  stop(sprintf("Could not download %s after %d attempts.", symbol, tries))
}

adjusted_list <- list()
close_list <- list()
for (symbol in tickers) {
  message("Downloading ", symbol)
  series <- fetch_one(symbol, from_date, to_date)
  adjusted <- Ad(series)
  close <- Cl(series)
  colnames(adjusted) <- symbol
  colnames(close) <- symbol
  adjusted_list[[symbol]] <- adjusted
  close_list[[symbol]] <- close
  Sys.sleep(1)
}

merge_wide <- function(series_list) {
  Reduce(function(left, right) merge(left, right, join = "outer"), series_list)
}

adjusted_wide <- merge_wide(adjusted_list)
close_wide <- merge_wide(close_list)

write_wide <- function(series, path) {
  frame <- data.frame(Date = format(index(series), "%Y-%m-%d"),
                      coredata(series), check.names = FALSE)
  write.csv(frame, path, row.names = FALSE)
  message("Wrote ", path, " (", nrow(frame), " rows)")
}

write_wide(adjusted_wide, file.path(data_dir, "prices_public.csv"))
write_wide(close_wide, file.path(data_dir, "prices_public_close.csv"))

info <- c(
  paste("Source:", source_name),
  paste("Downloaded:", format(Sys.Date(), "%Y-%m-%d")),
  paste("Range requested:", from_date, "to", to_date),
  paste("Tickers:", paste(tickers, collapse = ", ")),
  "",
  "prices_public.csv      split- and dividend-adjusted close (total-return basis).",
  "prices_public_close.csv split-adjusted close, not dividend-adjusted.",
  "",
  "Citigroup trades under ticker C. Adjusted levels reflect its 2011 reverse",
  "split, which does not affect daily returns."
)
writeLines(info, file.path(data_dir, "DOWNLOAD_INFO.txt"))
message("Done.")
