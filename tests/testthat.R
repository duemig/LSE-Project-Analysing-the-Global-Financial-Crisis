#!/usr/bin/env Rscript
# Test entry point. Loads the function library from R/ and runs the testthat
# suite. Run with: Rscript tests/testthat.R
library(testthat)
suppressMessages({
  library(xts)
  library(zoo)
  library(PerformanceAnalytics)
  library(copula)
})

args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", grep("^--file=", args, value = TRUE))
root <- if (length(file_arg) == 1L) {
  normalizePath(file.path(dirname(file_arg), ".."))
} else {
  getwd()
}

for (file in list.files(file.path(root, "R"), pattern = "[.]R$", full.names = TRUE)) {
  source(file)
}

test_dir(file.path(root, "tests", "testthat"), reporter = "summary",
         stop_on_failure = TRUE)
