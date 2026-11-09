# Market risk of ten S&P 500 stocks before and during the 2007-09 crisis

How well did standard value-at-risk models, estimated on the calm period before the financial crisis, describe the risk that followed, and how did the dependence between bank stocks change? This project measures the market risk of an equal-weight, buy-and-hold portfolio of ten S&P 500 stocks, compares five VaR models through the crisis, and fits copulas to the dependence between the four financials. It began as a course project in risk management at the London School of Economics and has since been extended.

The report is [LSE Project Report.pdf](LSE%20Project%20Report.pdf). Every number, table and figure in it is produced by the code here, from public data.

## Data and portfolio

| Item | Detail |
|---|---|
| Stocks | Coca-Cola, ExxonMobil, General Electric, Johnson & Johnson, Disney, Intel, and four financials: JPMorgan Chase, Citigroup, Morgan Stanley, Goldman Sachs |
| Prices | Daily split-adjusted closes, 2004 to 2010, from Yahoo Finance via `quantmod`; cached in `data/` with the download date |
| Portfolio | Equal amounts in each stock on 3 July 2006, then buy and hold to 30 June 2009 |
| Estimation window | 1 July 2006 to 31 December 2007 |
| Backtest window | 1 January 2008 to 30 June 2009 (377 trading days) |

The portfolio was first built from a private price file that keyed stocks on CRSP identifiers. One financial was mislabelled in the original code; matching its price path against the public series identifies it as JPMorgan Chase, which is the label used here. The public prices reproduce the original pre-crisis VaR to the digit, so the study reads the same on the data anyone can download.

## Method

- **Static VaR and ES** at 95% on the pre-crisis window, by historical simulation and under a normal model, then backtested unchanged through the crisis.
- **Rolling VaR and ES** at 95% and 99% on a 250-day window, re-estimated daily by five methods: historical simulation, normal, EWMA (RiskMetrics), GARCH(1,1) with Student-t innovations, and filtered historical simulation.
- **Backtests:** Kupiec unconditional coverage, Christoffersen independence and conditional coverage, the Basel traffic light for the 99% VaR, and a McNeil-Frey test of the expected shortfall.
- **Stressed VaR** on the worst twelve-month window, against the pre-crisis level.
- **Dependence:** t, Gaussian, Clayton and Gumbel copulas fitted to the GARCH-standardised residuals of the four financials, before and during the crisis, with log-likelihood, AIC and tail-dependence coefficients; a rolling correlation; and the portfolio VaR against the sum of the stand-alone VaRs.

## Results

A VaR estimated once on the calm pre-crisis window badly understated the risk of the crisis.

| Measure (95%, daily) | Historical | Normal |
|---|---|---|
| VaR, pre-crisis | 1.61% | 1.66% |
| Expected shortfall, pre-crisis | 2.51% | 2.09% |
| VaR exceptions in the crisis window | 86 of 377 (22.8%) | 83 of 377 (22.0%) |
| Expected exceptions | about 19 (5%) | about 19 (5%) |

Re-estimating volatility every day cut the exceptions by about half. At 99%, where about 5 exceptions were expected over the 503-day evaluation period, the counts were 19 (historical), 23 (normal), 12 (EWMA), 11 (GARCH(1,1)-t) and 14 (filtered historical simulation). The historical and normal VaR reached the Basel red zone during the crisis; the dynamic models stayed in the yellow zone. No model passed the Kupiec and Christoffersen tests cleanly. The stressed 95% VaR, 5.71%, is about 3.5 times the pre-crisis figure.

The average pairwise correlation across the ten stocks rose from 0.48 before the crisis to 0.56 during it, so the portfolio's undiversifiable risk grew when diversification was most wanted. The t copula fits the dependence between the financials best in both periods, and its advantage widens in the crisis (log-likelihood 642.6 against 588.8 for the Gaussian copula). Its fitted degrees of freedom fall from 6.7 to 5.1 and its tail-dependence coefficient rises to 0.42: the banks' losses arrived together, in the joint tail that a correlation matrix or a Gaussian model cannot see.

## Reproducing

The analysis runs in R. Install the packages in `DESCRIPTION` (`xts`, `zoo`, `PerformanceAnalytics`, `copula`, `ggplot2`, `GGally`, `scales`, and `quantmod` for the download; `rmarkdown` and `knitr` for the report). Then, from the repository root:

```bash
Rscript scripts/00_download_data.R   # refresh the cached public prices
Rscript scripts/run_study.R          # write results/ and figures/
Rscript tests/testthat.R             # run the tests
Rscript scripts/build_report.R       # render the report to Word and PDF
```

The study reads the cached prices in `data/`, so it runs offline; re-run the download script to refresh them. Building the PDF needs LibreOffice for the Word-to-PDF step.

## Repository layout

```
R/            functions: data, VaR/ES, backtests, copulas, analysis, plots
tests/        testthat tests
scripts/      download, run the study, build the report
data/         cached public prices and the download date
results/      every number the report cites, as CSV
figures/      every figure, as PNG
report/       report.Rmd and the Word style reference
```

Tests also run on every push through GitHub Actions (`.github/workflows/ci.yaml`).
