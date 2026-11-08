# Market risk of ten S&P 500 stocks before and during the 2007-09 crisis

Course project in risk management, London School of Economics, summer 2018.
Report: [LSE Project Report.pdf](LSE%20Project%20Report.pdf). Code: [LSE Project.R](LSE%20Project.R).

## Question

How well did standard value-at-risk models, estimated on the calm period before the financial crisis, describe the risk that followed? And how did the dependence between bank stocks change?

## Data and portfolio

| Item | Detail |
|---|---|
| Stocks | Coca-Cola, ExxonMobil, General Electric, Johnson & Johnson, Disney, Intel and four financials (Citigroup, Goldman Sachs, Morgan Stanley and one further bank; see limitations) |
| Prices | Daily, 1 July 2006 to 30 June 2009, from `stocks.csv` (not included; source not recorded) |
| Portfolio | $1,000 in each stock at the start, then buy and hold |
| Estimation window | 1 July 2006 to 31 December 2007 |
| Backtest window | 1 January 2008 to 30 June 2009 (377 trading days) |

## Method

- **Value at risk and expected shortfall** of daily portfolio returns at the 95% level, both historical and Gaussian, estimated once on the pre-crisis window (`PerformanceAnalytics`).
- **Backtest:** count the days in the crisis window on which the portfolio loss exceeded the pre-crisis VaR.
- **Correlation:** compare return correlation matrices before and during the crisis.
- **Copulas:** fit t, Gaussian and Clayton copulas to the pseudo-observations (ranks) of the four financial stocks' returns (`QRM`) and compare maximised log-likelihoods.

## Results

| Measure (95%, daily) | Historical | Gaussian |
|---|---|---|
| VaR, pre-crisis | 1.61% | 1.66% |
| Expected shortfall, pre-crisis | 2.50% | 2.08% |
| VaR violations in the crisis window | 86 of 377 (22.8%) | 83 of 377 (22.0%) |
| Expected violations | about 19 (5%) | about 19 (5%) |

A Kupiec proportion-of-failures test rejects the historical VaR decisively (likelihood ratio 140, p < 1e-30). This test was computed for this README and is not in the original report.

| Copula (four financials) | t | Gaussian | Clayton |
|---|---|---|---|
| Maximised log-likelihood | 1,802.0 | 1,009.5 | 874.3 |

The t copula fits far better. It has only one more parameter than the Gaussian copula, so the gap cannot be explained by model size. The dependence between the bank stocks is strongest in the joint tails, which is exactly what a correlation matrix or a Gaussian model misses.

## Takeaways

- With the VaR estimated on a low-volatility window, losses in the crisis exceeded it more than four times as often as the model allows. This is the problem that led to stressed VaR under Basel 2.5.
- Correlations between the stocks rose sharply during the crisis, which reduced the benefit of diversification when it was needed most.
- Tail dependence among the financials argues for t or other heavy-tailed copulas in portfolio risk models.

## Limitations

- The VaR is static. It is estimated once and never updated, which is the point of the exercise but not how a risk system works. A rolling or GARCH-filtered VaR would be the natural comparison.
- The copulas are fitted to raw return ranks, not to residuals after removing volatility clustering.
- Ticker labels in the code are inconsistent: one price column is named JPM while the return columns and the report refer to Wells Fargo. The fourth financial is therefore ambiguous.
- The price file is not included, so the script cannot be re-run from this repository.

## Reproducing

R with packages `QRM`, `xts`, `PerformanceAnalytics`, `ggplot2` and `GGally`. Place `stocks.csv` (long format: ticker, date as YYYYMMDD, price) next to the script and update the `setwd()` path.
