# An-Economic-Analysis-of-Market-Indicators-and-Sales-Forecasting
An econometric analysis evaluating the predictive power of macroeconomic variables and financial market indicators on firm-level sales performance using R. Features comprehensive CLRM diagnostic testing and robust standard error adjustments.

## Key Findings & Structural Insights
* The "Accounting Exercise" Paradox: The baseline multiple linear regression model explains **97% of the variance** in sales Adjusted R^2 = 0.97 with an MSE approx 28,537.21 . However, this near-perfect fit is driven almost entirely by the `Competitor_sales` variable approx 1.626, p < 2e-16).
* Macro & Market Insignificance: When controlling for concurrent competitor metrics, traditional macroeconomic variables (`gdp_growth`, `unemployment_rate`, `inflation_rate`) and general financial indexes (`market_indicator_1`, `market_indicator_2`) yield statistically non-significant coefficients (p > 0.05).
* Model Specification Limitation: The extreme dominance of competitor metrics suggests that the variable acts as a direct structural proxy for overall market capacity, potentially deflating and obscuring the subtle, non-linear transmission channels of broader economic shifts.

## CLRM Diagnostics Summary
The model successfully undergoes validation across all key Gauss-Markov / CLRM assumptions using formal statistical tests and visual diagnostics:

| CLRM Assumption Check | Test / Method Employed | Statistical Outcome | Status |

| Linearity | Residuals vs. Fitted Plots | Random scatter around a flat line at 0 |  Satisfied |
| No Autocorrelation | Durbin-Watson & Breusch-Godfrey (4 Lags) | DW = 2.0249 (p = 0.6542), BG p = 0.4085 |  Satisfied |
| Homoscedasticity | Studentized Breusch-Pagan & NCV Score Test | BP-p = 0.1806, NCV_p = 0.1245 | Satisfied |
| Normality of Errors| Shapiro-Wilk Test & Quantile-Quantile Plot | W = 0.9976 (p = 0.1628), linear Q-Q distribution | Satisfied |
| Multicollinearity | Variance Inflation Factors (VIF) | All values approx 1.00 (well below the critical threshold of 5.0) | Satisfied |

*Note: In the final analytical stages, heteroscedasticity-robust covariance matrix estimations (`HC1` & `HC3`) were applied to guarantee that the standard errors and p-values remained completely trustworthy under alternative data shapes.*

## Tech Stack & R Packages Used
The analysis was executed entirely using the R statistical computing framework. Core packages utilized include:
* `readxl` – Spreadsheet data ingestion
* `dplyr` & `tidyr` – Data wrangling, pivoting, and structural cleaning
* `psych` – Descriptive summary statistics (skewness and kurtosis checks)
* `ggplot2` & `ggfortify` – Layered exploratory plots and automated four-panel regression diagnostics
* `car` & `lmtest` – VIF verification, Durbin-Watson, Breusch-Godfrey, and Breusch-Pagan scoring
* `sandwich` – Calculation of heteroscedasticity-robust standard errors

## Repository File Structure
* `Code.R`: The core executable script compiling data loading, data cleaning (`na.omit`), univariate distribution checks, bivariate scatter plotting with line-of-best-fit overlays, OLS modeling, metric extractions, and diagnostic evaluations.
* `simulated_financial_forecasting_data (1) (1).xlsx`: The foundational master dataset consisting of 1,000 observations mapping firm sales alongside target market indices and macroeconomic predictors.

---
