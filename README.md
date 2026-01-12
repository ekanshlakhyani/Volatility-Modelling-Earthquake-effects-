#Read Me
##Do Earthquakes Affect Stock Market Volatility? A GARCH-based Approach
Research Project
- Built and estimated a series of GARCH-family models (GARCH, EGARCH, GJR-GARCH) to investigate whether U.S. earthquakes influence S&P 500 volatility.
- Introduced exogenous earthquake dummy variables in the conditional variance equation and used a Student’s t-distribution to better capture fat tails.
- Found a statistically significant increase in volatility on earthquake days, with the effect strongest on the same day and diminishing over a 5-day window.
- Final model: EGARCH(1,1)-t with AR(1) mean equation and exogenous volatility shock, validated through extensive residual diagnostics and robustness tests.

Tools: Python (arch_model, pandas, numpy, statsmodels),R (rugarch, zoo, xts), LaTeX (for full academic documentation), and daily financial + geophysical data from 2010–2024.
