library(parallel)
library(zoo)
library(readr)
library(rugarch)
library(xts)

quakefile <- readr::read_csv(
  "Desktop/Winter 25/ECON 423/Project/code_space/quakefile.csv",
  col_types = readr::cols(
    date = readr::col_date(format = "%Y-%m-%d"),
    earthquake_dummy = readr::col_integer()
  )
)
# Assuming your existing data is stored in a data frame named quakefile
quakefile$earthquake_5day_dummy <- 0

# Find the row indices where the earthquake occurred
quake_indices <- which(quakefile$earthquake_dummy == 1)

# Set 1 for the earthquake day and next 4 days
for (i in quake_indices) {
  quakefile$earthquake_5day_dummy[i:min(i+4, nrow(quakefile))] <- 1
}

# View result
head(quakefile, 10)

df <- quakefile
df$date <- as.Date(df$date)
returns_xts <- xts(df$returns, order.by = df$date)

spec_base <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
  mean.model = list(armaOrder = c(1, 0)),
  distribution.model = "norm"
)

spec_eq_dummy <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1,1), external.regressors = matrix(df$earthquake_dummy)),
  mean.model = list(armaOrder = c(1, 0)),
  distribution.model = "norm"
)


spec_5day_dummy <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1,1), external.regressors = matrix(df$earthquake_5day_dummy)),
  mean.model = list(armaOrder = c(1, 0)),
  distribution.model = "norm"
)

# AR(1) + GARCH(1,1) + t-distribution

spec_base_t <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
  mean.model = list(armaOrder = c(1, 0)),
  distribution.model = "std"
)

spec_eq_dummy_t <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1,1), external.regressors = matrix(df$earthquake_dummy)),
  mean.model = list(armaOrder = c(1, 0)),
  distribution.model = "std"
)

spec_5day_dummy_t <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1,1), external.regressors = matrix(df$earthquake_5day_dummy)),
  mean.model = list(armaOrder = c(1, 0)),
  distribution.model = "std"
)

spec_egarch_t <- ugarchspec(
  variance.model = list(model = "eGARCH", garchOrder = c(1,1), external.regressors = matrix(df$earthquake_dummy)),
  mean.model = list(armaOrder = c(1,0)),
  distribution.model = "std"
)

spec_gjr_t <- ugarchspec(
  variance.model = list(model = "gjrGARCH", garchOrder = c(1,1), external.regressors = matrix(df$earthquake_dummy)),
  mean.model = list(armaOrder = c(1,0)),
  distribution.model = "std"
)

fit_base <- ugarchfit(spec_base, data = returns_xts)
fit_eq_dummy <- ugarchfit(spec_eq_dummy, data = returns_xts)
fit_5day_dummy <- ugarchfit(spec_5day_dummy, data = returns_xts)

fit_base_t <- ugarchfit(spec_base_t, data = returns_xts)
fit_eq_dummy_t <- ugarchfit(spec_eq_dummy_t, data = returns_xts)
fit_5day_dummy_t <- ugarchfit(spec_5day_dummy_t, data = returns_xts)

fit_egarch_t <- ugarchfit(spec_egarch_t, data = returns_xts)
fit_gjr_t <- ugarchfit(spec_gjr_t, data = returns_xts)


# View summaries
show(fit_eq_dummy_t)  # Replace with any fit to inspect

# Get t-stats and p-values
coef(fit_eq_dummy_t)

show(fit_egarch_t)

spec_egarch_5day <- ugarchspec(
  variance.model = list(model = "eGARCH", garchOrder = c(1,1), external.regressors = matrix(df$earthquake_5day_dummy)),
  mean.model = list(armaOrder = c(1, 0)),
  distribution.model = "std"
)

fit_egarch_5day <- ugarchfit(spec_egarch_5day, data = returns_xts)
show(fit_egarch_5day)

# Create 2-day and 3-day dummies
df$earthquake_2day <- zoo::rollapply(df$earthquake_dummy, width = 2, FUN = max, align = "left", fill = 0)
df$earthquake_3day <- zoo::rollapply(df$earthquake_dummy, width = 3, FUN = max, align = "left", fill = 0)

# Initialize
df$earthquake_2day <- 0
df$earthquake_3day <- 0

# Loop to assign 1s
for (i in which(df$earthquake_dummy == 1)) {
  if (i <= nrow(df) - 1) df$earthquake_2day[i:(i+1)] <- 1
  if (i <= nrow(df) - 2) df$earthquake_3day[i:(i+2)] <- 1
}
# EGARCH with 2-day dummy
spec_egarch_2day <- ugarchspec(
  variance.model = list(model = "eGARCH", garchOrder = c(1,1), external.regressors = matrix(df$earthquake_2day)),
  mean.model = list(armaOrder = c(1, 0)),
  distribution.model = "std"
)
fit_egarch_2day <- ugarchfit(spec_egarch_2day, data = returns_xts)

# EGARCH with 3-day dummy
spec_egarch_3day <- ugarchspec(
  variance.model = list(model = "eGARCH", garchOrder = c(1,1), external.regressors = matrix(df$earthquake_3day)),
  mean.model = list(armaOrder = c(1, 0)),
  distribution.model = "std"
)
fit_egarch_3day <- ugarchfit(spec_egarch_3day, data = returns_xts)

show(fit_egarch_2day)
show(fit_egarch_3day)
