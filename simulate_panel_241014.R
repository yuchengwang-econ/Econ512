
rm(list=ls())
library(data.table)
library(ggplot2)

# one-input production function
# revenue = exp(tfp) * f(labor) = exp(tfp) * labor^alpha
# labor is selected on tfp: labor = gamma * exp(tfp)
# tfp has MA(1) process, tfp_t = eta_t + delta * epsilon_{t-1}
# also add a measurement error that is not endogenous

# simulate
set.seed(42)
N <- 1000
T <- 10
delta <- 0.5
gamma <- 0.3
alpha <- 0.8

# create a dataset
production_data <- data.table(expand.grid(firm_ID = 1:N, year = 1:T))
production_data <- production_data[, eta_current := rnorm(N*T, mean = 0, sd = 1)]
eta_data <- production_data[, list(firm_ID, year = year+1, eta_lag = eta_current)]
production_data <- merge(production_data, eta_data, by = c("firm_ID", "year"), all.x = TRUE)
production_data[year == 1, eta_lag := rnorm(N, mean = 0, sd = 1)] # fill the lagged eta at t=1
production_data[, TFP := eta_current + delta * eta_lag]

# create labor variable with measurement error
production_data[, labor := gamma * exp(TFP) + exp(rnorm(N*T, mean = 0, sd = 0.01))]
production_data[, measurement_error := exp(rnorm(N*T, mean = 0, sd = 1))]
production_data[, revenue := exp(TFP)*labor^alpha * measurement_error]
production_data[, logrevenue := log(revenue)]
production_data[, loglabor := log(labor)]
# in real data only firm_ID, year, labor and revenue are observable

# form the second lag of loglabor
labor_lags = production_data[, list(firm_ID, year = year + 2, loglabor_lag2 = loglabor)]
production_data = merge(production_data, labor_lags, by = c("firm_ID", "year"), all.x = TRUE)

# estimate alpha by OLS
ols_result <- lm(logrevenue ~ loglabor, data = production_data)

# construct predicted TFP for a given guess of alpha
moment_fit <- function(alpha_guess, dataset){
  dataset[, TFP_hat := logrevenue - alpha_guess * loglabor]
  moment = dataset[!is.na(loglabor_lag2), mean(TFP_hat * loglabor_lag2)]
  return(moment^2)
}

# plot the moment_fit across alpha_guess from -1 to 2


print(moment_fit(0.8, production_data))
