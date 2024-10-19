#################################
## Question 1
#################################

rm(list=ls())
library(dplyr)
library(data.table)

## Question 1(a)
## Extract data
setwd("C:/Users/Yucheng Wang/Desktop/Study/Econ543 IO/PS2")
product_dataset <- fread("product_data_ps2.csv", header = TRUE, sep = ",")
census_dataset <- fread("consumer_census.csv", header = TRUE, sep = ",")
T_market <- 50
J_product <- 50

## Compute ybar, sigma_y and pbar
average_income <- census_dataset[, .(mean = mean(income)), by = t]
variance_income <- census_dataset[, .(var = var(income)), by = t]
product_dataset[, sales := price * market_share]
average_price_paid <- product_dataset[, .(sum = sum(sales)), by = t]

## Compute correlation between ybar and pbar
print(cor(x = average_income$mean, y =average_price_paid$sum))

## Question 1(b)
draw_consumers <- function(income_list, S, seed){
  set.seed(seed)
  income <- matrix(sample(income_list, S, replace = TRUE), nrow = S, ncol = 1)
  v <- matrix(rnorm(S * 2), nrow = S, ncol = 2)
  draws <- cbind(income, v)
  return(draws)
}

## Draw 50 samples of size 100
sample_number <- 50
sample_size <- 100
sample_list <- vector("list", sample_number)
seed_list <- seq(101, 150)
## Draw samples using the distribution of incomes at t=1
for (i in 1:sample_number){
  sample_list[[i]] <- draw_consumers(census_dataset[t==1, income], sample_size, seed_list[i])
}
## Compute mean and variance of each column in each sample
sample_mean_table <- matrix(NA, nrow = sample_number, ncol=3)
sample_variance_table <- matrix(NA, nrow = sample_number, ncol=3)
for (i in 1:sample_number){
  sample_mean_table[i, ] <- colMeans(sample_list[[i]])
  sample_variance_table[i, ] <- apply(sample_list[[i]], 2, var)
}
sample_mean_variance_table <- cbind(sample_mean_table, sample_variance_table)

## Draw samples of size 500 for each t
seed <- 42
sample_size <- 500
sample_list <- vector("list", T_market)
for (tau in 1:T_market){
  matrix <- draw_consumers(census_dataset[t==tau, income], sample_size, seed)
  matrix <- cbind(matrix, SourceMatrix = tau)
  colnames(matrix) <- c("income", "v1", "v2", "t")
  sample_list[[tau]] <- matrix
}
consumer_sample <- data.table(do.call(rbind, sample_list))

## Question 1(c)
## Compute market share as function of delta=(delta_1t,...,delta_50t) and theta=(alpha^y,sigma1,sigma2)
compute_consumer_share <- function(delta_list, theta_list, tau){
  u_matrix <- data.table()
  for (j in 1:J_product){
    col_name <- paste0("u", j)
    ## Compute u_ijt=mu_ijt+delta_jt
    u_matrix <- u_matrix[, (col_name) := theta_list[1] * product_dataset[t==tau & product_ID==j, price] * consumer_sample[t==tau, income]
                         + theta_list[2] * product_dataset[t==tau & product_ID==j, sugar] * consumer_sample[t==tau, v1]
                         + theta_list[3] * product_dataset[t==tau & product_ID==j, caffeine] * consumer_sample[t==tau, v2]
                         + delta_list[j]]
  }
  ## Compute IncVal_it
  u_matrix <- u_matrix[, IncVal := log(rowSums(exp(.SD))+1), .SDcols = patterns("^u")]
  ## Compute s_ijt
  for (j in 1:J_product){
    target_col_name <- paste0("u", j)
    col_name <- paste0("s", j)
    u_matrix <- u_matrix[, (col_name) := exp(get(target_col_name) - IncVal)]
  }
  s_matrix <- u_matrix[, .SD, .SDcols = patterns("^s")]
  return(s_matrix)
}

## Question 1(d)
## Given mean parameters lambda=(alpha, beta_1, beta_2, gamma), compute delta for each jt
lambda_example <- c(-1, 0.5, 0.5, -1.5)
product_dataset <- product_dataset[, delta_mean := lambda_example[1] * price + lambda_example[2] * sugar + lambda_example[3] * caffeine + lambda_example[4]]
## Given theta vectors, compute market shares
theta_example1 <- c(0,0,0)
market_share_example1_list <- vector("list", T_market)
for (tau in 1:T_market){
  market_share_example1_list[[tau]] <- compute_consumer_share(product_dataset[t==tau, delta_mean], theta_example1, tau)
}
market_share_example1 <- data.table(do.call(rbind, market_share_example1_list))
theta_example2 <- c(0,1,1)
market_share_example2_list <- vector("list", T_market)
for (tau in 1:T_market){
  market_share_example2_list[[tau]] <- compute_consumer_share(product_dataset[t==tau, delta_mean], theta_example2, tau)
}
market_share_example2 <- data.table(do.call(rbind, market_share_example2_list))

## Question 1(e)
compute_market_share <- function(delta_list, theta_list, tau){
  consumer_share <- compute_consumer_share(delta_list, theta_list, tau)
  market_share <- consumer_share[, lapply(.SD, mean)]
  return(market_share)
}
## Create market share table
market_share_table <- data.table(matrix(0, nrow = T_market, ncol = J_product))
setnames(market_share_table, paste0("s", 1:J_product))
## Compute market shares
for (tau in 1:T_market){
  market_share_table[tau, ] <- compute_market_share(product_dataset[t==tau, delta_mean], theta_example2, tau)
  print(c(tau))
}
## Reshape the market share table to accommodate format in product_dataset
market_share_table_reshaped <- melt(market_share_table, measure.vars = patterns("^s"), variable.name = "product_id", value.name = "market_share")
market_share_table_reshaped[, time := rep(1:J_product, times = J_product)]
market_share_table_reshaped[, product_id := as.numeric(sub("s", "", product_id))]
setcolorder(market_share_table_reshaped, c("time", "product_id", "market_share"))
setorder(market_share_table_reshaped, time, product_id)

## Compute average income and sales (from estimated market shares)
sample_average_income <- consumer_sample[, .(mean = mean(income)), by = t]
market_share_table_reshaped[, price := product_dataset[, price]]
market_share_table_reshaped[, sales := price * market_share]
sample_average_price_paid <- market_share_table_reshaped[, .(sum = sum(sales)), by = time]

## Compute correlation between ybar and pbar
print(cor(x = sample_average_income$mean, y =sample_average_price_paid$sum))


#################################
## Question 2
#################################

## Question 2(a)
## Write a function to obtain $delta_t^(\tau+1)$ given $delta_t^\tau$ and how its prediction matches data
find_delta <- function(delta_guess, real_market_share, theta_list, tau, max_iteration, tolerance_rate){
  delta <- delta_guess
  round <- 0
  while (TRUE){
    predicted_market_share <- compute_market_share(delta, theta_list, tau)
    new_delta <- delta + log(real_market_share) - log(predicted_market_share)
    new_delta <- unlist(new_delta)
    round <- round + 1
    error <- sqrt(sum((delta - new_delta)^2))
    print(c(error))
    if (round >= max_iteration | error <= tolerance_rate){
      break
    }
    delta <- new_delta
  }
  return(new_delta)
}
## An example
example <- find_delta(rep(0, J_product), product_dataset[t==1, market_share], c(0,1,1), 1, 10, 0.1)

## Question 2(b)
## Draw 100 samples with size 100



#################################
## Question 3
#################################

## Question 3(a)
## Construct price instruments and construct phat
product_dataset[, corn_sugar_product := corn_syrup_price * sugar]
product_dataset[, extract_caf_product := caffeine_extract_price * caffeine]
price_model <- lm(price ~ corn_sugar_product + extract_caf_product, data = product_dataset)
product_dataset[, price_hat := predict(price_model)]

## Construct instruments and add them to product_dataset
## x_jt and phat_jt are already there
product_dataset[, average_income := rep(sample_average_income$mean, each = T_market)]
product_dataset[, price_hat_average_income_product := price_hat * average_income]
compute_ssd <- function(values) {
  sapply(values, function(x) sum((x - values)^2))
}
product_dataset[, caffeine_ssd := compute_ssd(caffeine), by = t]
product_dataset[, sugar_ssd := compute_ssd(sugar), by = t]
## Remove redundant variables from product_dataset
product_dataset[, average_income := NULL]

## Question 3(b)
gmm_moment <- function(theta_list){
  ## Find delta_hat, X, Z, W
  delta_hat <- c()
  for (tau in 1:T_market){
    delta_hat_t <- find_delta(rep(0,50), product_dataset[t==tau, market_share], theta_list, tau, 100, 0.1)
    delta_hat <- c(delta_hat, delta_hat_t)
  }
  x_columns <- product_dataset[, .(sugar, caffeine)]
  X <- as.matrix(x_columns)
  one_column <- matrix(1, nrow = nrow(X), ncol = 1)
  X <- cbind(one_column, X)
  z_columns <- product_dataset[, .(sugar, caffeine, price_hat, caffeine_ssd, sugar_ssd)]
  Z <- as.matrix(z_columns)
  Z <- cbind(one_column, Z)
  W <- solve(t(Z) %*% Z / (J_product * T_market))
  ## Do GMM estimation to find lambda_hat
  lambda_hat <- solve(t(X) %*% Z %*% W %*% t(Z) %*% X) %*% t(X) %*% Z %*% W %*% t(Z) %*% delta_hat
  ## Compute xi_hat
  price <- as.matrix(product_dataset[, price])
  X_with_price <- cbind(price, X)
  xi_hat <- delta_hat - X_with_price %*% lambda_hat
  ## Compute moment g1
  g1 <- t(Z) %*% xi_hat / (J_product * T_market)
  ## Find moment funcion q
  q <- t(g1) %*% W %*% g1
  return(q)
}

theta_example3 <- c(-0.5, 2, 2)
moment_example <- gmm_moment(theta_example3)




