#################################
## Multinomial logit model
#################################

## Extract data
setwd("C:/Users/Yucheng Wang/Desktop/Study/Econ543 IO/PS1")
df = read.csv("product_data.csv", header = TRUE, sep = ",")
T_length = 100
J_product = 10

## Convert to Dummies
dummy_vars <- model.matrix(~ nest - 1, data = df)
df <- cbind(df, dummy_vars)

## Compute outside share, Save log of share ratio (LHS variable)
total_share = aggregate(market_share ~ t, data = df, FUN = sum)
outside_share = rep(1,T_length) - total_share[,2]
df$log_share_ratio <- log(df$market_share)-log(rep(outside_share, each = J_product))

## OLS regression
ols_model <- lm(log_share_ratio ~ price + sugar + caffeine + nestDiet + nestRegular - 1, data = df) # No intercept
ols_coef = ols_model$coefficients
ols_se = summary(ols_model)$coefficients[,"Std. Error"]

## GMM estimation
library(gmm)
moment_function <- function(theta, data){
  Y <- data$log_share_ratio
  X1 <- data$price
  X2 <- data$sugar
  X3 <- data$caffeine
  X4 <- data$nestDiet
  X5 <- data$nestRegular
  X <- cbind(X1, X2, X3, X4, X5)
  residuals <- c(Y - X %*% theta)
  moment_conditions <- residuals * X
  return(moment_conditions)
}

initial_theta <- c(0,0,0,0,0)
gmm_model <- gmm(moment_function, x = df, t0 = initial_theta)
summary(gmm_model)

## Assuming endogeneity of price, use instruments and do GMM again
moment_function_iv <- function(theta, data){
  Y <- data$log_share_ratio
  X1 <- data$price
  X2 <- data$sugar
  X3 <- data$caffeine
  X4 <- data$nestDiet
  X5 <- data$nestRegular
  X6 <- data$caffeine_extract_price
  X7 <- data$corn_syrup_price
  X <- cbind(X1, X2, X3, X4, X5)
  X_exo <- cbind(X2, X3, X4, X5, X6, X7)
  residuals <- c(Y - X %*% theta)
  moment_conditions <- residuals * X_exo
  return(moment_conditions)
}
gmm_iv_model <- gmm(moment_function_iv, x = df, t0 = initial_theta)
gmm_coef = gmm_iv_model$coefficients

## Compute share-own-price derivatives and share-own-price elasticities
df$share_price_derivative <- gmm_coef[1] * df$market_share * (1-df$market_share)
df$share_price_elasticity <- gmm_coef[1] * (1-df$market_share) * df$price

## Find mean elasticity of Diet and Regular products
library(dplyr)
mean_elasticity <- df %>%
  group_by(nest) %>%
  summarize(mean_value = mean(share_price_elasticity))

## Compute share-1-price derivatives and share-1-price elasticities
market_share_1 <- df %>%
  filter(product_ID == 1) %>%
  select(t, share_1 = market_share)
price_1 <- df %>%
  filter(product_ID == 1) %>%
  select(t, price_1 = price)
df <- df %>%
  right_join(market_share_1, by = "t") %>%
  right_join(price_1, by = "t")

df$share_price1_derivative <- -1 * gmm_coef[1] * df$market_share * df$share_1
df$share_price1_elasticity <- -1 * gmm_coef[1] * df$price_1 * df$share_1

## Derive Jacobian matrix of price derivatives
jacobian_derivative <- function(alpha, date, share){
  jacobian <- matrix(0, nrow = J_product, ncol = J_product)
  for (i in 1:J_product){
    for (j in 1:J_product){
      if (i == j){
        jacobian[i,j] <- alpha * df[df$product_ID == i & df$t == date, share] * (1-df[df$product_ID == i & df$t == date, share])
      }
      else{
        jacobian[i,j] <- -1 * alpha * df[df$product_ID == i & df$t == date, share] * df[df$product_ID == j & df$t == date, share]
      }
    }
  }
  return(jacobian)
}
print(jacobian_derivative(gmm_coef[1],100,"market_share"))
print(jacobian_derivative(ols_coef[2],100,"market_share"))


#######################
## Nested logit
#######################

J_nest = 5

## Compute and add nest shares & within-nest market shares
nest_total_share = aggregate(market_share ~ nest + t, data = df, FUN = sum)
df$nest_share <- rep(nest_total_share[,3], each = J_nest)
df$log_nest_share_ratio <- log(df$market_share) - log(df$nest_share)

## OLS estimation
ols_model <- lm(log_share_ratio ~ price + sugar + caffeine + nestDiet + nestRegular + log_nest_share_ratio - 1, data = df) # No intercept


## GMM estimation
moment_function_nest <- function(theta, data){
  Y <- data$log_share_ratio
  X1 <- data$price
  X2 <- data$sugar
  X3 <- data$caffeine
  X4 <- data$nestDiet
  X5 <- data$nestRegular
  X6 <- data$caffeine_extract_price
  X7 <- data$corn_syrup_price
  X8 <- data$log_nest_share_ratio
  X <- cbind(X1, X2, X3, X4, X5, X8)
  X_exo <- cbind(X2, X3, X4, X5, X6, X7)
  residuals <- c(Y - X %*% theta)
  moment_conditions <- residuals * X_exo
  return(moment_conditions)
}

initial_theta = c(0,0,0,0,0,0)
gmm_nest_model <- gmm(moment_function_nest, x = df, t0 = initial_theta)
gmm_nest_coef = gmm_nest_model$coefficients

## Compute share-price derivatives
df$share_price_derivative <- gmm_nest_coef[1] / (1-gmm_nest_coef[6])


