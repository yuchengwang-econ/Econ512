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

## Assuming endogeneity of price, use instruments and do 2SLS
## install package "AER"
library(AER)
iv_model <- ivreg(df$log_share_ratio ~ df$price + df$sugar + df$caffeine + df$nestDiet + df$nestRegular - 1 | df$caffeine_extract_price + 
                    df$corn_syrup_price + df$sugar + df$caffeine + df$nestDiet + df$nestRegular)
alpha_hat <- iv_model$coefficients[1]

## Compute share-own-price derivatives and share-own-price elasticities
df$share_price_derivative <- alpha_hat * df$market_share * (1-df$market_share)
df$share_price_elasticity <- alpha_hat * (1-df$market_share) * df$price

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

df$share_price1_derivative <- -1 * alpha_hat * df$market_share * df$share_1
df$share_price1_elasticity <- -1 * alpha_hat * df$price_1 * df$share_1

## Derive Jacobian matrix of price derivatives
jacobian_derivative <- function(alpha, data, date){
  # Compute nest shares and nest share ratios
  share <- data[data$t == date, "market_share"][["market_share"]]
  jacobian <- matrix(0, nrow = J_product, ncol = J_product)
  for (i in 1:J_product){
    for (j in 1:J_product){
      if (i == j){
        jacobian[i,j] <- alpha * share[i] * (1-share[i])
      }
      else{
        jacobian[i,j] <- -1 * alpha * share[i] * share[j]
      }
    }
  }
  return(jacobian)
}
jacobian <- jacobian_derivative(alpha_hat,df,100)
print(round(jacobian, digits=3))




#######################
## Nested logit
#######################

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

J_nest = 5

## Compute and add nest shares & within-nest market shares
nest_total_share = aggregate(market_share ~ nest + t, data = df, FUN = sum)
df$nest_share <- rep(nest_total_share[,3], each = J_nest)
df$nest_share_ratio <- df$market_share / df$nest_share
df$log_nest_share_ratio <- log(df$nest_share_ratio)

## Compute average ch. of other products in same nest
df$mean_sugar_others_nest <- (rep(aggregate(sugar ~ nest + t, data = df, FUN = sum)[,3], each = J_nest) - df$sugar) / (J_nest - 1)
df$mean_caffeine_others_nest <- (rep(aggregate(caffeine ~ nest + t, data = df, FUN = sum)[,3], each = J_nest) - df$caffeine) / (J_nest - 1)

## OLS estimation
ols_model <- lm(log_share_ratio ~ price + sugar + caffeine + nestDiet + nestRegular + log_nest_share_ratio - 1, data = df) # No intercept

## 2SLS estimation
nest_model <- ivreg(df$log_share_ratio ~ df$price + df$sugar + df$caffeine + df$nestDiet + df$nestRegular + df$log_nest_share_ratio - 1 
                    | df$caffeine_extract_price + df$corn_syrup_price + df$sugar + df$caffeine + df$nestDiet + df$nestRegular + df$mean_sugar_others_nest + df$mean_caffeine_others_nest)
alpha_hat_nest <- nest_model$coefficients[1]
sigma_hat_nest <- nest_model$coefficients[6]

## Compute share-price derivatives and elasticities
df$share_price_derivative <- alpha_hat_nest / (1-sigma_hat_nest) * df$market_share * (1-exp(df$log_nest_share_ratio)) + alpha_hat_nest * df$market_share * exp(df$log_nest_share_ratio) *(1-df$nest_share)
df$share_price_elasticity <- alpha_hat_nest / (1-sigma_hat_nest) * df$price * (1-exp(df$log_nest_share_ratio)) + alpha_hat_nest * df$price * exp(df$log_nest_share_ratio) *(1-df$nest_share)

## Find mean elasticity of Diet and Regular products
library(dplyr)
mean_elasticity <- df %>%
  group_by(nest) %>%
  summarize(mean_value = mean(share_price_elasticity))

## Generate new variables about product 1
df <- df %>%
  group_by(t) %>%
  mutate(market_share_1 = market_share[product_ID == 1]) %>% ## Generates s_1t
  mutate(nest_share_1 = nest_share_ratio[product_ID == 1]) %>% ## Generates s_1t|g(1)
  mutate(price_1 = price[product_ID == 1]) %>%  ## Generates p_1t
  ungroup()

## Compute share-1-price derivatives and elasticities
df$share_price1_derivative <- (df$nestDiet==1)*(-1 * alpha_hat_nest / (1-sigma_hat_nest) * df$market_share * df$nest_share_1 + alpha_hat_nest * df$nest_share_ratio * df$market_share_1 * (1-df$nest_share)) + (df$nestRegular==1)*(-1 * alpha_hat_nest * df$market_share * df$market_share_1)
df$share_price1_elasticity <- df$share_price1_derivative * df$price_1 / df$market_share

## Find mean elasticity of Diet and Regular products
mean_1_elasticity <- df %>%
  group_by(nest) %>%
  summarize(mean_value = mean(share_price1_elasticity))

## Find Jacobian matrix
jacobian_derivative_nest <- function(alpha, sigma, data, date){
  # Compute nest shares and nest share ratios
  nest <- data[data$t == date, "nest"][["nest"]]
  share <- data[data$t == date, "market_share"][["market_share"]]
  nest_share <- sapply(nest, function(x) sum(share[nest == x]))
  nest_share_ratio <- share / nest_share
  jacobian <- matrix(0, nrow = J_product, ncol = J_product)
  for (i in 1:J_product){
    for (j in 1:J_product){
      if (i == j){
        jacobian[i,j] <- alpha_hat_nest / (1-sigma_hat_nest) * share[i] * (1-nest_share_ratio[i]) + alpha_hat_nest * share[i] * nest_share_ratio[i] * (1-nest_share[i])
      }
      else if (nest[i] == nest[j]){ ## i,j in the same nest
        jacobian[i,j] <- -1 * alpha_hat_nest / (1-sigma_hat_nest) * nest_share_ratio[j] * share[i] + alpha_hat_nest * nest_share_ratio[j] * share[i] * (1-nest_share[i])
      }
      else{ ## i,j not in the same nest
        jacobian[i,j] <- -1 * alpha_hat_nest * share[i] * share[j]
      }
    }
  }
  return(jacobian)
}
jacobian_nest <- jacobian_derivative_nest(alpha_hat_nest,sigma_hat_nest,df,100)
print(round(jacobian_nest, digits=3))


########################
## Add Supply Side
########################

## Obtain estimates of alpha and sigma, repeat codes in Question 2(a)
setwd("C:/Users/Yucheng Wang/Desktop/Study/Econ543 IO/PS1")
df = read.csv("product_data.csv", header = TRUE, sep = ",")
T_length = 100
J_product = 10
dummy_vars <- model.matrix(~ nest - 1, data = df)
df <- cbind(df, dummy_vars)
total_share = aggregate(market_share ~ t, data = df, FUN = sum)
outside_share = rep(1,T_length) - total_share[,2]
df$log_share_ratio <- log(df$market_share)-log(rep(outside_share, each = J_product))
J_nest = 5
nest_total_share = aggregate(market_share ~ nest + t, data = df, FUN = sum)
df$nest_share <- rep(nest_total_share[,3], each = J_nest)
df$nest_share_ratio <- df$market_share / df$nest_share
df$log_nest_share_ratio <- log(df$nest_share_ratio)
df$mean_sugar_others_nest <- (rep(aggregate(sugar ~ nest + t, data = df, FUN = sum)[,3], each = J_nest) - df$sugar) / (J_nest - 1)
df$mean_caffeine_others_nest <- (rep(aggregate(caffeine ~ nest + t, data = df, FUN = sum)[,3], each = J_nest) - df$caffeine) / (J_nest - 1)
nest_model <- ivreg(df$log_share_ratio ~ df$price + df$sugar + df$caffeine + df$nestDiet + df$nestRegular + df$log_nest_share_ratio - 1 
                    | df$caffeine_extract_price + df$corn_syrup_price + df$sugar + df$caffeine + df$nestDiet + df$nestRegular + df$mean_sugar_others_nest 
                    + df$mean_caffeine_others_nest)
alpha_hat_nest <- nest_model$coefficients[1]
sigma_hat_nest <- nest_model$coefficients[6]

## Obtain the Jacobian matrix as in Question 2(e)
jacobian_derivative_nest <- function(alpha, sigma, data, date){
  # Compute nest shares and nest share ratios
  nest <- data[data$t == date, "nest"]
  share <- data[data$t == date, "market_share"]
  nest_share <- sapply(nest, function(x) sum(share[nest == x]))
  nest_share_ratio <- share / nest_share
  jacobian <- matrix(0, nrow = J_product, ncol = J_product)
  for (i in 1:J_product){
    for (j in 1:J_product){
      if (i == j){
        jacobian[i,j] <- alpha_hat_nest / (1-sigma_hat_nest) * share[i] * (1-nest_share_ratio[i]) +
          alpha_hat_nest * share[i] * nest_share_ratio[i] * (1-nest_share[i])
      }
      else if (nest[i] == nest[j]){ ## i,j in the same nest
        jacobian[i,j] <- -1 * alpha_hat_nest / (1-sigma_hat_nest) * nest_share_ratio[j] * share[i] +
          alpha_hat_nest * nest_share_ratio[j] * share[i] * (1-nest_share[i])
      }
      else{ ## i,j not in the same nest
        jacobian[i,j] <- -1 * alpha_hat_nest * share[i] * share[j]
      }
    }
  }
  return(jacobian)
}

## Estimate marginal costs
df$marginal_cost <- rep(0, T_length * J_product)
for (tau in 1:T_length){
  for (j in 1:J_product){
    df[df$t == tau & df$product_ID == j, "marginal_cost"] <- df[df$t == tau & df$product_ID == j, "price"] + df[df$t == tau & df$product_ID == j, "market_share"] / jacobian_derivative_nest(alpha_hat_nest, sigma_hat_nest, df, tau)[j,j]
  }
}

## Compute Lerner indices
df$lerner <- (df$price - df$marginal_cost) / df$price

####### Question 3(b)
## Recover market shares from prices
share_f <- function(real_price, price, real_log_share_ratio, real_log_nest_share_ratio){
  deltas <- real_log_share_ratio - sigma_hat_nest * real_log_nest_share_ratio + alpha_hat_nest * (price - real_price)
  exps <- exp(deltas / (1-sigma_hat_nest))
  D_diet <- sum(exps[1:5])
  D_regular <- sum(exps[6:10])
  share <- rep(0,10)
  for (j in 1:5){
    share[j] <- (D_diet ^ (1-sigma_hat_nest)) / (D_diet ^ (1-sigma_hat_nest) + D_regular ^ (1-sigma_hat_nest) + 1) * exps[j] / D_diet
  }
  for (j in 6:10){
    share[j] <- (D_regular ^ (1-sigma_hat_nest)) / (D_diet ^ (1-sigma_hat_nest) + D_regular ^ (1-sigma_hat_nest) + 1) * exps[j] / D_regular
  }
  return(share)
}

## Derive Jacobian matrix at a given share (instead of real share from data)
jacobian_derivative_estimate <- function(share){
  # Compute nest shares and nest share ratios
  nest_share <- c(rep(sum(share[1:5]),5),rep(sum(share[6:10]),5))
  nest_share_ratio <- share / nest_share
  jacobian <- matrix(0, nrow = J_product, ncol = J_product)
  for (i in 1:J_product){
    for (j in 1:J_product){
      if (i == j){
        jacobian[i,j] <- alpha_hat_nest / (1-sigma_hat_nest) * share[i] * (1-nest_share_ratio[i]) + alpha_hat_nest * share[i] * nest_share_ratio[i] * (1-nest_share[i])
      }
      else if (nest_share[i] == nest_share[j]){ ## i,j in the same nest
        jacobian[i,j] <- -1 * alpha_hat_nest / (1-sigma_hat_nest) * nest_share_ratio[j] * share[i] + alpha_hat_nest * nest_share_ratio[j] * share[i] * (1-nest_share[i])
      }
      else{ ## i,j not in the same nest
        jacobian[i,j] <- -1 * alpha_hat_nest * share[i] * share[j]
      }
    }
  }
  return(jacobian)
}

## Derive FOCs if 1 and 2 merge at a given price
focs_merge12 <- function(real_price, price, real_log_share_ratio, real_log_nest_share_ratio, marginal_cost){
  share <- share_f(real_price, price, real_log_share_ratio, real_log_nest_share_ratio)
  jacobian <- jacobian_derivative_estimate(share)
  focs <- rep(0,10)
  focs[1] <- share[1] + (price[1] - marginal_cost[1]) * jacobian[1,1] + (price[2] - marginal_cost[2]) * jacobian[1,2]
  focs[2] <- share[2] + (price[1] - marginal_cost[1]) * jacobian[2,1] + (price[2] - marginal_cost[2]) * jacobian[2,2]
  for (j in 3:10){
    focs[j] <- share[j] + (price[j] - marginal_cost[j]) * jacobian[j,j]
  }
  return(focs)
}

## Set the objective to be sum of squared FOCs
objective_merge12 <- function(price){
  focs <- focs_merge12(df[df$t==100, "price"], price, df[df$t==100,"log_share_ratio"], df[df$t==100,"log_nest_share_ratio"], df[df$t==100,"marginal_cost"])
  return(sum(focs^2))
}

## Find equilibrium prices
initial_price <- rep(3,10)
result_merge12 <- optim(initial_price, objective_merge12, control = list(maxit = 10000), method = "Nelder-Mead")
merge12_price <- result_merge12$par

###### Problem 3(d)
## Derive total profit of all firms at a given price
total_profit <- function(price){
  share <- share_f(df[df$t==100, "price"], price, df[df$t==100,"log_share_ratio"], df[df$t==100,"log_nest_share_ratio"])
  profit <- -1 * sum((price - df[df$t==100,"marginal_cost"]) * share)
  return(profit)
}

initial_price <- rep(10,10)
result_mergeall <- optim(initial_price, total_profit, control = list(maxit = 10000), method = "Nelder-Mead")
mergeall_price <- result_mergeall$par
