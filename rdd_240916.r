############################
## An example of RDD
############################

## Generate Data
cutoff_parameter = 0
beta_parameter = 10
N_sample_size = 1000
X_running_variable = rnorm(N_sample_size, mean = 0, sd = 1)
epsilon_outcome_unobservable = X_running_variable + rnorm(N_sample_size, mean = 0, sd = 1)

D_treatment = X_running_variable >= cutoff_parameter
Y_outcome_of_interest = beta_parameter * D_treatment + epsilon_outcome_unobservable

## Combine variables into a data table
library(data.table)
data_set = data.table(Y_outcome_of_interest, D_treatment, X_running_variable)

## Plot data
library(ggplot2)
gg = ggplot(aes(x=X_running_variable, y=Y_outcome_of_interest), data=data_set) +
    geom_point() +
    theme_bw(base_size = 16) +
    labs(x="Running Variable (X)", y="Outcome of Interest (Y)") +
    geom_vline(xintercept = cutoff_parameter)

## Save the plot
setwd("~/github/Econ512")
ggsave(gg, file="RDD_simulator.pdf", width=8, height=5)

write.csv(data_set, file="RDD_simulator_data.csv", row.names = FALSE)

###################################
## Estimate the regression discontinuity model
###################################

rm(list=ls())   # clear memory
setwd("~/github/Econ512")

## load the data
data_set = read.csv("RDD_simulator_data.csv")
data_set = setDT(data_set)

## RDD estimator with simple threshold around cutoff
RDD_estimator_simple <- function(inputdata, c_threshold_point = 0, tau_bandwith = 0.5){
    meanY_above = inputdata[ X_running_variable >= c_threshold_point & X_running_variable <= c_threshold_point + tau_bandwith, mean(Y_outcome_of_interest)]
    meanY_below = inputdata[ X_running_variable <= c_threshold_point & X_running_variable >= c_threshold_point - tau_bandwith, mean(Y_outcome_of_interest)]
    RDD_beta_hat = meanY_above - meanY_below
    return(RDD_beta_hat)
}

## Adjust bandwidth tau
thresholds = seq(0.1, 1.0, by=0.1)
results = rep(0, length(thresholds))
for(i in 1:length(thresholds)){
  results[i] = RDD_estimator_simple(data_set, c_threshold_point = 0, tau_bandwith = thresholds[i])
}

gg = ggplot(data = data.table(thresholds, results), aes(x=thresholds, y=results)) +
  geom_point() +
  geom_line() +
  theme_bw(base_size=16) +
  labs(x="Bandwith (tau)", y="Treatment Effect Estimate") +
  geom_hline(yintercept = 10.0, linetype="dashed", color="red")

ggsave(gg, file="RDD_estimator_simple.pdf", width=8, height=5)


###################################
## RDD estimator with triangular kernel
###################################

rm(list=ls())   # clear memory
setwd("~/github/Econ512")

## load the data
data_set = read.csv("RDD_simulator_data.csv")
data_set = setDT(data_set)

RDD_estimator_triangular <- function(inputdata, c_threshold_point=0, tau_bandwith=0.5){
  inputdata[, normalized_distance := (X_running_variable - c_threshold_point) / tau_bandwith]
  inputdata[, kernel_weight := (1 - abs(normalized_distance))*(abs(normalized_distance <= 1))]
  
  triangular_loss_function <- function(params){
    alpha_guess = params[1]
    gamma_guess = params[2]
    
    ## Calculate the loss function value
    inputdata_subset[, objective_part <- (Y_outcome_of_interest - alpha_guess - gamma_guess * (X_running_variable - c_threshold_point))^2]
    objective_value = inputdata_subset[, sum(objective_value * kernel_weight)]
    return(objective_value)
  }
  
  ## Minimize the loss function, separately for treated and control groups
  optim_result_treated = optim(c(0,0), triangular_loss_function, inputdata_subset = inputdata[D_treatment==TRUE])
  optim_result_control = optim(c(0,0), triangular_loss_function, inputdata_subset = inputdata[D_treatment==FALSE])
  
  ## Extract the treatment effect
  alpha_treated = optim_result_treated$par[1]
  alpha_control = optim_result_control$par[1]
  RDD_triangular = alpha_treated - alpha_control
  return(RDD_triangular)
}

## Adjust bandwidth tau
thresholds = seq(0.1, 1.0, by=0.1)
results = rep(0, length(thresholds))
for(i in 1:length(thresholds)){
  results[i] = RDD_estimator_triangular(data_set, c_threshold_point = 0, tau_bandwith = thresholds[i])
}

gg = ggplot(data = data.table(thresholds, results), aes(x=thresholds, y=results)) +
  geom_point() +
  geom_line() +
  theme_bw(base_size=16) +
  labs(x="Bandwith (tau)", y="Treatment Effect Estimate") +
  geom_hline(yintercept = 10.0, linetype="dashed", color="red")

ggsave(gg, file="RDD_estimator_triangular.pdf", width=8, height=5)

