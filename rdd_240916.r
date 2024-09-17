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
