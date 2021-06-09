library(bayesAB)
library(ggplot2)
library (knitr)
library(plyr)
library(dplyr)
library(tidyr)
library(broom)
library(purrr)
source("basic_utils.R")
source("batch_utils.R")
TEST <- FALSE
source("parameter_space.R")
format_percent <- function(x, digits = 2) paste0(round(100 * x, digits), "%")

# Bernoulli (Proportion)

for(row in 1:nrow(ber_design)){ 
  effect <- ber_design[row,'effect']
  alpha <- as.numeric(ber_design[row, 'prior_alpha'])
  beta <- as.numeric(ber_design[row, 'prior_beta'])
  per_day <- ber_design[row, 'sample_size_per_day'] # small sample for dev
  thresholds <- c(1e-1, 1e-2, 1e-3, 1e-4, 1e-5)
  
  prefix <- paste0(
    '_e', effect,
    '_a', alpha,
    '_b', beta,
    '_s', per_day
  )
  
  ret <- simulate_bernoulli(
    nreps = nreps,
    days = days,
    pB = 0.028,
    effect = effect,
    per_day = per_day,
    alpha = alpha,
    beta = beta,
    unbalance_ratio = 0.25)
  ret <- ret %>%
    mutate(expected_loss = do.call(vec_expected_loss_bernoulli, .)) %>%
    mutate(pval = do.call(vec_two_prop_pval, .))
  print("Simulation step done!")
  
  ber_design <- evaluate_simulation(ber_design, row, ret, thresholds, prefix, cat='bernoulli')
  print("Evaluation step done!")
}
colnames(ber_design) <- gsub("1e-05", "0.00001", (gsub("1e-04", "0.0001", colnames(ber_design))))
write.csv(ber_design, "output/design_bernoulli.csv", row.names = FALSE)

# Poisson (Count Data)
for(row in 1:nrow(pois_design)){
  effect <- pois_design[row,'effect']
  alpha <- as.numeric(pois_design[row, 'prior_alpha'])
  beta <- as.numeric(pois_design[row, 'prior_beta'])
  per_day <- pois_design[row, 'sample_size_per_day'] # small sample for dev
  thresholds <- c(1e-1, 1e-2, 1e-3, 1e-4, 1e-5)
  
  prefix <- paste0(
    '_e', effect,
    '_a', alpha,
    '_b', beta,
    '_s', per_day
  )
  
  ret <- simulate_poisson(
    nreps = nreps,
    days = days,
    lambdaB = 23,
    effect = effect,
    per_day = per_day,
    alpha = alpha,
    beta = beta,
    unbalance_ratio = 0.25)
  ret <- ret %>%
    mutate(expected_loss = do.call(vec_expected_loss_poisson, .)) %>%
    mutate(pval = do.call(vec_two_t_pval_moves, .))
  print("Simulation step done!")
  
  pois_design <- evaluate_simulation(pois_design, row, ret, thresholds, prefix, cat='poisson')
  print("Evaluation step done!")
}

colnames(pois_design) <- gsub("1e-05", "0.00001", (gsub("1e-04", "0.0001", colnames(pois_design))))
write.csv(pois_design, "output/design_poisson.csv", row.names = FALSE)

# 
# # Bernoulli-Exponential (Continuous Data with Gating)
# for(row in 1:nrow(ber_exp_design)){
#   effect_p <- ber_exp_design[row,'effect_p']
#   effect_lambda <- ber_exp_design[row,'effect_lambda']
#   alpha1 <- ber_exp_design[row, 'prior_alpha1']
#   beta1 <- ber_exp_design[row, 'prior_beta1']
#   alpha2 <- ber_exp_design[row, 'prior_alpha2']
#   beta2 <- ber_exp_design[row, 'prior_beta2']
#   per_day <- ber_exp_design[row, 'sample_size_per_day'] # small sample for dev
#   thresholds <- c(0.1, 0.01, 0.001, 0.0001, 0.00001)
#   
#   prefix <- paste0(
#     '_ep', effect_p,
#     '_el', effect_lambda,
#     '_a1', alpha1,
#     '_b1', beta1,
#     '_a2', alpha2,
#     '_b2', beta2,
#     '_s', per_day
#   )
#   
#   ret <- simulate_bernoulli_exponential(
#     nreps=100,
#     days,
#     pB = 0.028,
#     lambdaB = 5,
#     effect_p, # A - B
#     effect_lambda, # A - B
#     per_day,
#     alpha1 = alpha1,
#     beta1 = beta1,
#     alpha2 = alpha2,
#     beta2 = beta2)
#   ret <- ret %>% 
#     mutate(expected_loss = do.call(vec_expected_loss_bernoulli_exponential, .)) %>%
#     mutate(pval = do.call(vec_two_t_pval_spend, .))
#   print("Simulation step done!")
#   
#   ber_exp_design <- evaluate_simulation(ber_exp_design, row, ret, thresholds, prefix, cat='bernoulli-exponential')
#   print("Evaluation step done!")
# }
# 
# colnames(ber_exp_design) <- gsub("1e-05", "0.00001", (gsub("1e-04", "0.0001", colnames(ber_exp_design))))
# write.csv(ber_exp_design, "output/design_bernoulli_exponential.csv", row.names = FALSE) 