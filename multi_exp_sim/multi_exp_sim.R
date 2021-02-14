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
source("parameter_space.R")
format_percent <- function(x, digits = 2) paste0(round(100 * x, digits), "%")
sig_thres <- 0.05
nreps <- 10
days <- 30

# Binomial (Proportion)
bin_design <- design %>% filter(category == 'binomial')

for(row in 1:nrow(bin_design)){
  effect <- bin_design[row,'effect']
  alpha <- bin_design[row, 'prior_alpha']
  beta <- bin_design[row, 'prior_beta']
  per_day <- bin_design[row, 'sample_size_per_day'] # small sample for dev
  thresholds <- c(1e-1, 1e-2, 1e-3, 1e-4, 1e-5)
  
  prefix <- paste0(
    '_e', effect,
    '_a', alpha,
    '_b', beta,
    '_s', per_day
  )
  
  ret <- simulate_binomial(
    nreps = nreps,
    days = days,
    pB = 0.028,
    effect = effect,
    per_day = per_day,
    alpha = alpha,
    beta = beta)
  ret <- ret %>% 
    mutate(expected_loss = do.call(vec_expected_loss_binomial, .)) %>%
    mutate(pval = do.call(vec_two_prop_pval, .))
  print("Simulation step done!")
  
  bin_design <- evaluate_simulation(bin_design, row, ret, thresholds, prefix, cat='binomial')
  print("Evaluation step done!")
}

# Poisson (Count Data)
pois_design <- design %>% filter(category == 'poisson')
for(row in 1:nrow(pois_design)){
  effect <- pois_design[row,'effect']
  alpha <- pois_design[row, 'prior_alpha']
  beta <- pois_design[row, 'prior_beta']
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
    beta = beta)
  ret <- ret %>% 
    mutate(expected_loss = do.call(vec_expected_loss_poisson, .)) %>%
    mutate(pval = do.call(vec_two_t_pval, .))
  print("Simulation step done!")

  pois_design <- evaluate_simulation(pois_design, row, ret, thresholds, prefix, cat='poisson')
  print("Evaluation step done!")
}

final_design <- rbind(bin_design, pois_design)
write.csv(final_design, "final_design.csv", row.names = FALSE) 

final_design <- read.csv("final_design.csv", stringsAsFactors = FALSE)

for(i in 1:nrow(final_design)){
  if(final_design$category[i] == 'binomial' & final_design$prior_alpha[i] == 3 & final_design$prior_beta[i] == 100){
    final_design$prior[i] = 'directional'
  }
  else if(final_design$category[i] == 'binomial' & final_design$prior_alpha[i] == 30 & final_design$prior_beta[i] == 1000){
    final_design$prior[i] = 'confident'
  }
  else if(final_design$category[i] == 'binomial' & final_design$prior_alpha[i] == 30 & final_design$prior_beta[i] == 10){
    final_design$prior[i] = 'wrong'
  }
  else if(final_design$category[i] == 'binomial' & final_design$prior_alpha[i] == 1 & final_design$prior_beta[i] == 1){
    final_design$prior[i] = 'neutral'
  }
  else if(final_design$category[i] == 'poisson' & final_design$prior_alpha[i] == 23 & final_design$prior_beta[i] == 1){
    final_design$prior[i] = 'directional'
  }
  else if(final_design$category[i] == 'poisson' & final_design$prior_alpha[i] == 230 & final_design$prior_beta[i] == 10){
    final_design$prior[i] = 'confident'
  }
  else if(final_design$category[i] == 'poisson' & final_design$prior_alpha[i] == 6 & final_design$prior_beta[i] == 1){
    final_design$prior[i] = 'wrong'
  }
  else{
    final_design$prior[i] = 'others'
  }
}

perc_as_num <- function(vec){
  as.numeric(gsub("%", "", vec))
}

final_design <- final_design %>%
  mutate(peek_multiplier_freq = round(perc_as_num(freq_treat_peek)/perc_as_num(freq_treat),1),
         peek_multiplier_bayes_0.1 = round(perc_as_num(bayes_treat_peek_0.1)/perc_as_num(bayes_treat_0.1),1),
         peek_multiplier_bayes_0.01 = round(perc_as_num(bayes_treat_peek_0.01)/perc_as_num(bayes_treat_0.01),1),
         peek_multiplier_bayes_0.001 = round(perc_as_num(bayes_treat_peek_0.001)/perc_as_num(bayes_treat_0.001),1),
         peek_multiplier_bayes_0.0001 = round(perc_as_num(bayes_treat_peek_1e.04)/perc_as_num(bayes_treat_1e.04),1),
         peek_multiplier_bayes_0.00001 = round(perc_as_num(bayes_treat_peek_1e.05)/perc_as_num(bayes_treat_1e.05),1),
         )
final_design[final_design == 'NaN'] = NA
final_design[final_design == 'Inf'] = NA

final_design <- final_design %>%
  select(category, effect, prior_alpha, prior_beta, prior, sample_size_per_day,
         freq_treat, freq_treat_peek, peek_multiplier_freq,
         bayes_treat_0.1, bayes_treat_peek_0.1, peek_multiplier_bayes_0.1,
         bayes_treat_0.01, bayes_treat_peek_0.01, peek_multiplier_bayes_0.01,
         bayes_treat_0.001, bayes_treat_peek_0.001, peek_multiplier_bayes_0.001,
         bayes_treat_1e.04, bayes_treat_peek_1e.04, peek_multiplier_bayes_0.0001,
         bayes_treat_1e.05, bayes_treat_peek_1e.05, peek_multiplier_bayes_0.00001,
         )
write.csv(final_design, "final_design_master.csv", row.names = FALSE)
