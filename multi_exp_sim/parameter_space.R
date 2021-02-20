library(bayesAB)
library(ggplot2)
library(tidyverse)
library(gridExtra)

#TEST <- TRUE
if(TEST==TRUE){
  sig_thres <- 0.05
  nreps <- 10
  days <- 7
} else {
  sig_thres <- 0.05
  nreps <- 500
  days <- 30
}
print(paste0("Frequentist Sig Level: ", sig_thres))
print(paste0("Number of Replicates: ", nreps))
print(paste0("Duration of Experiments (days): ", days))

## Prior Confidence
grid.arrange(
  plotBeta(alpha = 3, beta = 100) + ggtitle("Directional-Beta (alpha=3, beta=100)"),
  plotBeta(alpha = 30, beta = 1000) + ggtitle("Confident-Beta (alpha=30, beta=1000)"),
  plotBeta(alpha = 1, beta = 1) + ggtitle("Neutral-Beta (alpha=1, beta=1)"),
  plotBeta(alpha = 30, beta = 10) + ggtitle("Wrong-Beta (alpha=30, beta=10)"),
  nrow=2)
grid.arrange(
  plotGamma(shape = 23, rate = 1) + ggtitle("Directional-Gamma (shape=23, rate=1)"),
  plotGamma(shape = 230, rate = 10) + ggtitle("Confident-Gamma (shape=230, rate=10)"),
  plotGamma(shape = 6, rate = 1) + ggtitle("Wrong-Gamma (shape=6, rate=1)"),
  nrow=1)

## Parameter Space

### Bernoulli - test space
if(TEST==TRUE){
  category <- c('bernoulli')
  effect <- c(0, 0.001)
  prior_confidence <- c('1,1', '3,100')
  sample_size_per_day <- c(500)
} else {
  category <- c('bernoulli')
  effect <- c(0, 0.001, 0.005)
  prior_confidence <- c('1,1', '3,100', '30,1000', '30,10')
  sample_size_per_day <- c(500, 5000, 50000)
}

### Bernoulli - design grid
design <- expand.grid(
  category=category,
  effect=effect,
  prior_confidence=prior_confidence, 
  sample_size_per_day=sample_size_per_day, 
  stringsAsFactors = FALSE)
prior_params <- as.data.frame(do.call('rbind', strsplit(as.character(design$prior_confidence),',',fixed=TRUE)))
ber_design <- design %>% mutate(prior_alpha=prior_params$V1, prior_beta=prior_params$V2) %>% 
  select(category, effect, prior_alpha, prior_beta, sample_size_per_day) %>% 
  arrange(category, effect, prior_alpha, prior_beta, sample_size_per_day)
print(paste0("Number of Benoulli simulations: ", nrow(ber_design)))

if(TEST==TRUE){
  category <- c('poisson')
  effect <- c(0, 0.01, 0.1)
  prior_confidence <- c('23,1')
  sample_size_per_day <- c(500)
} else {
  category <- c('poisson')
  effect <- c(0, 0.01, 0.1)
  prior_confidence <- c('23,1', '230,10', '6,1')
  sample_size_per_day <- c(500, 5000, 50000)
}

design <- expand.grid(
  category=category,
  effect=effect,
  prior_confidence=prior_confidence, 
  sample_size_per_day=sample_size_per_day, 
  stringsAsFactors = FALSE)
prior_params <- as.data.frame(do.call('rbind', strsplit(as.character(design$prior_confidence),',',fixed=TRUE)))
pois_design <- design %>% mutate(prior_alpha=prior_params$V1, prior_beta=prior_params$V2) %>% 
  select(category, effect, prior_alpha, prior_beta, sample_size_per_day) %>% 
  arrange(category, effect, prior_alpha, prior_beta, sample_size_per_day)
print(paste0("Number of Poisson simulations: ", nrow(pois_design)))

if(TEST==TRUE){
  category <- c('bernoulli-exponential')
  effect_p <- c(0)
  effect_lambda <- c(0.1)
  prior_confidence_p <- c('1,1', '3,100')
  prior_confidence_lambda <- c('25,5')
  sample_size_per_day <- c(500)
} else {
  category <- c('bernoulli-exponential')
  effect_p <- c(0, 0.001)
  effect_lambda <- c(0, 0.01)
  prior_confidence_p <- c('3,100', '30,10')
  prior_confidence_lambda <- c('25,5', '2,1')
  sample_size_per_day <- c(500, 5000)
}

design <- expand.grid(
  category=category,
  effect_p=effect_p,
  effect_lambda=effect_lambda,
  prior_confidence_p=prior_confidence_p, 
  prior_confidence_lambda=prior_confidence_lambda,
  sample_size_per_day=sample_size_per_day, 
  stringsAsFactors = FALSE)
prior_params_p <- as.data.frame(do.call('rbind', strsplit(as.character(design$prior_confidence_p),',',fixed=TRUE)))
prior_params_lambda <- as.data.frame(do.call('rbind', strsplit(as.character(design$prior_confidence_lambda),',',fixed=TRUE)))
ber_exp_design <- design %>% 
  mutate(prior_alpha1=prior_params_p$V1,
         prior_beta1=prior_params_p$V2,
         prior_alpha2=prior_params_lambda$V1,
         prior_beta2=prior_params_lambda$V2) %>% 
  select(category, effect_p, effect_lambda, prior_alpha1, prior_beta1, prior_alpha2, prior_beta2, sample_size_per_day) %>% 
  arrange(category, effect_p, effect_lambda, prior_alpha1, prior_beta1, prior_alpha2, prior_beta2, sample_size_per_day)
print(paste0("Number of Bernoulli-Exponential simulations: ", nrow(ber_exp_design)))
