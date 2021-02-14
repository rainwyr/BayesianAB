library(bayesAB)
library(ggplot2)
library(tidyverse)
library(gridExtra)

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
category <- c('binomial')
effect <- c(0, 0.001, 0.005)
prior_confidence <- c('1,1', '3,100', '30,1000', '30,10')
sample_size_per_day <- c(50, 500)
design <- expand.grid(
  category=category,
  effect=effect,
  prior_confidence=prior_confidence, 
  sample_size_per_day=sample_size_per_day, 
  stringsAsFactors = FALSE)
prior_params <- as.data.frame(do.call('rbind', strsplit(as.character(design$prior_confidence),',',fixed=TRUE)))
bin_design <- design %>% mutate(prior_alpha=prior_params$V1, prior_beta=prior_params$V2) %>% 
  select(category, effect, prior_alpha, prior_beta, sample_size_per_day) %>% 
  arrange(category, effect, prior_alpha, prior_beta, sample_size_per_day)

category <- c('poisson')
effect <- c(0, 0.01, 0.1)
prior_confidence <- c('23,1', '230,10', '6,1')
sample_size_per_day <- c(50, 500)
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

design <- rbind(bin_design, pois_design)