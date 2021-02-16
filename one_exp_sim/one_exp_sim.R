library(bayesAB)
library(ggplot2)
library (knitr)
library(plyr)
library(dplyr)
library(tidyr)
library(broom)
library(purrr)
library(gganimate)
library(stringr)
source("daily_posterior.R")
format_percent <- function(x, digits = 2) paste0(round(100 * x, digits), "%")

days <- 30
per_day <- 500

# Bernoulli
ret <- simulate_bernoulli(
  days = days,
  pB = 0.028,
  effect = 0,
  per_day = per_day,
  alpha = 3,
  beta = 100)
ret <- ret %>% 
  mutate(posterior = do.call(vec_posterior_bernoulli, .))
for (i in 1:nrow(ret)){
  p <- ret$posterior[i]$Probability + 
    ggtitle(paste0("bernoulli - Day: ", ret$day[i]))
  ggsave(paste0("plot_bernoulli/plot", str_pad(i, 2, pad = "0"), ".png"), 
         p, device = png())
  dev.off()
}
system("convert -delay 60 plot_bernoulli/plot*.png demo_bernoulli.gif")

# Poisson
ret <- simulate_poisson(
  days = days,
  lambdaB = 23,
  effect = 0.1,
  per_day = per_day,
  alpha = 23,
  beta = 1)
ret <- ret %>% 
  mutate(posterior = do.call(vec_posterior_poisson, .))

for (i in 1:nrow(ret)){
  p <- ret$posterior[i]$Lambda + 
    ggtitle(paste0("Poisson - Day: ", ret$day[i]))
  ggsave(paste0("plot_poisson/plot", str_pad(i, 2, pad = "0"), ".png"), 
         p, device = png())
  dev.off()
}
system("convert -delay 60 plot_poisson/plot*.png demo_poisson.gif")

# Benoulli-Exponential
ret <- simulate_bernoulli_exponential(
  days = days,
  pB = 0.028,
  lambdaB = 5,
  effect_p = 0.1,
  effect_lambda = 0.2,
  per_day = per_day,
  alpha1 = 3,
  beta1 = 100,
  alpha2 = 25,
  beta2 = 5)
ret <- ret %>% 
  mutate(posterior = do.call(vec_posterior_bernoulli_exponential, .))

for (i in 1:nrow(ret)){
  p <- ret$posterior[i]$Expectation + 
    ggtitle(paste0("Bernoulli-Exponential - Day: ", ret$day[i]))
  ggsave(paste0("plot_ber_exp/plot", str_pad(i, 2, pad = "0"), ".png"), 
         p, device = png())
  dev.off()
}
system("convert -delay 60 plot_ber_exp/plot*.png demo_ber_exp.gif")
