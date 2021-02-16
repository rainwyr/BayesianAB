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

# Case 1 - Bernoulli - Effect False
ret <- simulate_bernoulli(
  days = days,
  pB = 0.03,
  effect = 0,
  per_day = per_day,
  alpha = 3,
  beta = 100)
ret <- ret %>% 
  mutate(posterior = do.call(vec_posterior_bernoulli, .))
for (i in 1:nrow(ret)){
  p <- ret$posterior[i]$Probability + 
    ggtitle(paste0("Bernoulli - Day: ", ret$day[i]))
  ggsave(paste0("case1/plot", str_pad(i, 2, pad = "0"), ".png"), 
         p, device = png())
  dev.off()
}
system("convert -delay 90 case1/plot*.png demo_case1.gif")

# Case 1 - Bernoulli - Effect True
ret <- simulate_bernoulli(
  days = days,
  pB = 0.03,
  effect = 0.001,
  per_day = per_day,
  alpha = 3,
  beta = 100)
ret <- ret %>% 
  mutate(posterior = do.call(vec_posterior_bernoulli, .))
for (i in 1:nrow(ret)){
  p <- ret$posterior[i]$Probability + 
    ggtitle(paste0("Bernoulli - Day: ", ret$day[i]))
  ggsave(paste0("case2/plot", str_pad(i, 2, pad = "0"), ".png"), 
         p, device = png())
  dev.off()
}
system("convert -delay 90 case2/plot*.png demo_case2.gif")

# Case 3 - Poisson - Effect False
ret <- simulate_poisson(
  days = days,
  lambdaB = 23,
  effect = 0,
  per_day = per_day,
  alpha = 23,
  beta = 1)
ret <- ret %>% 
  mutate(posterior = do.call(vec_posterior_poisson, .))

for (i in 1:nrow(ret)){
  p <- ret$posterior[i]$Lambda + 
    ggtitle(paste0("Poisson - Day: ", ret$day[i]))
  ggsave(paste0("case3/plot", str_pad(i, 2, pad = "0"), ".png"), 
         p, device = png())
  dev.off()
}
system("convert -delay 90 case3/plot*.png demo_case3.gif")

# Case 4 - Poisson - Effect True
ret <- simulate_poisson(
  days = days,
  lambdaB = 23,
  effect = 0.2,
  per_day = per_day,
  alpha = 23,
  beta = 1)
ret <- ret %>% 
  mutate(posterior = do.call(vec_posterior_poisson, .))

for (i in 1:nrow(ret)){
  p <- ret$posterior[i]$Lambda + 
    ggtitle(paste0("Poisson - Day: ", ret$day[i]))
  ggsave(paste0("case4/plot", str_pad(i, 2, pad = "0"), ".png"), 
         p, device = png())
  dev.off()
}
system("convert -delay 90 case4/plot*.png demo_case4.gif")

# Case 5 - Benoulli-Exponential - Effect False
ret <- simulate_bernoulli_exponential(
  days = days,
  pB = 0.03,
  lambdaB = 5,
  effect_p = 0,
  effect_lambda = 0,
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
  ggsave(paste0("case5/plot", str_pad(i, 2, pad = "0"), ".png"), 
         p, device = png())
  dev.off()
}
system("convert -delay 90 case5/plot*.png demo_case5.gif")

# Case 6 - Benoulli-Exponential - Effect True
ret <- simulate_bernoulli_exponential(
  days = days,
  pB = 0.03,
  lambdaB = 5,
  effect_p = 0.01,
  effect_lambda = 0.1,
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
  ggsave(paste0("case6/plot", str_pad(i, 2, pad = "0"), ".png"), 
         p, device = png())
  dev.off()
}
system("convert -delay 90 case6/plot*.png demo_case6.gif")
