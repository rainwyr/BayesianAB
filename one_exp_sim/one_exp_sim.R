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

# Binomial
alpha <- 3
beta <- 100
effect <- 0
ret <- simulate_binomial(
  days = days,
  pB = 0.028,
  effect = effect,
  per_day = per_day,
  alpha = alpha,
  beta = beta)
ret <- ret %>% 
  mutate(posterior = do.call(vec_posterior_binomial, .))
for (i in 1:nrow(ret)){
  p <- ret$posterior[i]$Probability + 
    ggtitle(paste0("Binomial - Day: ", ret$day[i]))
  ggsave(paste0("plot_binomial/plot", str_pad(i, 2, pad = "0"), ".png"), 
         p, device = png())
  dev.off()
}
system("convert -delay 60 plot_binomial/plot*.png demo_binomial.gif")

# Poisson
alpha <- 23
beta <- 1
effect <- 0.1
ret <- simulate_poisson(
  days = days,
  lambdaB = 23,
  effect = effect,
  per_day = per_day,
  alpha = alpha,
  beta = beta)
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
