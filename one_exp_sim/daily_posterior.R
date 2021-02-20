# Bernoulli

simulate_bernoulli <- function(days,
                               pB,
                               effect, # A - B
                               per_day,
                               alpha,
                               beta){
  tibble(day = seq_len(days)) %>%
    tidyr::crossing(type = c("A", "B")) %>% 
    ungroup() %>%
    mutate(total = rbinom(n(), per_day, .5)) %>% # half A, half B
    mutate(success = rbinom(n(), total, pB + effect * (type == "A"))) %>% 
    group_by(type) %>%
    mutate(n = cumsum(total),  s = cumsum(success)) %>%
    ungroup() %>%
    select(-success, -total) %>%
    tidyr::gather(metric, value, n:s) %>%
    tidyr::unite(metric2, metric, type, sep = "") %>%
    tidyr::spread(metric2, value) %>%
    mutate(alpha = alpha, beta = beta)
}

vec_posterior_bernoulli <- function(nA, nB, sA, sB, alpha, beta, ...) {
  if (length(sA) > 1) {
    ret <- sapply(seq_along(nA), function(i) {
      posterior_bernoulli(nA[i], nB[i], sA[i], sB[i], alpha[i], beta[i], ...)
    })
    return(ret)
  }
  posterior_bernoulli(nA, nB, sA, sB, alpha, beta, ...)
}

posterior_bernoulli <- function(nA, nB, sA, sB, alpha, beta, ...){
  A_binom <- sample(c(rep(1, sA), rep(0, nA - sA)))
  B_binom <- sample(c(rep(1, sB), rep(0, nB - sB)))
  
  binom_test <- bayesTest(
    A_binom, 
    B_binom, 
    priors = c('alpha' = alpha, 'beta' = beta), 
    n_samples = 1e5, 
    distribution = 'bernoulli')
  percLift = 0
  #test_result = summary(binom_test, percentLift = percLift)
  p <- plot(binom_test, percentLift = percLift)$posteriors
  return(p)
}

# Poisson

cumconcat <- function(x)
  Reduce(function(x1, x2) c(x1, x2), x, accumulate = TRUE)

simulate_poisson <- function(days,
                             lambdaB,
                             effect, # A - B
                             per_day,
                             alpha,
                             beta){
  lambdaA <- lambdaB + effect
  tibble(day = seq_len(days)) %>%
    ungroup() %>%
    mutate(peopleA = rbinom(n(), per_day, .5)) %>% # half A
    mutate(peopleB = rbinom(n(), per_day, .5)) %>% # half B
    mutate(movesA = map(peopleA, rpois, lambda = lambdaA)) %>%
    mutate(movesB = map(peopleB, rpois, lambda = lambdaB)) %>%
    mutate(cumulative_movesA = cumconcat(movesA)) %>%
    mutate(cumulative_movesB = cumconcat(movesB)) %>%
    mutate(alpha = alpha, beta = beta)
}

vec_posterior_poisson <- function(cumulative_movesA, cumulative_movesB, alpha, beta, ...) {
  sapply(seq_along(cumulative_movesA), function(i) {
    posterior_poisson(cumulative_movesA[i], cumulative_movesB[i], alpha[i], beta[i],  ...)
  })
}

posterior_poisson <- function(cumulative_movesA, cumulative_movesB, alpha, beta, ...){
  A_pois <- cumulative_movesA[[1]]
  B_pois <- cumulative_movesB[[1]]
  
  poisson_test <- bayesTest(
    A_pois, 
    B_pois, 
    priors = c('shape' = alpha, 'rate' = beta), 
    n_samples = 1e5, 
    distribution = 'poisson')
  percLift = 0
  #test_result = summary(poisson_test, percentLift = percLift)
  p <- plot(poisson_test, percentLift = percLift)$posteriors
  #print(length(p))
  return(p)
}

# Combined Bernoulli & Exponential

simulate_bernoulli_exponential <- function(days,
                                           pB,
                                           lambdaB,
                                           effect_p, # A - B
                                           effect_lambda, # A - B
                                           per_day,
                                           alpha1,
                                           beta1,
                                           alpha2,
                                           beta2){
  lambdaA <- lambdaB + effect_lambda
  tibble(day = seq_len(days)) %>%
    tidyr::crossing(type = c("A", "B")) %>% 
    ungroup() %>%
    mutate(total = rbinom(n(), per_day, .5)) %>% # half A, half B
    mutate(success = rbinom(n(), total, pB + effect_p * (type == "A"))) %>% 
    group_by(type) %>%
    mutate(n = cumsum(total),  s = cumsum(success)) %>%
    ungroup() %>%
    select(-success, -total) %>%
    tidyr::gather(metric, value, n:s) %>%
    tidyr::unite(metric2, metric, type, sep = "") %>%
    tidyr::spread(metric2, value) %>%
    mutate(alpha1 = alpha1, beta1 = beta1) %>% 
    mutate(cumulative_spendA = map(sA, rexp, rate = 1/lambdaA)) %>%
    mutate(cumulative_spendB = map(sB, rexp, rate = 1/lambdaB)) %>%
    mutate(alpha2 = alpha2, beta2 = beta2)
}
vec_posterior_bernoulli_exponential <- function(nA, nB, sA, sB, 
                                                cumulative_spendA, cumulative_spendB,
                                                alpha1, beta1, alpha2, beta2,  ...) {
  if (length(sA) > 1) {
    ret <- sapply(seq_along(nA), function(i) {
      posterior_bernoulli_exponential(nA[i], nB[i], sA[i], sB[i],
                                      cumulative_spendA[i], cumulative_spendB[i],
                                      alpha1[i], beta1[i], alpha2[i], beta2[i], ...)
    })
    return(ret)
  }
  posterior_bernoulli_exponential(nA, nB, sA, sB, cumulative_spendA, cumulative_spendB, alpha1, beta1, alpha2, beta2, ...)
}

posterior_bernoulli_exponential <- function(nA, nB, sA, sB, 
                                            cumulative_spendA, cumulative_spendB,
                                            alpha1, beta1, alpha2, beta2, ...){
  sampleA_idx <- sample(1:nA)
  sampleB_idx <- sample(1:nB)
  A_binom <- c(rep(1, sA), rep(0, nA - sA))[sampleA_idx]
  B_binom <- c(rep(1, sB), rep(0, nB - sB))[sampleB_idx]
  A_exp <- c(cumulative_spendA[[1]], rep(0, nA - sA))[sampleA_idx]
  B_exp <- c(cumulative_spendB[[1]], rep(0, nB - sB))[sampleB_idx]
  
  test1 <- bayesTest(
    A_binom, 
    B_binom, 
    priors = c('alpha' = alpha1, 'beta' = beta1), 
    n_samples = 1e5, 
    distribution = 'bernoulli')
  
  test2 <- bayesTest(
    A_exp, 
    B_exp, 
    priors = c('shape' = alpha2, 'rate' = beta2), 
    n_samples = 1e5, 
    distribution = 'exponential')
  
  ber_exp_test <- bayesAB::combine(test1, test2, f = `*`, params = c('Probability', 'Lambda'), newName = 'Expectation')
  
  percLift = 0
  #test_result = summary(binom_test, percentLift = percLift)
  p <- plot(ber_exp_test, percentLift = percLift)$posteriors
  return(p)
}