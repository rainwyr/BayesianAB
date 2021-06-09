# Bernoulli

simulate_bernoulli <- function(nreps,
                               days,
                               pB,
                               effect, # A - B
                               per_day,
                               alpha,
                               beta,
                               unbalance_ratio){
  tibble(replicate = seq_len(nreps)) %>%
    tidyr::crossing(day = seq_len(days)) %>%
    tidyr::crossing(type = c("A", "B")) %>% 
    ungroup() %>%
    mutate(total = rbinom(n(), per_day, .5)) %>% # half A, half B
    mutate(total = round(ifelse(type == "B", total * unbalance_ratio, total))) %>%
    mutate(success = rbinom(n(), total, pB + effect * (type == "A"))) %>% 
    group_by(replicate, type) %>%
    mutate(n = cumsum(total),  s = cumsum(success)) %>%
    ungroup() %>%
    select(-success, -total) %>%
    tidyr::gather(metric, value, n:s) %>%
    tidyr::unite(metric2, metric, type, sep = "") %>%
    tidyr::spread(metric2, value) %>%
    mutate(alpha = alpha, beta = beta)
}

vec_prob_bernoulli <- function(nA, nB, sA, sB, alpha, beta, ...) {
  if (length(sA) > 1) {
    ret <- sapply(seq_along(nA), function(i) {
      prob_bernoulli(nA[i], nB[i], sA[i], sB[i], alpha[i], beta[i], ...)
    })
    return(ret)
  }
  prob_bernoulli(nA, nB, sA, sB, alpha, beta, ...)
}

prob_bernoulli <- function(nA, nB, sA, sB, alpha, beta, ...){
  A_binom <- c(rep(1, sA), rep(0, nA - sA))
  B_binom <- c(rep(1, sB), rep(0, nB - sB))
  
  binom_test <- bayesTest(
    A_binom, 
    B_binom, 
    priors = c('alpha' = alpha, 'beta' = beta), 
    n_samples = 1e5, 
    distribution = 'bernoulli')
  percLift = 0
  test_result = summary(binom_test, percentLift = percLift)
  return(round(test_result$probability$Probability,6))
}

vec_two_prop_pval <- function(nA, nB, sA, sB, ...) {
  if (length(sA) > 1) {
    ret <- sapply(seq_along(nA), function(i) {
      two_prop_pval(nA[i], nB[i], sA[i], sB[i], ...)
    })
    return(ret)
  }
  two_prop_pval(nA, nB, sA, sB)
}

two_prop_pval <- function(nA, nB, sA, sB, ...){
  return(prop.test(x = c(sA, sB), n = c(nA, nB))$p.value)
}

# Poisson

cumconcat <- function(x)
  Reduce(function(x1, x2) c(x1, x2), x, accumulate = TRUE)

simulate_poisson <- function(nreps,
                             days,
                             lambdaB,
                             effect, # A - B
                             per_day,
                             alpha,
                             beta,
                             unbalance_ratio){
  lambdaA <- lambdaB + effect
  tibble(replicate = seq_len(nreps)) %>%
    tidyr::crossing(day = seq_len(days)) %>%
    ungroup() %>%
    mutate(peopleA = rbinom(n(), per_day, .5)) %>% # half A
    mutate(peopleB = rbinom(n(), per_day * unbalance_ratio, .5)) %>% # half B
    mutate(movesA = map(peopleA, rpois, lambda = lambdaA)) %>%
    mutate(movesB = map(peopleB, rpois, lambda = lambdaB)) %>%
    group_by(replicate) %>%
    mutate(cumulative_movesA = cumconcat(movesA)) %>%
    mutate(cumulative_movesB = cumconcat(movesB)) %>%
    ungroup() %>%
    mutate(alpha = alpha, beta = beta)
}

vec_prob_poisson <- function(cumulative_movesA, cumulative_movesB, alpha, beta, ...) {
  sapply(seq_along(cumulative_movesA), function(i) {
    prob_poisson(cumulative_movesA[i], cumulative_movesB[i], alpha[i], beta[i],  ...)
  })
}

prob_poisson <- function(cumulative_movesA, cumulative_movesB, alpha, beta, ...){
  A_pois <- cumulative_movesA[[1]]
  B_pois <- cumulative_movesB[[1]]
  
  poisson_test <- bayesTest(
    A_pois, 
    B_pois, 
    priors = c('shape' = alpha, 'rate' = beta), 
    n_samples = 1e5, 
    distribution = 'poisson')
  percLift = 0
  test_result = summary(poisson_test, percentLift = percLift)
  return(round(test_result$probability$Lambda,6))
}

vec_two_t_pval_moves <- function(cumulative_movesA, cumulative_movesB, ...) {
  sapply(seq_along(cumulative_movesA), function(i) {
    two_t_pval_moves(cumulative_movesA[i], cumulative_movesB[i], ...)
  })
}

two_t_pval_moves <- function(cumulative_movesA, cumulative_movesB, ...){
  x <- cumulative_movesA[[1]]
  y <- cumulative_movesB[[1]]
  return(t.test(x, y)$p.value)
}

# Bernoulli-Exponential

simulate_bernoulli_exponential <- function(nreps,
                                           days,
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
  tibble(replicate = seq_len(nreps)) %>%
    tidyr::crossing(day = seq_len(days)) %>%
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

vec_prob_bernoulli_exponential <- function(nA, nB, sA, sB, 
                                                    cumulative_spendA, cumulative_spendB,
                                                    alpha1, beta1, alpha2, beta2, ...) {
  sapply(seq_along(cumulative_spendA), function(i) {
    prob_bernoulli_exponential(nA[i], nB[i], sA[i], sB[i],
                                        cumulative_spendA[i], cumulative_spendB[i], 
                                        alpha1[i], beta1[i], alpha2[i], beta2[i],  ...)
  })
}

prob_bernoulli_exponential <- function(nA, nB, sA, sB, 
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
  
  rm("A_binom")
  rm("B_binom")
  
  test2 <- bayesTest(
    A_exp, 
    B_exp, 
    priors = c('shape' = alpha2, 'rate' = beta2), 
    n_samples = 1e5, 
    distribution = 'exponential')
  
  rm("A_exp")
  rm("B_exp")
  
  ber_exp_test <- bayesAB::combine(test1, test2, f = `*`, params = c('Probability', 'Lambda'), newName = 'Expectation')
  
  rm("test1")
  rm("test2")
  
  percLift = 0
  test_result = summary(ber_exp_test, percentLift = percLift)
  return(round(test_result$probability$Expectation,6))
}

vec_two_t_pval_spend <- function(cumulative_spendA, cumulative_spendB, ...) {
  sapply(seq_along(cumulative_spendA), function(i) {
    two_t_pval_spend(cumulative_spendA[i], cumulative_spendB[i], ...)
  })
}

two_t_pval_spend <- function(cumulative_spendA, cumulative_spendB, ...){
  if((length(cumulative_spendA) < 5)|(length(cumulative_spendB) < 5)){
    return(NA)
  }
  x <- cumulative_spendA[[1]]
  y <- cumulative_spendB[[1]]
  return(t.test(x, y)$p.value)
}