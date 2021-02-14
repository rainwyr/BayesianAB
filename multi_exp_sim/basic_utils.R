# Binomial

simulate_binomial <- function(nreps,
                              days,
                              pB,
                              effect, # A - B
                              per_day,
                              alpha,
                              beta){
  tibble(replicate = seq_len(nreps)) %>%
  tidyr::crossing(day = seq_len(days)) %>%
  tidyr::crossing(type = c("A", "B")) %>% 
  ungroup() %>%
  mutate(total = rbinom(n(), per_day, .5)) %>% # half A, half B
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

vec_expected_loss_binomial <- function(nA, nB, sA, sB, alpha, beta, ...) {
  if (length(sA) > 1) {
    ret <- sapply(seq_along(nA), function(i) {
      expected_loss_binomial(nA[i], nB[i], sA[i], sB[i], alpha[i], beta[i], ...)
    })
    return(ret)
  }
  expected_loss_binomial(nA, nB, sA, sB, alpha, beta, ...)
}

expected_loss_binomial <- function(nA, nB, sA, sB, alpha, beta, ...){
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
  return(round(test_result$posteriorExpectedLoss$Probability,6))
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
                             beta){
  lambdaA <- lambdaB + effect
  tibble(replicate = seq_len(nreps)) %>%
    tidyr::crossing(day = seq_len(days)) %>%
    ungroup() %>%
    mutate(peopleA = rbinom(n(), per_day, .5)) %>% # half A
    mutate(peopleB = rbinom(n(), per_day, .5)) %>% # half B
    mutate(movesA = map(peopleA, rpois, lambda = lambdaA)) %>%
    mutate(movesB = map(peopleB, rpois, lambda = lambdaB)) %>%
    group_by(replicate) %>%
    mutate(cumulative_movesA = cumconcat(movesA)) %>%
    mutate(cumulative_movesB = cumconcat(movesB)) %>%
    ungroup() %>%
    mutate(alpha = alpha, beta = beta)
}

vec_expected_loss_poisson <- function(cumulative_movesA, cumulative_movesB, alpha, beta, ...) {
  sapply(seq_along(cumulative_movesA), function(i) {
    expected_loss_poisson(cumulative_movesA[i], cumulative_movesB[i], alpha[i], beta[i],  ...)
  })
}

expected_loss_poisson <- function(cumulative_movesA, cumulative_movesB, alpha, beta, ...){
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
  return(round(test_result$posteriorExpectedLoss$Lambda,6))
}

vec_two_t_pval <- function(cumulative_movesA, cumulative_movesB, ...) {
  sapply(seq_along(cumulative_movesA), function(i) {
      two_t_pval(cumulative_movesA[i], cumulative_movesB[i], ...)
  })
}

two_t_pval <- function(cumulative_movesA, cumulative_movesB, ...){
  x <- cumulative_movesA[[1]]
  y <- cumulative_movesB[[1]]
  return(t.test(x, y)$p.value)
}