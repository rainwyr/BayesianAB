perc_as_num <- function(vec){
  as.numeric(gsub("%", "", vec))
}

ber_design <- read.csv("output/design_bernoulli.csv", stringsAsFactors = FALSE)
pois_design <- read.csv("output/design_poisson.csv", stringsAsFactors = FALSE)
ber_exp_design <- read.csv("output/design_bernoulli_exponential.csv", stringsAsFactors = FALSE)

# Bernoulli

for(i in 1:nrow(ber_design)){
  if(ber_design$category[i] == 'bernoulli' & ber_design$prior_alpha[i] == 3 & ber_design$prior_beta[i] == 100){
    ber_design$prior[i] = 'directional'
  }
  else if(ber_design$category[i] == 'bernoulli' & ber_design$prior_alpha[i] == 30 & ber_design$prior_beta[i] == 1000){
    ber_design$prior[i] = 'confident'
  }
  else if(ber_design$category[i] == 'bernoulli' & ber_design$prior_alpha[i] == 30 & ber_design$prior_beta[i] == 10){
    ber_design$prior[i] = 'wrong'
  }
  else if(ber_design$category[i] == 'bernoulli' & ber_design$prior_alpha[i] == 1 & ber_design$prior_beta[i] == 1){
    ber_design$prior[i] = 'neutral'
  }
  else{
    ber_design$prior[i] = 'others'
  }
}

ber_design <- ber_design %>%
  mutate(peek_multiplier_freq = round(perc_as_num(freq_treat_peek)/perc_as_num(freq_treat),1),
         peek_multiplier_bayes_0.1 = round(perc_as_num(bayes_treat_peek_0.1)/perc_as_num(bayes_treat_0.1),1),
         peek_multiplier_bayes_0.01 = round(perc_as_num(bayes_treat_peek_0.01)/perc_as_num(bayes_treat_0.01),1),
         peek_multiplier_bayes_0.001 = round(perc_as_num(bayes_treat_peek_0.001)/perc_as_num(bayes_treat_0.001),1),
         peek_multiplier_bayes_0.0001 = round(perc_as_num(bayes_treat_peek_0.0001)/perc_as_num(bayes_treat_0.0001),1),
         peek_multiplier_bayes_0.00001 = round(perc_as_num(bayes_treat_peek_0.00001)/perc_as_num(bayes_treat_0.00001),1),
  )
ber_design[ber_design == 'NaN'] = NA
ber_design[ber_design == 'Inf'] = NA

ber_design$case_id <- ifelse(ber_design$effect == 0, 1, 2)

ber_design <- ber_design %>%
  select(case_id, category, effect, sample_size_per_day,
         prior_alpha, prior_beta, prior,
         freq_treat, freq_treat_peek, peek_multiplier_freq,
         bayes_treat_0.1, bayes_treat_peek_0.1, peek_multiplier_bayes_0.1,
         bayes_treat_0.01, bayes_treat_peek_0.01, peek_multiplier_bayes_0.01,
         bayes_treat_0.001, bayes_treat_peek_0.001, peek_multiplier_bayes_0.001,
         bayes_treat_0.0001, bayes_treat_peek_0.0001, peek_multiplier_bayes_0.0001,
         bayes_treat_0.00001, bayes_treat_peek_0.00001, peek_multiplier_bayes_0.00001,
  )


# Poisson

for(i in 1:nrow(pois_design)){
  if(pois_design$category[i] == 'poisson' & pois_design$prior_alpha[i] == 23 & pois_design$prior_beta[i] == 1){
    pois_design$prior[i] = 'directional'
  }
  else if(pois_design$category[i] == 'poisson' & pois_design$prior_alpha[i] == 230 & pois_design$prior_beta[i] == 10){
    pois_design$prior[i] = 'confident'
  }
  else if(pois_design$category[i] == 'poisson' & pois_design$prior_alpha[i] == 6 & pois_design$prior_beta[i] == 1){
    pois_design$prior[i] = 'wrong'
  }
  else{
    pois_design$prior[i] = 'others'
  }
}

pois_design <- pois_design %>%
  mutate(peek_multiplier_freq = round(perc_as_num(freq_treat_peek)/perc_as_num(freq_treat),1),
         peek_multiplier_bayes_0.1 = round(perc_as_num(bayes_treat_peek_0.1)/perc_as_num(bayes_treat_0.1),1),
         peek_multiplier_bayes_0.01 = round(perc_as_num(bayes_treat_peek_0.01)/perc_as_num(bayes_treat_0.01),1),
         peek_multiplier_bayes_0.001 = round(perc_as_num(bayes_treat_peek_0.001)/perc_as_num(bayes_treat_0.001),1),
         peek_multiplier_bayes_0.0001 = round(perc_as_num(bayes_treat_peek_0.0001)/perc_as_num(bayes_treat_0.0001),1),
         peek_multiplier_bayes_0.00001 = round(perc_as_num(bayes_treat_peek_0.00001)/perc_as_num(bayes_treat_0.00001),1),
  )
pois_design[pois_design == 'NaN'] = NA
pois_design[pois_design == 'Inf'] = NA

pois_design$case_id <- ifelse(pois_design$effect == 0, 3, 4)

pois_design <- pois_design %>%
  select(case_id, category, effect, sample_size_per_day,
         prior_alpha, prior_beta, prior,
         freq_treat, freq_treat_peek, peek_multiplier_freq,
         bayes_treat_0.1, bayes_treat_peek_0.1, peek_multiplier_bayes_0.1,
         bayes_treat_0.01, bayes_treat_peek_0.01, peek_multiplier_bayes_0.01,
         bayes_treat_0.001, bayes_treat_peek_0.001, peek_multiplier_bayes_0.001,
         bayes_treat_0.0001, bayes_treat_peek_0.0001, peek_multiplier_bayes_0.0001,
         bayes_treat_0.00001, bayes_treat_peek_0.00001, peek_multiplier_bayes_0.00001,
  )

# Bernoulli-Exponential

for(i in 1:nrow(ber_exp_design)){
  if(ber_exp_design$category[i] == 'bernoulli-exponential' & ber_exp_design$prior_alpha1[i] == 3 & ber_exp_design$prior_beta1[i] == 100){
    ber_exp_design$prior1[i] = 'directional'
  }
  else if(ber_exp_design$category[i] == 'bernoulli-exponential' & ber_exp_design$prior_alpha1[i] == 30 & ber_exp_design$prior_beta1[i] == 1000){
    ber_exp_design$prior1[i] = 'confident'
  }
  else if(ber_exp_design$category[i] == 'bernoulli-exponential' & ber_exp_design$prior_alpha1[i] == 30 & ber_exp_design$prior_beta1[i] == 10){
    ber_exp_design$prior1[i] = 'wrong'
  }
  else if(ber_exp_design$category[i] == 'bernoulli-exponential' & ber_exp_design$prior_alpha1[i] == 1 & ber_exp_design$prior_beta1[i] == 1){
    ber_exp_design$prior1[i] = 'neutral'
  }
  else{
    ber_exp_design$prior1[i] = 'others'
  }
}

for(i in 1:nrow(ber_exp_design)){
  if(ber_exp_design$category[i] == 'bernoulli-exponential' & ber_exp_design$prior_alpha2[i] == 25 & ber_exp_design$prior_beta2[i] == 5){
    ber_exp_design$prior2[i] = 'directional'
  }
  else if(ber_exp_design$category[i] == 'bernoulli-exponential' & ber_exp_design$prior_alpha2[i] == 250 & ber_exp_design$prior_beta2[i] == 50){
    ber_exp_design$prior2[i] = 'confident'
  }
  else if(ber_exp_design$category[i] == 'bernoulli-exponential' & ber_exp_design$prior_alpha2[i] == 2 & ber_exp_design$prior_beta2[i] == 1){
    ber_exp_design$prior2[i] = 'wrong'
  }
  else{
    ber_exp_design$prior2[i] = 'others'
  }
}

ber_exp_design <- ber_exp_design %>%
  mutate(peek_multiplier_freq = round(perc_as_num(freq_treat_peek)/perc_as_num(freq_treat),1),
         peek_multiplier_bayes_0.1 = round(perc_as_num(bayes_treat_peek_0.1)/perc_as_num(bayes_treat_0.1),1),
         peek_multiplier_bayes_0.01 = round(perc_as_num(bayes_treat_peek_0.01)/perc_as_num(bayes_treat_0.01),1),
         peek_multiplier_bayes_0.001 = round(perc_as_num(bayes_treat_peek_0.001)/perc_as_num(bayes_treat_0.001),1),
         peek_multiplier_bayes_0.0001 = round(perc_as_num(bayes_treat_peek_0.0001)/perc_as_num(bayes_treat_0.0001),1),
         peek_multiplier_bayes_0.00001 = round(perc_as_num(bayes_treat_peek_0.00001)/perc_as_num(bayes_treat_0.00001),1),
  )
ber_exp_design[ber_exp_design == 'NaN'] = NA
ber_exp_design[ber_exp_design == 'Inf'] = NA

ber_exp_design$case_id <- ifelse((ber_exp_design$effect_p == 0) & (ber_exp_design$effect_lambda == 0), 5, 6)

ber_exp_design <- ber_exp_design %>%
  select(case_id, category, effect_p, effect_lambda, sample_size_per_day,
         prior_alpha1, prior_beta1, prior_alpha2, prior_beta2, prior1, prior2, 
         freq_treat, freq_treat_peek, peek_multiplier_freq,
         bayes_treat_0.1, bayes_treat_peek_0.1, peek_multiplier_bayes_0.1,
         bayes_treat_0.01, bayes_treat_peek_0.01, peek_multiplier_bayes_0.01,
         bayes_treat_0.001, bayes_treat_peek_0.001, peek_multiplier_bayes_0.001,
         bayes_treat_0.0001, bayes_treat_peek_0.0001, peek_multiplier_bayes_0.0001,
         bayes_treat_0.00001, bayes_treat_peek_0.00001, peek_multiplier_bayes_0.00001,
  )
write.csv(ber_design, "output/design_labeled_bernoulli.csv", row.names = FALSE)
write.csv(pois_design, "output/design_labeled_poisson.csv", row.names = FALSE)
write.csv(ber_exp_design, "output/design_labeled_bernoulli_exponential.csv", row.names = FALSE)