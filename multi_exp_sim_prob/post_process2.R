perc_as_num <- function(vec){
  as.numeric(gsub("%", "", vec))
}

ber_design <- read.csv("output/design_bernoulli_0.95.csv", stringsAsFactors = FALSE)
pois_design <- read.csv("output/design_poisson_0.95.csv", stringsAsFactors = FALSE)

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
         peek_multiplier_bayes = round(perc_as_num(bayes_treat_peek_0.95)/perc_as_num(bayes_treat_0.95),1),
  )

ber_design$case_id <- ifelse(ber_design$effect == 0, 1, 2)

ber_design <- ber_design %>%
  select(case_id, category, effect, sample_size_per_day,
         prior_alpha, prior_beta, prior,
         freq_treat, freq_treat_peek, peek_multiplier_freq,
         bayes_treat_0.95, bayes_treat_peek_0.95, peek_multiplier_bayes
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
         peek_multiplier_bayes = round(perc_as_num(bayes_treat_peek_0.95)/perc_as_num(bayes_treat_0.95),1),
  )

pois_design$case_id <- ifelse(pois_design$effect == 0, 3, 4)

pois_design <- pois_design %>%
  select(case_id, category, effect, sample_size_per_day,
         prior_alpha, prior_beta, prior,
         freq_treat, freq_treat_peek, peek_multiplier_freq,
         bayes_treat_0.95, bayes_treat_peek_0.95, peek_multiplier_bayes
  )

write.csv(ber_design, "output/design_labeled_bernoulli_0.95.csv", row.names = FALSE)
write.csv(pois_design, "output/design_labeled_poisson_0.95.csv", row.names = FALSE)
