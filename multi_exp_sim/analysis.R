perc_as_num <- function(vec) as.numeric(gsub("%", "", vec))/100
format_percent <- function(x, digits = 2) paste0(round(100 * x, digits), "%")

library(dplyr)
library(ggplot2)
ber_design <- read.csv("output/design_labeled_bernoulli.csv", stringsAsFactors = FALSE)
pois_design <- read.csv("output/design_labeled_poisson.csv", stringsAsFactors = FALSE)
ber_exp_design <- read.csv("output/design_labeled_bernoulli_exponential.csv", stringsAsFactors = FALSE)

get_relevant_data <- function(case_id){
  if(case_id %in% c(1,2)){
    case_df = ber_design %>% 
      filter(case_id == case_id) %>%
      select(effect,
             sample_size_per_day, 
             prior,
             freq_treat, 
             freq_treat_peek, 
             peek_multiplier_freq,
             bayes_treat = bayes_treat_0.001, 
             bayes_treat_peek = bayes_treat_peek_0.001, 
             peek_multiplier_bayes = peek_multiplier_bayes_0.001)
    if(case_id == 1){
      case_df = case_df %>% 
        filter(effect == 0) 
    } else {
      case_df = case_df %>% 
        filter(effect != 0) 
    }
  }
  else if(case_id %in% c(3,4)){
    case_df = pois_design %>% 
      filter(case_id == case_id) %>%
      select(effect,
             sample_size_per_day, 
             prior,
             freq_treat, 
             freq_treat_peek, 
             peek_multiplier_freq,
             bayes_treat = bayes_treat_0.00001, 
             bayes_treat_peek = bayes_treat_peek_0.00001, 
             peek_multiplier_bayes = peek_multiplier_bayes_0.00001)
    if(case_id == 3){
      case_df = case_df %>% 
        filter(effect == 0) 
    } else {
      case_df = case_df %>% 
        filter(effect != 0) 
    }
  }
  else if(case_id %in% c(5,6)){
    case_df = ber_exp_design %>% 
      filter(case_id == case_id) %>%
      mutate(prior=paste0(prior1, '-', prior2)) %>%
      select(effect_p,
             effect_lambda,
             sample_size_per_day, 
             prior,
             freq_treat, 
             freq_treat_peek, 
             peek_multiplier_freq,
             bayes_treat = bayes_treat_0.001, 
             bayes_treat_peek = bayes_treat_peek_0.001, 
             peek_multiplier_bayes = peek_multiplier_bayes_0.001)
    if(case_id == 5){
      case_df = case_df %>% 
        filter((effect_p == 0) & (effect_lambda == 0))
    } else {
      case_df = case_df %>% 
        filter((effect_p != 0) | (effect_lambda != 0)) 
    }
  }
}

analyze_peeking <- function(case_df){
  case_df %>% 
    filter(sample_size_per_day == 500) %>%
    summarise(
      avg_peek_multiplier_freq = round(mean(peek_multiplier_freq),2), 
      avg_peek_multiplier_bayes = round(mean(peek_multiplier_bayes),2))
}
analyze_sample_size <- function(case_df){
  case_df %>% 
    group_by(sample_size_per_day) %>%
    summarise(
      avg_freq_treat=format_percent(mean(perc_as_num(freq_treat))),
      avg_freq_treat_peek=format_percent(mean(perc_as_num(freq_treat_peek))),
      avg_bayes_treat=format_percent(mean(perc_as_num(bayes_treat))),
      avg_bayes_treat_peek=format_percent(mean(perc_as_num(bayes_treat_peek)))
    )
}
analyze_prior <- function(case_df){
  case_df %>% 
    group_by(prior) %>%
    summarise(
      avg_freq_treat=format_percent(mean(perc_as_num(freq_treat))),
      avg_freq_treat_peek=format_percent(mean(perc_as_num(freq_treat_peek))),
      avg_bayes_treat=format_percent(mean(perc_as_num(bayes_treat))),
      avg_bayes_treat_peek=format_percent(mean(perc_as_num(bayes_treat_peek)))
    )
}
