# given ret from simulation

evaluate_simulation <- function(design, row, ret, thresholds, prefix, cat, sig_thres=0.05){

  # Evaluation
  last_pvalues <- ret %>%
    group_by(replicate) %>%
    summarize(last = last(pval)) %>%
    .$last
  min_pvalues <- ret %>%
    group_by(replicate) %>%
    summarize(min = min(pval)) %>%
    .$min
  
  design[row, 'freq_treat'] <- format_percent(mean(last_pvalues < sig_thres))
  design[row, 'freq_treat_peek'] <- format_percent(mean(min_pvalues < sig_thres))
  
  last_prob <- ret %>%
    group_by(replicate) %>%
    summarize(last = last(prob)) %>%
    .$last
  max_prob <- ret %>%
    group_by(replicate) %>%
    summarize(max = max(prob)) %>% # actually P(A>B)
    .$max
  
  for(thres in thresholds){
    design[row, paste0('bayes_treat_', thres)] <- format_percent(mean(last_prob > thres))
    design[row, paste0('bayes_treat_peek_', thres)] <- format_percent(mean(max_prob > thres))
  }
  
  return(design)
  
}