# given ret from simulation

evaluate_simulation <- function(design, row, ret, thresholds, prefix, cat='binomial', sig_thres=0.05){

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
  
  last_loss <- ret %>%
    group_by(replicate) %>%
    summarize(last = last(expected_loss)) %>%
    .$last
  min_loss <- ret %>%
    group_by(replicate) %>%
    summarize(min = min(expected_loss)) %>%
    .$min
  
  for(thres in thresholds){
    design[row, paste0('bayes_treat_', thres)] <- format_percent(mean(last_loss < thres))
    design[row, paste0('bayes_treat_peek_', thres)] <- format_percent(mean(min_loss < thres))
  }
  
  return(design)
  
}