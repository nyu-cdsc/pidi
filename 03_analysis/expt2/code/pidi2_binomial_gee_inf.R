# Binomial GEE 
# Looking at trials where participant made inference
# (Yes to mentioned and no to previously mentioned) by: 
# (2) condition (generic/specific)
expt2_inference <- expt2_inference %>% 
  arrange(id, trial_order)

expt2_inference_results <- geeglm(
  inf ~ condition, 
  data = expt2_inference, 
  family = binomial(logit), 
  id = new_id, 
  corstr = "exchangeable")
summary(expt2_inference_results)
expt2_inference_anova <- anova(expt2_inference_results)
expt2_inference_anova

