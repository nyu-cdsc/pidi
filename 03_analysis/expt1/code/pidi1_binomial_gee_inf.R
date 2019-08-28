# Binomial GEE looking at both adult and child data
# Looking at trials where participant made inference
# (Yes to mentioned and no to previously mentioned) by: 
# (1) age (categorical), 
# (2) condition (generic/specific)

expt1_inference <- expt1_inference %>% 
  arrange(id, trial_order)

expt1_inference_results <- geeglm(
  inf ~ condition + age_categorical + condition*age_categorical, 
  data = expt1_inference, 
  family = binomial(logit), 
  id = new_id, 
  corstr = "exchangeable")
summary(expt1_inference_results)
expt1_inference_anova <- anova(expt1_inference_results)
expt1_inference_anova


expt1_inference_results_pair <- as.data.frame(pairs(emmeans(expt1_inference_results, 
                                              ~ condition | age_categorical)))
