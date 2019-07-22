# Binomial GEE looking at both adult and child data
# Looking at trials where participant made inference
# (Yes to mentioned and no to previously mentioned) by: 
# (1) age (categorical), 
# (2) condition (generic/specific)

expt1_inference_results <- geeglm(
  inf ~ subjectGroup + age_categorical + subjectGroup*age_categorical, 
  data = expt1_inference, 
  family = binomial(logit), 
  id = id, 
  corstr = "exchangeable")
summary(expt1_inference_results)
expt1_inference_anova <- anova(expt1_inference_results)
expt1_inference_anova

pairs(emmeans(expt1_inference_results, ~ age_categorical | subjectGroup))
pairs(emmeans(expt1_inference_results, ~ subjectGroup | age_categorical))