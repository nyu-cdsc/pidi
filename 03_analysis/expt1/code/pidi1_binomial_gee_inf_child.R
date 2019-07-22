# Binomial GEE looking at child data only
# Looking at trials where participant made inference
# (Yes to mentioned and no to previously mentioned) by: 
# (1) age (categorical), 
# (2) condition (generic/specific)

# 1 kid doesn't have a dob, so no exact age
# omitted from analyses
expt1_inference_child_gee <- expt1_inference_child %>%
  filter(!is.na(age_exact))

expt1_inference_child_results <- geeglm(
  inf ~ subjectGroup + age_exact + subjectGroup*age_exact, 
  data = expt1_inference_child_gee, 
  family = binomial(logit), 
  id = id, 
  corstr = "exchangeable")
summary(expt1_inference_child_results)
expt1_inference_child_anova <- anova(expt1_inference_child_results)
expt1_inference_child_anova
