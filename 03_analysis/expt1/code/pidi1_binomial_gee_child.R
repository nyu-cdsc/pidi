# Binomial GEE looking at child data only
# age is treated categorically
# Looking at yes/no responses by: 
# (1) age (continuous), 
# (2) condition (generic/specific), and 
# (3) stimulus type (mentioned/unmentioned) [within-subject]

# 1 kid doesn't have a dob, so no exact age
# omitted from analyses
expt1_test_child_gee <- expt1_test_child %>%
  filter(!is.na(age_exact))

expt1_test_child_results <- geeglm(
  response ~ subjectGroup + age_exact + stimulus + subjectGroup*age_exact + 
    subjectGroup*stimulus + age_exact*stimulus + subjectGroup*age_exact*stimulus, 
  data = expt1_test_child_gee, 
  family = binomial(logit), 
  id = as.character(id), 
  corstr = "exchangeable")
summary(expt1_test_child_results)
expt1_test_child_anova <- anova(expt1_test_child_results)
expt1_test_child_anova
