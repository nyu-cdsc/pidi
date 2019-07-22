# Binomial GEE looking at both adult and child data
# Looking at yes/no responses by: 
# (1) age (categorical), 
# (2) condition (generic/specific), and 
# (3) stimulus type (mentioned/unmentioned) [within-subject]

expt1_test_results <- geeglm(
  response ~ subjectGroup + age_categorical + stimulus + subjectGroup*age_categorical + 
    subjectGroup*stimulus + age_categorical*stimulus + subjectGroup*age_categorical*stimulus, 
  data = expt1_test, 
  family = binomial(logit), 
  id = id, 
  corstr = "exchangeable")
summary(expt1_test_results)
expt1_test_anova <- anova(expt1_test_results)
expt1_test_anova

# pairs(emmeans(expt1_test_results, ~ age_categorical | subjectGroup | stimulus))
