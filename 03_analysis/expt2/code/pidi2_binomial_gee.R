# Binomial GEE 
# Looking at yes/no responses by: 
# (1) condition (generic/specific), and 
# (2) group type (mentioned/unmentioned) [within-subject]
expt2_test <- expt2_test %>% 
  arrange(id, trial_order)

expt2_test_results <- geeglm(
  response ~ condition + group + condition*group, 
  data = expt2_test, 
  family = binomial(logit), 
  id = new_id, 
  corstr = "exchangeable")
summary(expt2_test_results)
expt2_test_anova <- anova(expt2_test_results)
expt2_test_anova

emmeans(expt2_test_results, pairwise ~ condition | group)
