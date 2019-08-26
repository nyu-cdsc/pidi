# Binomial GEE 
# Looking at yes/no responses by: 
# (1) condition (generic/specific), and 
# (2) group type (mentioned/unmentioned) [within-subject]

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
# 
# # simple slopes
# expt2_test_emtrends <- as.data.frame(emtrends(expt2_test_results, 
#                                                     ~ condition | group, 
#                                                     var = "age_exact"))
# 
# expt2_test_emtrends_tvalues <- expt2_test_emtrends$age_exact.trend/expt2_test_emtrends$SE
# expt2_test_emtrends_dfs <- expt2_test_emtrends$df
# expt2_test_emtrends_pvalues <- 2*pt(-abs(expt2_test_emtrends_tvalues), expt2_test_emtrends_dfs)
# expt2_test_emtrends$pvalues <- expt2_test_emtrends_pvalues
# 
# # comparing slopes 
# expt2_test_pairs <- as.data.frame(pairs(emtrends(expt2_test_results, 
#                                          ~ condition | group, 
#                                          var = "age_exact")))