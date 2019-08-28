# Binomial GEE looking at both adult and child data
# Looking at yes/no responses by: 
# (1) age (categorical), 
# (2) condition (generic/specific), and 
# (3) group type (mentioned/unmentioned) [within-subject]

expt3_test <- expt3_test %>% 
  arrange(age_categorical, id, trial_order)

expt3_test_results <- geeglm(
  response ~ condition + age_categorical + group + condition*age_categorical + 
    condition*group + age_categorical*group + group*condition*age_categorical, 
  data = expt3_test, 
  family = binomial(logit), 
  id = new_id, 
  corstr = "exchangeable")
summary(expt3_test_results)
expt3_test_anova <- anova(expt3_test_results)
expt3_test_anova


emmeans(expt3_test_results, pairwise ~ condition | age_categorical | group)

expt3_test4 <- expt3_test %>% 
  filter(age_categorical == "4")

expt3_test5 <- expt3_test %>% 
  filter(age_categorical == "5")

expt3_test6 <- expt3_test %>% 
  filter(age_categorical == "6")

expt3_test_adult <- expt3_test %>% 
  filter(age_categorical == "8")

expt3_test4_results <- geeglm(
  response ~ condition +  group + condition*group, 
  data = expt3_test4, 
  family = binomial(logit), 
  id = new_id, 
  corstr = "exchangeable")
summary(expt3_test4_results)
expt3_test4_anova <- anova(expt3_test4_results)
expt3_test4_anova

expt3_test5_results <- geeglm(
  response ~ condition +  group + condition*group, 
  data = expt3_test5, 
  family = binomial(logit), 
  id = new_id, 
  corstr = "exchangeable")
summary(expt3_test5_results)
expt3_test5_anova <- anova(expt3_test5_results)
expt3_test5_anova

expt3_test6_results <- geeglm(
  response ~ condition +  group + condition*group, 
  data = expt3_test6, 
  family = binomial(logit), 
  id = new_id, 
  corstr = "exchangeable")
summary(expt3_test6_results)
expt3_test6_anova <- anova(expt3_test6_results)
expt3_test6_anova

expt3_testadult_results <- geeglm(
  response ~ condition +  group + condition*group, 
  data = expt3_test_adult, 
  id = new_id, 
  family = binomial(logit),
  corstr = "independence")
summary(expt3_testadult_results)
expt3_testadult_anova <- anova(expt3_testadult_results)
expt3_testadult_anova



