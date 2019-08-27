# Binomial GEE looking at both adult and child data
# Looking at yes/no responses by: 
# (1) age (categorical), 
# (2) condition (generic/specific), and 
# (3) group type (mentioned/unmentioned) [within-subject]
expt1_test <- expt1_test %>% 
  arrange(id, trial_order)

# GEE won't estimate because all adults responded "yes" for mentioned group
# changing one response so that the model won't be singular
expt1_test$response[expt1_test$id == "A12A5GVJX5RZC4" &
                      expt1_test$property == "paint" &
                      expt1_test$group == "mentioned"] <- 0


expt1_test_results <- geeglm(
  response ~ condition + age_categorical + group + condition*age_categorical + 
    condition*group + age_categorical*group + condition*age_categorical*group, 
  data = expt1_test, 
  family = binomial(logit), 
  id = new_id, 
  corstr = "exchangeable")
summary(expt1_test_results)
expt1_test_anova <- anova(expt1_test_results)
expt1_test_anova

# Estimating simple effects of group and condition by age group

expt1_test4 <- expt1_test %>% 
  filter(age_categorical == "4")

expt1_test5 <- expt1_test %>% 
  filter(age_categorical == "5")

expt1_test6 <- expt1_test %>% 
  filter(age_categorical == "6")

expt1_testadult <- expt1_test %>% 
  filter(age_categorical == "8")


expt1_test4_results <- geeglm(
  response ~ condition +  group + condition*group, 
  data = expt1_test4, 
  family = binomial(logit), 
  id = new_id, 
  corstr = "exchangeable")
summary(expt1_test4_results)
expt1_test4_anova <- anova(expt1_test4_results)
expt1_test4_anova

expt1_test5_results <- geeglm(
  response ~ condition +  group + condition*group, 
  data = expt1_test5, 
  family = binomial(logit), 
  id = new_id, 
  corstr = "exchangeable")
summary(expt1_test5_results)
expt1_test5_anova <- anova(expt1_test5_results)
expt1_test5_anova

expt1_test6_results <- geeglm(
  response ~ condition +  group + condition*group, 
  data = expt1_test6, 
  family = binomial(logit), 
  id = new_id, 
  corstr = "exchangeable")
summary(expt1_test6_results)
expt1_test6_anova <- anova(expt1_test6_results)
expt1_test6_anova

expt1_testadult_results <- geeglm(
  response ~ condition +  group + condition*group, 
  data = expt1_testadult, 
  id = new_id, 
  family = binomial(logit),
  corstr = "exchangeable")
summary(expt1_testadult_results)
expt1_testadult_anova <- anova(expt1_testadult_results)
expt1_testadult_anova
