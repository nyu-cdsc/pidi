# Binomial GEE looking at child data only
# age is treated categorically
# Looking at yes/no responses by: 
# (1) age (continuous), 
# (2) condition (generic/specific), and 
# (3) group type (mentioned/unmentioned) [within-subject]

# 1 kid doesn't have a dob, so no exact age
# omitted from analyses
expt3_test_child_gee <- expt3_test_child %>%
  arrange(id, trial_order) 

expt3_test_child_results <- geeglm(
  response ~ condition + age_exact + group + condition*age_exact + 
    condition*group + age_exact*group + condition*age_exact*group, 
  data = expt3_test_child_gee, 
  family = binomial(logit), 
  id = new_id, 
  corstr = "exchangeable")
summary(expt3_test_child_results)
expt3_test_child_anova <- anova(expt3_test_child_results)
expt3_test_child_anova

expt3_test_child_emtrends <- as.data.frame(emtrends(expt3_test_child_results, 
                                                    ~ condition | group, 
                                                    var = "age_exact"))

expt3_test_child_pairs <- pairs(emtrends(expt3_test_child_results, 
                                         ~ condition | group, 
                                         var = "age_exact"))

expt3_test_child_gee_nf <- expt3_test_child_gee %>% 
  ungroup() %>%
  mutate(
    condition = ifelse(condition == "knowledge", 1, 0),
    group = ifelse(group == "mentioned", 1, 0)
  ) %>% 
  arrange(id)

# sim_slopes can't handle GEE data structure so estimating using glmer
test3 <- glmer(response ~ condition + age_exact + group + condition*age_exact + 
                 condition*group + age_exact*group + condition*age_exact*group + (1|new_id), 
               data = expt3_test_child_gee_nf,
               family = "binomial",
               control=glmerControl(optCtrl=list(maxfun=2e4)))

# getting glmer to converge by starting where previous one ended, 
# bumping up max iterations, and using a different optimizer
ss <- getME(test3, c("theta","fixef"))

test3_converge <- update(test3,
                         start = ss,
                         control = glmerControl(optimizer = "bobyqa",
                                                optCtrl = list(maxfun = 2e5)))

# Estimating exact age where slopes differ using Johnson-Neyman
expt3_test_child_jn <- sim_slopes(test3_converge, 
                                  pred = condition,
                                  modx = age_exact,
                                  mod2 = group,
                                  jnplot = TRUE)


# Getting separate plots for JN so they can be editable by ggplot
expt3_test_child_gee_nf_mentioned <- expt3_test_child_gee_nf %>% 
  filter(group == 1)

# sim_slopes can't handle GEE data structure so estimating using glmer
test1_mentioned <- glmer(response ~ condition + age_exact + condition*age_exact + (1|new_id), 
                         data = expt3_test_child_gee_nf_mentioned,
                         family = "binomial",
                         control=glmerControl(optCtrl=list(maxfun=2e4)))

expt3_jn_mentioned <- johnson_neyman(test1_mentioned, 
                                     pred = condition, 
                                     modx = age_exact,
                                     # sig.color = "#e39b2b",
                                     # insig.color =  "#619f97",
                                     mod.range = c(3.95,7.22),
                                     title = "")


# Getting separate plots for JN so they can be editable by ggplot
expt3_test_child_gee_nf_unmentioned <- expt3_test_child_gee_nf %>% 
  filter(group == 0)

# sim_slopes can't handle GEE data structure so estimating using glmer
test1_unmentioned <- glmer(response ~ condition + age_exact + condition*age_exact + (1|new_id), 
                           data = expt3_test_child_gee_nf_unmentioned,
                           family = "binomial",
                           control=glmerControl(optCtrl=list(maxfun=2e4)))

expt3_jn_unmentioned <- johnson_neyman(test1_unmentioned, 
                                       pred = condition, 
                                       modx = age_exact,
                                       # sig.color = "#e39b2b",
                                       # insig.color =  "#619f97",
                                       mod.range = c(3.95,7.25),
                                       title = "")

expt3_test_jn_unmentioned_value <- as.character(round(as.vector(expt3_test_child_jn$jn[[1]]$bounds[2]), 2) + .01)

expt3_test_jn_mentioned_value <- as.character(round(as.vector(expt3_test_child_jn$jn[[2]]$bounds[2]), 2) + .01)




