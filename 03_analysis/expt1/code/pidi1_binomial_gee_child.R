# Binomial GEE looking at child data only
# age is treated categorically
# Looking at yes/no responses by: 
# (1) age (continuous), 
# (2) condition (generic/specific), and 
# (3) group type (mentioned/unmentioned) [within-subject]

# Some kids don't have a dob, so no exact age
# omitted from analyses
expt1_test_child_gee <- expt1_test_child %>%
  filter(!is.na(age_exact)) %>% 
  arrange(id, trial_order)

expt1_test_child_results <- geeglm(
  response ~ condition + age_exact + group + condition*age_exact + 
    condition*group + age_exact*group + condition*age_exact*group, 
  data = expt1_test_child_gee, 
  family = binomial(logit), 
  id = new_id, 
  corstr = "exchangeable")
summary(expt1_test_child_results)
expt1_test_child_anova <- anova(expt1_test_child_results)
expt1_test_child_anova

# Estimating marginal slopes and means
expt1_test_child_emtrends <- as.data.frame(emtrends(expt1_test_child_results, 
                                      ~ condition | group, 
                                      var = "age_exact"))
  
expt1_test_child_pairs <- pairs(emtrends(expt1_test_child_results, 
                                         ~ condition | group, 
                                         var = "age_exact"))

expt1_test_child_gee_nf <- expt1_test_child_gee %>% 
  ungroup() %>%
  mutate(
    condition = ifelse(condition == "generic", 1, 0),
    group = ifelse(group == "mentioned", 1, 0)
  )

# sim_slopes can't handle GEE data structure so estimating using glmer
test1 <- glmer(response ~ condition + age_exact + group + condition*age_exact + 
                 condition*group + age_exact*group + condition*age_exact*group + (1|new_id), 
               data = expt1_test_child_gee_nf,
               family = "binomial",
               control=glmerControl(optCtrl=list(maxfun=2e4)))

# getting glmer to converge by starting where previous one ended, 
# bumping up max iterations, and using a different optimizer
ss <- getME(test1, c("theta","fixef"))

test1_converge <- update(test1,
                         start = ss,
                         control = glmerControl(optimizer = "bobyqa",
                                       optCtrl = list(maxfun = 2e5)))

# Estimating exact age where slopes differ using Johnson-Neyman
expt1_test_child_jn <- sim_slopes(test1_converge, 
                                  pred = condition,
                                  modx = age_exact,
                                  mod2 = group,
                                  jnplot = TRUE)


# Getting separate plots for JN so they can be editable by ggplot
expt1_test_child_gee_nf_mentioned <- expt1_test_child_gee_nf %>% 
 filter(group == 1)

# sim_slopes can't handle GEE data structure so estimating using glmer
test1_mentioned <- glmer(response ~ condition + age_exact + condition*age_exact + (1|new_id), 
               data = expt1_test_child_gee_nf_mentioned,
               family = "binomial",
               control=glmerControl(optCtrl=list(maxfun=2e4)))

expt1_jn_mentioned <- johnson_neyman(test1_mentioned, 
                           pred = condition, 
                           modx = age_exact,
                           # sig.color = "#e39b2b",
                           # insig.color =  "#619f97",
                           mod.range = c(3.4,7.5),
                           title = "")


# Getting separate plots for JN so they can be editable by ggplot
expt1_test_child_gee_nf_unmentioned <- expt1_test_child_gee_nf %>% 
  filter(group == 0)

# sim_slopes can't handle GEE data structure so estimating using glmer
test1_unmentioned <- glmer(response ~ condition + age_exact + condition*age_exact + (1|new_id), 
                         data = expt1_test_child_gee_nf_unmentioned,
                         family = "binomial",
                         control=glmerControl(optCtrl=list(maxfun=2e4)))

expt1_jn_unmentioned <- johnson_neyman(test1_unmentioned, 
                                     pred = condition, 
                                     modx = age_exact,
                                     # sig.color = "#e39b2b",
                                     # insig.color =  "#619f97",
                                     mod.range = c(3.4,7.5),
                                     title = "")

expt1_test_jn_unmentioned_value <- as.character(round(as.vector(expt1_test_child_jn$jn[[1]]$bounds[2]), 2) + .01)

expt1_test_jn_mentioned_value <- as.character(round(as.vector(expt1_test_child_jn$jn[[2]]$bounds[2]), 2) + .01)

