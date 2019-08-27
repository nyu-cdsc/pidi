# Binomial GEE looking at child data only
# Looking at trials where participant made inference
# (Yes to mentioned and no to previously mentioned) by: 
# (1) age (continuous), 
# (2) condition (generic/specific)

# Sme kids didn't have a dob, so no exact age
# omitted from analyses
expt1_inference_child_gee <- expt1_inference_child %>%
  filter(!is.na(age_exact))  %>% 
  arrange(id)

expt1_inference_child_results <- geeglm(
  inf ~ condition + age_exact + condition*age_exact, 
  data = expt1_inference_child_gee, 
  family = binomial(logit), 
  id = new_id, 
  corstr = "exchangeable")
summary(expt1_inference_child_results)
expt1_inference_child_anova <- anova(expt1_inference_child_results)
expt1_inference_child_anova


# Johnson-Neyman Technique
# first converting condition to numeric factor
expt1_inference_child_gee_nf <- expt1_inference_child_gee %>% 
  mutate(
    condition = ifelse(condition == "generic", 1, 0)
  )

# sim_slopes can't handle GEE data structure so estimating using glmer
test1 <- glmer(inf ~ condition + age_exact + condition*age_exact + (1|new_id), 
              data = expt1_inference_child_gee_nf,
              family = "binomial")

# Estimating exact age where slopes differ using Johnson-Neyman
expt1_inference_child_jn <- sim_slopes(test1, pred = condition, modx = age_exact , jnplot = TRUE)

expt1_jn <- johnson_neyman(test1, 
                           pred = condition, 
                           modx = age_exact,
                           # sig.color = "#e39b2b",
                           # insig.color =  "#619f97",
                           mod.range = c(3.4,7.5),
                           title = "")

expt1_jnplot <- expt1_jn$plot

expt1_jn_value <- as.character(round(as.vector(expt1_jn$bounds[2]), 2) + .01)

# Marginal slopes and means
expt1_inf_child_emtrends <- as.data.frame(
  emtrends(expt1_inference_child_results, 
           ~ condition, 
           var = "age_exact"))

expt1_inf_child_pairs <- pairs(emtrends(expt1_inference_child_results, 
                                         ~ condition, 
                                         var = "age_exact"))