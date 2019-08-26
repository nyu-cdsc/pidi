# Binomial GEE looking at both adult and child data
# Looking at trials where participant made inference
# (Yes to mentioned and no to previously mentioned) by: 
# (1) age (categorical), 
# (2) condition (generic/specific)
expt3_inference_gee <- expt3_inference

expt3_inference_gee$age_categorical <- ifelse(expt3_inference_gee$age_categorical == 7, 
                                              6, expt3_inference_gee$age_categorical)
expt3_inference_gee$age_categorical <- as.factor(expt3_inference_gee$age_categorical)

expt3_inference_gee <- expt3_inference_gee %>% 
  arrange(age_categorical, id)

expt3_inference_results <- geeglm(
  inf ~ condition + age_categorical + condition*age_categorical, 
  data = expt3_inference_gee, 
  family = binomial(logit), 
  id = new_id, 
  corstr = "exchangeable")
summary(expt3_inference_results)
expt3_inference_anova <- anova(expt3_inference_results)
expt3_inference_anova


expt3_inference_summary <- expt3_inference_gee %>% 
  group_by(age_categorical, condition) %>% 
  summarize(mean_inf = mean(inf))

expt3_inference_results_pair <- as.data.frame(
  pairs(
    emmeans(expt3_inference_results, 
            ~ condition | age_categorical)
    ))
