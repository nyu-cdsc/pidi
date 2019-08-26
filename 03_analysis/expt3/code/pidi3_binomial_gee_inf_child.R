# Binomial GEE looking at child data only
# Looking at trials where participant made inference
# (Yes to mentioned and no to previously mentioned) by: 
# (1) age (categorical), 
# (2) condition (generic/specific)

# 1 kid doesn't have a dob, so no exact age
# omitted from analyses
expt3_inference_child <- expt3_inference %>%
  filter(age_exact < 7.5 & age_exact > 3.99)

expt3_inference_child_results <- geeglm(
  inf ~ condition + age_exact + condition*age_exact, 
  data = expt3_inference_child, 
  family = binomial(logit), 
  id = new_id, 
  corstr = "exchangeable")
summary(expt3_inference_child_results)
expt3_inference_child_anova <- anova(expt3_inference_child_results)
expt3_inference_child_anova

expt3_inference_child_nf <- expt3_inference_child
expt3_inference_child_nf$condition <- ifelse(expt3_inference_child_nf$condition == "knowledge", 1, 0)

test <- glmer(inf ~ condition + age_exact + condition*age_exact + (1|new_id), 
              data = expt3_inference_child_nf,
              family = "binomial")

expt3_inference_child_jn <- sim_slopes(test, pred = condition, modx = age_exact , jnplot = TRUE)

expt3_jn <- johnson_neyman(test, 
                           pred = condition, 
                           modx = age_exact,
                           # sig.color = "#e39b2b",
                           # insig.color =  "#619f97",
                           mod.range = c(3.4,7.5),
                           title = "")

expt3_jnplot <- expt3_jn$plot

expt3_jn_value <- as.character(round(as.vector(expt3_jn$bounds[2]), 2) + .01)
# expt3_jnplot <- expt3_jnplot + 
#   labs(x = "Age" ,
#        y = "Difference in simple slopes between knowledge conditions") + 
#   geom_segment(aes(x = as.vector(expt3_jn$bounds[2]) + .3, 
#                          y = 4, 
#                         xend = as.vector(expt3_jn$bounds[2]) + .05,
#                         yend = 4),
#                     colour = 'black',
#                     size = .5,
#                     arrow = arrow(length = unit(0.3, "cm")))+
#   annotate("label", 
#            x = as.vector(expt3_jn$bounds[2]) + .6, 
#            y = 4, 
#            label = paste(jn_value, "years of age"),
#            label.padding = unit(0.55, "lines"),
#            fontface = "bold") 


expt3_inf_child_emtrends <- as.data.frame(
  emtrends(expt3_inference_child_results, 
           ~ condition, 
           var = "age_exact"))

expt3_inf_child_pairs <- pairs(emtrends(expt3_inference_child_results, 
                                        ~ condition, 
                                        var = "age_exact"))