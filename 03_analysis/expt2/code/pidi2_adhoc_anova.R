# Only kid data, age as continuous
adhoc2_results_child = glm(pragmatic_avg ~ age_exact*condition, data = expt2_adhoc_avg_child)
adhoc2_anova_child <- Anova(adhoc2_results_child, type="III") 

# logistic mixed model effects
summary(glmer(pragmatic_yes ~ age_exact + (1 | id) + (1 | trial), 
              family="binomial",data=expt2_adhoc_test))
