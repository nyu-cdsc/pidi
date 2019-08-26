# Only kid data, age as continuous
adhoc3_results_child = glm(pragmatic_avg ~ age_exact*condition, data = expt3_adhoc_avg)
adhoc3_anova_child <- Anova(adhoc3_results_child, type="III") 

# logistic mixed model effects
summary(glmer(pragmatic_yes ~ age_exact + (1 | id) + (1 | trial), 
              family="binomial",data=expt3_adhoc_test))
