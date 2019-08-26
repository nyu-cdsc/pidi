# looking at proportion of trials participants selected the pragmatic response
# by age (categorically, both adults and kids) and condition
adhoc_results = glm(pragmatic_avg ~ age_categorical*condition, data = expt1_adhoc_avg)
adhoc_anova <- Anova(adhoc_results, type="III") 

# now looking at just kid data, age as continuous
adhoc_results_child = glm(pragmatic_avg ~ age_exact*condition, data = expt1_adhoc_avg_child)
adhoc_anova_child <- Anova(adhoc_results_child, type="III") 

# logistic mixed model effects
summary(glmer(pragmatic_yes ~ age_categorical + (1 | id) + (1 | trial), 
              family="binomial",data=expt1_adhoc_test))
