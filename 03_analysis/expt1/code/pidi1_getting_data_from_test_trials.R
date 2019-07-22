# creating dataframe with just the test trials, removing some unneeded columns
expt1_test <- expt1 %>%
  filter(trialType == "test") %>%
  select(-trialType, -attn_correct, -attn_correct_sum, -stim1) 

# making sure our variables are in the correct formats
expt1_test$age_categorical <- as.factor(expt1_test$age_categorical)
expt1_test$stimulus <- as.factor(expt1_test$stimulus)
expt1_test$subjectGroup <- as.factor(expt1_test$subjectGroup)
expt1_test$response <- as.numeric(expt1_test$response)
expt1_test$property <- as.factor(expt1_test$property)

# Computing average response across stimulus type (whether it was previously mentioned
# or unmentioned)
expt1_test <- expt1_test %>%
  group_by(id, stimulus) %>%
  mutate(response_avg = mean(response))

# Creating another data frame where we compare trial by trial if participants
# responded "yes" for mentioned group and "no" for previously unmentioned group
expt1_inference <- expt1_test %>%
  select(-orderPres, -response_avg) %>%
  tidyr::spread(value = response, key = stimulus) %>% 
  dplyr::mutate(same = as.numeric(same),
                opposite = as.numeric(opposite)) %>%
  dplyr::mutate(inf = ifelse(same == 1 & opposite == 0, 1, 0)) %>% 
  dplyr::group_by(id) %>% 
  dplyr::mutate(inf_avg = mean(inf))

# Making dfs with just adult and child data 
# just the child data will be used for analyses when age is treated continuously
# having the adult data separate is useful when we later need to plot age 
# continuously for kids but categorically for adults
expt1_test_child <- expt1_test %>%
  filter(!startsWith(id, "A")) 
  
expt1_test_adult <- expt1_test %>%
  filter(startsWith(id, "A"))

expt1_inference_child <- expt1_inference %>%
  filter(!startsWith(id, "A"))

expt1_inference_adult <- expt1_inference %>%
  filter(startsWith(id, "A"))

