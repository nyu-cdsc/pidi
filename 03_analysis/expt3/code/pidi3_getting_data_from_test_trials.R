# creating dataframe with just the test trials, removing some unneeded columns
expt3_test <- expt3 %>%
  filter(trial_type == "test") %>%
  select(-trial_type, -trial) 

# Computing average response across stimulus type (whether it was previously mentioned
# or unmentioned)
# We will use this for graphing later on
expt3_test <- expt3_test %>%
  group_by(id, group) %>%
  mutate(response_avg = mean(response)) %>% 
  ungroup() %>%
  arrange(id)

expt3_test$new_id <- cumsum(!duplicated(expt3_test$id))

# Creating another data frame where we compare trial by trial if participants
# responded "yes" for mentioned group and "no" for previously unmentioned group
expt3_inference <- expt3_test %>%
  select(-response_avg, -response_as_string) %>%
  tidyr::spread(value = response, key = group) %>% 
  dplyr::mutate(inf = ifelse(mentioned == 1 & unmentioned == 0, 1, 0)) %>%
  dplyr::group_by(id) %>% 
  dplyr::mutate(inf_avg = mean(inf))

# Making dfs with just adult and child data 
# just the child data will be used for analyses when age is treated continuously
# having the adult data separate is useful when we later need to plot age 
# continuously for kids but categorically for adults
expt3_test_child <- expt3_test %>%
  filter(!startsWith(id, "R")) 

expt3_test_adult <- expt3_test %>%
  filter(startsWith(id, "R"))

expt3_inference_child <- expt3_inference %>%
  filter(!startsWith(id, "R"))

expt3_inference_adult <- expt3_inference %>%
  filter(startsWith(id, "R"))
