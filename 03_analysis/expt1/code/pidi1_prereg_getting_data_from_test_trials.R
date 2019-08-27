# creating dataframe with just the test trials, removing some unneeded columns
expt1_first_180_test <- expt1_first_180 %>%
  filter(trial_type == "test") %>%
  select(-trial_type, -trial) 

# Computing average response across stimulus type (whether it was previously mentioned
# or unmentioned)
# We will use this for graphing later on
expt1_first_180_test <- expt1_first_180_test %>%
  group_by(id, group) %>%
  mutate(response_avg = mean(response))

expt1_first_180_test$new_id <- cumsum(!duplicated(expt1_first_180_test$id))

expt1_first_180_inference_trial_order <- expt1_first_180_test %>% 
  ungroup() %>%
  arrange(trial_order) %>% 
  distinct(id, property, .keep_all = TRUE) %>% 
  select(id, property, trial_order) %>% 
  mutate(trial_order = ifelse(trial_order %% 2 == 0, trial_order - 1,
                              trial_order))

# Creating another data frame where we compare trial by trial if participants
# responded "yes" for mentioned group and "no" for previously unmentioned group
expt1_first_180_inference <- expt1_first_180_test %>%
  select(-trial_order, -response_avg, -response_as_string) %>%
  tidyr::spread(value = response, key = group) %>% 
  dplyr::mutate(inf = ifelse(mentioned == 1 & unmentioned == 0, 1, 0)) %>%
  dplyr::group_by(id) %>% 
  dplyr::mutate(inf_avg = mean(inf)) %>% 
  full_join(expt1_first_180_inference_trial_order)

# Making dfs with just adult and child data 
# just the child data will be used for analyses when age is treated continuously
# having the adult data separate is useful when we later need to plot age 
# continuously for kids but categorically for adults
expt1_first_180_test_child <- expt1_first_180_test %>%
  filter(!startsWith(id, "A")) 

expt1_first_180_test_adult <- expt1_first_180_test %>%
  filter(startsWith(id, "A"))

expt1_first_180_inference_child <- expt1_first_180_inference %>%
  filter(!startsWith(id, "A"))

expt1_first_180_inference_adult <- expt1_first_180_inference %>%
  filter(startsWith(id, "A"))
