# creating dataframe with just the test trials, removing some unneeded columns
expt2_test <- expt2 %>%
  filter(trial_type == "test") %>%
  select(-trial_type, -trial) 

expt2_test$age_categorical <- as.factor(as.character(expt2_test$age_categorical))

# Computing average response across stimulus type (whether it was previously mentioned
# or unmentioned)
# We will use this for graphing later on
expt2_test <- expt2_test %>%
  group_by(id, group) %>%
  mutate(response_avg = mean(response))

expt2_test$new_id <- cumsum(!duplicated(expt2_test$id))

# Creating another data frame where we compare trial by trial if participants
# responded "yes" for mentioned group and "no" for previously unmentioned group
expt2_inference <- expt2_test %>%
  select(-trial_order, -response_avg, -response_as_string) %>%
  tidyr::spread(value = response, key = group) %>% 
  dplyr::mutate(inf = ifelse(mentioned == 1 & unmentioned == 0, 1, 0)) %>%
  dplyr::group_by(id) %>% 
  dplyr::mutate(inf_avg = mean(inf))