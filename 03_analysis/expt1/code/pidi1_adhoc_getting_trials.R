# pulling out adhoc trials
expt1_adhoc <- expt1 %>% 
  filter(trial_type == "adhoc") %>%
  select(-group, -property, -trial_order) 

# double checking that no one got attn checks wrong (that we correctly
# dropped people)
expt1_adhoc_wrong <- expt1_adhoc %>% 
  filter(response_as_string == "wrong")

# getting just the test trials 
expt1_adhoc_test <- expt1_adhoc %>% 
  filter(!startsWith(trial, "adhoc_attn")) %>% 
  arrange(id) 

# getting individual averages across trials 
expt1_adhoc_test <- expt1_adhoc_test %>% 
  group_by(id) %>% 
  mutate(pragmatic_yes = ifelse(response_as_string == "pragmatic", TRUE, FALSE)) %>%
  mutate(pragmatic_avg = mean(pragmatic_yes)) 

# getting dataset with distinct IDS
expt1_adhoc_avg <- expt1_adhoc_test %>% 
  distinct(id, .keep_all = TRUE)


# getting separate child and adult dataframes
expt1_adhoc_avg_child <- expt1_adhoc_avg %>% 
  filter(age_exact < 8)

expt1_adhoc_avg_adult <- expt1_adhoc_avg %>% 
  filter(age_categorical == 8) %>% 
  mutate(age_exact = 8)


# Getting summaries by age and trial
expt1_adhoc_test_by_age <- summarySE(measurevar = "pragmatic_yes",
                                       groupvars = "age_categorical",
                                       data = expt1_adhoc_test)

expt1_adhoc_test_by_trial <- summarySE(measurevar = "pragmatic_yes",
                                       groupvars = c("trial", "age_categorical"),
                                       data = expt1_adhoc_test)

expt1_adhoc_counts_by_age <- expt1_adhoc_avg %>% 
  group_by(pragmatic_avg, age_categorical) %>% 
  summarize(counts = length(id)) %>% 
  ungroup() %>% 
  group_by(age_categorical) %>%
  mutate(total_n = sum(counts)) %>% 
  mutate(prop = counts/total_n)

# number of kids who selected the neither example 
# for at least one trial
expt1_adhoc_neither <- expt1_adhoc_test %>% 
  filter(response_as_string == "neither") %>% 
  distinct(id)