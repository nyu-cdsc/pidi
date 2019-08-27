# Filtering out adult participants who didn't pass the audio check and 
# got both attention checks incorrect
# Filtering out 

# Checking if participants incorrectly responded to audio check (adults only),
# second category check and adhoc attn checks
expt1_prereg_drop <- expt1 %>% 
  filter(
    trial == "audio_check" | 
    trial == "category_check3" | 
    trial == "category_check4" |
    startsWith(trial, "adhoc_attn_check")
  ) %>% 
  filter(response_as_string == "wrong") %>% 
  group_by(id) %>%
  summarize(total_wrong = length(id))

# Filtering out participants who didn't pass checks
expt1_strict_exclusion <- expt1 %>% 
  filter(!(id %in% expt1_prereg_drop$id))

# Getting first 180 participants that meet strict exclusion criteria
# This maps onto our pre-regged sampling plan to only include the first 180 kids
# 60 per age group, that participated in the study

# Doing exclusions based on adhoc attention checks
expt1_first_180 <- expt1_strict_exclusion %>% 
  group_by(age_categorical) %>% 
  arrange(dot, id) %>%
  mutate(new_id = cumsum(!duplicated(id))) %>% 
  filter(new_id <= 60)

# confirming that we have 60 kids per group
expt1_first_180 %>% 
  group_by(age_categorical) %>% 
  distinct(id, .keep_all = TRUE) %>% 
  summarize(total_N = length(id))
