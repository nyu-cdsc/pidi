# Checking if adult participants correctly responded to audio checks and
# second category check
expt1 <- expt1 %>% 
  mutate(attn_correct = 
           case_when(
             trial == "audio_check" & response == "1" ~ TRUE,
             (trial == "category_check3" | trial == "category_check4")
             & startsWith(id, "A") & response == "1" ~ TRUE
           )) %>%
  group_by(id) %>% 
  mutate(attn_correct_sum = sum(attn_correct, na.rm = TRUE)) 

# Filtering out adult participants who didn't pass the audio check and / or
# responded incorrectly to second set of category checks
expt1 <- expt1 %>% 
  filter(!(startsWith(id, "A") & attn_correct_sum < 3))


# Doing exclusions based on adhoc attention checks
expt1_adhoc_exclusions <- expt1 %>%
  filter(startsWith(trial, "adhoc_attn_check") & response_as_string == "wrong") %>%
  distinct(id)

# filtering excluded participants from final dataset
expt1 <- expt1 %>% 
  filter(!(id %in% expt1_adhoc_exclusions$id)) %>% 
  select(-attn_correct, -attn_correct_sum)
