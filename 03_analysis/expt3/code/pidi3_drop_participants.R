# Checking if children responded correctly to attention checks in pragmatic task
# and if adults correctly responded to manipulation checks, sound check, and winograd
expt3 <- expt3 %>% 
  mutate(attn_correct = 
           case_when(
            startsWith(trial, "adhoc_attn_check") & 
              response_as_string == "correct" ~ TRUE),
         winograd_correct = 
           case_when(
            startsWith(trial, "winograd") & 
              response_as_string == "correct" ~ TRUE),
         sound_correct = 
           case_when(
             trial == "audio_check" & 
               response_as_string == "correct" ~ TRUE),
         manip_correct = 
           case_when(
             startsWith(trial, "manipulation_check") & 
               response_as_string == "correct" ~ TRUE)
         ) %>%
  group_by(id) %>% 
  mutate(attn_correct_sum = sum(attn_correct, na.rm = TRUE),
         winograd_correct_sum = sum(winograd_correct, na.rm = TRUE),
         manip_correct_sum = sum(manip_correct, na.rm = TRUE)) %>% 
  ungroup()

# Should be 1 adult and 7 children (2 of those children have incomplete data
# because they did not finish the task)
expt3_dropped <- expt3 %>% 
  filter(attn_correct_sum < 2 & !startsWith(id, "R") | 
           (startsWith(id, "R") & 
              (sound_correct == 0 | 
              winograd_correct_sum < 3 | 
              manip_correct_sum == 0))) %>% 
  distinct(id)

expt3 <- expt3 %>% 
  filter(!(id %in% expt3_dropped$id)) %>% 
  ungroup() %>% 
  select(-attn_correct, -attn_correct_sum, -winograd_correct, -winograd_correct_sum,
         -manip_correct, -manip_correct_sum, -sound_correct)


