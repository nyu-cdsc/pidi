# Filtering out adult participants who didn't pass the audio check and got both attention checks incorrect

# Checking if adult participants correctly responded
expt1 <- expt1 %>% 
  mutate(attn_correct = 
           case_when(
             stim1 == "AudioCheck" & response == "4" ~ TRUE,
             orderPres == "4" & startsWith(id, "A") & response == "1" ~ TRUE,
             orderPres == "5" & startsWith(id, "A") & response == "2" ~ TRUE
           )) %>%
  group_by(id) %>% 
  mutate(attn_correct_sum = sum(attn_correct, na.rm = TRUE)) 

expt1 <- expt1 %>% 
  filter(!(startsWith(id, "A") & attn_correct_sum < 2))


# Doing exclusions based on pragmatic test
# filtering out just pragmatic task
expt1_pragtask <- expt1 %>%
  filter(trialType == "pragTest") %>%
  mutate(trialType = ifelse(stim1 == "PiDi_5c_Moment" | stim1 == "PiDi_4_Moment", "pragControl", trialType))

# getting participants who responded incorrectly on prag control items
expt1_pragtask_exclude <- expt1_pragtask %>%
  filter((stim1 == "PiDi_5c_Moment" & response != "3") | (stim1 == "PiDi_4_Moment" & response != "1")) %>%
  distinct(id)

# filtering excluded participants from final dataset
expt1 <- expt1 %>% 
  filter(!(id %in% expt1_pragtask_exclude$id))



# assigning correct response
# dataset_prag$correct <- ifelse(grepl("5a", dataset_prag$stim1) & dataset_prag$response == "2", 1,
# ifelse(grepl("4|5b|5d", dataset_prag$stim1) & dataset_prag$response == "1", 1,
# ifelse(grepl("5c|5e", dataset_prag$stim1) & dataset_prag$response == "3", 1, 0)))


# age_count_keep <- dataset_keep[!duplicated(dataset_keep$participant), ]
# count(age_count_keep, age_categorical)