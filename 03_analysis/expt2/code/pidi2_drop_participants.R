# Filtering out adult participants who didn't pass the audio check and got both attention checks incorrect

# Doing exclusions based on pragmatic test
# filtering out just pragmatic task
expt2_pragtask <- expt2 %>%
  filter(trialType == "pragTest") %>%
  mutate(trialType = ifelse(stim1 == "PiDi_5c_Moment" | stim1 == "PiDi_4_Moment", "pragControl", trialType))

# getting participants who responded incorrectly on prag control items
expt2_pragtask_exclude <- expt2_pragtask %>%
  filter((stim1 == "PiDi_5c_Moment" & response != "3") | (stim1 == "PiDi_4_Moment" & response != "1")) %>%
  distinct(id)

# filtering excluded participants from final dataset
expt2 <- expt2 %>% 
  filter(!(id %in% expt2_pragtask_exclude$id))



# assigning correct response
# dataset_prag$correct <- ifelse(grepl("5a", dataset_prag$stim1) & dataset_prag$response == "2", 1,
# ifelse(grepl("4|5b|5d", dataset_prag$stim1) & dataset_prag$response == "1", 1,
# ifelse(grepl("5c|5e", dataset_prag$stim1) & dataset_prag$response == "3", 1, 0)))

