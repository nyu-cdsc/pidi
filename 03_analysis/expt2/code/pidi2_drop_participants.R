# Doing exclusions based on adhoc attention checks
expt2_adhoc_exclusions <- expt2 %>%
  filter(startsWith(trial, "adhoc_attn_check") & response_as_string == "wrong") %>%
  distinct(id)

# filtering excluded participants from final dataset
expt2 <- expt2 %>% 
  filter(!(id %in% expt2_adhoc_exclusions$id)) 
