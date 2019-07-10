# This file is cleaning up the dataset so we have a nice tidy data file
expt1 <- expt1 %>%
  # Making "na" strings proper NAs 
  mutate(
    stim1 = ifelse(stim1 == "na", NA, stim1),
    head = ifelse(head == "na", NA, head),
    body = ifelse(body == "na", NA, body)
  ) %>%
  # Moving all the data from head and body columns to the stim1 column 
  mutate(head = ifelse(is.na(stim1) & is.na(body), head, body)) %>%
  mutate(stim1 = ifelse(is.na(stim1), head, stim1)) %>%
  # removing irrelevant head and body columns when done
  select(-head, -body) %>%
  # renaming conditions from numeric to condition name
  mutate(
    subjectGroup = 
       case_when(
         subjectGroup == "1" ~ "generic", 
         subjectGroup == "2" ~ "specific",
         TRUE ~ as.character(id)
         )
     ) %>%
  # Creating a column  notingwhether participants saw same or opposite groups
  mutate(
    stimulus = 
      ifelse(grepl("3_[1-4]d", expt1$stim1), "same", 
             ifelse(grepl("3_[1-4]c", expt1$stim1), "opposite", NA)
             )
    ) %>%
  # Creating a column for what property participants learned about    
  mutate(
    property = 
      ifelse(grepl("3_1", expt1$stim1), "pizza", 
             ifelse(grepl("3_2", expt1$stim1), "paint",
                    ifelse(grepl("3_3", expt1$stim1), "piano",
                           ifelse(grepl("3_4", expt1$stim1), "tree", NA))))
    ) %>%  
  # Creating a column specifying type of Trial
  mutate(
    trialType = 
      case_when(
        grepl("PiDi_2", expt1$stim1) | expt1$stim1 == "AudioCheck" ~ "attnCheck",
        startsWith(stim1, "PiDi_3") ~ "test",
        grepl("PiDi_[4-5]", expt1$stim1) ~ "pragTest",
        !startsWith(stim1, "PiDi") ~ "demographics"
      )
  ) %>% 
  # recoding response variables froms 1s -> 0s (no's) and 2s -> 1s (yes's) 
  mutate(response = 
           case_when(
             response == "2" & trialType == "test" ~ "0",
             TRUE ~ as.character(response)
             )
         )


