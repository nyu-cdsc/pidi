# Computing age for Study 1
# pulling out age demongraphic data
age_expt1 <- expt1 %>%
  filter(stim1 == "Date of Test:" | stim1 == "Date of Birth:" |  stim1 == "Day" |  stim1 == "Year" | stim1 == "Month") %>% 
  select(id, stim1, response) %>%
  spread(stim1, response) 
  
# renaming columns
names(age_expt1)[names(age_expt1)=="Date of Birth:"] <- "dob"
names(age_expt1)[names(age_expt1)=="Date of Test:"] <- "dot"

age_expt1 <- age_expt1 %>%
  # filling in a few dates where adult dob day and month weren't supplied / we don't care about adult's exact age
  mutate(
    Day = ifelse(startsWith(age_expt1$id, "A") & is.na(Day), "15", Day),
    Month = ifelse(startsWith(age_expt1$id, "A") & is.na(Month), "June", Month)
    ) %>%
  # combining Month Day and Year into dob column for adult ids
  mutate(
    dob = ifelse(startsWith(age_expt1$id, "A"), 
                 paste(Month, Day,", ", Year), dob)
  ) %>%
  # fixing DOTs and DOBs entered incorrectly/weren't entered
  mutate(
    dot = 
      case_when(
        id == "10634" ~ "7/1/18", # id number was in place of DOT
        id == "10265" ~ "4/29/18",
        id == "242K" ~ "4/25/18", # entered incorrectly as 4/35/2018
        startsWith(age_expt1$id, "A") ~ "4/30/18", # adult dot
        id == "9927" ~ "3/3/18", # dob and dot were reversed,
        id == "10210" ~ "4/15/2018", # dot was entered incorrectly
        id == "10333" ~ "5/13/2018", # dot was entered incorrectly
        id == "10589" ~ "6/24/2018", # dob was entered incorrectly
        id == "10594" ~ "6/24/2018", # dot was entered incorrectly
        id == "9786" ~ "2/4/2018", # dot was entered incorrectly
        TRUE ~ as.character(dot)
      ),
    dob = 
      case_when(
        id == "9927" ~ "8/2/13", # dob and dot were reversed
        id == "9970" ~ "11/22/13", # dob was entered incorrectly
        id == "9769" ~ "9/29/13", # dob was not entered 
        id == "9771" ~ "8/25/2012", # dob was not entered 
        id == "9818" ~ "6/6/2011", # dob was not entered 
        id == "10211" ~ "3/27/2012", # dob was entered incorrectly
        id == "10265" ~ "9/18/2011", # dob was not entered 
        id == "10360" ~ "8/22/2011", # dob was entered incorrectly
        id == "10538" ~ "4/29/2012", # dob was not entered 
        id == "10546" ~ "5/3/2012", # dob was not entered 
        id == "10589" ~ "6/23/2013", # dob was entered incorrectly
        id == "10594" ~ "2/12/2012", # dob was entered incorrectly
        id == "11065" ~ "11/6/2013", # dob was entered incorrectly
        id == "11066" ~ "1/15/2014", # dob was not entered
        id == "10409" ~ "9/22/2012", # dob was entered incorrectly
        TRUE ~ as.character(dob)
      )
  ) 

# formatting dates and computing age
age_expt1$dob <- mdy(age_expt1$dob)
age_expt1$dot <- mdy(age_expt1$dot)
age_expt1$age <-  age_expt1$dot - age_expt1$dob
age_expt1$age_exact <- as.numeric(round(age_expt1$age/365.25, 2))
age_expt1$age_year <- abs(floor(age_expt1$age/365))

# one kid didn't have a dob listed but was noted as being 6
age_expt1$age_year[age_expt1$id == "9899"] <- 6
age_expt1$age_year[age_expt1$age_exact >= 4.95 & age_expt1$age_exact < 5] <- 5

# keeping relevant age columns
age_expt1 <- age_expt1 %>%
  select(id, age_exact, age_year) %>% 
  mutate(age_categorical = ifelse(age_year > 10, 9, age_year))

# merging back into dataset
expt1 <- merge(expt1, age_expt1, by = "id")

# filtering out participants who are the wrong age
expt1 <- expt1 %>%
  filter((age_categorical >= 4 & age_categorical <= 6) | age_categorical == 9 )
