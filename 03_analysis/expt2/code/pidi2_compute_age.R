# Computing age for Study 1
# pulling out age demongraphic data
age_expt2 <- expt2 %>%
  filter(stim1 == "Date of Test:" | stim1 == "Date of Birth:" |  stim1 == "Day" |  stim1 == "Year" | stim1 == "Month") %>% 
  select(id, stim1, response) %>%
  spread(stim1, response) 

# renaming columns
names(age_expt2)[names(age_expt2)=="Date of Birth:"] <- "dob"
names(age_expt2)[names(age_expt2)=="Date of Test:"] <- "dot"

age_expt2 <- age_expt2 %>%
  # fixing DOTs and DOBs entered incorrectly/weren't entered
  mutate(
    dot = 
      case_when(
        # fixing DOTs entered incorrectly
        id == "11409" ~ "11/9/2018", #dot was entered 2010 not 2018
        id == "11493" ~ "12/2/2018", #dot was entered 2012 not 2018
        id == "11439" ~ "11/11/2018", #dot was entered 2014 not 2018
        id == "212P" ~ "11/15/2018", #dot was entered 2014 not 2018
        id == "121P" ~ "11/13/2018", #dot was entered 2014 not 2018
        id == "153P" ~ "11/13/2018", #dot was entered 2014 not 2018
        id == "176P" ~ "11/13/2018", #dot was entered 2014 not 2018
        TRUE ~ as.character(dot)
      ),
    dob = 
      case_when(
        # fixing DOBs entered incorrectly
        id == "11491" ~ "8/16/2014", 
        id == "11338" ~ "9/20/2014", 
        # inputting DOBS from online demo
        id == "11418" ~ "12/4/2011", 
        id == "11455" ~ "1/12/2008", 
        id == "11457" ~ "2/18/2012", 
        id == "11461" ~ "9/25/2014", 
        id == "11481" ~ "6/14/2012", 
        id == "11482" ~ "11/1/2013", 
        id == "11483" ~ "", 
        id == "11484" ~ "12/27/2012", 
        id == "11513" ~ "",
        id == "11517" ~ "7/11/2014", 
        id == "11518" ~ "8/20/2014", 
        id == "11530" ~ "5/1/2011", 
        id == "11570" ~ "6/5/2013", 
        id == "11572" ~ "2/11/2012", 
        id == "11573" ~ "11/9/2012", 
        id == "11574" ~ "2/8/2012", 
        id == "11575" ~ "3/10/2013", 
        id == "11671" ~ "", 
        id == "11675" ~ "", 
        id == "11676" ~ "", 
        id == "11681" ~ "", 
        id == "11684" ~ "6/18/2013", 
        id == "11694" ~ "", 
        id == "11633" ~ "5/24/2012", 
        TRUE ~ as.character(dob)
      )
  ) 

# formatting dates and computing age
age_expt2$dob <- mdy(age_expt2$dob)
age_expt2$dot <- mdy(age_expt2$dot)
age_expt2$age <-  age_expt2$dot - age_expt2$dob
age_expt2$age_exact <- as.numeric(round(age_expt2$age/365.25, 2))
age_expt2$age_year <- abs(floor(age_expt2$age/365))

# one kid didn't have a dob listed but was noted as being 5
age_expt2$age_year[age_expt2$id == "11671"] <- 5

# keeping relevant age columns
age_expt2 <- age_expt2 %>%
  select(id, age_exact, age_year) %>% 
  mutate(age_categorical = age_year)

# merging back into dataset
expt2 <- merge(expt2, age_expt2, by = "id")

# filtering out participants who are the wrong age
expt2 <- expt2 %>%
  filter(age_categorical >= 4 & age_categorical <= 6)
