## Compiling the data files into single dataset
# Because some of the testable files for children participants have a different number of columns, 
# I am importing them separately from two different folders, Testable 1 and Testable 2. 
                                                                   
# importing the data files
folders <- list.files(here::here("02_experiment", "expt1", "data"))
expt1 <- data.frame()
for(i in 1:length(folders)) {                                                                      
  files <- list.files(path = here::here("02_experiment", "expt1", "data", folders[i]))


  for(f in 1:length(files)) { 
    assign(files[f], read.csv(file = here::here("02_experiment", "expt1", "data", folders[i], files[f]),
                                                      header=F,
                                                      stringsAsFactors=F,
                                                      na.strings=c("", "NA")))
  }
  
    
  descript <- get(files[1])[1,][!is.na(get(files[1])[1,])]
  variables <- get(files[1])[3,][!is.na(get(files[1])[3,])]
  
  colNum <- length(variables)+length(descript)
  
  ## initialize dataframe
  d <- get(files[1])
  d <- d[-c(1:3),]
  colnames(d) <- variables
  
  jd <- data.frame(get(files[1])[2,])
  colnames(jd) <- descript
  
  d[,descript] <- jd[1,descript]
  
    ## combine all other 
    for(i in 2:length(files)){
      
      j <- get(files[i])
      j <- j[-c(1:3),]
      colnames(j) <- variables
      
      jd <- data.frame(get(files[i])[2,])
      colnames(jd) <- descript
      
      j[,descript] <- jd[1,descript]
      
      d <- rbind(d,j)
    }
  
  d <- d %>%
    select(id, stim1, head, body, response, orderPres, subjectGroup)
  
  if(i == 1){
    expt1 <- d
  } else {
    expt1 <- rbind(expt1, d)
  }
  
}

rm(list=setdiff(ls(), c("expt1")))

# fixing errors in participant number
expt1 <- expt1 %>%
  dplyr::mutate( 
    # manually fixing IDs that were entered incorrectly
    id = case_when(
      id == "10592" ~ "10594", 
      id == "8786" ~ "9786",
      TRUE ~ as.character(id)
    )
  )

# ID 10749 respones appear in dataset twice, so removing one instance of it
expt1 <- expt1[-c(7243:7264),]