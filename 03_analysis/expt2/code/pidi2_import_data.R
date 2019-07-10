## Compiling the data files into single dataset
# Because some of the testable files for children participants have a different number of columns, 
# I am importing them separately from two different folders, Testable 1 and Testable 2. 

# importing the data files
folders <- list.dirs(here::here("02_experiment", "expt2", "data"), recursive = FALSE, full.names = FALSE)
expt2 <- data.frame()
for(i in 1:length(folders)) {                                                                      
  files <- list.files(path = here::here("02_experiment", "expt2", "data", folders[i]))
  
  
  for(f in 1:length(files)) { 
    assign(files[f], read.csv(file = here::here("02_experiment", "expt2", "data", folders[i], files[f]),
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
    select(id, stim1, head, body, response, orderPres, subjectGroup, calibration)
  
  if(i == 1){
    expt2 <- d
  } else {
    expt2 <- rbind(expt2, d)
  }
  
  rm(list = files)
}

# for cleaning up the global environment
rm(d)
rm(j)
rm(jd)
rm(files)
rm(folders)
rm(i)
rm(variables)
rm(f)
rm(colNum)

# fixing errors in participant number
expt2 <- expt2 %>%
  dplyr::mutate(
    # manually fixing IDs that were entered incorrectly
    id = case_when(
      id == "11667" & calibration == "0.45" ~ "11668",
      id == "16333" ~ "11633",
      TRUE ~ as.character(id)
    )
  )

expt2 <- expt2 %>%
  select(-calibration)
