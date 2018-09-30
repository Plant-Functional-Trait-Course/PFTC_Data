##############################
###    LOADING DATA       ###
##############################


drop_and_load <- function(myfile, localpath){
  drop_download(path = myfile, local_path = localpath, overwrite = TRUE)
  dat <- get(load(file = localpath))
  return(dat)
}

drop_and_load.csv <- function(myfile, localpath){
  drop_download(path = myfile, local_path = localpath, overwrite = TRUE)
  dat <- read_csv(file = localpath, col_names = TRUE, stringsAsFactors = FALSE)
  return(dat)
}


drop_and_load.xlsx <- function(myfile, localpath){
  drop_download(path = myfile, local_path = localpath, overwrite = TRUE)
  dat <- read_excel(path = localpath, col_names = TRUE)
  return(dat)
}