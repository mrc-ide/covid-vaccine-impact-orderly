## First Get Data From Server

if(one_drive){
  if(one_drive_loc == ""){
    stop("Must specify one_drive_loc")
  }
  #load from onedrive location
  loc <- file.path(one_drive_loc, ifelse(excess, "excess", "standard"))
  #list files
  files <- setdiff(unlist(strsplit(list.files(loc), "\\.")), "Rds")
  #only keep countries
  files <- intersect(files, squire::population$iso3c)
  #load into one list
  fits <- lapply(files, function(file){readRDS(file.path(loc, paste0(file, ".Rds")))})
  names(fits) <- files
} else {
  if(repo == ""){
    stop("Must specify repo")
  }
  fits <- get_fits(repo = repo, date = date, iso3cs = NULL, excess = excess)
}

#save fits in single Rds file
saveRDS(fits, "countryfits.Rds")
