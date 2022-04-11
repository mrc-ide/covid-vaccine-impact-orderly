## First Get Data From Server
if(dummy){
  saveRDS(NULL, "countryfits.Rds")
} else{
if(one_drive){
  if(one_drive_loc == ""){
    stop("Must specify one_drive_loc")
  }
  #load from onedrive location
  loc <- file.path(one_drive_loc, ifelse(excess, "excess", "reported"))
  #list files
  files <- setdiff(unlist(strsplit(list.files(loc), "\\.")), "Rds")
  #only keep countries
  files <- intersect(files, squire::population$iso3c)
  #load into one list
  names(files) <- files
  fits <- map(files, ~readRDS(file.path(loc, paste0(.x, ".Rds"))))
} else {
  if(repo == ""){
    stop("Must specify repo")
  }
  fits <- get_fits(repo = repo, date = date, iso3cs = NULL, excess = excess)
}

#remove any empty or non-fitted object+ remove chains
fits <- map(fits, function(fit){
  #pass checks to keep fit
  if("lmic_nimue_simulation" %in% class(fit) |
     "excess_nimue_simulation" %in% class(fit)){
    #keep inputs
    inputs <- fit$pmcmc_results$inputs
    #remove mcmc chains
    fit$pmcmc_results <- NULL
    #re add inputs
    fit$pmcmc_results <- list(
      inputs = inputs
    )
    return(fit)
  }
}) %>% discard(is.null)

#save fits in single Rds file
saveRDS(fits, "countryfits.Rds")
}
