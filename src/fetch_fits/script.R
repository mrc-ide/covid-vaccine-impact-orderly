## First Get Data From Server

fits <- get_fits(repo = repo, date = date, iso3cs = NULL, excess = excess)

#save fits in single Rds file
saveRDS(fits, "countryfits.Rds")
