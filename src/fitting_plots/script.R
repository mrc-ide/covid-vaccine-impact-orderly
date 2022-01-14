#must use the preset draws so that it's the same values as those used in the simulation
draws <- NULL

#similarly to simulations we temporarily store the fits for faster access
fits <-  readRDS("countryfits.Rds")
#create a temporary subdirectory to make access quicker
dir.create("temp", showWarnings = FALSE)
iso3cs <- c()
for(i in seq_along(fits)){
  if(!is.null(fits[[i]][["pmcmc_results"]])){
    iso3cs <- c(iso3cs, names(fits[i]))
    saveRDS(fits[[i]], paste0("temp/",names(fits[i]), ".Rds"))
  }
}
remove(i)
remove(fits)

plots_list <- map(iso3cs, function(iso3c){
  #load fit and simulate
  out <- squire.page::generate_draws(
    readRDS(paste0("temp/", iso3c, ".Rds")),
    draws = draws
    )

  #generate plots
  list(
    cdp = squire.page::cdp_plot(out),
    dp = squire.page::dp_plot(out),
    ar = squire.page::ar_plot(out)
  )
})

saveRDS(plots_list, "fitting_plots.Rds")
