#must use the preset draws so that it's the same values as those used in the simulation

fits <-  readRDS("countryfits.Rds")
dir.create("temp")
pdfs <- unlist(lapply(seq_along(fits), function(x){
  iso <- names(fits)[x]
  if((!(iso %in% exclude_iso3cs)) & !is.null(fits[[x]])){
    fit <- generate_draws(fits[[x]], draws = NULL, pars.list = NULL)
    fit_1 <- dp_plot(fit) + labs(
      title = paste0(countrycode::countrycode(iso, origin = "iso3c", destination = "country.name"), ":", iso)
    )
    fit_2 <- cdp_plot(fit)
    plot <- ggarrange(
      fit_1,
      fit_2,
      nrow = 2
    )
    dest <- paste0("temp", "\\", iso, ".pdf")
    ggsave(dest, plot)
    return(dest)
  }
}))
#combine
pdf_combine(
  pdfs,
  output = "fitting_plots.pdf"
)
unlink("temp", recursive = TRUE)
