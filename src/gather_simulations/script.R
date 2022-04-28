if(excess){
  orderly_loc <- file.path(orderly_loc, "data", "excess_mortality")
  empty_files <- NULL
} else {
  orderly_loc <- file.path(orderly_loc, "data", "reported_deaths")
  empty_files <- c("Baseline-Direct & No Healthcare Surging", "Baseline-Direct",
                   "Baseline-No Healthcare Surging", "COVAX", "No Vaccines-No Healthcare Surging",
                   "WHO", "excess_deaths")
}
#now copy over fitting plots
file.copy(
  file.path(orderly_loc, "fitting_plots.pdf"),
  "fitting_plots.pdf"
)

#summarise counterfactual results
files <- list.files(file.path(orderly_loc, "counterfactual_data"))
cfs <- map_chr(files, ~str_split(.x, "_")[[1]][1]) %>%
  unique()
walk(cfs, function(cf){
  files_to_merge <- files[str_split_fixed(files, "_", 2)[,1] == cf]
  map_dfr(file.path(orderly_loc, "counterfactual_data", files_to_merge), ~readRDS(.x)) %>%
    saveRDS(paste0(cf, ".Rds"))
})
walk(empty_files, ~saveRDS(NULL, paste0(.x, ".Rds")))
#setup the counterfactual file
#need total vaccines and full dose coverage for baseline and then full dose coverage for WHO and COVAX
counterfactuals <- readRDS(file.path(orderly_loc, "counterfactuals.Rds"))
iso3cs <- names(counterfactuals)
names(iso3cs) <- iso3cs
map_dfr(iso3cs, function(iso3c){
  #get fit
  out <- readRDS(paste0(
    orderly_loc, "/model_fits/", iso3c, ".Rds"
  ))
  first_doses <- out$interventions$max_vaccine %>% sum()
  second_doses <- (out$interventions$dose_ratio %>% tail(1)) * first_doses
  tibble(
    `Baseline (Total Vaccines)` = first_doses + second_doses,
    `Baseline` = second_doses
  )
}, .id = "iso3c") %>%
  left_join(
    map_dfr(counterfactuals, function(counterfactual){
      if(is.null(counterfactual$COVAX)){
        COVAX <- as.numeric(NA)
      } else {
        COVAX <- sum(counterfactual$COVAX$max_vaccine) *
          tail(counterfactual$COVAX$dose_ratio, 1)
      }
      if(is.null(counterfactual$WHO)){
        WHO <- as.numeric(NA)
      } else {
        WHO <- sum(counterfactual$WHO$max_vaccine) *
          tail(counterfactual$WHO$dose_ratio, 1)
      }
      tibble(
        COVAX = COVAX,
        WHO = WHO
      )
    }, .id = "iso3c"),
    by = "iso3c"
  ) %>%
  mutate(
    `No Vaccines` = 0,
  ) %>%
  saveRDS("counterfactuals.Rds")
#get the number of excess deaths
if(excess){
  map_dfr(iso3cs, function(iso3c){
    #get fit
    readRDS(paste0(
      orderly_loc, "/model_fits/", iso3c, ".Rds"
    ))$pmcmc_results$inputs$data
  }, .id = "iso3c") %>%
    group_by(week_start, week_end) %>%
    summarise(
      deaths = sum(deaths)
    ) %>%
    mutate(
      obsDate = (week_end - week_start)/2 + week_start,
      deaths = deaths/as.numeric(week_end - week_start)
    ) %>%
    ungroup() %>%
    select(obsDate, deaths) %>%
    saveRDS("excess_deaths.Rds")

}
