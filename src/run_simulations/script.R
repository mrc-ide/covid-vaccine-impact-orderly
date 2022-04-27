if(excess){
  direct <- TRUE
  baseline_cf_names <- c("Baseline", "Baseline-Direct", "Baseline-No Healthcare Surging", "Baseline-Direct & No Healthcare Surging")
  empty_cf_files <- c()
} else {
  direct <- FALSE
  baseline_cf_names <- c("Baseline")
  empty_cf_files <- c("Baseline-Direct", "Baseline-No Healthcare Surging",
                      "Baseline-Direct & No Healthcare Surging", "No Vaccines-No Healthcare Surging",
                      "COVAX", "WHO")
}

if(get_from_cluster){
  load_files <-
    c("Baseline", "No Vaccines", "COVAX", "WHO", "Baseline-Direct", "Baseline-No Healthcare Surging",
                  "Baseline-Direct & No Healthcare Surging", "No Vaccines-No Healthcare Surging") %>%
    #remove uneeded files
    setdiff(empty_cf_files) %>%
    #add file extensions
    paste0(".Rds") %>%
    #add fitting plot
    c("fitting_plots.pdf")
  #add file path
  load_from <- file.path(cluster_file_loc, "final_objects", load_files)
  load_to <- load_files
  #save values in this task
  file.copy(load_from, load_to)
  #load in counterfactual data for the vaccines df
  counterfactuals <-
    readRDS(file.path(cluster_file_loc, "final_objects/counterfactuals.Rds"))
  iso3cs <- names(counterfactuals)
} else {
  #set seeed
  if(!is.na(seed)){
    set.seed(seed)
  }

  if(!is.na(exclude_iso3cs) & exclude_iso3cs != "NA"){
    #get around orderlys input limitations
    exclude_iso3cs <- strsplit(exclude_iso3cs, " ")[[1]]
  } else {
    exclude_iso3cs <- NULL
  }

  #set draws
  # if(is.na(draws)){
  #   draws <- NULL
  # }
  draws <- NULL

  ## Generate Deaths Averted Data for all countries with nimue fits
  if(parallel){
    future::future(future::multisession())
  } else {
    future::future(future::sequential())
  }

  #which countries do we have
  fits <-  readRDS("countryfits.Rds")
  #create a temporary subdirectory to make access quicker
  dir.create("temp", showWarnings = FALSE)
  iso3cs <- c()
  for(i in seq_along(fits)){
    if(!is.null(fits[[i]][["pmcmc_results"]])){
      iso3cs <- c(iso3cs, names(fits[i]))
      # #need to update fits to work with latest changes to nimue
      # fit <- fits[[i]]
      # #TEMP REMOVE REPLICATES
      # fit$replicate_parameters <- fit$replicate_parameters[1:15,]

      saveRDS(fits[[i]], paste0("temp/",names(fits[i]), ".Rds"))
    }
  }
  remove(i)
  remove(fits)

  iso3cs <- setdiff(iso3cs, exclude_iso3cs)

  ##to calculate COVAX vaccine assignments
  covaxIso3c <- get_covax_iso3c()
  #20% by end of 2021, assume start in 2021 Jan if current coverage less than 20%
  #get current coverages
  covax_data <- lapply(iso3cs, function(iso3c){
    if(iso3c %in% covaxIso3c){
      fit <- readRDS(paste0("temp/", iso3c, ".Rds"))
      eligible_pop <- sum(squire::population$n[squire::population$iso3c == iso3c][tail(fit$parameters$vaccine_coverage_mat, 1)!=0])
      #check if vacciens are given at all
      if(any(fit$interventions$max_vaccine > 0)){
        #check if over 20% full dose coverage (assuming all vaccines are given)
        second_dosed <- sum(fit$interventions$max_vaccine)*tail(fit$interventions$dose_ratio, 1)
        if(second_dosed/eligible_pop < 0.2){
          #figure out if we need to scale up first doses or just second
          first_doses <- fit$interventions$max_vaccine
          dose_ratio <- fit$interventions$dose_ratio
          if(sum(first_doses)/eligible_pop < 0.2){
            #first doses too low scale up first_doses
            first_doses <- first_doses/(sum(first_doses)/eligible_pop/0.2)
            #second doses to 100%
            dose_ratio_final <- 1
          }else{
            #calculate 2nd dose target
            dose_ratio_final <- 0.2*eligible_pop/sum(first_doses)
          }
          #scale up dose ratio to meet target
          dose_ratio <- dose_ratio/tail(dose_ratio, 1)*dose_ratio_final
          #correct if increases above 1
          dose_ratio <- if_else(dose_ratio > 1, as.double(1), dose_ratio)
          list(
            date_vaccine_change = fit$interventions$date_vaccine_change,
            date_vaccine_efficacy = fit$interventions$date_vaccine_change,
            max_vaccine = first_doses,
            dose_ratio = dose_ratio
          )
        } else {
          #doesn't need changing
          NULL
        }
      } else {
        #if country doesn't have any vaccines yet, we assume they start mid 2021
        start_date <- as.Date("2021-06-01")
        dates <- seq(start_date, as.Date(date), 1)
        #calculate dose ratio changes
        first_dose_only_period <- 18
        build_up <- 21
        dose_ratio <- c(rep(0, first_dose_only_period),
                        seq(0, 1, length.out = build_up + 1)[-1],
                        rep(1, length(dates) + 1 - first_dose_only_period - build_up))
        #caclulate first doses
        v_r_b <- eligible_pop*0.2/(sum(1:build_up) + build_up*(length(dates)-build_up))
        v_r_pb <- build_up*v_r_b
        first_doses <- c(0, v_r_b*seq(1, build_up), rep(v_r_pb, length(dates)-build_up))
        list(
          date_vaccine_change = dates,
          date_vaccine_efficacy = dates,
          max_vaccine = first_doses,
          dose_ratio = dose_ratio[-1]
        )
      }
    } else{
      NULL
    }
  })
  names(covax_data) <- iso3cs
  #convert to list of lists format

  #WHO goal counterfactual 40% total pop full dose
  who_data <- lapply(iso3cs, function(iso3c){
    fit <- readRDS(paste0("temp/", iso3c, ".Rds"))
    eligible_pop <- sum(squire::population$n[squire::population$iso3c == iso3c])
    #check if vacciens are given at all
    if(any(fit$interventions$max_vaccine > 0)){
      #check if over 40% full dose coverage (assuming all vaccines are given)
      second_dosed <- sum(fit$interventions$max_vaccine)*tail(fit$interventions$dose_ratio, 1)
      if(second_dosed/eligible_pop < 0.4){
        #figure out if we need to scale up first doses or just second
        first_doses <- fit$interventions$max_vaccine
        dose_ratio <- fit$interventions$dose_ratio
        if(sum(first_doses)/eligible_pop < 0.4){
          #first doses too low scale up first_doses
          first_doses <- first_doses/(sum(first_doses)/eligible_pop/0.4)
          #second doses to 100%
          dose_ratio_final <- 1
        }else{
          #calculate 2nd dose target
          dose_ratio_final <- 0.4*eligible_pop/sum(first_doses)
        }
        #scale up dose ratio to meet target
        dose_ratio <- dose_ratio/tail(dose_ratio, 1)*dose_ratio_final
        #correct if increases above 1
        dose_ratio <- if_else(dose_ratio > 1, as.double(1), dose_ratio)
        list(
          date_vaccine_change = fit$interventions$date_vaccine_change,
          date_vaccine_efficacy = fit$interventions$date_vaccine_change,
          max_vaccine = first_doses,
          dose_ratio = dose_ratio
        )
      } else {
        #doesn't need changing
        NULL
      }
    } else {
      #if country doesn't have any vaccines yet, we assume they start mid 2021
      start_date <- as.Date("2021-06-01")
      dates <- seq(start_date, as.Date(date), 1)
      #calculate dose ratio changes
      first_dose_only_period <- 18
      build_up <- 21
      dose_ratio <- c(rep(0, first_dose_only_period),
                      seq(0, 1, length.out = build_up + 1)[-1],
                      rep(1, length(dates) + 1 - first_dose_only_period - build_up))
      #caclulate first doses
      v_r_b <- eligible_pop*0.4/(sum(1:build_up) + build_up*(length(dates)-build_up))
      v_r_pb <- build_up*v_r_b
      first_doses <- c(0, v_r_b*seq(1, build_up), rep(v_r_pb, length(dates)-build_up))
      list(
        date_vaccine_change = dates,
        date_vaccine_efficacy = dates,
        max_vaccine = first_doses,
        dose_ratio = dose_ratio[-1]
      )
    }
  })
  names(who_data) <- iso3cs


  if(direct){
    counterfactuals <- lapply(iso3cs, function(iso3c){
      list(
        `No Vaccines` = list(max_vaccine = c(0,0),
                             date_vaccine_change = as.Date(date) - 1,
                             dose_ratio = 0,
                             date_vaccine_efficacy = as.Date(date) - 1),
        `No Vaccines-No Healthcare Surging` = list(max_vaccine = c(0,0),
                                                   date_vaccine_change = as.Date(date) - 1,
                                                   date_vaccine_efficacy = as.Date(date) - 1,
                                                   dose_ratio = 0,
                                                   no_healthcare = TRUE),

        `COVAX` = covax_data[[iso3c]],
        `WHO` = who_data[[iso3c]]
      )
    })
  } else {
    counterfactuals <- lapply(iso3cs, function(iso3c){
      list(
        `No Vaccines` = list(max_vaccine = c(0,0),
                             dose_ratio = 0,
                             date_vaccine_change = as.Date(date) - 1,
                             date_vaccine_efficacy = as.Date(date) - 1
        ))
    })
  }
  names(counterfactuals) <- iso3cs
  #remove uneeded
  rm(covax_data)

    #for fitting plots create a temporary folder
    dir.create("temp_plots", showWarnings = FALSE)
    #run in parts
    indexes <- seq_along(counterfactuals)
    groups <- split(indexes, ceiling(indexes/50))
    walk(
      groups,
      function(group){
        #generate counter factuals for group
        deaths_averted_list <- lapply(group, function(country_index){
          iso3c <- names(counterfactuals)[country_index]
          message(paste0(country_index, ":", names(counterfactuals)[country_index]))
          out <- readRDS(paste0("temp/", as.character(iso3c), ".Rds"))

          df <- suppressMessages(
            deaths_averted(out, draws = draws,
                           counterfactual = counterfactuals[[country_index]],
                           reduce_age = TRUE,
                           direct = direct,
                           plot_name = paste0("temp_plots/", iso3c, ".pdf"),
                           excess = excess)
          )

          #shorten data to current date
          df <- df %>%
            rename(obs_date=date) %>%
            filter(obs_date <= date) %>%
            rename(date = obs_date)
          return(df)
        })
        #save each counterfactual seperately in temp files
        for(thisCounterfactual in
            c(baseline_cf_names, names(counterfactuals[[1]]))
        ){
          temp_list <- list()
          for(j in seq_along(deaths_averted_list)){
            temp_list[[j]] <- filter(deaths_averted_list[[j]],
                                     .data$counterfactual == thisCounterfactual) %>%
              ungroup() %>%
              select(!.data$counterfactual)
          }
          saveRDS(
            do.call(
              rbind,
              temp_list),
            paste0(thisCounterfactual, head(group, 1), ".Rds")
          )
        }
      }
    )

    #delete temporary folder
    unlink("temp", recursive = TRUE)

    #combine seperate counterfactual files
    walk(
      c(baseline_cf_names, names(counterfactuals[[1]])),
      function(thisCounterfactual){
        #get names of files
        temp_files <- paste0(thisCounterfactual, map_dbl(groups, ~head(.x, 1)), ".Rds")
        #load files and append to one then save
        saveRDS(
          do.call(
            rbind,
            map(temp_files, ~readRDS(.x))
          ),
          paste0(thisCounterfactual, ".Rds")
        )
        #remove temp files
        unlink(temp_files, recursive = TRUE)
      }
    )

    #combine fitting plots into a single pdf
    pdf_combine(
      list.files("temp_plots", full.names = TRUE),
      output = "fitting_plots.pdf"
    )
    unlink("temp_plots", recursive = TRUE)
}

##vaccines given out per country
saveRDS(
  readRDS("owid.Rds") %>%
    filter(iso3c %in% iso3cs) %>%
    group_by(iso3c) %>%
    summarise(
      `Baseline (Total Vaccines)` = max(total_vaccinations, na.rm = T)
    ) %>%
    mutate(`Baseline (Total Vaccines)` = if_else(
      is.infinite(`Baseline (Total Vaccines)`),
      0,
      `Baseline (Total Vaccines)`
    )) %>%
    rbind(
      c(iso3c = "PRK", `Baseline (Total Vaccines)` = 0) #add missing country
    ) %>%#add missing country, calculated from modelled values
    mutate(`Baseline (Total Vaccines)` = if_else(
      iso3c == "FSM",
      77833,
      as.numeric(`Baseline (Total Vaccines)`))) %>%
    left_join(
      map_df(
        seq_along(counterfactuals),
        function(counter_index){
          map_dbl(counterfactuals[[counter_index]],
                  function(counterfactual){
                    if(is.null(counterfactual)){
                      as.double(NA)
                    } else {
                      sum(counterfactual$max_vaccine)*tail(counterfactual$dose_ratio, 1) #changed to number fully dosed
                    }
                  })

        }
      ) %>%
        mutate(iso3c = names(counterfactuals))
    ),
  "counterfactuals.Rds"
)

#create empty files to avoid orderly errors
lapply(empty_cf_files, function(x){
  saveRDS(NULL, paste0(x, ".Rds"))
})
