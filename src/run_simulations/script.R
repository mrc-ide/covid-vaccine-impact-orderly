#set seeed
if(!is.na(seed)){
  set.seed(seed)
}

#set draws
# if(is.na(draws)){
#   draws <- NULL
# }
draws <- NULL

if(excess){
  direct <- TRUE
  baseline_cf_names <- c("Baseline", "Baseline-Direct", "Baseline-No Healthcare Surging", "Baseline-Direct & No Healthcare Surging")
  empty_cf_files <- c()
} else {
  direct <- FALSE
  baseline_cf_names <- c("Baseline")
  empty_cf_files <- c("Baseline-Direct", "Baseline-No Healthcare Surging", "Baseline-Direct & No Healthcare Surging", "No Vaccines-No Healthcare Surging")
}

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
    saveRDS(fits[[i]], paste0("temp/",names(fits[i]), ".Rds"))
  }
}
remove(i)
remove(fits)

##to calculate COVAX vaccine assignments
covaxIso3c <- get_covax_iso3c()
#20% by end of 2021, assume start in 2021 Jan if current coverage less than 20%
#get current coverages
covax_data <- lapply(iso3cs, function(iso3c){
  if(iso3c %in% covaxIso3c){
    fit <- readRDS(paste0("temp/", iso3c, ".Rds"))
    eligible_pop <- sum(squire::population$n[squire::population$iso3c == iso3c][tail(fit$parameters$vaccine_coverage_mat, 1)!=0])
    #check if over 20% full dose coverage
    #back calculate dose ratio
    #non delta effiacies
    #note this is hardcoded for now, pending rewrite to $interventions
    n_delta_i_f <- 0.6
    n_delta_i_s <- 0.8
    n_delta_d_f <- 0.8
    n_delta_d_s <- 0.98
    if(excess){
      delta_shift_end <- fit$interventions$delta_adjustments$start_date +
        fit$interventions$delta_adjustments$shift_duration
      #use delta values to back calculate from final efficacy
      delta_i_f <-  fit$interventions$delta_adjustments$ve_i_low_d
      delta_i_s <-  fit$interventions$delta_adjustments$ve_i_high_d
      delta_d_f <-  fit$interventions$delta_adjustments$ve_d_low_d
      delta_d_s <-  fit$interventions$delta_adjustments$ve_d_high_d
      shift_duration <- fit$interventions$delta_adjustments$shift_duration
      cur_i_f <- delta_i_f
      cur_i_s <- delta_i_s
    } else {
      #delta may not be used
      delta_shift_end <- tail(fit$pmcmc_results$inputs$pars_obs$delta_adjust$date_dur_R_change[1:2], 1)
      if(is.null(delta_shift_end)){
        delta_shift_end <- Inf
        delta_i_f = n_delta_i_f
        delta_i_s = n_delta_i_s
        delta_d_f = n_delta_d_f
        delta_d_s = n_delta_d_s
        cur_i_f <- delta_i_f
        cur_i_s <- delta_i_s
      } else {
        #use delta values to back calculate from final efficacy
        #need to hardcode for this
        delta_i_f = 0.224
        delta_i_s = 0.646
        delta_d_f = 0.75
        delta_d_s = 0.94
        cur_i_f <- delta_i_f
        cur_i_s <- delta_i_s
        shift_duration <- 60
      }
    }
    if(any(fit$interventions$max_vaccine > 0)){
      if(delta_shift_end > date & !is.infinite(delta_shift_end)){
        #scale values by how far into shift we are
        prop <- 1-as.numeric(delta_shift_end - as.Date(date))/shift_duration
        cur_i_f <- n_delta_i_f*(1-prop) + delta_i_f*prop
        cur_i_s <- n_delta_i_s*(1-prop) + delta_i_s*prop
      }
      percentage_second_dose <-
        (tail(fit$interventions$vaccine_efficacy_infection, 1)[[1]][1] - cur_i_f)/
        (cur_i_s - cur_i_f)
      if(sum(fit$interventions$max_vaccine)*percentage_second_dose/eligible_pop < 0.2){
        dates <- fit$interventions$date_vaccine_change
        start_date <- tail(dates, 1)
        #doesn't meet double dose requirements
        #just scale up efficacy so it does meet
        #calculate delta scaling
        delta_prop <- rep(0, length(dates))
        if(!is.infinite(delta_shift_end)){
          delta_prop[dates >= fit$interventions$delta_adjustments$start_date &
                       dates < delta_shift_end] <-
            as.numeric(dates[dates >= fit$interventions$delta_adjustments$start_date &
                               dates < delta_shift_end] - fit$interventions$delta_adjustments$start_date + 1)/
            fit$interventions$delta_adjustments$shift_duration
          delta_prop[dates >= delta_shift_end] <- 1
        }
        #add the initial value too, doesn't matter what it is since its before any vaccines are given
        delta_prop <- c(0, delta_prop)
        #adjust efficacies for delta
        a_delta_i_f <- n_delta_i_f*(1-delta_prop) + delta_i_f*(delta_prop)
        a_delta_i_s <- n_delta_i_s*(1-delta_prop) + delta_i_s*(delta_prop)
        a_delta_d_f <- n_delta_d_f*(1-delta_prop) + delta_d_f*(delta_prop)
        a_delta_d_s <- n_delta_d_s*(1-delta_prop) + delta_d_s*(delta_prop)
        #derive dose ratio
        dose_ratio <- (unlist(lapply(fit$interventions$vaccine_efficacy_infection, function(x){x[1]})) -
                         a_delta_i_f)/
          (a_delta_i_s - a_delta_i_f)
        #get vaccines
        vaccines <- fit$interventions$max_vaccine
        if(sum(vaccines)/eligible_pop < 0.2){
          #first doses too low scale up first_doses
          vaccines <- vaccines/(sum(vaccines)/eligible_pop/0.2)
          #second doses to 100%
          dose_ratio_target <- 1
        }else{
          #calculate 2nd dose target
          dose_ratio_target <- 0.2*eligible_pop/sum(vaccines)
        }
        #scale up dose ratio to meet target
        dose_ratio <- dose_ratio/tail(dose_ratio, 1)*dose_ratio_target
        #correct if increases above 1
        dose_ratio <- if_else(dose_ratio > 1, as.double(1), dose_ratio)
        #calculate new efficacies
        eff_i <- a_delta_i_f*(1 - dose_ratio) + a_delta_i_s*dose_ratio
        eff_d <- a_delta_d_f*(1 - dose_ratio) + a_delta_d_s*dose_ratio
        list(
          date_vaccine_change = dates,
          max_vaccine = vaccines,
          vaccine_efficacy_infection = eff_i,
          vaccine_efficacy_disease = eff_d
        )
      } else {
        #doesn't need changing
        NULL
      }
    } else {
      #if country doesn't have any vaccines yet, we assume they start mid 2021
      start_date <- as.Date("2021-06-01")
      dates <- seq(start_date, as.Date(date), 1)
      #calculate delta adjustments
      delta_prop <- rep(0, length(dates))
      if(!is.infinite(delta_shift_end)) {
        delta_prop[dates >= fit$interventions$delta_adjustments$start_date &
                     dates < delta_shift_end] <-
          as.numeric(dates[dates >= fit$interventions$delta_adjustments$start_date &
                             dates < delta_shift_end] - fit$interventions$delta_adjustments$start_date + 1)/
          fit$interventions$delta_adjustments$shift_duration
        delta_prop[dates >= delta_shift_end] <- 1
      }
      #add the initial value too, doesn't matter what it is since its before any vaccines are given
      delta_prop <- c(0, delta_prop)
      #calculate dose ratio changes
      first_dose_only_period <- 18
      build_up <- 21
      dose_ratio <- c(rep(0, first_dose_only_period),
                      seq(0, 1, length.out = build_up + 1)[-1],
                      rep(1, length(delta_prop) - first_dose_only_period - build_up))
      #calculate efficacies
      eff_i <- (n_delta_i_f * (1 - dose_ratio) + n_delta_i_s * dose_ratio) * (1 - delta_prop) +
        (delta_i_f * (1 - dose_ratio) + delta_i_s * dose_ratio) * delta_prop
      eff_d <- (n_delta_d_f * (1 - dose_ratio) + n_delta_d_s * dose_ratio) * (1 - delta_prop) +
        (delta_d_f * (1 - dose_ratio) + delta_d_s * dose_ratio) * delta_prop
      #caclulate first doses
      v_r_b <- eligible_pop*0.2/(sum(1:build_up) + build_up*(length(dates)-build_up))
      v_r_pb <- build_up*v_r_b
      first_doses <- c(0, v_r_b*seq(1, build_up), rep(v_r_pb, length(dates)-build_up))
      list(
        date_vaccine_change = dates,
        max_vaccine = first_doses,
        vaccine_efficacy_infection = eff_i,
        vaccine_efficacy_disease = eff_d
      )
    }
  } else{
    NULL
  }
})
names(covax_data) <- iso3cs
#convert to list of lists format

if(direct){
  counterfactuals <- lapply(iso3cs, function(iso3c){
    list(
      `No Vaccines` = list(max_vaccine = c(0,0),
                           date_vaccine_change = as.Date(date) - 1,
                           vaccine_efficacy_infection = c(0,0),
                           vaccine_efficacy_disease = c(0,0)),
      `No Vaccines-No Healthcare Surging` = list(max_vaccine = c(0,0),
                                                 date_vaccine_change = as.Date(date) - 1,
                                                 vaccine_efficacy_infection = c(0,0),
                                                 vaccine_efficacy_disease = c(0,0),
                                                 no_healthcare = TRUE),

      `COVAX` = covax_data[[iso3c]]
    )
  })
} else {
  counterfactuals <- lapply(iso3cs, function(iso3c){
    list(
      `No Vaccines` = list(max_vaccine = c(0,0),
                           date_vaccine_change = as.Date(date) - 1,
                           vaccine_efficacy_infection = c(0,0),
                           vaccine_efficacy_disease = c(0,0)),
      `COVAX` = covax_data[[iso3c]]
    )
  })
}
names(counterfactuals) <- iso3cs
#remove uneeded
rm(covax_data)

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
                       direct = direct)
      )

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
    left_join(
      map_df(
        seq_along(counterfactuals),
        function(counter_index){
          map_dbl(counterfactuals[[counter_index]],
                  function(counterfactual){
                    if(is.null(counterfactual)){
                      as.double(NA)
                    } else {
                      sum(counterfactual$max_vaccine)
                    }
                  })

        }
      ) %>%
        mutate(iso3c = names(counterfactuals))
    ),
  "counterfactuals.Rds"
)

#create empty files to avoid errors
lapply(empty_cf_files, function(x){
  saveRDS(NULL, paste0(x, ".Rds"))
})
