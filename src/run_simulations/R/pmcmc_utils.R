#' Generate Deaths Averted
#'
#' @param out Output of `squire::pmcmc`
#' @param draws Number of draws from mcmc chain. Default = 10
#' @param parallel Are simulations done in parallel? Default = TRUE
#' @param counterfactual Which counter-factual to compare to? Default = "No Vaccine",
#' and can take a vector to indicate more than one. Should not include Baseline
#' as this is used to store the baseline (not counterfactual data).
#' @param reduce_age Should the output be aggregated by age? Can be TRUE, FALSE,
#' or "Both" to return values both with the aggregated in a special age_group "Total"
#' @param assignedVaccine A vector of the number of vaccines assigned to the
#' country in each counter factual. If this has length = 1 then this number is
#' given to all counter factuals.
#' @param vaccineStart The dates to start the vaccination campaign in each counter
#' factual. If = NULL then each counter factual is assumed to start when it did
#' in reality, else use NA in a vector to indicate this. If the country has not
#' begun this will be the day before the last observation. Dates should be characters
#' in the format "YYYY-MM-DD". If length=1 this date is given to all counter
#' factuals. Default = NULL.
#' @param noWarnings Suppress warnings around just changing the name of a counter
#' factual. Default  = FALSE.
#' @param direct Should there be an estimate of direct effect of the vaccine, i.e.
#' no protection against infection. Default = FALSE.
#' @param excess Use the excess loglikelihood? Default = FALSE
deaths_averted <- function(out, draws, parallel, counterfactual = "No Vaccine", reduce_age = TRUE,
                           assignedVaccine = 0, vaccineStart = NULL,
                           direct = FALSE, noWarnings = FALSE, excess = FALSE) {
  #error if Baseline in counterfactual
  if(any(c("Baseline","baseline","BASELINE") %in% counterfactual)){
    stop('"Baseline" is a reserved name for a counterfactual, please choose another name')
  }
  #warnings for incorrect assignments
  if(!noWarnings & length(counterfactual) == 1){
    if(assignedVaccine == 0 & counterfactual != "No Vaccine"){
      warning("No vaccines assigned to country, supress this warning with noWarnings = TRUE")
    }
  }
  #errors to inconsistent dimensions
  if(length(assignedVaccine) == 1){
    assignedVaccine <- rep(assignedVaccine, length(counterfactual))
    #if only one value use for all counter factuals
  } else if(length(counterfactual) != length(assignedVaccine)){
    stop("Inconsistent number of counterfactuals and vaccine assignments")
  }
  if(!is.null(vaccineStart)){
    if(length(vaccineStart) == 1){
      vaccineStart <- rep(vaccineStart, length(counterfactual))
      #if only one date use for all counter factuals
    } else if(length(vaccineStart) != length(counterfactual)){
      stop("Inconsistent number of counterfactuals and vaccine start dates")
    }
  } else{
    vaccineStart <- rep(NA, length(counterfactual))
  }


  # return NULL if nothing
  if(!("pmcmc_results" %in% names(out))) {
    return(NULL)
  }

  # get the real data
  data <- out$pmcmc_results$inputs$data
  if("week_start" %in% names(data)){
    data$date <- data$week_start
    out$pmcmc_results$inputs$data$date <- out$pmcmc_results$inputs$data$week_start
  }
  country <- out$parameters$country
  iso3c <- squire::population$iso3c[squire::population$country == country][1]

  #draw the parameters
  pars.list <- generate_parameters(out, draws)

  #Set up the baseline results
  #need to draw this since we need infections (+ maybe age-disaggregated)
  baseline <- generate_draws(out, pars.list, draws, parallel, excess = excess)
  # format the counter factual run
  baseline_deaths <- nimue_format(baseline, c("deaths", "infections"), date_0 = max(data$date),
                                  reduce_age = reduce_age) %>%
    dplyr::distinct() %>%
    tidyr::pivot_wider(names_from = .data$compartment, values_from = .data$y) %>%
    na.omit() %>%
    dplyr::mutate(counterfactual = "Baseline")

  if(!reduce_age){
    baseline_deaths <- dplyr::mutate(baseline_deaths, age_group = as.character(.data$age_group))
  }

  baseline_deaths$t <- NULL

  dataframeLength <- nrow(baseline_deaths)

  #if needed generate the direct effect results
  if(direct){
    baseline_direct <- generate_draws(out, pars.list, draws, parallel, noInfectionProtect = TRUE, excess = excess)
    # format the counter factual run
    baseline_direct_deaths <- nimue_format(baseline_direct, c("deaths", "infections"), date_0 = max(data$date),
                                           reduce_age = reduce_age) %>%
      dplyr::distinct() %>%
      tidyr::pivot_wider(names_from = .data$compartment, values_from = .data$y) %>%
      na.omit() %>%
      dplyr::mutate(counterfactual = "Baseline-Direct")

    if(!reduce_age){
      baseline_direct_deaths <- dplyr::mutate(baseline_direct_deaths, age_group = as.character(.data$age_group))
    }

    baseline_direct_deaths$t <- NULL

    baseline_deaths <- rbind(
      baseline_deaths,
      baseline_direct_deaths
    )
  }

  #set up data frame to hold results
  columns <- ncol(baseline_deaths)
  deaths_df <- as.data.frame(
    matrix(NA,
           ncol = columns,
           nrow = dataframeLength*length(counterfactual))
  )
  names(deaths_df) <- names(baseline_deaths)
  class(deaths_df$date) <- "Date"

  #run the counter-factual
  for(counterIndex in seq_along(counterfactual)){
    #generate draws with pars.list
    counter <- generate_draws_counterfactual(out = out, pars.list = pars.list,
                                             assignedVaccine = assignedVaccine[counterIndex],
                                             vaccineStart = vaccineStart[counterIndex],
                                             draws = draws, parallel = parallel,
                                             excess = excess
    )
    #format the counter factual run
    counter_df <- nimue_format(counter, c("deaths", "infections"),
                               date_0 = max(data$date),
                               reduce_age = reduce_age) %>%
      dplyr::distinct() %>%
      tidyr::pivot_wider(names_from = .data$compartment, values_from = .data$y) %>%
      na.omit() %>%
      dplyr::mutate(counterfactual = counterfactual[counterIndex])

    if(!reduce_age){
      counter_df <- dplyr::mutate(counter_df, age_group = as.character(.data$age_group))
    }
    counter_df$t <- NULL

    #attach to counter-factual data frame
    deaths_df[seq(dataframeLength) + dataframeLength*(counterIndex-1),] <- counter_df
  }

  #add baseline data
  deaths_df <- rbind(
    deaths_df,
    baseline_deaths
  ) #ADD REMOVED DEATHS

  if(out$interventions$pre_epidemic_isolated_deaths > 0){
    if(reduce_age){
      deaths_df <- rbind(deaths_df,
                         expand.grid(replicate = unique(deaths_df$replicate),
                                     counterfactual = unique(deaths_df$counterfactual)) %>%
                           mutate(
                             date = NA,
                             deaths = out$interventions$pre_epidemic_isolated_deaths,
                             infections = 0
                           )
      )
    } else {
      deaths_df <- rbind(deaths_df,
                         expand.grid(replicate = unique(deaths_df$replicate),
                                     counterfactual = unique(deaths_df$counterfactual),
                                     age_group = unique(deaths_df$age_group)) %>%
                           group_by(replicate, counterfactual) %>%
                           mutate(
                             date = NA,
                             deaths = out$interventions$pre_epidemic_isolated_deaths/length(age_group),
                             infections = 0
                           ) %>% ungroup()
      )
    }
  }

  deaths_df <- arrange(deaths_df, counterfactual, replicate, date)


  # and add country info
  deaths_df$country <- country
  deaths_df$iso3c <- iso3c
  return(deaths_df)
}
