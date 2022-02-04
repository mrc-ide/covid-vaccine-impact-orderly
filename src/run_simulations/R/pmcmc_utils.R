#' Generate Deaths Averted
#'
#' @param out Output of `squire::pmcmc`
#' @param draws Number of draws from mcmc chain. Default = 10
#' @param counterfactual A named list of lists. Each list should contain the
#' dates that vaccines change, the number of vaccines, and efficacies. If NULL
#' that counter-factual will be skipped.
#' @param reduce_age Should the output be aggregated by age? Can be TRUE, FALSE,
#' or "Both" to return values both with the aggregated in a special age_group "Total"
#' @param direct Should there be an estimate of direct effect of the vaccine, i.e.
#' no protection against infection. Default = FALSE.
deaths_averted <- function(out, draws, counterfactual, reduce_age = TRUE,
                           direct = FALSE) {
  #error if Baseline in counterfactual
  if(any(c("Baseline","baseline","BASELINE") %in% names(counterfactual))){
    stop('"Baseline" is a reserved name for a counterfactual, please choose another name')
  }

  # return NULL if nothing
  if(!("pmcmc_results" %in% names(out))) {
    return(NULL)
  }

  # get the real data
  data <- out$pmcmc_results$inputs$data
  country <- out$parameters$country
  iso3c <- squire::population$iso3c[squire::population$country == country][1]
  if(is.null(suppressWarnings(data$week_end))){
    date_0 <- max(data$date)
  } else {
    date_0 <- max(data$week_end)
  }
  #draw the parameters
  if(is.null(draws)){
    pars.list <- NULL
  } else {
    pars.list <- squire.page::generate_parameters(out, draws)
  }

  #Set up the baseline results
  baseline <- squire.page::generate_draws(out, pars.list, draws)
  # format the counter factual run
  baseline_deaths <- nimue_format(baseline, c("deaths", "infections"), date_0 = date_0,
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
    #indirect protection
    baseline_direct <- squire.page::generate_draws(remove_indirect(out), pars.list, draws)
    # format the counter factual run
    baseline_direct_deaths <- nimue_format(baseline_direct, c("deaths", "infections"), date_0 = date_0,
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
    #healthcare
    baseline_healthcare <- squire.page::generate_draws(remove_healthcare(out), pars.list, draws)
    # format the counter factual run
    baseline_healthcare_deaths <- nimue_format(baseline_healthcare, c("deaths", "infections"), date_0 = date_0,
                                           reduce_age = reduce_age) %>%
      dplyr::distinct() %>%
      tidyr::pivot_wider(names_from = .data$compartment, values_from = .data$y) %>%
      na.omit() %>%
      dplyr::mutate(counterfactual = "Baseline-No Healthcare Surging")

    if(!reduce_age){
      baseline_healthcare_deaths <- dplyr::mutate(baseline_healthcare_deaths, age_group = as.character(.data$age_group))
    }

    baseline_healthcare_deaths$t <- NULL

    baseline_deaths <- rbind(
      baseline_deaths,
      baseline_healthcare_deaths
    )
    #healthcare+direct
    baseline_healthcare_direct <- squire.page::generate_draws(remove_indirect(remove_healthcare(out)), pars.list, draws)
    # format the counter factual run
    baseline_healthcare_direct_deaths <- nimue_format(baseline_healthcare_direct, c("deaths", "infections"), date_0 = date_0,
                                               reduce_age = reduce_age) %>%
      dplyr::distinct() %>%
      tidyr::pivot_wider(names_from = .data$compartment, values_from = .data$y) %>%
      na.omit() %>%
      dplyr::mutate(counterfactual = "Baseline-Direct & No Healthcare Surging")

    if(!reduce_age){
      baseline_healthcare_direct_deaths <- dplyr::mutate(baseline_healthcare_direct_deaths, age_group = as.character(.data$age_group))
    }

    baseline_healthcare_direct_deaths$t <- NULL

    baseline_deaths <- rbind(
      baseline_deaths,
      baseline_healthcare_direct_deaths
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
    if(!is.null(counterfactual[[counterIndex]])){
      counter <- squire.page::generate_draws(out = update_counterfactual(out, counterfactual[[counterIndex]]),
                                             pars.list = pars.list, draws = draws)
      #format the counter factual run
      counter_df <- nimue_format(counter, c("deaths", "infections"),
                                 date_0 = date_0,
                                 reduce_age = reduce_age) %>%
        dplyr::distinct() %>%
        tidyr::pivot_wider(names_from = .data$compartment, values_from = .data$y) %>%
        na.omit() %>%
        dplyr::mutate(counterfactual = names(counterfactual)[counterIndex])

      if(!reduce_age){
        counter_df <- dplyr::mutate(counter_df, age_group = as.character(.data$age_group))
      }
      counter_df$t <- NULL

      #attach to counter-factual data frame
      deaths_df[seq(dataframeLength) + dataframeLength*(counterIndex-1),] <- counter_df
    }
  }

  #remove unused row (i.e. if a counter factual is null)
  deaths_df <- deaths_df %>%
    filter(!is.na(counterfactual))

  #add baseline data
  deaths_df <- rbind(
    deaths_df,
    baseline_deaths
  )

  if(!is.null(out$interventions$pre_epidemic_isolated_deaths)){
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
  }

  deaths_df <- arrange(deaths_df, counterfactual, replicate, date)


  # and add country info
  deaths_df$country <- country
  deaths_df$iso3c <- iso3c
  return(deaths_df)
}
remove_indirect <- function(out){
    out$odin_parameters$vaccine_efficacy_infection <- matrix(0.5,
                                                             nrow = nrow(out$odin_parameters$vaccine_efficacy_infection),
                                                             ncol = ncol(out$odin_parameters$vaccine_efficacy_infection))
    #scale up protection against disease to keep the same efficacy
    if(any(out$interventions$date_vaccine_efficacy_infection_change != out$interventions$date_vaccine_efficacy_disease_change)){
      "Different efficacy change times against disease and infection"
    }
    trueEff <-
      Map("+",
          out$interventions$vaccine_efficacy_infection,
          Map("*",
              Map("-", 1, out$interventions$vaccine_efficacy_infection),
              out$interventions$vaccine_efficacy_disease
          )
      )
    out$parameters$vaccine_efficacy_disease <- trueEff
    out$interventions$vaccine_efficacy_disease <- trueEff
    out$pmcmc_results$inputs$interventions$vaccine_efficacy_disease <- trueEff

    #set infection efficacy in interventions
    out$pmcmc_results$inputs$interventions$vaccine_efficacy_infection <- lapply(
      out$pmcmc_results$inputs$interventions$vaccine_efficacy_infection, function(x) {
        rep(0,17)
      })
    out$interventions$vaccine_efficacy_infection <- lapply(
      out$pmcmc_results$inputs$interventions$vaccine_efficacy_infection, function(x) {
        rep(0,17)
      })
    # the calc_loglikelihood function samples from the model_params for vaccine pars rather than recalculating
    # so we need to update this here
    out$pmcmc_results$inputs$model_params$vaccine_efficacy_infection <- nimue:::format_ve_i_for_odin(
      vaccine_efficacy_infection = out$interventions$vaccine_efficacy_infection,
      tt_vaccine_efficacy_infection = out$pmcmc_results$inputs$model_params$tt_vaccine_efficacy_infection
    )

    out$pmcmc_results$inputs$model_params$prob_hosp <- nimue:::format_ve_d_for_odin(
      vaccine_efficacy_disease = trueEff,
      tt_vaccine_efficacy_disease = out$pmcmc_results$inputs$model_params$tt_vaccine_efficacy_disease,
      prob_hosp = nimue:::probs$prob_hosp)

    #set relative infectiousness to 1
    out$pmcmc_results$inputs$model_params$rel_infectiousness_vaccinated <-
      matrix(1,
             nrow = nrow(out$pmcmc_results$inputs$model_params$rel_infectiousness_vaccinated),
             ncol = ncol(out$pmcmc_results$inputs$model_params$rel_infectiousness_vaccinated)
      )
    out$odin_parameters$rel_infectiousness_vaccinated <-
      matrix(1,
             nrow = nrow(out$odin_parameters$rel_infectiousness_vaccinated),
             ncol = ncol(out$odin_parameters$rel_infectiousness_vaccinated)
      )
    out$parameters$rel_infectiousness_vaccinated <-
      rep(1, length(out$parameters$rel_infectiousness_vaccinated))
    return(out)
}
remove_healthcare <- function(out){
  out$parameters$hosp_bed_capacity <- 10^7
  out$parameters$ICU_bed_capacity <- 10^7
  out$odin_parameters$hosp_beds <- 10^7
  out$odin_parameters$ICU_beds <- 10^7
  out$interventions$hosp_bed_capacity <- 10^7
  out$interventions$ICU_bed_capacity <- 10^7
  out$pmcmc_results$inputs$model_params$hosp_beds <- 10^7
  out$pmcmc_results$inputs$model_params$ICU_beds <- 10^7
  out$pmcmc_results$inputs$interventions$hosp_bed_capacity <- 10^7
  out$pmcmc_results$inputs$interventions$ICU_bed_capacity <- 10^7
  return(out)
}
update_counterfactual <- function(out, counterfactual){
  out$pmcmc_results$inputs$interventions$date_vaccine_change <-
    counterfactual$date_vaccine_change
  out$pmcmc_results$inputs$interventions$date_vaccine_efficacy_disease_change <-
    counterfactual$date_vaccine_change
  out$pmcmc_results$inputs$interventions$date_vaccine_efficacy_infection_change <-
    counterfactual$date_vaccine_change
  out$pmcmc_results$inputs$interventions$date_vaccine_efficacy <-
    counterfactual$date_vaccine_change
  out$pmcmc_results$inputs$interventions$max_vaccine <-
    counterfactual$max_vaccine
  out$pmcmc_results$inputs$interventions$dose_ratio <-
    counterfactual$dose_ratio
  out$pmcmc_results$inputs$interventions$vaccine_efficacy_disease <-
    lapply(counterfactual$vaccine_efficacy_disease, function(x){rep(x, 17)})
  out$pmcmc_results$inputs$interventions$vaccine_efficacy_infection <-
    lapply(counterfactual$vaccine_efficacy_infection, function(x){rep(x, 17)})

  out$pmcmc_results$inputs$model_params$vaccine_efficacy_infection <- nimue:::format_ve_i_for_odin(
    vaccine_efficacy_infection = lapply(counterfactual$vaccine_efficacy_infection, function(x){rep(x, 17)}),
    tt_vaccine_efficacy_infection = c(0, seq_along(counterfactual$date_vaccine_change))
  )
  out$pmcmc_results$inputs$model_params$tt_vaccine_efficacy_infection <- c(0, seq_along(counterfactual$date_vaccine_change))

  out$pmcmc_results$inputs$model_params$prob_hosp <- nimue:::format_ve_d_for_odin(
    vaccine_efficacy_disease = lapply(counterfactual$vaccine_efficacy_disease, function(x){rep(x, 17)}),
    tt_vaccine_efficacy_disease =  c(0, seq_along(counterfactual$date_vaccine_change)),
    prob_hosp = out$parameters$prob_hosp
  )
  out$pmcmc_results$inputs$model_params$tt_vaccine_efficacy_disease <- c(0, seq_along(counterfactual$date_vaccine_change))
  #don't need to update the max vaccine as it uses intervention data

  out$interventions$date_vaccine_change <-
    counterfactual$date_vaccine_change
  out$interventions$date_vaccine_efficacy_disease_change <-
    counterfactual$date_vaccine_change
  out$interventions$date_vaccine_efficacy_infection_change <-
    counterfactual$date_vaccine_change
  out$interventions$date_vaccine_efficacy <-
    counterfactual$date_vaccine_change
  out$interventions$max_vaccine <-
    counterfactual$max_vaccine
  out$interventions$dose_ratio <-
    counterfactual$dose_ratio
  out$interventions$vaccine_efficacy_disease <-
    lapply(counterfactual$vaccine_efficacy_disease, function(x){rep(x, 17)})
  out$interventions$vaccine_efficacy_infection <-
    lapply(counterfactual$vaccine_efficacy_infection, function(x){rep(x, 17)})

  #also remove healthcare if requested
  if(!is.null(counterfactual$no_healthcare)){
    if(counterfactual$no_healthcare){
      out <- remove_healthcare(out)
    }
  }
  return(out)
}
