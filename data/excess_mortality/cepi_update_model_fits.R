#NOTE that this does not modify the model fit objects themselves, just the
#baseline counterfactual data
library(tidyverse)
iso3cs <- list.files(here::here("data", "excess_mortality", "model_fits")) %>%
  str_remove_all(".Rds")

need <- purrr::map_dfr(iso3cs, function(iso3c){
  fit <- readRDS(here::here("data", "excess_mortality", "model_fits", paste0(iso3c, ".Rds")))
  effects_baseline <- any(fit$interventions$date_vaccine_change[1] < fit$replicate_parameters$start_date)
  effects_cepi <- any(as.Date("2020-01-10") + 100 < fit$replicate_parameters$start_date)
  tibble::tibble(
    effects_baseline = effects_baseline,
    effects_cepi = effects_cepi,
    iso3c = iso3c
  )
})

vaccination_model <- odin::odin({
  N_age <- user() # Number of age groups
  N_vaccine <- user() # Number of vaccine groups
  time <- t
  output(time) <- TRUE

  S_0[,] <- user()
  dim(S_0) <- c(N_age, N_vaccine)
  initial(S[,]) <- S_0[i,j]
  dim(S) <- c(N_age, N_vaccine)

  deriv(S[,1]) <- (vr * vaccination_target[i] * S[i,j])
  deriv(S[,2]) <- (vr * vaccination_target[i] * S[i,j-1]) - (gamma_vaccine[j] * S[i,j])
  deriv(S[,3:N_vaccine]) <- (gamma_vaccine[j-1] * S[i,j-1]) - (gamma_vaccine[j] * S[i,j])

  # Vaccination
  # Vaccine prioritisation coverage matrix
  N_prioritisation_steps <- user()
  vaccine_coverage_mat[,] <- user()
  dim(vaccine_coverage_mat) <- c(N_prioritisation_steps, N_age)

  gamma_vaccine[] <- user() # Vector of vaccine progression parameters by vaccination status (First = 0 as handled separately as time-varying vaccination rate, Last = 0 as no progression from "previously vaccinated group)
  dim(gamma_vaccine) <- N_vaccine

  # Interpolation of vaccination rate over time
  mv <- interpolate(tt_vaccine, max_vaccine, "constant")
  tt_vaccine[] <- user()
  max_vaccine[] <- user()
  dim(tt_vaccine) <- user()
  dim(max_vaccine) <- length(tt_vaccine)


  # Track the proportion who have received vaccine in each age group

  # Population size
  pop_size[] <- sum(S[i,])
  dim(pop_size) <- N_age
  # Proportion who have received vaccine
  pr[] <- 1 - (sum(S[i,1]) / pop_size[i])
  dim(pr) <- N_age

  # Isolate age groups below current target coverage which must be targeted
  vaccination_target_mat[,] <- if(pr[j] < vaccine_coverage_mat[i,j]) 1 else 0
  dim(vaccination_target_mat) <- c(N_prioritisation_steps, N_age)

  vaccine_target_vec[] <- if(sum(vaccination_target_mat[i,]) == 0) 1 else 0
  dim(vaccine_target_vec) <- N_prioritisation_steps
  current_index <- min(sum(vaccine_target_vec) + 1, N_prioritisation_steps)

  vaccination_target[] <- vaccination_target_mat[as.integer(current_index),i]
  dim(vaccination_target) <- N_age

  vr_temp[] <- S[i,1] * vaccination_target[i]
  dim(vr_temp) <- N_age
  # Catch so vaccination rate does not exceed 1 if the number of people available for vaccination < number of vaccines
  vr_den <- if(sum(vr_temp) <= mv) mv else sum(vr_temp)
  vr <- if(mv == 0) 0 else mv / vr_den  # Vaccination rate to achieve capacity given number in vaccine-eligible population
})
#updated version of dp plot to make it similar to cdp_plot
dp_plot_2 <- function (res, excess) {
  date_0 <- squire.page:::get_data_end_date.excess_nimue_simulation(res)
  data <- res$pmcmc_results$inputs$data
  #data$date <- squire.page:::get_dates_greater.excess_nimue_simulation(res)
  data$adjusted_deaths <- data$deaths/as.numeric(data$week_end -
                                                   data$week_start)
  suppressWarnings(dp <- plot(res, "deaths", date_0 = date_0,
                              x_var = "date", summary_f = median) + ggplot2::theme_bw() + ggplot2::theme(legend.position = "none",
                                                                                     axis.title.x = ggplot2::element_blank()) + ggplot2::ylab("Daily Deaths") +
                     ggplot2::scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") +
                     ggplot2::xlab(""))
  if(excess){
    dp + ggplot2::geom_segment(data = data,
                               ggplot2::aes(x = .data$week_start, xend = .data$week_end,
                                            y = .data$adjusted_deaths, yend = .data$adjusted_deaths),
                               linetype = "dashed")
  } else {
    dp + ggplot2::geom_point(data = data,
                             ggplot2::aes(x = .data$week_end, y = .data$adjusted_deaths),
                             size = 1)
  }
}
cdp_plot_2 <- function(res, extra_df = NULL) {

  date_0 <- squire.page:::get_data_end_date(res)

  #summarise deaths
  data <- squire.page:::get_data(res)
  data$date <- squire.page:::get_dates_greater(res) #assume reaches the cumulative sum at this
  #date works for both daily and weekly
  data$deaths <- cumsum(data$deaths)

  suppressWarnings(
    cdp <- plot(res, var_select = "D", date_0 = date_0, x_var = "date", summary_f = median) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = "none", axis.title.x = ggplot2::element_blank()) +
      ggplot2::ylab("Cumulative Deaths") +
      ggplot2::scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") +
      ggplot2::xlab("") +
      ggplot2::geom_line(
        data = data,
        ggplot2::aes(
          x = .data$date, y = .data$deaths
        ),
        linetype = "dashed"
      )
  )

  if(!is.null(extra_df)){
    cdp <- cdp +
      ggplot2::geom_line(
        data = extra_df,
        ggplot2::aes(
          x = .data$date, y = cumsum(.data$deaths)
        ),
        alpha = 0.5,
        colour = "green"
      )+
      ggplot2::geom_ribbon(
        data = extra_df,
        ggplot2::aes(
          x = .data$date, ymin = cumsum(.data$bot), ymax = cumsum(.data$top)
        ),
        alpha = 0.25,
        fill = "green"
      )
  }

  cdp
}
correct_vaccines <- function(fit){
  max_start_date <- fit$pmcmc_results$inputs$data$week_start[1] - 10
  max_vaccine <- fit$interventions$max_vaccine[-1][fit$interventions$date_vaccine_change < max_start_date]
  date_vaccs_before <- fit$interventions$date_vaccine_change[fit$interventions$date_vaccine_change < max_start_date]
  #correct the time series so that we don't over count for the earlier start dates
  fit$interventions$max_vaccine <- c(0, fit$interventions$max_vaccine[fit$interventions$date_vaccine_change >= max_start_date])
  fit$interventions$date_vaccine_change <- fit$interventions$date_vaccine_change[fit$interventions$date_vaccine_change >= max_start_date]
  #model vaccinations until latest start date
  tt_vaccine <- as.numeric(date_vaccs_before - max_start_date)
  temp_model <- vaccination_model$new(tt_vaccine = tt_vaccine, max_vaccine = max_vaccine,
                                      S_0 = fit$pmcmc_results$inputs$model_params$S_0,
                                      N_age = 17, N_vaccine = 6, gamma_vaccine = fit$pmcmc_results$inputs$model_params$gamma_vaccine,
                                      N_prioritisation_steps = nrow(fit$pmcmc_results$inputs$model_params$vaccine_coverage_mat),
                                      vaccine_coverage_mat = fit$pmcmc_results$inputs$model_params$vaccine_coverage_mat,
                                      use_dde = TRUE
  )
  output <- temp_model$run(c(0, -min(tt_vaccine)))[2,]
  #convert to a matrix
  new_S_0 <- matrix(NA, nrow = 17, ncol = 6)
  for(row_i in 1:nrow(new_S_0)){
    new_S_0[row_i,] <- output[stringr::str_detect(names(output), paste0("\\[", row_i, ","))]
  }
  new_S_0[new_S_0 < 0] <- 0
  fit$pmcmc_results$inputs$model_params$S_0 <- new_S_0
  fit
}
dir.create(here::here("data", "excess_mortality", "temp"))

purrr::walk(iso3cs, function(iso3c){
  message(paste0(which(iso3c == iso3cs), ":", iso3c))
  country <- countrycode::countrycode(iso3c, "iso3c", "country.name")
  fit <- readRDS(here::here("data", "excess_mortality", "model_fits", paste0(iso3c, ".Rds")))
  #modify in model params
  adjust_fit <- fit$interventions$date_vaccine_change[1] < max(fit$replicate_parameters$start_date)
  if(adjust_fit){
    fit <- correct_vaccines(fit)
    #plot
    if(iso3c == "FSM"){
      fit$replicate_parameters$start_date[49] <- "2021-08-12"
    } else if(iso3c == "NCL"){
      fit$replicate_parameters$start_date[c(49, 87, 88)] <-
        fit$replicate_parameters$start_date[c(49, 87, 88)] + 10
    } #We will refit these anyway!
    fit <- squire.page::generate_draws(fit, NULL, NULL)
    fit_1 <- dp_plot_2(fit, TRUE) + ggplot2::labs(
      title = country
    )
    fit_2 <- cdp_plot_2(fit) +
      ggplot2::ylab("Cumulative Daily Deaths")
    text <- ggpubr::text_grob(
      "Daily deaths are included with estimated and reported weekly excess mortality
 shown as dashed black lines. The red line represents median model estimates of
 daily deaths, and the shaded region represents the 95% quantiles of the
 estimated deaths. Plots of the cumulative estimated deaths are also shown."
    )
    plot <- ggpubr::ggarrange(
      fit_1,
      fit_2,
      text,
      ncol = 1,
      heights = c(1,1,0.4)
    )
    ggplot2::ggsave(here::here("data", "excess_mortality", "temp", paste0(iso3c, ".pdf")), plot)#save baseline
    baseline_deaths <- squire.page::nimue_format(fit, c("deaths", "infections"),
                                                 date_0 = max(fit$pmcmc_results$inputs$data$week_end),
                                                 reduce_age = FALSE) %>%
      dplyr::distinct() %>%
      tidyr::pivot_wider(names_from = .data$compartment, values_from = .data$y) %>%
      na.omit() %>%
      dplyr::transmute(replicate = replicate, date = date, deaths = deaths,
                       infections = infections, country = country, iso3c = iso3c)
    saveRDS(baseline_deaths, here::here("data", "excess_mortality", "counterfactual_data", paste0("Baseline_", iso3c, ".Rds")))
  }
})

#Not good fits so we'll have to refit them
iso3cs <- list.files(here::here("data", "excess_mortality", "temp")) %>%
  stringr::str_remove_all(".pdf")
#generate some seeds, will make it easier to debug and reproduce
set.seed(10001)
seeds <- purrr::map(iso3cs, ~runif(1, 0, 1e10))
dir.create(here::here("data", "excess_mortality", "cepi_fits"))

purrr::walk(iso3cs, function(iso3c){
  message(paste0(which(iso3c == iso3cs), ":", iso3c))
  country <- countrycode::countrycode(iso3c, "iso3c", "country.name")
  fit <- readRDS(here::here("data", "excess_mortality", "model_fits", paste0(iso3c, ".Rds")))
  #correct vaccines
  fit <- correct_vaccines(fit)
  #extact what we need
  model_params <- fit$pmcmc_results$inputs$model_params
  interventions <- fit$interventions
  pars_obs <- fit$pmcmc_results$inputs$pars_obs
  #initial values
  interventions$date_Rt_change <- seq(fit$pmcmc_results$inputs$data$week_start[1] - 10 + 14, as.Date("2021-12-08") - 7 - 14, by = 14)
  avg_params <- fit$replicate_parameters
  avg_params$start_date <- as.numeric(avg_params$start_date - fit$pmcmc_results$inputs$data$week_start[1])
  avg_params <- colMeans(avg_params)
  avg_params <- avg_params[seq_len(length(interventions$date_Rt_change) + 3)]
  #setup mcmc
  params <- tibble(
    name = names(avg_params),
    min = c(-55, 1, 0, rep(0, length(avg_params) - 3)),
    max = c(-10, 10, 1, rep(10, length(avg_params) - 3)),
    init = avg_params
  )
  n_burnin <- 100
  #move start date closer for no death small islands
  if(iso3c %in% c("FJI", "VUT")){
    params$min[1] <- -20
    params$init[1] <- -15
  } else if (iso3c == "MNG"){
    params$min[1] <- -15
    params$init[1] <- -11
    params$init[seq_along(params$init)[-c(1,2)]] <- 0.5
  } else if (iso3c %in% c("FSM", "MYS", "NCL")){
    params$min[1] <- -35
    params$init[1] <- -31
    params$max[1] <- -30
    params$init[seq_along(params$init)[-c(1,2)]] <- 0.5
    params$init[2] <- 1
    n_burnin <- 300
  }
  data <- list(df = fit$pmcmc_results$inputs$data)
  misc <- list(
    model_params = model_params,
    interventions = interventions,
    pars_obs = pars_obs,
    data_start_date = fit$pmcmc_results$inputs$data$week_start[1]
  )
  lprior <- function(params, misc){
    ret <- stats::dunif(x = params[["start_date"]], min = -55, max = -10, log = TRUE) +
      stats::dunif(x = params[["R0"]], min = 1, max = 10, log = TRUE) +
      stats::dnorm(x = params[["ves"]], mean = 0.5, sd = 0.1, log = TRUE)
    #changes for direct Rt estimation
    #assume that changes to Rt are penalised and should occur slowy
    if(any(grepl("Rt_", names(params)))) {
      Rts <- c(params[["R0"]], unlist(params[grepl("Rt_", names(params))]))
      ratio_R <- Rts[-1]/lag(Rts, 1)[-1]
      ret <- ret + sum(stats::df(x = ratio_R, 40, 40, log = TRUE))
    }
    return(ret)
  }
  llike <- function(params, data, misc){
    params <- as.list(params)
    params[["start_date"]] <- round(lubridate::as_date(misc$data_start_date + params[["start_date"]]))
    squire.page:::excess_log_likelihood_vaccine(
      params, data$df, nimue::nimue_deterministic_model(), misc$model_params,
      pars_obs = misc$pars_obs, 1, 0, "ll", list(), misc$interventions
    )$log_likelihood
  }
  set.seed(seeds[[iso3c]])
  cl <- parallel::makeCluster(min(3, parallel::detectCores() - 1))
  mcmc_output <- drjacoby::run_mcmc(
    data, params, misc, llike, lprior, n_burnin, 100, 25, chains = 3, alpha = 1.5,
    cluster = cl
  )
  #update model fit
  fit$pmcmc_results$inputs$interventions <- fit$interventions <- interventions
  its_from_chain <- diff(round(seq(0, 100, length.out = 4)))
  its_to_keep <- purrr::map(its_from_chain, ~round(seq(0, 100, length.out = .x)[-1]))
  fit$replicate_parameters <- mcmc_output$output %>%
    filter(phase == "sampling") %>%
    group_by(chain) %>%
    filter(iteration %in% (min(iteration) - 1 + its_to_keep[[chain[1]]])) %>%
    ungroup() %>%
    select(!c(chain, phase, iteration, logprior, loglikelihood)) %>%
    mutate(start_date = round(fit$pmcmc_results$inputs$data$week_start[1] + start_date))
  fit$pmcmc_results$inputs$pars_obs$rtol <-
    fit$pmcmc_results$inputs$pars_obs$atol <- 1e-6
  #save fit
  saveRDS(fit, here::here("data", "excess_mortality", "cepi_fits", paste0(iso3c, ".Rds")))
  #generate values
  fit <- squire.page::generate_draws(fit, NULL, NULL)
  fit_1 <- dp_plot_2(fit, TRUE) + ggplot2::labs(
    title = country
  )
  fit_2 <- cdp_plot_2(fit) +
    ggplot2::ylab("Cumulative Daily Deaths")
  text <- ggpubr::text_grob(
    "Daily deaths are included with estimated and reported weekly excess mortality
 shown as dashed black lines. The red line represents median model estimates of
 daily deaths, and the shaded region represents the 95% quantiles of the
 estimated deaths. Plots of the cumulative estimated deaths are also shown."
  )
  plot <- ggpubr::ggarrange(
    fit_1,
    fit_2,
    text,
    ncol = 1,
    heights = c(1,1,0.4)
  )
  #fitting_plot
  ggsave(here::here("data", "excess_mortality", "temp", paste0(iso3c, ".pdf")),
         plot)
  #extract baseline data
  baseline_deaths <- squire.page::nimue_format(fit, c("deaths", "infections"),
                                               date_0 = max(fit$pmcmc_results$inputs$data$week_end),
                                               reduce_age = FALSE) %>%
    dplyr::distinct() %>%
    tidyr::pivot_wider(names_from = .data$compartment, values_from = .data$y) %>%
    na.omit() %>%
    dplyr::transmute(replicate = replicate, date = date, deaths = deaths,
                     infections = infections, country = country, iso3c = iso3c)
  saveRDS(baseline_deaths, here::here("data", "excess_mortality", "counterfactual_data", paste0("Baseline_", iso3c, ".Rds")))
})
#redo all with median proper
