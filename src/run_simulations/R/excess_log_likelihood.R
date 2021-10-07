excess_log_likelihood <- function(pars, data, squire_model, model_params, pars_obs, n_particles,
                                  forecast_days = 0, return = "ll", Rt_args, interventions, ...) {
  switch(return, full = {
    save_particles <- TRUE
    full_output <- TRUE
    pf_return <- "sample"
  }, ll = {
    save_particles <- FALSE
    forecast_days <- 0
    full_output <- FALSE
    pf_return <- "single"
  }, {
    stop("Unknown return type to calc_loglikelihood")
  })
  squire:::assert_in(c("R0", "start_date"), names(pars), message = "Must specify R0, start date to infer")
  R0 <- pars[["R0"]]
  start_date <- pars[["start_date"]]
  squire:::assert_pos(R0)
  squire:::assert_date(start_date)
  date_Rt_change <- interventions$date_Rt_change
  date_contact_matrix_set_change <- interventions$date_contact_matrix_set_change
  date_ICU_bed_capacity_change <- interventions$date_ICU_bed_capacity_change
  date_hosp_bed_capacity_change <- interventions$date_hosp_bed_capacity_change
  date_vaccine_change <- interventions$date_vaccine_change
  date_vaccine_efficacy_infection_change <- interventions$date_vaccine_efficacy_infection_change
  date_vaccine_efficacy_disease_change <- interventions$date_vaccine_efficacy_disease_change
  if (is.null(date_Rt_change)) {
    tt_beta <- 0
  }
  else {
    tt_list <- squire:::intervention_dates_for_odin(dates = c(start_date, date_Rt_change),
                                                    change = rep(1, length(date_Rt_change) + 1), start_date = start_date, steps_per_day = round(1/model_params$dt),
                                                    starting_change = 1)
    model_params$tt_beta <- tt_list$tt
    date_Rt_change <- tt_list$dates
  }
  if (is.null(date_contact_matrix_set_change)) {
    tt_contact_matrix <- 0
  }
  else {
    tt_list <- squire:::intervention_dates_for_odin(dates = date_contact_matrix_set_change,
                                                    change = seq_along(interventions$contact_matrix_set)[-1],
                                                    start_date = start_date, steps_per_day = round(1/model_params$dt),
                                                    starting_change = 1)
    model_params$tt_matrix <- tt_list$tt
    model_params$mix_mat_set <- model_params$mix_mat_set[tt_list$change,, ]
  }
  if (is.null(date_ICU_bed_capacity_change)) {
    tt_ICU_beds <- 0
  }
  else {
    tt_list <- squire:::intervention_dates_for_odin(dates = date_ICU_bed_capacity_change,
                                                    change = interventions$ICU_bed_capacity[-1], start_date = start_date,
                                                    steps_per_day = round(1/model_params$dt), starting_change = interventions$ICU_bed_capacity[1])
    model_params$tt_ICU_beds <- tt_list$tt
    model_params$ICU_beds <- tt_list$change
  }
  if (is.null(date_hosp_bed_capacity_change)) {
    tt_hosp_beds <- 0
  }
  else {
    tt_list <- squire:::intervention_dates_for_odin(dates = date_hosp_bed_capacity_change,
                                                    change = interventions$hosp_bed_capacity[-1], start_date = start_date,
                                                    steps_per_day = round(1/model_params$dt), starting_change = interventions$hosp_bed_capacity[1])
    model_params$tt_hosp_beds <- tt_list$tt
    model_params$hosp_beds <- tt_list$change
  }
  if (is.null(date_vaccine_change)) {
    tt_vaccine <- 0
  }
  else {
    tt_list <- squire:::intervention_dates_for_odin(dates = date_vaccine_change,
                                                    change = interventions$max_vaccine[-1], start_date = start_date,
                                                    steps_per_day = round(1/model_params$dt), starting_change = interventions$max_vaccine[1])
    model_params$tt_vaccine <- tt_list$tt
    model_params$max_vaccine <- tt_list$change
  }
  if (is.null(date_vaccine_efficacy_infection_change)) {
    tt_vaccine_efficacy_infection <- 0
  }
  else {
    tt_list <- squire:::intervention_dates_for_odin(dates = date_vaccine_efficacy_infection_change,
                                                    change = seq_along(interventions$vaccine_efficacy_infection)[-1],
                                                    start_date = start_date, steps_per_day = round(1/model_params$dt),
                                                    starting_change = 1)
    model_params$tt_vaccine_efficacy_infection <- tt_list$tt
    model_params$vaccine_efficacy_infection <- model_params$vaccine_efficacy_infection[tt_list$change,
                                                                                       , ]
  }
  if (is.null(date_vaccine_efficacy_disease_change)) {
    tt_vaccine_efficacy_disease <- 0
  }
  else {
    tt_list <- squire:::intervention_dates_for_odin(dates = date_vaccine_efficacy_disease_change,
                                                    change = seq_along(interventions$vaccine_efficacy_disease)[-1],
                                                    start_date = start_date, steps_per_day = round(1/model_params$dt),
                                                    starting_change = 1)
    model_params$tt_vaccine_efficacy_disease <- tt_list$tt
    model_params$prob_hosp <- model_params$prob_hosp[tt_list$change,
                                                     , ]
  }
  #calculate R0 using new function
  R0 <- evaluate_Rt_pmcmc_custom(R0 = R0, date_Rt_change = date_Rt_change,
                                 pars = pars, Rt_args = Rt_args)
  beta_set <- squire:::beta_est(squire_model = squire_model, model_params = model_params,
                                R0 = R0)
  model_params$beta_set <- beta_set
  if (inherits(squire_model, "stochastic")) {
    pf_result <- squire:::run_particle_filter(data = data, squire_model = squire_model,
                                              model_params = model_params, model_start_date = start_date,
                                              obs_params = pars_obs, n_particles = n_particles,
                                              forecast_days = forecast_days, save_particles = save_particles,
                                              full_output = full_output, return = pf_return)
  }
  else if (inherits(squire_model, "deterministic")) {
    pf_result <- run_deterministic_comparison_excess(data = data,
                                                     squire_model = squire_model, model_params = model_params,
                                                     model_start_date = start_date, obs_params = pars_obs,
                                                     forecast_days = forecast_days, save_history = save_particles,
                                                     return = pf_return)
  }
  pf_result

}



run_deterministic_comparison_excess <- function(data, squire_model, model_params, model_start_date = "2020-02-02",
                                                obs_params = list(
                                                  phi_cases = 0.1,
                                                  k_cases = 2,
                                                  phi_death = 1,
                                                  k_death = 2,
                                                  exp_noise = 1e02
                                                ), forecast_days = 0, save_history = FALSE,
                                                return = "ll") {

  if (!(return %in% c("full", "ll", "sample", "single"))) {
    stop("return argument must be full, ll, sample", "single")
  }
  if (as.Date(data$week_start[data$deaths > 0][1], "%Y-%m-%d") <
      as.Date(model_start_date, "%Y-%m-%d")) {
    stop("Model start date is later than data start date")
  }

  #set up to use our weekly data instead of perday
  model_params$tt_beta <- round(model_params$tt_beta * model_params$dt)
  model_params$tt_contact_matrix <- round(model_params$tt_contact_matrix *
                                            model_params$dt)
  model_params$tt_hosp_beds <- round(model_params$tt_hosp_beds *
                                       model_params$dt)
  model_params$tt_ICU_beds <- round(model_params$tt_ICU_beds *
                                      model_params$dt)
  #convert weeks into days relevant to our start_date
  data$date <- data$week_start
  data <- squire:::particle_filter_data(data = data, start_date = model_start_date,
                                        steps_per_day = round(1/model_params$dt))
  data$week_start <- data$day_start
  data$week_end <- data$day_end

  #set the last day to the same distance as the previous one
  data$week_end[nrow(data)] <- data$week_start[nrow(data)] +
    data$week_end[nrow(data)-1]  - data$week_start[nrow(data)-1]

  # here we change the dur_R for 60 days from delta_start_date
  if("dur_R" %in% names(obs_params)) {
    if(obs_params$dur_R != 365) {
      ch_dur_R <- as.integer(as.Date(obs_params$delta_start_date) - model_start_date)
      model_params$tt_dur_R <- c(0, ch_dur_R, ch_dur_R+obs_params$shift_duration)
      model_params$gamma_R <- c(model_params$gamma_R, 2/obs_params$dur_R, model_params$gamma_R)
    }
  }

  # here we change the prob_hosp as needed
  if("prob_hosp_multiplier" %in% names(obs_params)) {
    if(obs_params$prob_hosp_multiplier != 1) {
      ch_dur_R <- as.integer(as.Date(obs_params$delta_start_date) - model_start_date)
      model_params$tt_prob_hosp_multiplier <- c(0, seq(ch_dur_R, ch_dur_R + obs_params$shift_duration, by = 1))
      model_params$prob_hosp_multiplier <- seq(model_params$prob_hosp_multiplier,
                                               obs_params$prob_hosp_multiplier,
                                               length.out = length(model_params$tt_prob_hosp_multiplier))
    }
  }

  # run model
  model_func <- squire_model$odin_model(user = model_params,
                                        unused_user_action = "ignore")
  out <- model_func$run(t = seq(0, tail(data$week_end, 1), 1), atol = 1e-8, rtol = 1e-8, step_size_min_allow = TRUE)
  index <- squire:::odin_index(model_func)

  #calculate the deaths for each week
  cumDs <- rowSums(out[, index$D])
  Ds <- cumDs[data$week_end[-1]] - cumDs[data$week_start[-1]]
  Ds[Ds < 0] <- 0
  deaths <- data$deaths[-1]

  ll <- squire:::ll_nbinom(deaths, Ds, obs_params$phi_death, obs_params$k_death,
                           obs_params$exp_noise)

  # and wrap up as normal
  date <- data$date[[1]] + seq_len(nrow(out)) - 1L
  rownames(out) <- as.character(date)
  attr(out, "date") <- date
  pf_results <- list()
  pf_results$log_likelihood <- sum(ll)
  if (save_history) {
    pf_results$states <- out
  }
  else if (return == "single") {
    pf_results$sample_state <- out[nrow(out), ]
  }
  if (return == "ll") {
    ret <- pf_results$log_likelihood
  }
  else if (return == "sample") {
    ret <- pf_results$states
  }
  else if (return == "single" || return == "full") {
    ret <- pf_results
  }
  ret
}

evaluate_Rt_pmcmc_custom <- function(R0, date_Rt_change,
                                     pars, Rt_args){
  #first just double check all dates are set up correctly, other than the first
  #date which should be the start_date, then the difference between all dates should
  #Rt_rw_duration
  if(any(diff(date_Rt_change[-1]) != Rt_args$Rt_rw_duration)){
    stop("Incorrect Rt change times, please check R0/Rt code")
  }
  #now calculate the values
  Rt <- as.numeric(c(R0, R0*2*plogis(cumsum(-unlist(pars[grepl("Rt_rw", names(pars))])))))
  return(Rt)
}
