if(!is.na(seed) & seed != "NA"){
  set.seed(seed)
}

#run both in the same task
variable_parameters <- list(
  India_type = list(
    data = tibble(
      deaths = round(
        c(14506, 50601, 62041, 83553, 110407, 137149, 133691, 98801, 82238, 81455, 64927, 63541, 65007, 63094, 55199, 34818, 36804, 49677, 51421,
          64591, 163419, 271734, 600103, 624699, 407582, 140985, 126373, 129763, 136651, 118683, 101090, 92772, 94338, 94087, 90210, 81653, 76853, 43591)/10
      )
    ) %>%
      mutate(
        week_start = as_date("2021-12-01") - 7 * rev(seq_along(deaths)),
        week_end = week_start + 7
      ),
    population = c(
      116879507, 117982127, 126155952, 126045566, 122504804, 117397269, 112176098, 103460178, 90219894, 79440280, 68875962, 59256268,
      48890528, 38260283, 24091443, 15083955, 13284271
    ),
    # contact matrix
    baseline_contact_matrix = squire::get_mixing_matrix(iso3c = "IND"),
    pars_init = list(
      start_date = "2021-01-19",
      R0 = 4.56,
      ves = 0.5
    ),
    pars_obs = list(delta_start_date = as_date("2021-07-01"))
  ),
  USA_type = list(
    data = tibble(
      deaths = c(
        118, 378, 813, 935, 656, 331, 173, 88, 46, 24, 19, 33, 44, 40, 38, 62,
        114, 191, 263, 305, 348, 473, 659, 737, 606, 368, 183, 82, 36, 23, 24,
        21, 18, 43, 87, 148, 225, 298, 388, 467, 387, 309, 259, 218, 164
      )
    ) %>%
      mutate(
        week_start = as_date("2021-12-01") - 7 * rev(seq_along(deaths)),
        week_end = week_start + 7
      ),
    # large to ensure we don't use all susceptibles
    population = c(
      19676332, 20045152, 21089487, 21242908, 22258745, 23835330, 23052479,
      21615791, 20294599, 20053798, 20577807, 21542270, 20669143, 17819027,
      14354863, 9727734, 13147182
    ),
    # contact matrix
    baseline_contact_matrix = squire::get_mixing_matrix(iso3c = "USA"),
    pars_init = list(
      start_date = "2020-12-25",
      R0 = 6.04,
      ves = 0.5
    ),
    pars_obs = list(delta_start_date = as_date("2021-05-01"))
  )
)

#assign inits
#USA
#manually tuned to fit the middle case
pars_init_rw <- as.list(
  c(1.96257795597481, 0.572904547985007, 0.405516217240309,
    0.421488215063381, 1.08091911674942, 1.5445677211345, 0.64206572768861,
    2.09765149598958, 1.3415560778738, 1.12093887365526, 1.39000612311024,
    0.83519211774763, 0.32129168240854, 0.438065693805752, 1.04219226951067,
    1.29459029147726, 1.71356086399789, 1.49148193458791, 1.29459029147726,
    0.83519211774763, 0.83519211774763, 0.804625069747095)
)
names(pars_init_rw) <- paste0("Rt_", seq_along(pars_init_rw))
variable_parameters$USA_type$pars_init <- append(
  variable_parameters$USA_type$pars_init, pars_init_rw
)
#India
pars_init_rw <- as.list(
  c(1.74689357888058, 1.48168137073595, 1.20738652670364,
    0.909724460771528, 1.43271067623533, 1.24990778828819, 1.16609748483002,
    1.53199232757861, 1.58365742081333, 3.0667030796723, 3.74795124138497,
    1.58365742081333, 2.59881599600697, 4.92402374714676, 3.48613938210445,
    5.37204875861503, 4.92402374714676, 4.28672784790711)
)
names(pars_init_rw) <- paste0("Rt_", seq_along(pars_init_rw))
variable_parameters$India_type$pars_init <- append(
  variable_parameters$India_type$pars_init, pars_init_rw
)

results_df <- map(seq_along(variable_parameters), function(x){
  pmcmc_pars_list <- variable_parameters[[x]]
  # healthcare
  # pmcmc_pars_list$baseline_hosp_bed_capacity <- sum(pmcmc_pars_list$population) / 1000
  # pmcmc_pars_list$baseline_ICU_bed_capacity <- sum(pmcmc_pars_list$population) / 10000

  ## Vaccine Parameters
  pmcmc_pars_list <- append(
    pmcmc_pars_list,
    list(
      # similar to standard but we don't bother with double dose scaling
      baseline_vaccine_efficacy_infection = 0,
      baseline_vaccine_efficacy_disease = 0,
      dur_V = 365 * 1.5,
      vaccine_coverage_mat = purrr::map_df(.x = 1:15, .f = function(x) {
        out <- rep(0, 17)
        out[17 - 1:x + 1] <- 0.8
        names(out) <- paste0("age_group", 1:17)
        out
      }) %>% as.matrix(),
      baseline_max_vaccine = 0,
      vaccine_efficacies = list(
        ve_i_low = c(0.5, 0.6, 0.7),
        ve_i_high = c(0.5, 0.6, 0.7),
        ve_i_low_d = c(0.5, 0.6, 0.7),
        ve_i_high_d = c(0.5, 0.6, 0.7),
        ve_d_low = c(0.8, 0.9, 0.95),
        ve_d_high = c(0.8, 0.9, 0.95),
        ve_d_low_d = c(0.8, 0.9, 0.95),
        ve_d_high_d = c(0.8, 0.9, 0.95)
      ),
      dose_ratio = 0,
      vaccine_efficacy_infection = NULL,
      vaccine_efficacy_disease = NULL,
      date_vaccine_efficacy_infection_change = NULL,
      date_vaccine_efficacy_disease_change = NULL
    )
  )
  # regimen, starts in at first date should hit max by dec-2021
  pmcmc_pars_list$date_vaccine_change <- min(pmcmc_pars_list$data$week_start)
  pmcmc_pars_list$date_vaccine_efficacy <- min(pmcmc_pars_list$data$week_start)
  pmcmc_pars_list$max_vaccine <- sum(
    pmcmc_pars_list$population * tail(pmcmc_pars_list$vaccine_coverage_mat, 1)
  ) /
    as.numeric(as_date("2021-12-01") - pmcmc_pars_list$date_vaccine_change)
  ## COVID parameters
  pmcmc_pars_list$dur_R <- 365 # nolint
  # rest as defaults
  ## MCMC Options
  pmcmc_pars_list <- append(
    pmcmc_pars_list,
    list(
      # intial values etc simpler if we use the same already
      n_mcmc = 10000,
      squire_model = nimue::nimue_deterministic_model(),
      log_likelihood = excess_log_likelihood_vaccine,
      log_prior = function(pars) {
        0
      },
      n_particles = 1,
      steps_per_day = 1,
      n_chains = 1,
      scaling_factor = 1,
      pars_min = list(),
      pars_max = list(),
      pars_discrete = list(),
      proposal_kernel = NULL,
      Rt_args = list(
        Rt_date_spline_start = min(pmcmc_pars_list$data$week_start),
        Rt_rw_duration = 14
      ),
      burnin = 0,
      replicates = 50
    )
  )
  pmcmc_pars_list$pars_obs <- append(
    pmcmc_pars_list$pars_obs,
    list(
      phi_cases = 1, k_cases = 2, phi_death = 1, k_death = 7, exp_noise = 1e07,
      k_death_cumulative = 40,
      likelihood = function(model_deaths, data_deaths, par_obs) {
        phi_death <- 1
        k_death <- 7
        exp_noise <- 1e07
        k_death_cumulative <- 40
        # also add a term for cumulative deaths
        c(
          squire:::ll_nbinom(
            data_deaths, model_deaths, phi_death,
            k_death,
            exp_noise
          ),
          squire:::ll_nbinom(
            sum(data_deaths), sum(model_deaths), phi_death,
            k_death_cumulative,
            exp_noise
          )
        )
      },
      ## Delta Characteristics
      #add a shift date for ve stuff but no immune escape or hosp changes
      delta_dur_R = pmcmc_pars_list$dur_R,
      prob_hosp_multiplier = 1,
      shift_duration = 60
    )
  )
  # assign initials
  # start date
  pmcmc_pars_list$pars_min$start_date <- as_date(pmcmc_pars_list$pars_init$start_date) - 10
  pmcmc_pars_list$pars_max$start_date <- as_date(pmcmc_pars_list$pars_init$start_date) + 10
  pmcmc_pars_list$pars_discrete$start_date <- TRUE
  # R0
  pmcmc_pars_list$pars_min$R0 <- 3
  pmcmc_pars_list$pars_max$R0 <- 9
  pmcmc_pars_list$pars_discrete$R0 <- FALSE
  # ves
  pmcmc_pars_list$pars_min$ves <- 0
  pmcmc_pars_list$pars_max$ves <- 1
  pmcmc_pars_list$pars_discrete$ves <- FALSE
  # fitting parameters
  # how many needed
  n_rt_rw <- length(pmcmc_pars_list$pars_init) - 3
  pmcmc_pars_list$Rt_args$date_Rt_change <- pmcmc_pars_list$Rt_args$Rt_date_spline_start +
    pmcmc_pars_list$Rt_args$Rt_rw_duration * seq(0, n_rt_rw - 1)
  pmcmc_pars_list$date_Rt_change <- pmcmc_pars_list$Rt_args$date_Rt_change
  pars_min_rw <- as.list(rep(0, n_rt_rw))
  pars_max_rw <- as.list(rep(10, n_rt_rw))
  pars_discrete_rw <- as.list(rep(FALSE, n_rt_rw))
  names(pars_min_rw) <- names(pars_max_rw) <-
    names(pars_discrete_rw) <- paste0("Rt_", seq_len(n_rt_rw))
  pmcmc_pars_list$pars_min <- append(pmcmc_pars_list$pars_min, pars_min_rw)
  pmcmc_pars_list$pars_max <- append(pmcmc_pars_list$pars_max, pars_max_rw)
  pmcmc_pars_list$pars_discrete <- append(pmcmc_pars_list$pars_discrete, pars_discrete_rw)
  rm(pars_min_rw, pars_max_rw, pars_discrete_rw)
  # proposal covariance
  pmcmc_pars_list$proposal_kernel <- matrix(1,
                                            nrow = length(pmcmc_pars_list$pars_init),
                                            ncol = length(pmcmc_pars_list$pars_init)
  )
  colnames(pmcmc_pars_list$proposal_kernel) <- names(pmcmc_pars_list$pars_init)
  rownames(pmcmc_pars_list$proposal_kernel) <- names(pmcmc_pars_list$pars_init)
  #ifr to iterate over
  iterations <- seq(0, 1, length.out = 100)
  #assume prob hosp and prob severe remain the same then scale prob death to match
  prob_hosp <- squire:::default_probs()$prob_hosp
  prob_severe <- squire:::default_probs()$prob_severe
  prop_deaths_in_ICU <- (squire::default_probs()$prob_severe_death_treatment * prob_severe)/
    ((squire::default_probs()$prob_severe_death_treatment * prob_severe) + (squire::default_probs()$prob_non_severe_death_treatment * (1- prob_severe)))
  #note not sure how to get last value, just assume its usa?#
  # round(prob_hosp * (prob_severe * squire::default_probs()$prob_severe_death_treatment +
  #                (1-prob_severe) * squire::default_probs()$prob_non_severe_death_treatment)*100, 4)
  # weighted.mean(c(5.3,8.28,16.69),
  #               squire::get_elderly_population("Malta")$n)
  # squire::get_population(country = "Nicaragua")
  rep_34_ranges <- tibble(
    Age = factor(seq_len(17), levels = seq_len(17), labels = c(paste0(seq(0,15) * 5, "-",seq(0,15) * 5 + 4), "80+")),
    ifr = prob_hosp * (prob_severe * squire::default_probs()$prob_severe_death_treatment +
                         (1-prob_severe) * squire::default_probs()$prob_non_severe_death_treatment
    ),
    low = c(0, 0, 0, 0, 0, 0, 0.01, 0.01, 0.02, 0.03, 0.05, 0.10, 0.18, 0.35, 0.65, 1.21,
            weighted.mean(c(2.23, 4.06, 9.7), squire::get_elderly_population("United States")$n))/100,
    high = c(0.03, 0.06, 0.11, 0.18, 0.3, 0.46, 0.71, 1.03, 1.47, 2.03, 2.74, 3.64, 4.79, 6.27, 8.21, 10.81,
             weighted.mean(c(14.37, 19.36, 31.12), squire::get_elderly_population("United States")$n))/100
  )
  # prob_deaths <- map(iterations, function(x){
  #   if(x <= 1/2){
  #     x <- x*2
  #     new_ifr <- rep_34_ranges %>%
  #       mutate(temp = low * (1 - x) + ifr * x) %>%
  #       pull(temp)
  #   } else {
  #     x <- x*2 - 1
  #     new_ifr <- rep_34_ranges %>%
  #       mutate(temp = ifr * (1 - x) + high * x) %>%
  #       pull(temp)
  #   }
  #   #calculate the prob of deaths
  #   prob_severe_death_treatment <- new_ifr * prop_deaths_in_ICU / (
  #     prob_hosp * prob_severe
  #   )
  #   prob_non_severe_death_treatment <- (1 - prop_deaths_in_ICU)/prop_deaths_in_ICU *
  #     prob_severe/(1-prob_severe) * prob_severe_death_treatment
  #   #if ifr too great then we cannot fit with these prob_hosp/prob_severe values
  #   if(any(prob_severe_death_treatment > 1)){
  #     prob_severe_death_treatment
  #   }
  #   list(
  #     prob_severe_death_treatment = prob_severe_death_treatment,
  #     prob_non_severe_death_treatment = prob_non_severe_death_treatment
  #   )
  # })
  #just scale prob_hosp its easier
  prob_hosps <- map(iterations, function(x){
    if(x <= 1/2){
      x <- x*2
      new_ifr <- rep_34_ranges %>%
        mutate(temp = low * (1 - x) + ifr * x) %>%
        pull(temp)
    } else {
      x <- x*2 - 1
      new_ifr <- rep_34_ranges %>%
        mutate(temp = ifr * (1 - x) + high * x) %>%
        pull(temp)
    }
    list(
      prob_hosp = new_ifr/(
        prob_severe * squire::default_probs()$prob_severe_death_treatment +
          (1 - prob_severe) * squire::default_probs()$prob_non_severe_death_treatment),
      iteration = x
    )
  })


  ## Run fitting + calculate deaths averted
  #generate plot so we can check quality
  create_plot <- function(model_fit, iteration){
    dp_plot(model_fit) +
      labs(
        title =
          paste0(
            "Iteration:", iteration
          )
      )
  }

  #use dr jacoby
  pmcmc_pars_list$n_mcmc <- round(pmcmc_pars_list$n_mcmc/(10*length(pmcmc_pars_list$pars_init)))
  pmcmc_pars_list$drjacoby_list <- list(
    rungs = 10,
    alpha = 2.5
  )
  pmcmc_pars_list$use_drjacoby <- TRUE
  pmcmc_pars_list$burnin <- round(pmcmc_pars_list$n_mcmc)/2
  pmcmc_pars_list$n_mcmc <- round(pmcmc_pars_list$n_mcmc)/2
  model_fits <- map(
    .x = prob_hosps,
    .f = function(prob_hosp_list, pmcmc_pars) {
      prob_hosp <- prob_hosp_list$prob_hosp
      iteration <- prob_hosp_list$iteration
      pmcmc_pars$prob_hosp <- prob_hosp
      # fit to data
      model_fit <- exec(
        pmcmc_excess,
        !!!pmcmc_pars
      )
      # assign class because I didn't put that in pmcmc_excess for some reason?
      class(model_fit) <- c("vacc_durR_nimue_simulation", "excess_nimue_simulation", "nimue_simulation")
      #also assign prior since that's also missing
      model_fit$pmcmc_results$inputs$prior <- function(pars) {
        0
      }
      plot <- create_plot(model_fit, iteration)
      #generate parameter draws
      pars_list <- squire.page::generate_parameters(model_fit, draws = 50)
      #get baseline deaths
      baseline_deaths <- squire.page::nimue_format(
        squire.page::generate_draws(model_fit, pars_list),
        var_select = "deaths"
      )
      #reff
      model_fit$parameters$country <- "United States"
      #generate counterfactual deaths
      model_fit$interventions$max_vaccine <- c(0, 0)
      model_fit$odin_parameters$max_vaccine <- c(0, 0)
      model_fit$parameters$max_vaccine <- c(0, 0)
      model_fit$pmcmc_results$inputs$interventions$max_vaccine <- c(0, 0)
      model_fit$pmcmc_results$inputs$model_params$max_vaccine <- c(0, 0)
      counterfactual_deaths <- squire.page::nimue_format(
        squire.page::generate_draws(model_fit, pars_list),
        var_select = "deaths"
      )
      #merge and calculate total deaths averted
      deaths_averted <- baseline_deaths %>%
        group_by(replicate) %>%
        summarise(baseline_deaths = sum(y, na.rm = TRUE)) %>%
        left_join(counterfactual_deaths %>%
                    group_by(replicate) %>%
                    summarise(deaths = sum(y, na.rm = TRUE))
        ) %>%
        mutate(deaths_averted = deaths - baseline_deaths) %>%
        summarise(
          deaths_averted_median = median(deaths_averted),
          deaths_averted_025 = quantile(deaths_averted, 0.025),
          deaths_averted_975 = quantile(deaths_averted, 0.975)
        ) %>%
        mutate(
          iteration = iteration
        )
      return(list(
        plot = plot,
        result = deaths_averted
      ))
    },
    pmcmc_pars = pmcmc_pars_list
  )
  #pdf of plots
  pdf(paste0("fitting_plot_", x,".pdf"))
  map(model_fits, ~print(.x$plot))
  dev.off()
  #save results
  results_df <- do.call(
    rbind,
    lapply(model_fits, function(x){x$result})
  )
  #return both
  results_df
})

curve_plots <- map(seq_along(results_df), function(x) {
  data_obj <- list(
    data = variable_parameters[[x]]$data,
    pop = variable_parameters[[x]]$population,
    colour = c("purple", "red")[x],
    delta_start_date = variable_parameters[[x]]$pars_obs$delta_start_date
  )
  curve_plot <- ggplot(data_obj$data, aes(week_start, (deaths/sum(data_obj$pop))*1e5)) +
    geomtextpath::geom_textvline(label = "Delta Introduction",
                                 xintercept = unique(data_obj$delta_start_date),
                                 hjust = 0.2,
                                 linetype = 2) +
    geom_step(color = data_obj$colour) +
    theme_bw() + ylab("Weekly Deaths per 100,000\n") + xlab("")  +
    ggpubr::theme_pubr()
  curve_plot
})
names(curve_plots) <- names(variable_parameters)
names(results_df) <- names(results_df)

saveRDS(curve_plots, "death_curves.Rds")
saveRDS(results_df, "res.Rds")
