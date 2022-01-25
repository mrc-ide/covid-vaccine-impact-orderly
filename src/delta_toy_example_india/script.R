if(!is.na(seed)){
  set.seed(seed)
}


## Generate Data
pmcmc_pars_list <- list(
  # three peaks, large, small, delta
  data = tibble(
    deaths = c(14506, 50601, 62041, 83553, 110407, 137149, 133691, 98801, 82238, 81455, 64927, 63541, 65007, 63094, 55199, 34818, 36804, 49677, 51421,
               64591, 163419, 271734, 600103, 624699, 407582, 140985, 126373, 129763, 136651, 118683, 101090, 92772, 94338, 94087, 90210, 81653, 76853, 43591)
  ) %>%
    mutate(
      week_start = as_date("2021-12-01") - 7 * rev(seq_along(deaths)),
      week_end = week_start + 7
    )
)
## Country Parameters
pmcmc_pars_list <- append(
  pmcmc_pars_list,
  list(
    population = c(
      116879507, 117982127, 126155952, 126045566, 122504804, 117397269, 112176098, 103460178, 90219894, 79440280, 68875962, 59256268,
      48890528, 38260283, 24091443, 15083955, 13284271
    ),
    # contact matrix
    baseline_contact_matrix = squire::get_mixing_matrix(iso3c = "IND")
  )
)
# healthcare
pmcmc_pars_list$baseline_hosp_bed_capacity <- sum(pmcmc_pars_list$populations) / 1000
pmcmc_pars_list$baseline_ICU_bed_capacity <- sum(pmcmc_pars_list$populations) / 10000

## Vaccine Parameters
pmcmc_pars_list <- append(
  pmcmc_pars_list,
  list(
    # similar to standard but we don't bother with double dose scaling
    baseline_vaccine_efficacy_infection = 0.6,
    baseline_vaccine_efficacy_disease = 0.9,
    dur_V = 365 * 1.5,
    vaccine_coverage_mat = purrr::map_df(.x = 1:15, .f = function(x) {
      out <- rep(0, 17)
      out[17 - 1:x + 1] <- 0.8
      names(out) <- paste0("age_group", 1:17)
      out
    }) %>% as.matrix(),
    baseline_max_vaccine = 0
  )
)
# regimen, starts in Jan 2021 should hit max by dec-2021
pmcmc_pars_list$date_vaccine_change <- as_date("2021-01-01")
pmcmc_pars_list$max_vaccine <- sum(
  pmcmc_pars_list$population * tail(pmcmc_pars_list$vaccine_coverage_mat, 1)
) /
  as.numeric(as_date("2021-12-01") - pmcmc_pars_list$date_vaccine_change)
## COVID parameters
pmcmc_pars_list$dur_R <- 365
# rest as defaults
## MCMC Options
pmcmc_pars_list <- append(
  pmcmc_pars_list,
  list(
    # intial values etc simpler if we use the same already
    n_mcmc = 10000,
    squire_model = nimue::nimue_deterministic_model(),
    log_likelihood = excess_log_likelihood,
    log_prior = function(pars) {
      0
    },
    n_particles = 1,
    steps_per_day = 1,
    n_chains = 1,
    scaling_factor = 1,
    pars_init = list(),
    pars_min = list(),
    pars_max = list(),
    pars_discrete = list(),
    proposal_kernel = NULL,
    pars_obs = list(
      phi_cases = 1, k_cases = 2, phi_death = 1, k_death = 7, exp_noise = 1e07,
      k_death_cumulative = 40,
      likelihood = function(model_deaths, data_deaths) {
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
      }
    ),
    Rt_args = list(
      Rt_date_spline_start = min(pmcmc_pars_list$data$week_start),
      Rt_rw_duration = 14
    ),
    burnin = 0,
    replicates = 25
  )
)
# assign initials
# start date
pmcmc_pars_list$pars_init$start_date <- min(pmcmc_pars_list$data$week_start) - 50
pmcmc_pars_list$pars_min$start_date <- min(pmcmc_pars_list$data$week_start) - 50 - 10
pmcmc_pars_list$pars_max$start_date <- min(pmcmc_pars_list$data$week_start) - 50 + 10
pmcmc_pars_list$pars_discrete$start_date <- TRUE
# R0
pmcmc_pars_list$pars_init$R0 <- 4.56
pmcmc_pars_list$pars_min$R0 <- 3
pmcmc_pars_list$pars_max$R0 <- 5
pmcmc_pars_list$pars_discrete$R0 <- FALSE
# fitting parameters
# how many needed
pmcmc_pars_list$Rt_args$date_Rt_change <- seq(
  pmcmc_pars_list$Rt_args$Rt_date_spline_start,
  max(pmcmc_pars_list$data$week_end) - 21,
  by = pmcmc_pars_list$Rt_args$Rt_rw_duration
)
pmcmc_pars_list$date_Rt_change <- pmcmc_pars_list$Rt_args$date_Rt_change
#manually tuned to fit the no adjustments case
pars_init_rw <- as.list(
  c(1.44, 0.2, 0.24, 0.32, -0.52, 0.16, 0.08, -0.32, -0.04,
    -0.88, -0.32, 1.2, -0.64, -1.08, 0.64, -0.84, 0.2, 0.28)
)
pars_min_rw <- as.list(rep(-2, length(pmcmc_pars_list$Rt_args$date_Rt_change)))
pars_max_rw <- as.list(rep(2, length(pmcmc_pars_list$Rt_args$date_Rt_change)))
pars_discrete_rw <- as.list(rep(FALSE, length(pmcmc_pars_list$Rt_args$date_Rt_change)))
names(pars_init_rw) <- names(pars_min_rw) <- names(pars_max_rw) <-
  names(pars_discrete_rw) <- paste0("Rt_rw_", seq_along(pmcmc_pars_list$Rt_args$date_Rt_change))
pmcmc_pars_list$pars_init <- append(pmcmc_pars_list$pars_init, pars_init_rw)
pmcmc_pars_list$pars_min <- append(pmcmc_pars_list$pars_min, pars_min_rw)
pmcmc_pars_list$pars_max <- append(pmcmc_pars_list$pars_max, pars_max_rw)
pmcmc_pars_list$pars_discrete <- append(pmcmc_pars_list$pars_discrete, pars_discrete_rw)
rm(pars_init_rw, pars_min_rw, pars_max_rw, pars_discrete_rw)
# proposal covariance
pmcmc_pars_list$proposal_kernel <- matrix(1,
                                          nrow = length(pmcmc_pars_list$pars_init),
                                          ncol = length(pmcmc_pars_list$pars_init)
)
colnames(pmcmc_pars_list$proposal_kernel) <- names(pmcmc_pars_list$pars_init)
rownames(pmcmc_pars_list$proposal_kernel) <- names(pmcmc_pars_list$pars_init)
## Delta Characteristics, will do multiple just one for now
delta_characteristics <- tribble(
  ~start_date, ~immune_escape, ~hosp_modifier, ~delta_shift_dur,
  as_date("2021-07-01"), 0, 1, 60,
  as_date("2021-07-01"), 0.10, 1, 60,
  as_date("2021-07-01"), 0.20, 1, 60,
  as_date("2021-07-01"), 0.30, 1, 60,
  as_date("2021-07-01"), 0.40, 1, 60,
  as_date("2021-07-01"), 0.50, 1, 60,
  as_date("2021-07-01"), 0.60, 1, 60,
  as_date("2021-07-01"), 0.70, 1, 60,
  as_date("2021-07-01"), 0.80, 1, 60
) %>%
  mutate(
    # calculate dur_R shift
    dur_R_shift = 1 / (
      (delta_shift_dur / pmcmc_pars_list$dur_R - log(1 - immune_escape)) /
        delta_shift_dur
    )
  )

#delta_characteristics <- delta_characteristics[c(1, nrow(delta_characteristics)),]
## Run fitting + calculate deaths averted
create_plot <- function(model_fit, start_date, immune_escape, hosp_modifier, delta_shift_dur){
  dp_plot(model_fit) +
    labs(
      title =
        paste0(
          "Start Date:", start_date,
          " Immune Escape:", immune_escape,
          " Hospitalisation Modifier:", hosp_modifier,
          " Shift Duration:", delta_shift_dur
        )
    )
}
calculate_reff <- function(out){
  ratios <- squire.page::get_immunity_ratios_vaccine(out)
  reff <- get_Rt(out) %>% dplyr::group_by(rep) %>% dplyr::mutate(ratios = ratios[[unique(.data$rep)]][seq_along(.data$Rt)],
                                                                 Reff = .data$Rt * .data$ratios) %>%
    dplyr::ungroup()
  return(reff)
}
model_fits <- pmap(
  .l = delta_characteristics,
  pmcmc_pars_list,
  .f = function(start_date, immune_escape, dur_R_shift, hosp_modifier,
                delta_shift_dur, pmcmc_pars_list) {
    # add the delta adjustments to pars_obs
    pmcmc_pars_list$pars_obs$dur_R <- dur_R_shift
    pmcmc_pars_list$pars_obs$prob_hosp_multiplier <- hosp_modifier
    pmcmc_pars_list$pars_obs$delta_start_date <- start_date
    pmcmc_pars_list$pars_obs$shift_duration <- delta_shift_dur
    #
    # pmcmc_pars_list$pars_obs$dur_R <- delta_characteristics$dur_R_shift[1]
    # pmcmc_pars_list$pars_obs$prob_hosp_multiplier <- delta_characteristics$hosp_modifier[1]
    # pmcmc_pars_list$pars_obs$delta_start_date <- delta_characteristics$start_date[1]
    # pmcmc_pars_list$pars_obs$shift_duration <- delta_characteristics$delta_shift_dur[1]
    # pmcmc_pars_list$n_mcmc <- 100

    # fit to data
    model_fit <- exec(
      pmcmc_excess,
      !!!pmcmc_pars_list
    )
    # assign class because I didn't put that in pmcmc_excess for some reason?
    class(model_fit) <- c("excess_nimue_simulation", "nimue_simulation")
    #also assign prior since that's also missing
    model_fit$pmcmc_results$inputs$prior <- function(pars) {
      0
    }
    plot <- create_plot(model_fit, start_date, immune_escape, hosp_modifier, delta_shift_dur
    )
    #generate parameter draws
    pars_list <- squire.page::generate_parameters(model_fit, draws = 25)
    #get baseline deaths
    baseline_deaths <- squire.page::nimue_format(
      squire.page::generate_draws(model_fit, pars_list),
      var_select = "deaths"
    )
    #reff
    model_fit$parameters$country <- "India"
    baseline_reff <- calculate_reff(
      squire.page::generate_draws(model_fit, pars_list)
    )

    # squire.page::rt_plot_immunity(
    #   squire.page::generate_draws(model_fit, pars_list)
    # )
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
    counterfactual_reff <- calculate_reff(
      squire.page::generate_draws(model_fit, pars_list)
    )
    #   squire.page::rt_plot_immunity(
    #   squire.page::generate_draws(model_fit, pars_list)
    # )
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
        delta_start_date = start_date,
        delta_immune_escape = immune_escape,
        delta_hosp_modifier = hosp_modifier,
        delta_delta_shift_dur = delta_shift_dur
      )
    #as an experiment we also plot difference in reff from both
    reff_df <- #
      baseline_reff %>%
      select(date, Reff, rep) %>%
      rename(vacc_Reff = Reff) %>%
      full_join(
        counterfactual_reff %>%
          select(date, Reff, rep)
      ) %>%
      mutate(
        delta_start_date = start_date,
        delta_immune_escape = immune_escape,
        delta_hosp_modifier = hosp_modifier,
        delta_delta_shift_dur = delta_shift_dur
      )
    # baseline_reff$rts %>%
    # select(date, Reff_median) %>%
    # rename(vacc_Reff = Reff_median) %>%
    # full_join(
    #   counterfactual_reff$rts %>%
    #     select(date, Reff_median)
    # ) %>%
    # mutate(
    #   Reff_diff = Reff_median - vacc_Reff
    # ) %>%
    # select(date, Reff_diff) %>%
    # mutate(
    #   delta_start_date = start_date,
    #   delta_immune_escape = immune_escape,
    #   delta_hosp_modifier = hosp_modifier,
    #   delta_delta_shift_dur = delta_shift_dur
    # )

    return(list(
      plot = plot,
      reff = reff_df,
      result = deaths_averted
    ))
  }
)
#pdf of plots
pdf("fitting_plot.pdf")
map(model_fits, ~print(.x$plot))
dev.off()
#combine reff differences
saveRDS(
  do.call(
    rbind,
    map(
      model_fits, ~.x$reff
    )
  ),# %>%
  # rename(`Immune Escape:` = delta_immune_escape) %>%
  # select(date, Reff_diff, `Immune Escape:`) %>%
  # ggplot(aes(x = date, y = Reff_diff,
  #            colour = `Immune Escape:`, group = `Immune Escape:`)) +
  # geom_line() +
  # ggpubr::theme_pubclean() +
  # labs(x = "Date", y = "Reduction in effective reproduction\nfrom vaccine induced protection"),
  "reff_plot.Rds"
)
#save results
results_df <- do.call(
  rbind,
  lapply(model_fits, function(x){x$result})
)
saveRDS(results_df, "res.Rds")

