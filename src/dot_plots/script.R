
###Load data:
fig2_df <- loadCounterfactualData("No Vaccines",
                                  group_by = "iso3c",
                                  exclude_iso3cs = exclude_iso3cs)
counterfactuals_df <- readRDS(
  "counterfactuals.Rds"
)
iso3cs <- counterfactuals_df %>% pull(iso3c) %>% unique()
owid_df <- readRDS(
  "owid.Rds"
)

summarise_variable <- function(data, variable){
  data %>%
    group_by(.data$iso3c) %>%
    filter(!is.na(.data[[variable]])) %>%
    summarise(
      !! variable := max(.data[[variable]])
      )
}

fig2_df_extra_data <- fig2_df %>%
  left_join(
    owid_df %>%
      summarise_variable("total_vaccinations") %>%
      rename(vaccinations = total_vaccinations),
    by = "iso3c"
  ) %>%
  left_join(
    owid_df %>%
      summarise_variable("total_tests") %>%
      rename(tests = total_tests),
    by = "iso3c"
  ) %>%
  left_join(
    owid_df %>%
      summarise_variable("people_vaccinated"),
    by = "iso3c"
  ) %>%
  left_join(
    owid_df %>%
      summarise_variable("total_cases") %>%
      rename(reported_cases = total_cases) %>%
      left_join(
        owid_df %>%
          filter(obsDate <= "2020-12-31") %>%
          summarise_variable("total_cases") %>%
          rename(reported_cases_2020 = total_cases),
        by = "iso3c"
      ) %>%
      mutate(
        reported_cases_2020 = if_else(is.na(reported_cases_2020), 0, reported_cases_2020),
        reported_cases_2021 = reported_cases - reported_cases_2020
      ),
    by = "iso3c"
  ) %>%
  left_join(
    owid_df %>%
      summarise_variable("total_deaths") %>%
      rename(reported_deaths = total_deaths) %>%
      left_join(
        owid_df %>%
          filter(obsDate <= "2020-12-31") %>%
          summarise_variable("total_deaths") %>%
          rename(reported_deaths_2020 = total_deaths),
        by = "iso3c"
      ) %>%
      mutate(
        reported_deaths_2020 = if_else(is.na(reported_deaths_2020), 0, reported_deaths_2020),
        reported_deaths_2021 = reported_deaths - reported_deaths_2020
      ),
    by = "iso3c"
  ) %>%
  left_join(
    owid_df %>%
      group_by(iso3c) %>%
      filter(total_cases > 0) %>%
      summarise(
        start_date = min(obsDate)
      ),
    by = "iso3c"
  ) %>%
  mutate(
    income_group = get_income_group(iso3c),
    who_region = get_WHO_region(iso3c)
  ) %>%
  mutate(across(
    where(is.numeric),
    ~if_else(is.infinite(.x), as.numeric(NA), .x)
  )) %>%
  left_join(
    #add population
    squire::population %>%
      group_by(iso3c) %>%
      summarise(population = sum(n)),
    by = "iso3c"
  ) %>%
  mutate(across(
    #converted to per capita
    is.numeric,
    ~.x/population*10000
  ))

###Figure 2 dot plots:
alpha <- 0.75

fig2_vacc <- ggplot(fig2_df_extra_data %>%
                      mutate(label =
                               log(averted_deaths_avg) <= log(vaccinations)*1 -7.8
                               ) %>%
                      filter(vaccinations > 0 & averted_deaths_avg > 0),
                    aes(x = vaccinations,
                        y = averted_deaths_avg)) +
  geom_point(
    aes(colour = income_group),
    alpha = alpha
  ) +
  #geom_abline(aes(intercept = -7.8, slope = 1)) +
  geom_text(aes(label = if_else(label,
                                countrycode::countrycode(iso3c, origin = "iso3c", destination = "country.name"),
                                NULL)),
              nudge_y = -0.15,
            hjust = 0) +
     theme_pubr() +
    scale_x_log10() +
    scale_y_log10() +
  labs(x = "Vaccinations per 10k", y = "Median Deaths Averted per 10k",
       colour = "Income Group:") +
  theme(legend.key.size = unit(0.5, 'cm'),
        legend.margin = margin())

fig2_pvacc <- ggplot(fig2_df_extra_data) +
  geom_point(
    aes(x = people_vaccinated,
        y = averted_deaths_avg,
        colour = income_group),
    alpha = alpha
  ) +
     theme_pubr() +   scale_x_log10() +   scale_y_log10() +
  labs(x = "People with at least one dose", y = "Median Deaths Averted",
       colour = "Income Group:") +
  theme(legend.position = "none",
        plot.subtitle = element_text(face = "italic", hjust = 0.5)) +
  labs(subtitle = "All measures are per capita and log")

fig2_cases_2020 <- ggplot(fig2_df_extra_data) +
  geom_point(
    aes(x = reported_cases_2020,
        y = averted_deaths_avg,
        colour = income_group),
    alpha = alpha
  ) +
     theme_pubr() +   scale_x_log10() +   scale_y_log10() +
  labs(x = "Reported Cases(2020)", y = "Median Deaths Averted",
       colour = "Income Group:") +
  theme(legend.position = "none")

fig2_cases_2021 <- ggplot(fig2_df_extra_data) +
  geom_point(
    aes(x = reported_cases_2021,
        y = averted_deaths_avg,
        colour = income_group),
    alpha = alpha
  ) +
     theme_pubr() +   scale_x_log10() +   scale_y_log10() +
  labs(x = "Reported Cases(2021)", y = "Median Deaths Averted",
       colour = "Income Group:") +
  theme(legend.position = "none")

fig2_deaths_2020 <- ggplot(fig2_df_extra_data) +
  geom_point(
    aes(x = reported_deaths_2020,
        y = averted_deaths_avg,
        colour = income_group),
    alpha = alpha
  ) +
     theme_pubr() +   scale_x_log10() +   scale_y_log10() +
  labs(x = "Reported deaths(2020)", y = "Median Deaths Averted",
       colour = "Income Group:") +
  theme(legend.position = "none")

fig2_deaths_2021 <- ggplot(fig2_df_extra_data) +
  geom_point(
    aes(x = reported_deaths_2021,
        y = averted_deaths_avg,
        colour = income_group),
    alpha = alpha
  ) +
     theme_pubr() +   scale_x_log10() +   scale_y_log10() +
  labs(x = "Reported deaths(2021)", y = "Median Deaths Averted",
       colour = "Income Group:") +
  theme(legend.position = "none")

fig2_dates <- ggplot(fig2_df_extra_data) +
  geom_point(
    aes(x = as.Date(start_date),
        y = averted_deaths_avg,
        colour = income_group),
    alpha = alpha
  ) +
  scale_x_date(date_breaks = "2 months") +
     theme_pubr() +   scale_y_log10() +
  labs(x = "Date of first reported case", y = "Median Deaths Averted",
       colour = "Income Group:") +
  theme(legend.position = "none")

fig2_tests <- ggplot(fig2_df_extra_data) +
  geom_point(
    aes(x = tests,
        y = averted_deaths_avg,
        colour = income_group),
    alpha = alpha
  ) +
     theme_pubr() +   scale_x_log10() +   scale_y_log10() +
  labs(x = "Tests", y = "Median Deaths Averted",
       colour = "Income Group:") +
  theme(legend.position = "none")

fig2 <- list(
  vacc = fig2_vacc, pvacc = fig2_pvacc,
  cases_20 = fig2_cases_2020, cases_21 = fig2_cases_2021,
  deaths_20 = fig2_deaths_2020, deaths_21 = fig2_deaths_2021,
  dates = fig2_dates, tests = fig2_tests
)

dir.create("plots")
saveRDS(fig2, "plots/dot_plots.Rds")
