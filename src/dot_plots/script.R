
###Load data:
fig2_df <- loadCounterfactualData("No Vaccines",
                                  group_by = "iso3c")
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
    ~.x/population
  ))

###Figure 2 dot plots:
alpha <- 0.75

fig2_vacc <- ggplot(fig2_df_extra_data) +
  geom_point(
    aes(x = log(vaccinations),
        y = log(averted_deaths_avg),
        colour = income_group),
    alpha = alpha
  ) +
  theme_pubclean() +
  labs(x = "Vaccinations", y = "Median Deaths Averted",
       colour = "Income Group:") +
  theme(legend.key.size = unit(0.5, 'cm'),
        legend.margin = margin())

fig2_pvacc <- ggplot(fig2_df_extra_data) +
  geom_point(
    aes(x = log(people_vaccinated),
        y = log(averted_deaths_avg),
        colour = income_group),
    alpha = alpha
  ) +
  theme_pubclean() +
  labs(x = "People with at least one dose", y = "",
       colour = "Income Group:") +
  theme(legend.position = "none",
        plot.subtitle = element_text(face = "italic", hjust = 0.5)) +
  labs(subtitle = "All measures are per capita and log")

fig2_cases_2020 <- ggplot(fig2_df_extra_data) +
  geom_point(
    aes(x = log(reported_cases_2020),
        y = log(averted_deaths_avg),
        colour = income_group),
    alpha = alpha
  ) +
  theme_pubclean() +
  labs(x = "Reported Cases(2020)", y = "Median Deaths Averted",
       colour = "Income Group:") +
  theme(legend.position = "none")

fig2_cases_2021 <- ggplot(fig2_df_extra_data) +
  geom_point(
    aes(x = log(reported_cases_2021),
        y = log(averted_deaths_avg),
        colour = income_group),
    alpha = alpha
  ) +
  theme_pubclean() +
  labs(x = "Reported Cases(2021)", y = "",
       colour = "Income Group:") +
  theme(legend.position = "none")

fig2_deaths_2020 <- ggplot(fig2_df_extra_data) +
  geom_point(
    aes(x = log(reported_deaths_2020),
        y = log(averted_deaths_avg),
        colour = income_group),
    alpha = alpha
  ) +
  theme_pubclean() +
  labs(x = "Reported deaths(2020)", y = "Median Deaths Averted",
       colour = "Income Group:") +
  theme(legend.position = "none")

fig2_deaths_2021 <- ggplot(fig2_df_extra_data) +
  geom_point(
    aes(x = log(reported_deaths_2021),
        y = log(averted_deaths_avg),
        colour = income_group),
    alpha = alpha
  ) +
  theme_pubclean() +
  labs(x = "Reported deaths(2021)", y = "",
       colour = "Income Group:") +
  theme(legend.position = "none")

fig2_dates <- ggplot(fig2_df_extra_data) +
  geom_point(
    aes(x = as.Date(start_date),
        y = log(averted_deaths_avg),
        colour = income_group),
    alpha = alpha
  ) +
  scale_x_date(date_breaks = "2 months") +
  theme_pubclean() +
  labs(x = "Date of first reported case", y = "Median Deaths Averted",
       colour = "Income Group:") +
  theme(legend.position = "none")

fig2_tests <- ggplot(fig2_df_extra_data) +
  geom_point(
    aes(x = log(tests),
        y = log(averted_deaths_avg),
        colour = income_group),
    alpha = alpha
  ) +
  theme_pubclean() +
  labs(x = "Tests", y = "",
       colour = "Income Group:") +
  theme(legend.position = "none")

fig2 <- plot_grid(
  fig2_vacc, fig2_pvacc,
  fig2_cases_2020, fig2_cases_2021,
  fig2_deaths_2020, fig2_deaths_2021,
  fig2_dates, fig2_tests,
  nrow = 4, ncol = 2,
  hjust = 1
)

dir.create("plots")
saveRDS(fig2, "plots/dot_plots.Rds")
