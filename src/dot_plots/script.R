
###Load data:
fig2_df <- loadCounterfactualData("No Vaccines",
                                  group_by = "iso3c")
counterfactuals_df <- readRDS(
  "counterfactuals.Rds"
)
iso3cs <- counterfactuals_df %>% pull(iso3c) %>% unique()

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
    counterfactuals_df %>%
      transmute(vaccinations = `Baseline (Total Vaccines)`,
                iso3c = iso3c)
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
    where(is.numeric),
    ~.x/population*10000
  ))

###Figure 2 dot plots:
alpha <- 0.75

#calculate correlation by income + pvalue
corrs <- fig2_df_extra_data %>%
  group_by(income_group) %>%
  summarise(
    corr = cor(x = averted_deaths_avg, y = vaccinations, use = "complete.obs", method = "spearman"),
    #caculate a p value based on the z score
    p = (sqrt((length(averted_deaths_avg) - 3)/1.06) *
      atanh(corr)) %>%
        pnorm(lower.tail = FALSE),
    p = if_else(
      p < 0.0001,
      "<0.0001",
      paste0("=",as.character(
        signif(p, digits = 2)
      ))
    )
  ) %>%
  transmute(
    income_group = income_group,
    `Correlation:` = factor(
      x = seq_len(4),
      labels = paste0(income_group, " (", signif(corr, digits = 3), ", p", p, ")"),
      levels = seq_len(4),
      ordered = TRUE
      )
  )

fig2_vacc <- ggplot(fig2_df_extra_data %>%
                      mutate(label =
                               log(averted_deaths_avg) <= log(vaccinations)*0.98
                             -7.8 | (averted_deaths_avg > 1e+01 & vaccinations < 100)
                               ) %>%
                      filter(vaccinations > 0 & averted_deaths_avg > 0) %>%
                      left_join(corrs),
                    aes(x = vaccinations,
                        y = averted_deaths_avg)) +
  geom_point(
    aes(colour = `Correlation:`),
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
       colour = expression(paste("Income Group (r"["s"], ", ", italic("p"), "-value):"))) +
  theme(legend.key.size = unit(0.25, 'cm'),
        legend.margin = margin(),
        legend.title.align=0.5) +
  guides(colour = guide_legend(title.position="top"))

fig2 <- list(
  vacc = fig2_vacc
)

dir.create("plots")
saveRDS(fig2, "plots/dot_plots.Rds")
