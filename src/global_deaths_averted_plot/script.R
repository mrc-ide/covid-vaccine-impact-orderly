if(!is.na(seed)){
  set.seed(seed)
}

###Load data:
iso3cs <- readRDS(
  "counterfactuals.Rds"
) %>% pull(iso3c) %>% unique()
fig1_df_overall <- loadCounterfactualData(c("No Vaccines", "Baseline-Direct"),
                                          group_by = "date",
                                          exclude_iso3cs = "CHN"
                                          )
fig1_df_income <- loadCounterfactualData("No Vaccines",
                                         group_by = c("income_group", "date")
                                         ,
                                         exclude_iso3cs = "CHN")
fig1_df_owid <- readRDS(
  "owid.Rds"
) %>%
  group_by(obsDate) %>%
  filter(iso3c %in% iso3cs) %>%
  summarise(
    total_deaths = sum(total_deaths, na.rm = T)
  ) %>%
  mutate(deaths = total_deaths - lag(total_deaths, default = total_deaths[1]),
         obsDate = as.Date(obsDate))

###Figure 1 daily deaths over time:
baseline_col <- "olivedrab3"
novacc_col <- "firebrick1"
averted_col <- "deepskyblue3"
direct_col <- "yellow"
data_col <- "grey"
fig1_1 <- ggplot(fig1_df_overall %>%
                   filter(date > "2021-01-1",
                          counterfactual == "No Vaccines")) +
  geom_col(data = fig1_df_owid %>%
             filter(obsDate > "2021-01-1"),
           aes(x = obsDate, y = deaths, fill = data_col),
           alpha = 0.5,
           width = 1,
           colour = "white") +
  geom_ribbon(aes(x = date,
                  ymax = deaths_avg,
                  ymin = fig1_df_overall %>%
                    filter(date > "2021-01-1",
                           counterfactual == "Baseline-Direct") %>%
                    pull(deaths_avg),
                  fill = averted_col),
              alpha = 0.25) +
  geom_ribbon(data = fig1_df_overall %>%
                filter(date > "2021-01-1",
                       counterfactual == "Baseline-Direct"),
              aes(x = date,
                  ymax = deaths_avg,
                  ymin = baseline_deaths_avg,
                  fill = direct_col),
              alpha = 0.25) +
  geom_line(aes(x = date, y = baseline_deaths_avg,
                colour = baseline_col),
            size = 1) +
  geom_line(aes(x = date, y = deaths_avg,
                colour = novacc_col),
            size = 1) +
  expand_limits(y = 0) +
  labs(
    x = "Date",
    y = "Daily Deaths",
    title = paste0("Worldwide deaths with/without vaccines,\nup to ", date)
  ) +
  scale_color_identity(name = "Legend:",
                       breaks = c(baseline_col,
                                  novacc_col,
                                  averted_col,
                                  data_col,
                                  direct_col),
                       labels = c("Model fit", "Counter-factual:\nNo Vaccine", "Deaths Averted(Direct)", "Our World\nin Data",
                                  "Deaths Averted\n(Indirect)"),
                       guide = "legend",
                       aesthetics = c("colour", "fill")) +
  theme_pubclean() +
  theme(legend.position = c(0.25, 0.8))

fig1_2 <- ggplot(data = fig1_df_overall %>%
                   filter(date > "2021-01-1",
                          counterfactual == "No Vaccines"),
                 aes(x = date)) +
  geom_area(
    data = fig1_df_income %>%
      filter(date > "2021-01-01"),
    aes(x = date, y = averted_deaths_avg, fill = income_group),
    alpha = 1
  ) +
  #geom_line(aes(y = averted_deaths_avg),
  #          size = 1, colour = averted_col) +
  #geom_ribbon(aes(ymin = averted_deaths_025,
  #                ymax = averted_deaths_975),
  #            alpha = 0.55,
  #            fill = averted_col) +
  labs(
    x = "Date",
    y = NULL,
    title = paste0("Worldwide deaths averted by vaccines,\nup to ", date),
    fill = "Income Group:"
  ) +
  ylim(ggplot_build(fig1_1)$layout$panel_scales_y[[1]]$range$range - c(1,0)) +
  theme_pubclean() +
  theme(legend.position = c(0.25, 0.8))

fig_1 <- plot_grid(fig1_1, fig1_2)
dir.create("plots", showWarnings = FALSE)
saveRDS(fig_1, "plots/global_deaths_averted.Rds")
