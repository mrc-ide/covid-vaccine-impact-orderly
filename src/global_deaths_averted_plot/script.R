if(!is.na(seed)){
  set.seed(seed)
}
excess <- TRUE
###Load data:
iso3cs <- readRDS(
  "counterfactuals.Rds"
) %>% pull(iso3c) %>% unique()
fig1_df_overall <- loadCounterfactualData(c("No Vaccines", "Baseline-Direct"),
                                          group_by = "date"
                                          )
fig1_df_income <- loadCounterfactualData("No Vaccines",
                                         group_by = c("income_group", "date"))

if(excess){
  #need to extract data from model fits
  # fits <- readRDS("countryfits.Rds")
  # #for each iso3c
  # fig1_df_data <- map_dfr(iso3cs, function(iso3c){
  #   fits[[iso3c]]$pmcmc_results$inputs$data
  # }) %>%
  #   mutate(
  #     week_length = as.numeric(week_end - week_start),
  #     obsDate = week_end,
  #     deaths = deaths/week_length) %>%
  #   group_by(obsDate) %>%
  #   summarise(deaths = sum(deaths))
  fig1_df_data <- readRDS("excess_deaths.Rds")
  #ensure there is a week between dates
  if(any(as.numeric(diff(fig1_df_data$obsDate)) != 7)){
    warning("In excess data so weeks are inconsistenly spaced.")
  }
}

###Figure 1 daily deaths over time:
baseline_col <- "black"#"olivedrab3"
novacc_col <- "#d62728"#"firebrick1"
averted_col <- "#17becf"#"deepskyblue3"
direct_col <- "#98df8a"#"yellow"
data_col <- "grey"
fig1_1 <- ggplot(fig1_df_overall %>%
                   filter(date > "2021-01-1",
                          counterfactual == "No Vaccines")) +
  geom_col(data = fig1_df_data %>%
             filter(obsDate > "2021-01-1"),
           aes(x = obsDate + 7/2, y = deaths, fill = data_col),
           alpha = 0.75,
           #width = 7,
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
    y = "Daily Deaths"
  ) +
  scale_color_identity(name = "Legend:",
                       breaks = c(baseline_col,
                                  novacc_col,
                                  averted_col,
                                  data_col,
                                  direct_col),
                       labels = c("Model fit", "Counter-factual:\nNo Vaccine", "Deaths Averted\n(Direct)", if_else(excess,
                                                                                                                 "Excess Mortality\nEstimates",
                                                                                                                 "Our World\nin Data"),
                                  "Deaths Averted\n(Indirect)"),
                       guide = "legend",
                       aesthetics = c("colour", "fill")) +
  theme_pubr() +
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
    y = "Deaths Averted by Vaccinations per day",
    fill = "Income Group:"
  ) +
  ylim(ggplot_build(fig1_1)$layout$panel_scales_y[[1]]$range$range - c(1,0)) +
  theme_pubr() +
  theme(legend.position = c(0.25, 0.8))

fig_1 <- ggarrange(fig1_1, fig1_2, labels = "auto")
dir.create("plots", showWarnings = FALSE)
saveRDS(fig_1, "plots/global_deaths_averted.Rds")
