if(!is.na(seed)){
  set.seed(seed)
}

###Load data:
df_overall <- loadCounterfactualData(c("No Vaccines", "Baseline-Direct",
                                       "Baseline-Direct & No Healthcare Surging",
                                       "No Vaccines-No Healthcare Surging",
                                       "Baseline-No Healthcare Surging"),
                                          group_by = "date",
                                          exclude_iso3cs = exclude_iso3cs
                                          )


###Figure 1 daily deaths over time:
indirect_col <- "#d62728"#"firebrick1"
indirect_hosp_col <- "firebrick1"
direct_hosp_col <- "#17becf"#"deepskyblue3"
direct_col <- "#98df8a"#"yellow"

#sort data
df_sorted <- df_overall %>%
  select(date, counterfactual, deaths_avg) %>%
  rbind(df_overall %>%
          select(date, baseline_deaths_avg) %>%
          rename(deaths_avg = baseline_deaths_avg) %>%
          unique() %>%
          mutate(counterfactual = "Baseline")) %>%
  pivot_wider(id_cols =date, names_from = counterfactual, values_from = deaths_avg)

plot(df_sorted$`No Vaccines-No Healthcare Surging`, type = "l")
lines(df_sorted$`No Vaccines`)
#calculate levels, need to isolate vaccine impacts:
df_sorted %>%
  transmute(
    date = date,
    total_averted = `No Vaccines` - Baseline,
    hosp_caused = Baseline - `Baseline-No Healthcare Surging`,
    averted_no_hosp = `No Vaccines-No Healthcare Surging` - `Baseline-No Healthcare Surging`,
    averted_by_hosp_reduction = total_averted
  ) %>%
  ggplot() +
  geom_line(aes(x = date, y = total_averted)) +
  geom_line(aes(x = date, y = averted_no_hosp))

ggplot(df_sorted) +
  geom_line(aes(x = date, y = deaths_avg, colour = counterfactual))

fig <- ggplot(data = df_overall %>%
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
