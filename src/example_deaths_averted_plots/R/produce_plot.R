create_example_plot <- function(iso, vacc_start_date, df, df_replicate) {
  iso3c_df <- df %>%
  filter(iso3c == iso & date > vacc_start_date) %>%
  mutate(counterfactual = if_else(
    counterfactual == "Baseline_direct",
    "No protection against Infection\n
    & transmission",
    counterfactual
  ))
iso3c_df_replicate <- df_replicate %>%
  filter(iso3c == iso & date > vacc_start_date) %>%
  mutate(counterfactual = if_else(
    counterfactual == "Baseline_direct",
    "No protection against Infection\n
    & transmission",
    counterfactual
  ))

replicate_first_deaths_plot <- ggplot(iso3c_df_replicate %>% filter(replicate == 1), aes(x = date)) +
  geom_line(aes(y = deaths_avg), colour = "purple") +
  geom_line(aes(y = baseline_deaths_avg)) +
  geom_ribbon(
    aes(ymin = deaths_avg, ymax = baseline_deaths_avg, fill = counterfactual),
    alpha = 0.1
  ) +
  facet_wrap(vars(counterfactual)) +
  labs(x = "Date", y = "Deaths") +
  theme_pubclean() +
  theme(legend.position = "none")

# replicate_last_deaths_plot <- ggplot(iso3c_df_replicate %>% filter(replicate == max(replicate)), aes(x = date)) +
#   geom_line(aes(y = deaths_avg), colour = "red") +
#   geom_line(aes(y = baseline_deaths_avg)) +
#   geom_ribbon(
#     aes(ymin = deaths_avg, ymax = baseline_deaths_avg, fill = counterfactual),
#     alpha = 0.1
#   ) +
#   facet_wrap(vars(counterfactual)) +
#   labs(x = "Date", y = "Deaths") +
#   theme_pubclean() +
#   theme(legend.position = "none")

#now the cumulative deaths averted
# replicate_first_averted_plot <- ggplot(iso3c_df_replicate %>% filter(replicate == 1) %>% group_by(counterfactual) %>% arrange(date) %>%
#          mutate(averted_deaths_avg = cumsum(averted_deaths_avg)), aes(x = date)) +
#   geom_line(aes(y = averted_deaths_avg, colour = counterfactual)) +
#   labs(x = "Date", y = "Cumulative Deaths Averted") +
#   theme_pubclean() +
#   theme(legend.position = "none")
#
# replicate_first_averted_plot <- ggplot(iso3c_df_replicate %>% filter(replicate == max(replicate)) %>% group_by(counterfactual) %>% arrange(date) %>%
#                                          mutate(averted_deaths_avg = cumsum(averted_deaths_avg)), aes(x = date)) +
#   geom_line(aes(y = averted_deaths_avg, colour = counterfactual)) +
#   labs(x = "Date", y = "Cumulative Deaths Averted") +
#   theme_pubclean() +
#   theme(legend.position = "none")

final_plot <- ggplot(iso3c_df %>% group_by(counterfactual) %>% arrange(date) %>%
                       mutate(averted_deaths_avg = cumsum(averted_deaths_avg),
                              averted_deaths_025 = cumsum(averted_deaths_025),
                              averted_deaths_975 = cumsum(averted_deaths_975)),
                     aes(x = date, fill = counterfactual)) +
  geom_line(aes(y = averted_deaths_avg, colour = counterfactual)) +
  geom_ribbon(aes(ymin = averted_deaths_025, ymax = averted_deaths_975), alpha = 0.1) +
  labs(x = "Date", y = "Median (95% quantile) Cumulative Deaths Averted") +
  theme_pubclean() +
  theme(legend.position = "none")

#combine plots
return(
ggarrange(replicate_first_deaths_plot, final_plot,
          labels = "auto",
          ncol = 2)
)
}
