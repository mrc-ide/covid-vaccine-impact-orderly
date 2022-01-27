library(ggplot2)

#delta sensitivity analysis
no_res <- readRDS("usa_res.Rds")
res <- readRDS("india_res.Rds")
delta_fig <-
  ggarrange(
    nrow = 1,
    ggplot(no_res, aes(x = delta_immune_escape, y = deaths_averted_median,
                       ymin = deaths_averted_025, ymax = deaths_averted_975)) +
      geom_line(colour = "red") + geom_ribbon(alpha = 0.1, fill = "red") +
      #ylim(c(0, max(no_res$deaths_averted_975))) +
      scale_x_continuous(labels = scales::percent) +
      labs(x = "Delta Immune Escape", y = "Deaths Averted by Vaccinations\nMedian and 95% quantile") +
      theme_pubr(),
    ggplot(res, aes(x = delta_immune_escape, y = deaths_averted_median,
                       ymin = deaths_averted_025, ymax = deaths_averted_975)) +
      geom_line(colour = "purple") + geom_ribbon(alpha = 0.1, fill = "purple") +
      #ylim(c(0, max(res$deaths_averted_975))) +
      scale_x_continuous(labels = scales::percent) +
      labs(x = "Delta Immune Escape", y = "Deaths Averted by Vaccinations\nMedian and 95% quantile") +
      theme_pubr(),
    labels = "auto"
  )
#reffs
# no_reffs <- readRDS("usa_reff_plot.Rds")
# reffs <- readRDS("india_reff_plot.Rds")
# delta_eff_fig <- ggarrange(
#   nrow = 1,
# reffs %>%
#   group_by(date, delta_immune_escape) %>%
#   mutate(reff_loss = Reff-vacc_Reff) %>%
#   summarise(reff_loss_avg = median(reff_loss),
#             reff_loss_025 = quantile(reff_loss, 0.025),
#             reff_loss_975 = quantile(reff_loss, 0.975)) %>%
#   ggplot(
#     aes(x = date, y = reff_loss_avg, ymin = reff_loss_025, ymax = reff_loss_975, group = delta_immune_escape,
#         fill = delta_immune_escape)
#   ) +
#   geom_line(aes(colour = delta_immune_escape)) +
#   geom_ribbon(alpha = 0.1) +
#   labs(x = "Date", y = "Loss in Reff", fill = "Delta Immune Escape", colour = "Delta Immune Escape"),
# no_reffs %>%
#   group_by(date, delta_immune_escape) %>%
#   mutate(reff_loss = Reff-vacc_Reff) %>%
#   summarise(reff_loss_avg = median(reff_loss),
#             reff_loss_025 = quantile(reff_loss, 0.025),
#             reff_loss_975 = quantile(reff_loss, 0.975)) %>%
#   ggplot(
#     aes(x = date, y = reff_loss_avg, ymin = reff_loss_025, ymax = reff_loss_975, group = delta_immune_escape,
#         fill = delta_immune_escape)
#   ) +
#   geom_line(aes(colour = delta_immune_escape)) +
#   geom_ribbon(alpha = 0.1) +
#   labs(x = "Date", y = "Loss in Reff", fill = "Delta Immune Escape", colour = "Delta Immune Escape"),
# labels = "auto",  common.legend = TRUE
# )
ggsave("delta_sensitivity.png", delta_fig, width = 12, height = 7)
ggsave("delta_sensitivity.pdf", delta_fig, width = 12, height = 7)

#deaths averted excess
averted_table <- readRDS("averted_table.Rds") %>%
  rename(`Deaths Averted by Vaccinations\nTotal` = `Deaths Averted by Vaccinations`)
write.csv(
  averted_table,
  "reported_averted_deaths_table.csv")

#excess reported comparison
reported_comparison <- readRDS("reported_excess_comparison.Rds")
ggsave("reported_excess_comparison.png", reported_comparison$total, width = 8, height = 5)
ggsave("reported_excess_comparison.pdf", reported_comparison$total, width = 8, height = 5)

#hospital direct effects
hospital_effects <- readRDS("hospital_effects.Rds")
ggsave("hospital_direct_indirect.png", hospital_effects, width = 10, height = 5)
ggsave("hospital_direct_indirect.pdf", hospital_effects,  width = 10, height = 5)
