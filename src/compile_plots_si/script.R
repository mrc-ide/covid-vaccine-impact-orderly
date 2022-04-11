library(ggplot2)

#delta sensitivity analysis
no_res <- readRDS("usa_res.Rds")
res <- readRDS("india_res.Rds")
#due to ggplot2 environment issues, we'll need to recreate the death curves
data_1 <- tibble(
  deaths = c(
    118, 378, 813, 935, 656, 331, 173, 88, 46, 24, 19, 33, 44, 40, 38, 62,
    114, 191, 263, 305, 348, 473, 659, 737, 606, 368, 183, 82, 36, 23, 24,
    21, 18, 43, 87, 148, 225, 298, 388, 467, 387, 309, 259, 218, 164
  )
) %>%
  mutate(
    week_start = as_date("2021-12-01") - 7 * rev(seq_along(deaths)),
    start_date  = as_date("2021-05-01"),
    population = sum(
      c(
        19676332, 20045152, 21089487, 21242908, 22258745, 23835330, 23052479,
        21615791, 20294599, 20053798, 20577807, 21542270, 20669143, 17819027,
        14354863, 9727734, 13147182
      )
    )
  )
data_2 <- tibble(
  deaths = c(14506, 50601, 62041, 83553, 110407, 137149, 133691, 98801, 82238, 81455, 64927, 63541, 65007, 63094, 55199, 34818, 36804, 49677, 51421,
                      64591, 163419, 271734, 600103, 624699, 407582, 140985, 126373, 129763, 136651, 118683, 101090, 92772, 94338, 94087, 90210, 81653, 76853, 43591)
) %>%
  mutate(
    week_start = as_date("2021-12-01") - 7 * rev(seq_along(deaths)),
    start_date  = as_date("2021-07-01"),
    population = sum(
      c(
        116879507, 117982127, 126155952, 126045566, 122504804, 117397269, 112176098, 103460178, 90219894, 79440280, 68875962, 59256268,
        48890528, 38260283, 24091443, 15083955, 13284271
      )
    )
  )
delta_fig <-
  ggarrange(
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
    ggplot(data_1, aes(week_start, (deaths/population)*1e5)) +
      geomtextpath::geom_textvline(label = "Delta Introduction",
                                   aes(xintercept = unique(start_date)),
                                   hjust = 0.2,
                                   linetype = 2) +
      geom_step(color = "red") +
      theme_bw() + ylab("Weekly Deaths per 100,000\n") + xlab("")  +
      ggpubr::theme_pubr(),
    ggplot(data_2, aes(week_start, (deaths/population)*1e5)) +
      geomtextpath::geom_textvline(label = "Delta Introduction",
                                   aes(xintercept = unique(start_date)),
                                   hjust = 0.2,
                                   linetype = 2) +
      geom_step(color = "purple") +
      theme_bw() + ylab("Weekly Deaths per 100,000\n") + xlab("")  +
      ggpubr::theme_pubr(),
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
ggsave("delta_sensitivity.png", delta_fig, width = 12, height = 10)
ggsave("delta_sensitivity.pdf", delta_fig, width = 12, height = 10)

#ifr sensitivity
ifr_res <- readRDS("ifr_res.Rds")
ifr_data <- readRDS("ifr_death_curves.Rds")
ifr_fig <-
  ggarrange(
    ggplot(ifr_res[[2]], aes(x = iteration, y = deaths_averted_median,
                       ymin = deaths_averted_025, ymax = deaths_averted_975)) +
      geom_line(colour = "red") + geom_ribbon(alpha = 0.1, fill = "red") +
      #ylim(c(0, max(no_res$deaths_averted_975))) +
      scale_x_continuous(labels = scales::percent) +
      labs(x = "IFR Scaling", y = "Deaths Averted by Vaccinations\nMedian and 95% quantile") +
      theme_pubr(),
    ggplot(ifr_res[[1]], aes(x = iteration, y = deaths_averted_median,
                    ymin = deaths_averted_025, ymax = deaths_averted_975)) +
      geom_line(colour = "purple") + geom_ribbon(alpha = 0.1, fill = "purple") +
      #ylim(c(0, max(res$deaths_averted_975))) +
      scale_x_continuous(labels = scales::percent) +
      labs(x = "IFR Scaling", y = "Deaths Averted by Vaccinations\nMedian and 95% quantile") +
      theme_pubr(),
    ifr_data[[2]],
    ifr_data[[1]],
    labels = "auto"
  )
ggsave("ifr_sensitivity.png", ifr_fig, width = 12, height = 10)
ggsave("ifr_sensitivity.pdf", ifr_fig, width = 12, height = 10)

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
