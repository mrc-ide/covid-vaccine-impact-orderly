#example plot for the USA
example_plots <- readRDS("example_plots.Rds")
ggsave("figure1.png", example_plots$USA,
       width = 13.5, height = 6)
ggsave("figure1.pdf", example_plots$USA,
       width = 13.5, height = 6)

#deaths averted over time plot
global_deaths_averted <- readRDS("global_deaths_averted.Rds")
ggsave("figure2.png", global_deaths_averted,
       width = 17, height = 7)
ggsave("figure2.pdf", global_deaths_averted,
       width = 17, height = 7)

#deaths averted table
averted_table <- readRDS("averted_table.Rds")
write.csv(
  averted_table[[1]],
  "table2.csv")
#add updated percentages to table 3
write.csv(
  averted_table[[2]] %>%
    left_join(readRDS("COVAX_WHO_percentages.Rds") %>%
                rename(
                  ` ` = measure
                ) %>%
                mutate(
                  ` ` = if_else(` ` == "Worldwide",
                                ` `,
                                paste0("   ", ` `))
                ),
              by = " ") %>%
    mutate(
      `Additional Deaths Averted if COVAX Targets met (%)` = covax_p_averted,
      `Additional Deaths Averted if WHO Targets met (%)` = who_p_averted,
      `Countries Failing COVAX Target` = n_fail_covax,
      `Countries Failing WHO Target` = n_fail_who
    ) %>%
    select(!c(covax_p_averted, who_p_averted, n_fail_covax, n_fail_who)),
  "table3.csv")

#world map
deaths_averted_map <- readRDS("deaths_averted_map.Rds")
ggsave("figure3.png", deaths_averted_map,
       width = 10, height = 5)
ggsave("figure3.pdf", deaths_averted_map,
       width = 10, height = 5)

#log vaccine/log deaths averted figure
dot_plots <- readRDS("dot_plots.Rds")
ggsave("figure4.png", dot_plots$vacc,
       width = 9, height = 5)
ggsave("figure4.pdf", dot_plots$vacc,
       width = 9, height = 5)
