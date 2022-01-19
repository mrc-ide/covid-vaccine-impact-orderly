library(ggplot2)


#example plot for Uk
ggsave("example_plot_usa.png", example_plots$USA,
       width = 13.5, height = 6)

#deaths averted over time plot
ggsave("global_deaths_averted.png", global_deaths_averted,
       width = 17, height = 7)

#deaths averted table
write.csv(
  averted_table,
  "averted_table.csv")

#world map
ggsave("deaths_averted_map.png", deaths_averted_map,
       width = 10, height = 5)

#log vaccine/log deaths averted figure
ggsave("vaccagainstaverted.png", dot_plots$vacc,
       width = 7, height = 5)
