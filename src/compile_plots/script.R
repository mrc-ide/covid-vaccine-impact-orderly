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
#modify the covax stuff
get_value <- function(var, position){
  as.numeric(gsub(",", "", unlist(lapply(strsplit(var, "( \\()|( - )|\\)"), function(x){
    x[position]
  }))))
}
averted_table <- averted_table %>%
  rename(`Additional Deaths Averted if COVAX Targets met\nTotal` = `Deaths Reduced if COVAX Targets met`,
         `Deaths Averted by Vaccinations\nTotal` = `Deaths Averted by Vaccinations`) %>%
  mutate(`Additional Deaths Averted if COVAX Targets met\n% Additional` =
           if_else(
             `Additional Deaths Averted if COVAX Targets met\nTotal` == "",
             "",
             paste0(
               signif(get_value(`Additional Deaths Averted if COVAX Targets met\nTotal`, 1)/
                 get_value(`Deaths Averted by Vaccinations\nTotal`, 1) * 100, digits = 3),
               " (",
               signif(get_value(`Additional Deaths Averted if COVAX Targets met\nTotal`, 2)/
                 get_value(`Deaths Averted by Vaccinations\nTotal`, 1) * 100, digits = 3),
               ", ",
               signif(get_value(`Additional Deaths Averted if COVAX Targets met\nTotal`, 3)/
                 get_value(`Deaths Averted by Vaccinations\nTotal`, 1) * 100, digits = 3),
               ")"
             )
           )) %>%
  select(!contains("Deaths Reduced"))
write.csv(
  averted_table,
  "table1.csv")

#world map
deaths_averted_map <- readRDS("deaths_averted_map.Rds")
ggsave("figure3.png", deaths_averted_map,
       width = 10, height = 5)
ggsave("figure3.pdf", deaths_averted_map,
       width = 10, height = 5)

#log vaccine/log deaths averted figure
dot_plots <- readRDS("dot_plots.Rds")
ggsave("figure4.png", dot_plots$vacc,
       width = 7, height = 5)
ggsave("figure4.pdf", dot_plots$vacc,
       width = 7, height = 5)
