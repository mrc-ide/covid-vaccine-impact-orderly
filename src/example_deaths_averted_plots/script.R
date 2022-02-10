
if(!is.na(seed)){
  set.seed(seed)
}

iso3cs <- readRDS("counterfactuals.Rds") %>%
  pull(iso3c)

example_iso3cs <- c("USA")

exclude_iso3cs <- c(setdiff(iso3cs, example_iso3cs))

###Load data:
df <- loadCounterfactualData(c("No Vaccines", "Baseline_direct"),
                                            group_by = c("iso3c", "date"),
                                            exclude_iso3cs = exclude_iso3cs)
df_replicate <- loadCounterfactualData(c("No Vaccines", "Baseline_direct"),
                             group_by = c("iso3c", "date", "replicate"),
                             exclude_iso3cs = exclude_iso3cs) %>%
  filter(replicate == 1)

plots <- map(example_iso3cs, ~create_example_plot(.x, "2021-01-01", df, df_replicate))
names(plots) <- example_iso3cs
saveRDS(plots, "example_plots.Rds")
