
if(!is.na(seed)){
  set.seed(seed)
}

###Load data:
#need to compare on the intersection of countries in both
iso3c_reported <- readRDS("Baseline_reported.Rds") %>% pull(iso3c) %>% unique()
iso3c_excess <- readRDS("Baseline_excess.Rds") %>% pull(iso3c) %>% unique()
iso3cs <- intersect(iso3c_excess, iso3c_reported)
iso3cs_drop_reported <- setdiff(iso3c_reported, iso3cs)
iso3cs_drop_excess <- setdiff(iso3c_excess, iso3cs)
#we'll need to rename the baseline file for each version
file.copy("Baseline_reported.Rds", "Baseline.Rds", overwrite = TRUE)
df_reported_total <- loadCounterfactualData(c("No Vaccines_reported"),
                                            group_by = NULL,
                                            exclude_iso3cs = iso3cs_drop_reported)
df_reported_income <- loadCounterfactualData(c("No Vaccines_reported"),
                                           group_by = "income_group",
                                           exclude_iso3cs = iso3cs_drop_reported)
file.copy("Baseline_excess.Rds", "Baseline.Rds", overwrite = TRUE)
df_excess_total <- loadCounterfactualData(c("No Vaccines_excess"),
                                            group_by = NULL,
                                            exclude_iso3cs = iso3cs_drop_excess)
df_excess_income <- loadCounterfactualData(c("No Vaccines_excess"),
                                             group_by = "income_group",
                                             exclude_iso3cs = iso3cs_drop_excess)
unlink("Baseline.Rds")

#merge data
df <-
  rbind(df_reported_total, df_excess_total) %>%
  mutate(income_group = "Total") %>%
  rbind(df_reported_income, df_excess_income) %>%
  mutate(
    counterfactual = if_else(
      counterfactual == "No Vaccines_excess",
      "Excess Mortality",
      "Reported Deaths"
    )
  ) %>%
  rename(
    `Data:` = counterfactual
  )
#create bar plots to compare
total_bar <- ggplot(df) +
  geom_col(aes(x = fct_reorder(income_group, case_when(income_group == "Total" ~ 1,
                                                       income_group == "HIC" ~ 2,
                                                       income_group == "UMIC" ~ 3,
                                                       income_group == "LMIC" ~ 4,
                                                       income_group == "LIC" ~ 5,)), fill = `Data:`, y = averted_deaths_avg),
           position = "dodge") +
  labs(x = "Income Group", y = "Median Deaths Averted by Vaccinations") +
  ggpubr::theme_pubr()

df <- df  %>%
  filter(income_group != "Total") %>%
  left_join(
    df %>%
      filter(income_group == "Total") %>%
      select(`Data:`, averted_deaths_avg) %>%
      rename(averted_deaths_total = averted_deaths_avg),
    by = "Data:"
  ) %>%
  mutate(averted_deaths_avg = averted_deaths_avg/averted_deaths_total)

relative_bar <- ggplot(df) +
  geom_col(aes(x = fct_reorder(income_group, case_when(income_group == "Total" ~ 1,
                                                       income_group == "HIC" ~ 2,
                                                       income_group == "UMIC" ~ 3,
                                                       income_group == "LMIC" ~ 4,
                                                       income_group == "LIC" ~ 5,)), fill = `Data:`, y = averted_deaths_avg),
           position = "dodge") +
  labs(x = "Income Group", y = "Percentage of Total Deaths") +
  ggpubr::theme_pubr()

#save objects
saveRDS(
  list(
    total = total_bar,
    relative = relative_bar
  ),
  "reported_excess_comparison.Rds"
)
