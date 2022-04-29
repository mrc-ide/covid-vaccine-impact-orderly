if(!is.na(seed)){
  set.seed(seed)
}

df_overall <- loadCounterfactualData(c("No Vaccines", "Baseline-Direct"),
                                     group_by = NULL
)

#get deaths averted due to direct protection, on (close to) average
total <- df_overall %>% filter(counterfactual == "No Vaccines") %>% pull(averted_deaths_avg)
indirect <-  df_overall %>% filter(counterfactual == "Baseline-Direct") %>% pull(averted_deaths_avg)
percent_direct <- (total-indirect)/total * 100

#get deaths in 2021
all_iso3c_all_reps <- readRDS("No Vaccines.Rds") %>%
  filter(date > as.Date("2020-12-08") &
           date <= as.Date("2021-12-08")) %>%
  group_by(iso3c, replicate) %>%
  summarise(deaths = sum(deaths))
#add country numbers
all_iso3c_all_reps <- all_iso3c_all_reps %>%
  group_by(iso3c) %>%
  mutate(country_num  = which(unique(iso3c) == unique(all_iso3c_all_reps$iso3c))) %>%
  ungroup()

#randomly combine
reps = unique(all_iso3c_all_reps$replicate)
n_iso = length(unique(all_iso3c_all_reps$iso3c))
deaths <- map(seq(50000), function(x){
  replicates <- sample(reps, size = n_iso, replace = TRUE)
  #calculate
  all_iso3c_all_reps %>%
    filter(
      replicate == replicates[country_num]
    ) %>%
    pull(deaths) %>%
    sum()
}) %>%
  unlist()
rm(all_iso3c_all_reps)

#repeat for reported deaths
all_iso3c_all_reps <- readRDS("No Vaccines_reported.Rds") %>%
  filter(date > as.Date("2020-12-08") &
           date <= as.Date("2021-12-08")) %>%
  group_by(iso3c, replicate) %>%
  summarise(deaths = sum(deaths))
#add country numbers
all_iso3c_all_reps <- all_iso3c_all_reps %>%
  group_by(iso3c) %>%
  mutate(country_num  = which(unique(iso3c) == unique(all_iso3c_all_reps$iso3c))) %>%
  ungroup()

#randomly combine
reps = unique(all_iso3c_all_reps$replicate)
n_iso = length(unique(all_iso3c_all_reps$iso3c))
deaths_reported <- map(seq(50000), function(x){
  replicates <- sample(reps, size = n_iso, replace = TRUE)
  #calculate
  all_iso3c_all_reps %>%
    filter(
      replicate == replicates[country_num]
    ) %>%
    pull(deaths) %>%
    sum()
}) %>%
  unlist()
rm(all_iso3c_all_reps)

#deaths averted in covax countries
non_covax_iso3cs <- c(setdiff(readRDS("counterfactuals.Rds") %>% pull(iso3c),
                              get_covax_iso3c())) %>%
  unique()
covax_deaths_averted <- loadCounterfactualData(
  c("No Vaccines"),
  group_by = NULL,
  exclude_iso3cs = non_covax_iso3cs
)

saveRDS(
  data.frame(
    percent_averted_direct = percent_direct,
    percent_averted_direct_denom = total,
    percent_averted_direct_numer = total-indirect,
    no_vacc_deaths_2021_excess = median(deaths),
    no_vacc_deaths_2021_025_excess = quantile(deaths, 0.025),
    no_vacc_deaths_2021_975_excess = quantile(deaths, 0.975),
    no_vacc_deaths_2021_reported = median(deaths_reported),
    no_vacc_deaths_2021_025_reported = quantile(deaths_reported, 0.025),
    no_vacc_deaths_2021_975_reported = quantile(deaths_reported, 0.975),
    covax_deaths_averted = covax_deaths_averted$averted_deaths_avg,
    covax_deaths_averted_025 = covax_deaths_averted$averted_deaths_025,
    covax_deaths_averted_975 = covax_deaths_averted$averted_deaths_975,
    covax_deaths = covax_deaths_averted$deaths_avg,
    covax_deaths_025 = covax_deaths_averted$deaths_025,
    covax_deaths_975 = covax_deaths_averted$deaths_975
  ),
  "report_numbers.Rds"
)
