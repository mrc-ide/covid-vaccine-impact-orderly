if(!is.na(seed)){
  set.seed(seed)
}

df_overall <- loadCounterfactualData(c("No Vaccines", "Baseline-Direct"),
                                          group_by = NULL,
                                          exclude_iso3cs = exclude_iso3cs
                                          )

#get deaths averted due to direct protection, on average
total <- df_overall %>% filter(counterfactual == "No Vaccines") %>% pull(averted_deaths_avg)
indirect <-  df_overall %>% filter(counterfactual == "Baseline-Direct") %>% pull(averted_deaths_avg)
percent_direct <- (total-indirect)/total * 100

#get deaths in 2021
all_iso3c_all_reps <- readRDS("No Vaccines.Rds") %>%
  filter(date > as.Date("2020-12-08") &
           date <= as.Date("2021-12-05"),
         !(iso3c %in% exclude_iso3cs)) %>%
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

#deaths averted in covax countries
non_covax_iso3cs <- c(setdiff(readRDS("counterfactuals.Rds") %>% pull(iso3c),
                            get_covax_iso3c()), exclude_iso3cs) %>%
  unique()
covax_deaths_averted <- loadCounterfactualData(
  c("No Vaccines"),
  group_by = NULL,
  exclude_iso3cs = non_covax_iso3cs
)

#number of LIC covax not meeting target
lic_not_meet_covax <- readRDS("counterfactuals.Rds") %>%
  filter((iso3c %in% get_covax_iso3c()) &
           get_income_group(iso3c) == "LIC" &
           !is.na(`COVAX`)) %>%
  nrow()

saveRDS(
  data.frame(
    percent_averted_direct = percent_direct,
    no_vacc_deaths_2021 = median(deaths),
    no_vacc_deaths_2021_025 = quantile(deaths, 0.025),
    no_vacc_deaths_2021_975 = quantile(deaths, 0.975),
    covax_deaths = covax_deaths_averted$averted_deaths_avg,
    covax_deaths_025 = covax_deaths_averted$averted_deaths_025,
    covax_deaths_975 = covax_deaths_averted$averted_deaths_975,
    lic_not_meet_covax = lic_not_meet_covax
  ),
  "report_numbers.Rds"
)
