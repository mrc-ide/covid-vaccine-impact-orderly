if(!is.na(seed)){
  set.seed(seed)
}
#more complex script to calculate percentage extra deaths averted
n_replicates <- readRDS("COVAX.Rds") %>% pull(replicate) %>% unique() %>%
  length()
iso3cs <- readRDS("Baseline.Rds") %>% pull(iso3c) %>% unique()
#set up our summarised data frame ready to go
total_data <- readRDS("No Vaccines.Rds") %>%
  group_by(iso3c, replicate) %>%
  summarise(deaths = sum(deaths), .groups = "keep") %>%
  full_join(readRDS("Baseline.Rds") %>%
              group_by(iso3c, replicate) %>%
              summarise(baseline_deaths = sum(deaths), .groups = "keep"),
            by = c("iso3c", "replicate")
            ) %>%
  transmute(
    averted = deaths - baseline_deaths,
    iso3c = iso3c,
    replicate = replicate
  )
covax_data <- readRDS("COVAX.Rds") %>%
  group_by(iso3c, replicate) %>%
  summarise(deaths = sum(deaths), .groups = "keep") %>%
  left_join(readRDS("Baseline.Rds") %>%
              group_by(iso3c, replicate) %>%
              summarise(baseline_deaths = sum(deaths), .groups = "keep"),
            by = c("iso3c", "replicate")
  ) %>%
  transmute(
    averted = - deaths + baseline_deaths,
    iso3c = iso3c,
    replicate = replicate
  )
who_data <- readRDS("who.Rds") %>%
  group_by(iso3c, replicate) %>%
  summarise(deaths = sum(deaths), .groups = "keep") %>%
  left_join(readRDS("Baseline.Rds") %>%
              group_by(iso3c, replicate) %>%
              summarise(baseline_deaths = sum(deaths), .groups = "keep"),
            by = c("iso3c", "replicate")
  ) %>%
  transmute(
    averted = - deaths + baseline_deaths,
    iso3c = iso3c,
    replicate = replicate
  )
reduce_df <- function(df, reps){
  df %>%
    filter(replicate == reps[iso3c]) %>%
    ungroup()
}
get_averted_final <- function(df){
  df %>%
    summarise(
      averted = sum(averted)
    ) %>%
    pull(averted)
}
incs <- c("HIC", "UMIC", "LMIC", "LIC")
get_averted_income <- function(df){
  map_dbl(incs,
          ~get_averted_final(
            filter(df, get_income_group(iso3c) == .x)
          )
  )
}
whos <- c("AFR", "AMR", "EMR", "EUR", "SEAR", "WPR")
get_averted_who <- function(df){
  map_dbl(whos,
          ~get_averted_final(
            filter(df, get_WHO_region(iso3c) == .x)
          )
  )
}
#generate our 2000 random samples
percentage_deaths_averted <- map_dfr(seq_len(2000), function(it){
  reps <- sample(n_replicates, length(iso3cs), replace = TRUE)
  names(reps) <- iso3cs
  #reduce data sets
  total_data <- reduce_df(total_data, reps)
  covax_data <- reduce_df(covax_data, reps)
  who_data <- reduce_df(who_data, reps)

  averted <- c(get_averted_final(total_data), get_averted_income(total_data), get_averted_who(total_data))

  tibble(
    measure = c("Worldwide", incs, whos),
    covax_p_averted =
      c(get_averted_final(covax_data), get_averted_income(covax_data), get_averted_who(covax_data))/averted,
    who_p_averted =
      c(get_averted_final(who_data), get_averted_income(who_data), get_averted_who(who_data))/averted
  )
}) %>%
#summarise and format
  group_by(measure) %>%
  summarise(
    covax_p_averted = paste0(
      signif(median(covax_p_averted) * 100, 3), "% (",
      signif(quantile(covax_p_averted, 0.025) * 100, 3), "%, ",
      signif(quantile(covax_p_averted, 0.975) * 100, 3), "%)"
    ),
    who_p_averted = paste0(
      signif(median(who_p_averted) * 100, 3), "% (",
      signif(quantile(who_p_averted, 0.025) * 100, 3), "%, ",
      signif(quantile(who_p_averted, 0.975) * 100, 3), "%)"
    )
  )

#add number of countries
percentage_deaths_averted <- percentage_deaths_averted %>%
  full_join(readRDS("counterfactuals.Rds") %>%
  summarise(
    n_fail_covax = sum(!is.na(COVAX)),
    n_fail_who = sum(!is.na(WHO))
  ) %>%
  mutate(measure = "Worldwide") %>%
  rbind(
    readRDS("counterfactuals.Rds") %>%
      mutate(measure = get_income_group(iso3c)) %>%
      group_by(measure) %>%
      summarise(
        n_fail_covax = sum(!is.na(COVAX)),
        n_fail_who = sum(!is.na(WHO))
      )
  ) %>%
  rbind(
    readRDS("counterfactuals.Rds") %>%
      mutate(measure = get_WHO_region(iso3c)) %>%
      group_by(measure) %>%
      summarise(
        n_fail_covax = sum(!is.na(COVAX)),
        n_fail_who = sum(!is.na(WHO))
      )
  ))

#save
saveRDS(percentage_deaths_averted, "COVAX_WHO_percentages.Rds")
