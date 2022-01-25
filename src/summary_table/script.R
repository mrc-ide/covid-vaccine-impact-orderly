
if(!seed == ""){
  set.seed(seed)
}

if(excess){
  cf <- c("No Vaccines", "COVAX")
} else {
  cf <- "No Vaccines"
}

#vector of iso3cs of bad fits
bad_fits <- c(
  "ARE", "IRQ", "SAU", "PER", "NAM"
)


###Load data:
df_overall <- loadCounterfactualData(cf,
                                     group_by = NULL,
                                     exclude_iso3cs = exclude_iso3cs)
df_income <- loadCounterfactualData(cf,
                                    group_by = "income_group",
                                    exclude_iso3cs = exclude_iso3cs)
df_who <- loadCounterfactualData(cf,
                                 group_by = "who_region",
                                 exclude_iso3cs = exclude_iso3cs)
df_ind <- loadCounterfactualData(cf,
                                 group_by = "iso3c",
                                 exclude_iso3cs = exclude_iso3cs) %>%
  select(!country)

df_vaccine <- readRDS(
  "counterfactuals.Rds"
)%>%
  filter(iso3c %in% df_ind$iso3c) %>%
  rename(vaccines = `Baseline (Total Vaccines)`) %>%
  select(iso3c, vaccines) %>%
  left_join(
    squire::population %>%
      group_by(iso3c) %>%
      summarise(
        population = sum(n)
      )
  )
df_vaccine <- df_vaccine %>%
  mutate(counterfactual = "No Vaccines") %>%
  rbind(df_vaccine %>%
        filter(iso3c %in% get_covax_iso3c()) %>%
          mutate(counterfactual = "COVAX")
        )

df_vaccine <- df_vaccine %>%
  mutate(ind = "Worldwide") %>%
  rbind(df_vaccine %>%
          mutate(
            ind = get_income_group(iso3c)
            )
        ) %>%
  rbind(df_vaccine %>%
          mutate(
            ind = get_WHO_region(iso3c)
          )
        ) %>%
  rbind(df_vaccine %>%
          mutate(
            ind = iso3c
          )
  ) %>%
  group_by(ind, counterfactual) %>%
  summarise(
    vaccines = sum(vaccines, na.rm =  TRUE),
    population = sum(population)
  )

#combine together
writeText <- function(row, name){
  if(is.na(row[[paste0(name,"_avg")]])){
    ""
  } else{
    paste0(
      format(row[[paste0(name,"_avg")]], scientific = FALSE, digits = 4, big.mark = ","),
      " (",
      format(row[[paste0(name,"_025")]], scientific = FALSE, digits = 4, big.mark = ","),
      " - ",
      format(row[[paste0(name,"_975")]], scientific = FALSE, digits = 4, big.mark = ","),
      ")"
    )
  }
}
###table 1 overall/summary counts:
df <- df_overall %>%
  mutate(` ` = "Worldwide") %>%
  rbind(
    df_income %>%
      rename(` ` = income_group)
  ) %>%
  rbind(
    df_who %>%
      rename(` ` = who_region)
  ) %>%
  rbind(
    df_ind %>%
      rename(` ` = iso3c)
  ) %>%
  select(` `, counterfactual, baseline_deaths_avg, baseline_deaths_025,
         baseline_deaths_975, averted_deaths_025, averted_deaths_avg, averted_deaths_975) %>%
  #Add population and vaccine data
  left_join(
    df_vaccine %>%
      rename(` ` = ind)
  ) %>%
  #swap sign on COVAX
  mutate(
    averted_deaths_avg = if_else(
      counterfactual == "COVAX",
      -averted_deaths_avg,
      averted_deaths_avg
    ),
    old_025 = averted_deaths_025,
    averted_deaths_025 = if_else(
      counterfactual == "COVAX",
      -averted_deaths_975,
      averted_deaths_025
    ),
    averted_deaths_975 = if_else(
      counterfactual == "COVAX",
      -old_025,
      averted_deaths_975
    )
  ) %>%
  mutate(across(
    .col = starts_with("averted_deaths_"),
    list(
      per_pop = ~ .x/population * 10000,
      per_vacc = ~ .x/vaccines * 10000
    ),
    .names = "{.fn}_{.col}"),
  ` ` = case_when(
    ` ` %in% df_ind$iso3c ~ countrycode::countrycode(` `, origin = "iso3c", destination = "country.name"),
    TRUE ~ ` `
  )
  ) %>%
  #pivot COVAX into own column
  pivot_wider(id_cols = c(` `),
              names_from = counterfactual,
              values_from = contains("deaths"),
              names_glue = "{counterfactual}_{.value}") %>%
  rowwise()
if(excess){
  df <- df %>% mutate(#neaten
    `Modelled Deaths` = writeText(.data, "No Vaccines_baseline_deaths"),
    `Averted Deaths` = writeText(.data, "No Vaccines_averted_deaths"),
    `Averted Deaths Per 10k People` = writeText(.data, "No Vaccines_per_pop_averted_deaths"),
    `Averted Deaths Per 10k Vaccines` = writeText(.data, "No Vaccines_per_vacc_averted_deaths"),
    `Modelled Deaths in COVAX Countries` = writeText(.data, "COVAX_baseline_deaths"),
    `Reduction in Deaths if COVAX target met` = writeText(.data, "COVAX_averted_deaths"),
    `Reduction in Deaths if COVAX target met Per 10k People` = writeText(.data, "COVAX_per_pop_averted_deaths"),
    `Reduction in Deaths if COVAX target met Per 10k Vaccines` = writeText(.data, "COVAX_per_vacc_averted_deaths")
  ) %>%
    select(` `, `Modelled Deaths`, `Averted Deaths`,
           `Averted Deaths Per 10k People`, `Averted Deaths Per 10k Vaccines`,
           `Modelled Deaths in COVAX Countries`,
           `Reduction in Deaths if COVAX target met`,
           `Reduction in Deaths if COVAX target met Per 10k People`,
           `Reduction in Deaths if COVAX target met Per 10k Vaccines`
    ) %>%
    mutate(
      #fill info for COVAX countries that didn't need more vaccines
      country_in_covax = countrycode(` `, origin = "country.name", destination = "iso3c") %in% get_covax_iso3c(),
      `Notes:` = if_else(
        country_in_covax & `Modelled Deaths in COVAX Countries` == "",
        "Country met targets for 2021, so COVAX counterfactual not modelled",
        ""
      ),
      `Modelled Deaths in COVAX Countries` = if_else(
        country_in_covax & `Modelled Deaths in COVAX Countries` == "",
        `Modelled Deaths`,
        `Modelled Deaths in COVAX Countries`
      )
    ) %>%
    select(!country_in_covax)
} else {
  df <- df %>% mutate(#neaten
    `Modelled Deaths` = writeText(.data, "No Vaccines_baseline_deaths"),
    `Averted Deaths` = writeText(.data, "No Vaccines_averted_deaths"),
    `Averted Deaths Per 10k People` = writeText(.data, "No Vaccines_per_pop_averted_deaths"),
    `Averted Deaths Per 10k Vaccines` = writeText(.data, "No Vaccines_per_vacc_averted_deaths")
  ) %>%
    select(` `, `Modelled Deaths`, `Averted Deaths`,
           `Averted Deaths Per 10k People`, `Averted Deaths Per 10k Vaccines`
    ) %>%
    mutate(`Notes:` = "")
}
#add note for poor fits
df <- df %>%
  ungroup() %>%
  mutate(`Notes:` = if_else(
  countrycode(` `, origin = "country.name", destination = "iso3c") %in% bad_fits,
  paste0(`Notes:`, "\n", "Fit unable to recreate estimated deaths. Modelled deaths are lower than predicted excess mortality."),
  `Notes:`
)
)


readr::write_csv(df, "summary_table.csv")


