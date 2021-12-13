
if(!seed == ""){
  set.seed(seed)
}

#vector of iso3cs of bad fits
bad_fits <- c(
)


###Load data:
df_overall <- loadCounterfactualData(c("No Vaccines"),
                                            group_by = NULL,
                                     exclude_iso3cs = "CHN")
# df_overall_good <- loadCounterfactualData(c("No Vaccines"),
#                                      group_by = NULL,
#                                      exclude_iso3cs = c(bad_fits, "CHN"))
df_income <- loadCounterfactualData(c("No Vaccines"),
                                           group_by = "income_group",
                                    exclude_iso3cs = "CHN")
# df_income_good <- loadCounterfactualData(c("No Vaccines"),
#                                     group_by = "income_group",
#                                     exclude_iso3cs = c(bad_fits, "CHN"))
df_who <- loadCounterfactualData(c("No Vaccines"),
                                        group_by = "who_region",
                                 exclude_iso3cs = "CHN")
# df_who_good <- loadCounterfactualData(c("No Vaccines"),
#                                  group_by = "who_region",
#                                  exclude_iso3cs = c(bad_fits, "CHN"))
df_ind <- loadCounterfactualData(c("No Vaccines"),
                                 group_by = "iso3c",
                                 exclude_iso3cs = "CHN") %>%
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
  mutate(ind = "Worldwide",
         `Quality of fit(s):` = "-") %>%
  rbind(df_vaccine %>%
          mutate(
            ind = get_income_group(iso3c),
            `Quality of fit(s):` = "-"
            )
        ) %>%
  rbind(df_vaccine %>%
          mutate(
            ind = get_WHO_region(iso3c),
            `Quality of fit(s):` = "-"
          )
        ) %>%
  rbind(df_vaccine %>%
          mutate(
            ind = get_income_group(iso3c),
            `Quality of fit(s):` = "Good"
          ) %>%
          filter(!iso3c %in% bad_fits)
  ) %>%
  rbind(df_vaccine %>%
          mutate(
            ind = get_WHO_region(iso3c),
            `Quality of fit(s):` = "Good"
          ) %>%
          filter(!iso3c %in% bad_fits)
  ) %>%
  rbind(
    df_vaccine %>%
      mutate(ind = "Worldwide",
             `Quality of fit(s):` = "Good") %>%
      filter(!iso3c %in% bad_fits)
  ) %>%
  rbind(df_vaccine %>%
          mutate(
            ind = iso3c,
            `Quality of fit(s):` = if_else(
              iso3c %in% bad_fits,
              "Poor",
              "Good"
            )
          )
  ) %>%
  group_by(ind, `Quality of fit(s):`) %>%
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
  mutate(
    `Quality of fit(s):` = "-"
  ) %>%
  # rbind(
  #   rbind(
  #     df_overall_good %>%
  #       mutate(` ` = "Worldwide"),
  #     df_income_good %>%
  #       rename(` ` = income_group),
  #     df_who_good %>%
  #       rename(` ` = who_region)
  #   ) %>%
  #     mutate(
  #       `Quality of fit(s):` = "Good"
  #     )
  # ) %>%
  rbind(
    df_ind %>%
      mutate(`Quality of fit(s):` = if_else(
        iso3c %in% bad_fits,
        "Poor",
        "Good"
      )) %>%
      rename(` ` = iso3c)
  ) %>%
  select(` `, `Quality of fit(s):`, baseline_deaths_avg, baseline_deaths_025,
         baseline_deaths_975, averted_deaths_025, averted_deaths_avg, averted_deaths_975) %>%
  #Add population and vaccine data
  left_join(
    df_vaccine %>%
      rename(` ` = ind)
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
  rowwise() %>%
  mutate(#neaten
    `Modelled Deaths` = writeText(.data, "baseline_deaths"),
    `Averted Deaths` = writeText(.data, "averted_deaths"),
    `Averted Deaths Per 10k People` = writeText(.data, "per_pop_averted_deaths"),
    `Averted Deaths Per 10k Vaccines` = writeText(.data, "per_vacc_averted_deaths")
  ) %>%
  select(` `, `Quality of fit(s):`, `Modelled Deaths`, `Averted Deaths`, `Averted Deaths Per 10k People`, `Averted Deaths Per 10k Vaccines`)


readr::write_csv(df, "summary_table.csv")


