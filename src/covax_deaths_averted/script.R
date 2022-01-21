
if(!is.na(seed)){
  set.seed(seed)
}

###Load data:
covax_iso3cs <- get_covax_iso3c()
table1_df_overall <- loadCounterfactualData(c("COVAX"),
                                            group_by = NULL,
                                            exclude_iso3cs = exclude_iso3cs)
table1_df_income <- loadCounterfactualData(c("COVAX"),
                                           group_by = "income_group",
                                           exclude_iso3cs = exclude_iso3cs)

table1_df_who <- loadCounterfactualData(c("COVAX"),
                                        group_by = "who_region",
                                        exclude_iso3cs = exclude_iso3cs)
table1_df_vaccine <- readRDS(
  "counterfactuals.Rds"
)%>%
  rename(vaccines = `Baseline (Total Vaccines)`) %>%
  select(iso3c, vaccines) %>%
  filter(iso3c %in% covax_iso3cs & !(iso3c %in% exclude_iso3cs)) %>%
  left_join(
    squire::population %>%
      group_by(iso3c) %>%
      summarise(
        population = sum(n)
      )
  )
table1_df_vaccine <- table1_df_vaccine %>%
  mutate(ind = "Worldwide") %>%
  rbind(table1_df_vaccine %>%
          mutate(
            ind = get_income_group(iso3c)
            )
        ) %>%
  rbind(table1_df_vaccine %>%
          mutate(
            ind = get_WHO_region(iso3c)
          )
        ) %>%
  group_by(ind) %>%
  summarise(
    vaccines = sum(vaccines),
    population = sum(population)
  )

#function to convert to text
writeText <- function(row, name){
  if(is.na(row[[paste0(name,"_avg")]])){
    ""
  } else{
    paste0(
      format(-row[[paste0(name,"_avg")]], scientific = FALSE, digits = 4, big.mark = ","),
      " (",
      format(-row[[paste0(name,"_025")]], scientific = FALSE, digits = 4, big.mark = ","),
      " - ",
      format(-row[[paste0(name,"_975")]], scientific = FALSE, digits = 4, big.mark = ","),
      ")"
    )
  }
}

###table 1 overall/summary counts:
table1 <- table1_df_overall %>%
  mutate(` ` = "Worldwide") %>%
  add_row(` ` = "by Income-Group:") %>%
  rbind(
    table1_df_income %>%
      rename(` ` = income_group)
  ) %>%
  add_row(` ` = "by WHO-Region:") %>%
  rbind(
    table1_df_who %>%
      rename(` ` = who_region)
  ) %>%
  select(` `, averted_deaths_025, averted_deaths_avg, averted_deaths_975) %>%
  #Add population and vaccine data
  left_join(
    table1_df_vaccine %>%
      rename(` ` = ind)
  ) %>%
  mutate(across(.col = starts_with("averted_deaths_"), list(
    per_pop = ~ .x/population * 10000,
    per_vacc = ~ .x/vaccines * 10000
  ),
  .names = "{.fn}_{.col}"),
  ` ` = if_else(
    ` ` %in% c("by Income-Group:", "by WHO-Region:", "Worldwide"),
    ` `,
    paste0("   ", ` `)
  )
  ) %>%
  rowwise() %>%
  mutate(`Reduction in Deaths` = writeText(.data, "averted_deaths"),
         `Reduction in Deaths\nPer 10k People` = writeText(.data, "per_pop_averted_deaths"),
         `Reduction in Deaths\nPer 10k Vaccines` = writeText(.data, "per_vacc_averted_deaths")) %>%
  select(` `, `Reduction in Deaths`, `Reduction in Deaths\nPer 10k People`,  `Reduction in Deaths\nPer 10k Vaccines`)


saveRDS(table1, "covax_averted_table.Rds")
