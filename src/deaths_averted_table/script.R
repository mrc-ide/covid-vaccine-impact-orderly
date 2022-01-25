
if(!is.na(seed)){
  set.seed(seed)
}

if(excess){
  cf <- c("No Vaccines", "COVAX")
} else {
  cf <- "No Vaccines"
}

###Load data:
table1_df_overall <- loadCounterfactualData(cf,
                                            group_by = NULL,
                                            exclude_iso3cs = exclude_iso3cs)
table1_df_income <- loadCounterfactualData(cf,
                                           group_by = "income_group",
                                           exclude_iso3cs = exclude_iso3cs)

table1_df_who <- loadCounterfactualData(cf,
                                        group_by = "who_region",
                                        exclude_iso3cs = exclude_iso3cs)
table1_df_vaccine <- readRDS(
  "counterfactuals.Rds"
)%>%
  rename(vaccines = `Baseline (Total Vaccines)`) %>%
  select(iso3c, vaccines) %>%
  filter(!(iso3c %in% exclude_iso3cs)) %>%
  left_join(
    squire::population %>%
      group_by(iso3c) %>%
      summarise(
        population = sum(n)
      )
  )
table1_df_vaccine_covax <- table1_df_vaccine %>%
  filter(iso3c %in% get_covax_iso3c())

table1_df_vaccine <- table1_df_vaccine %>%
  mutate(counterfactual = "No Vaccines") %>%
  rbind(table1_df_vaccine_covax %>%
          mutate(
            counterfactual = "COVAX"
          )
  ) %>%
  mutate(ind = "Worldwide") %>%
  rbind(table1_df_vaccine %>%
          mutate(counterfactual = "No Vaccines") %>%
          rbind(
            table1_df_vaccine_covax %>%
              mutate(counterfactual = "COVAX")
          ) %>%
          mutate(
            ind = get_income_group(iso3c)
            )
        ) %>%
  rbind(table1_df_vaccine %>%
          mutate(counterfactual = "No Vaccines") %>%
          rbind(
            table1_df_vaccine_covax %>%
              mutate(counterfactual = "COVAX")
          ) %>%
          mutate(
            ind = get_WHO_region(iso3c)
          )
        ) %>%
  group_by(ind, counterfactual) %>%
  summarise(
    vaccines = sum(vaccines),
    population = sum(population),
    .groups = "keep"
  )

#function to convert to text
writeText <- function(row, name){
  if(is.na(row[[paste0(name,"_avg")]])){
    ""
  } else{
    format_func <- function(x){
      format(signif(x, digits = 4), scientific = FALSE, digits = 4, big.mark = ",")
    }
    paste0(
      format_func(row[[paste0(name,"_avg")]]),
      " (",
      format_func(row[[paste0(name,"_025")]]),
      " - ",
      format_func(row[[paste0(name,"_975")]]),
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
  #correct the sign of the covax data due to counterfacual framework
  mutate(
    averted_deaths_avg = if_else(
      counterfactual == "COVAX",
      -averted_deaths_avg,
      averted_deaths_avg
    ),
    old_averted_025 = averted_deaths_025,
    averted_deaths_025 = if_else(
      counterfactual == "COVAX",
      -averted_deaths_975,
      averted_deaths_025
    ),
    averted_deaths_975 = if_else(
      counterfactual == "COVAX",
      -old_averted_025,
      averted_deaths_975
    )
  ) %>%
  select(` `, counterfactual, averted_deaths_025, averted_deaths_avg, averted_deaths_975) %>%
  #Add population and vaccine data
  left_join(
    table1_df_vaccine %>%
      rename(` ` = ind)
  ) %>%
  rename(
    `_025` = averted_deaths_025,
    `_avg` = averted_deaths_avg,
    `_975` = averted_deaths_975
  ) %>%
  mutate(across(.col = starts_with("_"), list(
    per_pop = ~ .x/population * 10000,
    per_vacc = ~ .x/vaccines * 10000
  ),
  .names = "_{.fn}{.col}")) %>%
  #pivot to extra columns
  pivot_wider(id_cols = ` `, names_from = counterfactual,
              values_from = contains("_"), names_glue = "{counterfactual}{.value}") %>%
  mutate(
  ` ` = if_else(
    ` ` %in% c("by Income-Group:", "by WHO-Region:", "Worldwide"),
    ` `,
    paste0("   ", ` `)
  )
  ) %>%
  rowwise()
if(excess){
  table1 <- table1 %>%
    mutate(`Deaths Averted by Vaccinations` = writeText(.data, "No Vaccines"),
           `Deaths Averted by Vaccinations\nPer 10k People` = writeText(.data, "No Vaccines_per_pop"),
           `Deaths Averted by Vaccinations\nPer 10k Vaccines` = writeText(.data, "No Vaccines_per_vacc"),
           `Deaths Reduced if COVAX Targets met` = writeText(.data, "COVAX"),
           `Deaths Reduced if COVAX Targets met\nPer 10k People` = writeText(.data, "COVAX_per_pop"),
           `Deaths Reduced if COVAX Targets met\nPer 10k Vaccines` = writeText(.data, "COVAX_per_vacc")) %>%
    select(` `, `Deaths Averted by Vaccinations`,
           `Deaths Averted by Vaccinations\nPer 10k People`,
           `Deaths Averted by Vaccinations\nPer 10k Vaccines`,
           `Deaths Reduced if COVAX Targets met`,
           `Deaths Reduced if COVAX Targets met\nPer 10k People`,
           `Deaths Reduced if COVAX Targets met\nPer 10k Vaccines`)
} else {
  table1 <- table1 %>%
    mutate(`Deaths Averted by Vaccinations` = writeText(.data, "No Vaccines"),
           `Deaths Averted by Vaccinations\nPer 10k People` = writeText(.data, "No Vaccines_per_pop"),
           `Deaths Averted by Vaccinations\nPer 10k Vaccines` = writeText(.data, "No Vaccines_per_vacc")) %>%
    select(` `, `Deaths Averted by Vaccinations`,
           `Deaths Averted by Vaccinations\nPer 10k People`,
           `Deaths Averted by Vaccinations\nPer 10k Vaccines`)
}


saveRDS(table1, "averted_table.Rds")


