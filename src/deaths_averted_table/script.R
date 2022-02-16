
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
#total vaccine doses
table1_df_vaccine <- readRDS(
  "counterfactuals.Rds"
)%>%
  rename(vaccines = `Baseline (Total Vaccines)`) %>%
  select(iso3c, vaccines) %>%
  left_join(#number of people with 1+ dose
    map_dfr(readRDS("countryfits.Rds"), ~tibble(vaccinated = sum(.x$interventions$max_vaccine)),
            .id = "iso3c")
  ) %>%
  filter(!(iso3c %in% exclude_iso3cs)) %>%
  left_join(
    squire::population %>%
      group_by(iso3c) %>%
      summarise(
        population = sum(n)
      )
  )
#add new covax coverage if target met
if(excess){
  table1_df_vaccine_covax <- table1_df_vaccine %>%
    #vaccines and population does not matter as this is not used
    left_join(
      readRDS(
        "counterfactuals.Rds"
      ) %>%
        select(iso3c, COVAX)
    ) %>%
    #update
    mutate(
      vaccinated = if_else(
        is.na(COVAX),
        vaccinated,
        COVAX
      )
    ) %>%
    select(!COVAX)
} else {
  table1_df_vaccine_covax <- table1_df_vaccine
  #placeholder not used
}

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
    vaccinated = sum(vaccinated),
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
format_coverage <- function(x){
  if_else(is.na(x), "", paste0(as.character(signif(x * 100, digits = 3)),
         "%"))
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
  select(` `, counterfactual, averted_deaths_025, averted_deaths_avg, averted_deaths_975,
         baseline_deaths_avg, baseline_deaths_025, baseline_deaths_975) %>%
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
    #calculate averted per vaccine dose
    per_vacc = ~ .x/vaccines * 10000
  ),
  .names = "_{.fn}{.col}")) %>%
  #pivot to extra columns
  pivot_wider(id_cols = ` `, names_from = counterfactual,
              values_from = c(vaccinated, population, contains("_")), names_glue = "{counterfactual}{.value}") %>%
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
    mutate(`Total Deaths,\nwith vaccinations` = writeText(.data, "No Vaccinesbaseline_deaths"),
           `Deaths Averted by Vaccinations` = writeText(.data, "No Vaccines"),
           `Deaths Averted by Vaccinations\nPer 10k People` = writeText(.data, "No Vaccines_per_pop"),
           `Deaths Averted by Vaccinations\nPer 10k Vaccines` = writeText(.data, "No Vaccines_per_vacc"),
           `Deaths Reduced if COVAX Targets met` = writeText(.data, "COVAX"),
           #calculate coverage of 1+dose
           `Vaccination Coverage` = format_coverage(`No Vaccinesvaccinated`/`No Vaccinespopulation`),
           `Increase in Vaccination Coverage\nif COVAX targets met` = format_coverage((`COVAXvaccinated`/`COVAXpopulation`)/
                                                                                        (`No Vaccinesvaccinated`/`No Vaccinespopulation`) -
                                                                                        1)) %>%
    select(` `,`Total Deaths,\nwith vaccinations`, `Deaths Averted by Vaccinations`,
           `Deaths Averted by Vaccinations\nPer 10k People`,
           `Deaths Averted by Vaccinations\nPer 10k Vaccines`,
           `Vaccination Coverage`,
           `Deaths Reduced if COVAX Targets met`,
           `Increase in Vaccination Coverage\nif COVAX targets met`) %>%
    #correct the coverage increase in HICs for COVAX
    mutate(
      `Increase in Vaccination Coverage\nif COVAX targets met` = if_else(
        ` ` == "   HIC" & `Increase in Vaccination Coverage\nif COVAX targets met` == "",
        "0%",
        `Increase in Vaccination Coverage\nif COVAX targets met`
      )
    )
} else {
  table1 <- table1 %>%
    mutate(`Total Deaths,\nwith vaccinations` = writeText(.data, "No Vaccinesbaseline_deaths"),
           `Deaths Averted by Vaccinations` = writeText(.data, "No Vaccines"),
           `Deaths Averted by Vaccinations\nPer 10k People` = writeText(.data, "No Vaccines_per_pop"),
           `Deaths Averted by Vaccinations\nPer 10k Vaccines` = writeText(.data, "No Vaccines_per_vacc"),
           #calculate coverage of 1+dose
           `Vaccination Coverage` = format_coverage(`No Vaccinesvaccinated`/`No Vaccinespopulation`)) %>%
    select(` `, `Total Deaths,\nwith vaccinations`, `Deaths Averted by Vaccinations`,
           `Deaths Averted by Vaccinations\nPer 10k People`,
           `Deaths Averted by Vaccinations\nPer 10k Vaccines`,
           `Vaccination Coverage`)
}


saveRDS(table1, "averted_table.Rds")


