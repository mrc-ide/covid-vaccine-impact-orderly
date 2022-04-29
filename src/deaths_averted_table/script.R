
if(!is.na(seed)){
  set.seed(seed)
}

if(excess){
  cf <- c("No Vaccines", "COVAX", "WHO")
} else {
  cf <- "No Vaccines"
}

###Load data:
table1_df_overall <- loadCounterfactualData(cf,
                                            group_by = NULL,
                                            quantileSamples = 10000)
table1_df_income <- loadCounterfactualData(cf,
                                           group_by = "income_group")

table1_df_who <- loadCounterfactualData(cf,
                                        group_by = "who_region")

table1_df_ind <- loadCounterfactualData(cf,
                                 group_by = "iso3c") %>%
  select(!country)

#total vaccine doses
table1_df_vaccine <- readRDS(
  "counterfactuals.Rds"
)%>%
  rename(vaccines = `Baseline (Total Vaccines)`,
         vaccinated = `Baseline`) %>%
  select(iso3c, vaccines, vaccinated) %>%
  left_join(
    squire::population %>%
      group_by(iso3c) %>%
      summarise(
        population = sum(n)
      )
  )
#add new covax/who coverage if target met
if(excess){
  table1_df_vaccine_cf_vaccine <- rbind(
    table1_df_vaccine %>%
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
     select(!COVAX) %>%
      mutate(counterfactual = "COVAX"),
      table1_df_vaccine %>%
      #vaccines and population does not matter as this is not used
      left_join(
        readRDS(
           "counterfactuals.Rds"
        ) %>%
          select(iso3c, WHO)
      ) %>%
      #update
     mutate(
       vaccinated = if_else(
         is.na(WHO),
         vaccinated,
         WHO
        )
     ) %>%
    select(!WHO) %>%
      mutate(counterfactual = "WHO")
  )
} else {
  table1_df_vaccine_cf_vaccine <- table1_df_vaccine %>%
    mutate(counterfactual = "WHO")
  #placeholder not used
}

table1_df_vaccine <- table1_df_vaccine %>%
  mutate(counterfactual = "No Vaccines") %>%
  rbind(table1_df_vaccine_cf_vaccine
  ) %>%
  mutate(ind = "Worldwide") %>%
  rbind(table1_df_vaccine %>%
          mutate(counterfactual = "No Vaccines") %>%
          rbind(
            table1_df_vaccine_cf_vaccine
          ) %>%
          mutate(
            ind = get_income_group(iso3c)
            )
        ) %>%
  rbind(table1_df_vaccine %>%
          mutate(counterfactual = "No Vaccines") %>%
          rbind(
            table1_df_vaccine_cf_vaccine
          ) %>%
          mutate(
            ind = get_WHO_region(iso3c)
          )
        ) %>%
  rbind(table1_df_vaccine %>%
          mutate(counterfactual = "No Vaccines") %>%
          rbind(
            table1_df_vaccine_cf_vaccine
          ) %>%
          mutate(
            ind = iso3c
          )) %>%
  group_by(ind, counterfactual) %>%
  summarise(
    vaccines = sum(vaccines),
    vaccinated = sum(vaccinated),
    population = sum(population),
    .groups = "keep"
  )

#function to convert to text
writeText <- function(row, name, percentage = FALSE, whole = FALSE){
  if(percentage){
    percentage <- "%"
  } else {
    percentage <- ""
  }
  if(is.na(row[[paste0(name,"_avg")]])){
    ""
  } else if(whole){
    format_func <- function(x){
      case_when(x >= 10000 ~ format(signif(x, digits = 4), scientific = FALSE, digits = 4, big.mark = ","),
                x >= 10 ~ format(round(x, -1), big.mark = ","),
                TRUE ~ format(round(x), big.mark = ","))
    }
    paste0(
      paste0(format_func(row[[paste0(name,"_avg")]]), percentage),
      " (",
      paste0(format_func(row[[paste0(name,"_025")]]), percentage),
      " - ",
      paste0(format_func(row[[paste0(name,"_975")]]), percentage),
      ")"
    )
  } else {
    format_func <- function(x){
      case_when(x < 0 & x > -1 ~ rep("0", length(x)),
                TRUE ~ format(signif(x, digits = 4), scientific = FALSE, digits = 4, big.mark = ","))
    }
    paste0(
      paste0(format_func(row[[paste0(name,"_avg")]]), percentage),
      " (",
      paste0(format_func(row[[paste0(name,"_025")]]), percentage),
      " - ",
      paste0(format_func(row[[paste0(name,"_975")]]), percentage),
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
  add_row(` ` = "by Country/Admin Region:") %>%
  rbind(
    table1_df_ind %>%
      rename(` ` = iso3c)
  ) %>%
  #correct the sign of the covax data due to counterfacual framework
  mutate(
    averted_deaths_avg = if_else(
      counterfactual %in% c("COVAX", "WHO"),
      -averted_deaths_avg,
      averted_deaths_avg
    ),
    old_averted_025 = averted_deaths_025,
    averted_deaths_025 = if_else(
      counterfactual %in% c("COVAX", "WHO"),
      -averted_deaths_975,
      averted_deaths_025
    ),
    averted_deaths_975 = if_else(
      counterfactual %in% c("COVAX", "WHO"),
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
    ` ` %in% c("by Income-Group:", "by WHO-Region:", "Worldwide", "by Country/Admin Region:"),
    ` `,
    paste0("   ", ` `)
  )
  ) %>%
  rowwise() %>%
  #calculate terms
  mutate(`Total Deaths,\nwith vaccinations` = writeText(.data, "No Vaccinesbaseline_deaths", whole = TRUE),
         `Deaths Averted by Vaccinations` = writeText(.data, "No Vaccines", whole = TRUE),
         `Deaths Averted by Vaccinations\nPer 10k People` = writeText(.data, "No Vaccines_per_pop"),
         `Deaths Averted by Vaccinations\nPer 10k Vaccines` = writeText(.data, "No Vaccines_per_vacc"),
         #calculate coverage of 1+dose
         `Vaccination Coverage` = format_coverage(`No Vaccinesvaccinated`/`No Vaccinespopulation`))
if(excess){
  #extra terms for covax and who
  table1 <-
    table1 %>%
    mutate(
      `Additional Deaths Averted if COVAX Targets met` = writeText(.data, "COVAX", whole = TRUE),
      `Increase in Vaccination Coverage\nif COVAX targets met` = format_coverage((`COVAXvaccinated`/`COVAXpopulation`)/
                                                                                   (`No Vaccinesvaccinated`/`No Vaccinespopulation`) -
                                                                                   1),
      COVAX_percentage_avg = COVAX_avg/`No Vaccines_avg`*100,
      COVAX_percentage_025 = COVAX_025/`No Vaccines_avg`*100,
      COVAX_percentage_975 = COVAX_975/`No Vaccines_avg`*100,
      `Additional Deaths Averted if COVAX Targets met (%)` = writeText(.data, "COVAX_percentage", percentage = TRUE),
      `Additional Deaths Averted if WHO Targets met` = writeText(.data, "WHO", whole = TRUE),
      `Increase in Vaccination Coverage\nif WHO targets met` = format_coverage((`WHOvaccinated`/`WHOpopulation`)/
                                                                                 (`No Vaccinesvaccinated`/`No Vaccinespopulation`) -
                                                                                 1),
      WHO_percentage_avg = WHO_avg/`No Vaccines_avg`*100,
      WHO_percentage_025 = WHO_025/`No Vaccines_avg`*100,
      WHO_percentage_975 = WHO_975/`No Vaccines_avg`*100,
      `Additional Deaths Averted if WHO Targets met (%)` = writeText(.data, "WHO_percentage", percentage = TRUE)
    )

  #create first output
  output_table <- list(
    table1 = table1 %>%
      filter(!str_trim(` `) %in% c(table1_df_ind$iso3c, "by Country/Admin Region:")) %>%
    select(` `,`Total Deaths,\nwith vaccinations`,
           `Vaccination Coverage`, `Deaths Averted by Vaccinations`,
           `Deaths Averted by Vaccinations\nPer 10k People`,
           `Deaths Averted by Vaccinations\nPer 10k Vaccines`),
    table2 = table1 %>%
      filter(!str_trim(` `) %in% c(table1_df_ind$iso3c, "by Country/Admin Region:")) %>%
      select(` `,
             `Increase in Vaccination Coverage\nif COVAX targets met`,
             `Additional Deaths Averted if COVAX Targets met`,
             `Additional Deaths Averted if COVAX Targets met (%)`,
             `Increase in Vaccination Coverage\nif WHO targets met`,
             `Additional Deaths Averted if WHO Targets met`,
             `Additional Deaths Averted if WHO Targets met (%)`) %>%
      #correct the coverage increase in HICs for COVAX
      mutate(
        `Increase in Vaccination Coverage\nif COVAX targets met` = if_else(
          ` ` == "   HIC" & `Increase in Vaccination Coverage\nif COVAX targets met` == "",
          "0%",
          `Increase in Vaccination Coverage\nif COVAX targets met`
        )
      )
  )
} else {
  output_table <- table1 %>%
    filter(!str_trim(` `) %in% c(table1_df_ind$iso3c, "by Country/Admin Region:")) %>%
    mutate(`Total Deaths,\nwith vaccinations` = writeText(.data, "No Vaccinesbaseline_deaths", whole = TRUE),
           `Deaths Averted by Vaccinations` = writeText(.data, "No Vaccines", whole = TRUE),
           `Deaths Averted by Vaccinations\nPer 10k People` = writeText(.data, "No Vaccines_per_pop"),
           `Deaths Averted by Vaccinations\nPer 10k Vaccines` = writeText(.data, "No Vaccines_per_vacc"),
           #calculate coverage of 1+dose
           `Vaccination Coverage` = format_coverage(`No Vaccinesvaccinated`/`No Vaccinespopulation`)) %>%
    select(` `, `Total Deaths,\nwith vaccinations`,
           `Vaccination Coverage`, `Deaths Averted by Vaccinations`,
           `Deaths Averted by Vaccinations\nPer 10k People`,
           `Deaths Averted by Vaccinations\nPer 10k Vaccines`)
}
#save the output
saveRDS(output_table, "averted_table.Rds")

#produce summary tables

if(excess){
  df <-
    table1 %>%
    select(` `,`Total Deaths,\nwith vaccinations`,
           `Vaccination Coverage`, `Deaths Averted by Vaccinations`,
           `Deaths Averted by Vaccinations\nPer 10k People`,
           `Deaths Averted by Vaccinations\nPer 10k Vaccines`,
           `Increase in Vaccination Coverage\nif COVAX targets met`,
           `Additional Deaths Averted if COVAX Targets met`,
           `Additional Deaths Averted if COVAX Targets met (%)`,
           `Increase in Vaccination Coverage\nif WHO targets met`,
           `Additional Deaths Averted if WHO Targets met`,
           `Additional Deaths Averted if WHO Targets met (%)`) %>%
    mutate(`Notes:` = if_else(
      str_trim(` `) %in% get_covax_iso3c() &
        `Additional Deaths Averted if COVAX Targets met` == "",
      "COVAX AMC country met target so no increase in vaccination modelled",
      ""
    ),
    `Notes:` = case_when(
      str_trim(` `) %in% c("IRQ", "SDN") ~
        paste0(`Notes:`, " Fit unable to recreate estimated deaths. Modelled deaths are lower than predicted excess mortality."),
      TRUE ~ `Notes:`
    ))
} else {
  df <-
    table1 %>%
    select(` `,`Total Deaths,\nwith vaccinations`,
           `Vaccination Coverage`, `Deaths Averted by Vaccinations`,
           `Deaths Averted by Vaccinations\nPer 10k People`,
           `Deaths Averted by Vaccinations\nPer 10k Vaccines`) %>%
    mutate(`Notes:` = "")
}
df <- df %>%
  ungroup() %>%
  mutate(
    `Notes:` = if_else(str_trim(` `) %in% c("PRK", "ERI"),
                       paste0(`Notes:`, " No vaccinations given in country over period modelled."),
                       `Notes:`),
  ` ` = if_else(
    str_trim(` `) %in% table1_df_ind$iso3c,
    paste0("   ", countrycode(str_trim(` `), origin = "iso3c", destination = "country.name")),
    ` `
  ),
  `Notes:` = if_else(map_lgl(str_split(df$`Deaths Averted by Vaccinations`, "[ \\(-]"), ~"0" %in% .x),
    paste0(`Notes:`, " Deaths averted by Vaccinations rounded to nearest individual yielding 0 deaths averted. Deaths averted per 10K people and 10K vaccines has not been rounded."),
    `Notes:`)
  )

readr::write_csv(df, "summary_table.csv")
