#set seeed
if(!is.na(seed)){
  set.seed(seed)
}

## Generate Deaths Averted Data for all countries with nimue fits
parallel <- TRUE

#which countries do we have
fits <-  readRDS("countryfits.Rds")
iso3cs <- c()
for(i in seq_along(fits)){
  if(!is.null(fits[[i]][["pmcmc_results"]])){
    iso3cs <- c(iso3cs, names(fits[i]))
  }
}
remove(i)

#get baseline vaccines from nimue data (this doesn't get vaccine info for countries with out fits)
baseline <- lapply(iso3cs, function(x){
  country <- fits[[x]]
  if(is.null(country$interventions)){
    return(NULL)
  }
  max_vaccine <- country$interventions$max_vaccine[-1]
  daysAtEachLevel <- as.vector(diff(c(
    country$interventions$date_vaccine_change,
    as.Date(date)
  )))
  #calculate total
  data.frame(
    iso3c = x,
    `Baseline` = sum(
      max_vaccine*daysAtEachLevel
    )
  )
})
remove(fits)
baseline <- do.call(rbind, baseline)

#calculate total vaccines
vaccineCount <- sum(baseline$Baseline)

##data for the total deaths etc. We will limit these to countries with nimue simulations
#load OWID data
owid <- readRDS("owid.Rds") %>%
  filter(iso3c %in% iso3cs)

##Note that vaccines not identical to OWID data?:
ggplot(owid %>% select(iso3c, people_vaccinated) %>%
         na.omit %>%
         group_by(iso3c) %>%
         summarise(m = max(people_vaccinated, na.rm=TRUE)) %>%
         rename(OWID = m) %>%
         filter(iso3c %in% iso3cs) %>%
         full_join(baseline) %>%
         rename(nimue = Baseline) %>%
         filter(!is.na(OWID) | nimue != 0) %>%
         mutate(OWID = if_else(is.na(OWID), 0, OWID),
                diff = nimue - OWID)) +
  geom_col(aes(
    x = iso3c,
    y = diff
  ), position = position_dodge())


##to calculate vaccine assignments
#populations for Equitable-Population
pops <- squire::population %>%
  group_by(iso3c) %>%
  summarise(population = sum(n)) %>%
  filter(iso3c %in% iso3cs)
totalPop <- sum(pops$population)
#create assignment
popsVaccineAssignment <- pops %>%
  mutate(`Equitable-Population` = vaccineCount*population/totalPop) %>%
  select(iso3c, `Equitable-Population`)
#total number of cases up to end of 2020 for Equitable-Burden
burden <- owid %>%
  filter(obsDate <= "2020-12-31") %>%
  select(iso3c, total_deaths) %>%
  na.omit %>%
  group_by(iso3c) %>%
  summarise(deaths = max(total_deaths, na.rm=TRUE))
totalDeaths <- sum(burden$deaths)
burdenVaccineAssignment <- burden %>%
  mutate(`Equitable-Burden` = vaccineCount*deaths/totalDeaths) %>%
  select(iso3c, `Equitable-Burden`)
#COVAX counter-factual
covaxISO3 <- "Afghanistan, Benin, Burkina
Faso, Burundi, Central African Republic, Chad,
Congo Dem. Rep., Eritrea, Ethiopia, Gambia,
The Guinea, Guinea-Bissau, Haiti, Korea Dem.
People’s Rep., Liberia, Madagascar, Malawi,
Mali, Mozambique, Nepal, Niger, Rwanda,
Sierra Leone, Somalia, South Sudan, Syrian Arab
Republic, Tajikistan, Tanzania, Togo, Uganda,
Yemen, Angola, Algeria,
Bangladesh, Bhutan, Bolivia, Cabo Verde,
Cambodia, Cameroon, Comoros, Congo, Rep.
Côte d’Ivoire, Djibouti, Egypt Arab Rep., El
Salvador, Eswatini, Ghana, Honduras, India,
Indonesia, Kenya, Kiribati, Kyrgyz Republic, Lao
PDR, Lesotho, Mauritania, Micronesia Fed.
Sts., Moldova, Mongolia, Morocco, Myanmar,
Nicaragua, Nigeria, Pakistan, Papua New
Guinea, Philippines, Sao Tome and Principe,
Senegal, Solomon Islands, Sri Lanka, Sudan,
Timor-Leste, Tunisia, Ukraine, Uzbekistan,
Vanuatu, Vietnam, West Bank and Gaza,
Zambia, Zimbabwe, Dominica, Fiji, Grenada,
Guyana, Maldives, Marshall Islands,
Samoa, St. Lucia, St. Vincent and the Grenadines,
Tonga, Tuvalu" %>%
  str_replace_all("\n", " ") %>%
  str_split(",", simplify = T) %>%
  countrycode(origin = "country.name", destination = "iso3c")
#20% by end of 2021, assume start in 2021 Jan
percentageVacc <- as.numeric(0.2*(as.Date(date) - as.Date("2021-01-01"))/365)
covaxVaccineAssignment <- data.frame(iso3c = iso3cs) %>%
  left_join(pops) %>%
  mutate(`COVAX` = if_else(iso3c %in% covaxISO3, population*percentageVacc, as.numeric(NA))) %>%
  select(iso3c, COVAX)
#merge into one dataset
counterfactuals <- data.frame(iso3c = iso3cs) %>%
  mutate(`No Vaccines` = 0) %>%
  left_join(popsVaccineAssignment) %>%
  left_join(burdenVaccineAssignment) %>%
  mutate(`Equitable-Burden` = if_else(is.na(`Equitable-Burden`), 0, `Equitable-Burden`)) %>%
  left_join(covaxVaccineAssignment)
#if there is no deaths data from before 2021 we assign 0 vaccines

#find start dates for the counter factuals
#for no vaccine we set it to the last date a change in vaccinations occurs to
vaccineStart <- c(
  as.character(as.Date(date) - 1),
  rep(
    owid %>%
      filter(!is.na(total_vaccinations) & total_vaccinations != 0) %>%
      pull(obsDate) %>%
      min(),
    2
  ),
  "2021-01-01"
)

#for now only run baselines + no vaccine
counterfactuals <- counterfactuals %>%
  select(iso3c, `No Vaccines`)
vaccineStart <- vaccineStart[1]

#generate counter factuals for all countries
deaths_averted_list <- apply(counterfactuals,
                             1, function(x) {
                               message(x["iso3c"])
                               out <- readRDS("countryfits.Rds")[[x["iso3c"]]]

                               #set up the counter factual labels and the assigned vaccines
                               assignments <- as.vector(x[-1], mode = "double")
                               counterfactualNames <- names(counterfactuals)[-1]

                               #remove NA ones e.g. COVAX for UMIC/HIC
                               counterfactualNames <- counterfactualNames[!is.na(assignments)]
                               vaccineStart <- vaccineStart[!is.na(assignments)]
                               assignments <- assignments[!is.na(assignments)]

                               df <- suppressMessages(
                                 deaths_averted(out, draws = draws,
                                                parallel = parallel,
                                                counterfactual = counterfactualNames,
                                                reduce_age = FALSE,
                                                assignedVaccine = assignments,
                                                vaccineStart = vaccineStart,
                                                direct = TRUE,
                                                noWarnings = TRUE)
                               )
                               return(df)
                             })

#save each counterfactual seperately
for(thisCounterfactual in
    c("Baseline", "Baseline-Direct", setdiff(names(counterfactuals), "iso3c"))
){
  temp_list <- list()
  for(j in 1:nrow(counterfactuals)){
    temp_list[[j]] <- filter(deaths_averted_list[[j]],
                             .data$counterfactual == thisCounterfactual) %>%
      ungroup() %>%
      select(!.data$counterfactual)
  }
  saveRDS(
    do.call(
      rbind,
      temp_list),
    paste0(thisCounterfactual, ".Rds")
  )
}

##vaccines given out per country
baseline <- baseline %>%
  left_join(
    owid %>%
      group_by(iso3c) %>%
      summarise(
        `Baseline (Total Vaccines)` = max(total_vaccinations, na.rm = T)
      )
  ) %>%
  mutate(`Baseline (Total Vaccines)` = if_else(
    is.infinite(`Baseline (Total Vaccines)`),
    0,
    `Baseline (Total Vaccines)`
  ))
saveRDS(
  left_join(counterfactuals, baseline),
  "counterfactuals.Rds"
)
