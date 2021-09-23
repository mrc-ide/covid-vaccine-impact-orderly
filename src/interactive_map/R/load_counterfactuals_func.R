#' A function to load and summarise the deaths averted data
#' @param counterfactuals Which counterfactuals to load. A vector of strings.
#' Since these are large files, it is recommended to load only one counterfactual
#' at a time.
#' @param group_by A vector of strings of the variables to group the summary by,
#' e.g. Iso3c code, age_group or date.
#' @param quantileSamples Only used if dataset it not grouped by country. The
#' number of combinations to randomly draw. Defaults to 2000.
loadCounterfactualData <- function(counterfactuals, group_by, quantileSamples = 2000){
  #keep country if grouping by iso3c
  if("iso3c" %in% group_by | "country" %in% group_by){
    group_by <- unique(c(group_by, "iso3c", "country"))
    normalMedian <- TRUE
  } else{
    normalMedian <- FALSE
  }
  #if grouped by income
  if("income_group" %in% group_by){
    add_income <- TRUE
    group_by <- setdiff(group_by, "income_group")
  } else{
    add_income <- FALSE
  }
  if("who_region" %in% group_by){
    add_who <- TRUE
    group_by <- setdiff(group_by, "who_region")
  } else{
    add_who <- FALSE
  }
  if("week" %in% group_by){
    add_week <- TRUE
  } else{
    add_week <- FALSE
  }
  #Load the Baseline data
  baseline_data <- readRDS(
   "Baseline.Rds"
  )
  if(add_week){
    #get weeks if needed
    baseline_data <- baseline_data %>%
      dplyr::mutate(
        week = as.Date(cut(date, "week"))
      )
  }
  #summarise
  baseline_data <- suppressMessages(baseline_data %>%
    dplyr::group_by_at(unique(c(group_by, "replicate", "iso3c"))) %>%
    dplyr::summarise(
      baseline_infections = sum(.data$infections),
      baseline_deaths = sum(.data$deaths)
    ))

  #for each counterfactual
  start <- TRUE
  for(thisCounterfactual in counterfactuals){
    thisCounterfactual_data <- readRDS(
      paste0(thisCounterfactual, ".Rds")
    )
    if(add_week){
      #get weeks if needed
      thisCounterfactual_data <- thisCounterfactual_data %>%
        dplyr::mutate(
          week = as.Date(cut(date, "week"))
        )
    }
    #summarise data
    thisCounterfactual_data <- suppressMessages(thisCounterfactual_data %>%
      dplyr::group_by_at(unique(c(group_by, "replicate", "iso3c"))) %>%
      dplyr::summarise(
        infections = sum(.data$infections),
        deaths = sum(.data$deaths)
      )) %>%
      dplyr::mutate(counterfactual = thisCounterfactual)
    if(start){
      counterfactual_data <- thisCounterfactual_data
      start <- FALSE
    } else{
      counterfactual_data <- rbind(
        counterfactual_data,
        thisCounterfactual_data
      )
    }
  }
  remove(thisCounterfactual_data)
  #add baseline data
  counterfactual_data <- suppressMessages(dplyr::full_join(
    counterfactual_data,
    baseline_data
  ) %>%
    dplyr::mutate(
      averted_deaths = .data$deaths - .data$baseline_deaths,
      averted_infections = .data$infections - .data$baseline_infections
    ))
  #add extra categories
  if(add_income){
    counterfactual_data <-
      dplyr::left_join(
        counterfactual_data,
        readRDS(
          "income_group.rds"
        ),
        by = "iso3c")
    group_by <- unique(c(group_by, "income_group"))
  }
  if(add_who){
    counterfactual_data <-
      dplyr::left_join(
        counterfactual_data,
        readRDS(
          "who_region.Rds"
        ),
        by = "iso3c")
    group_by <- unique(c(group_by, "who_region"))
  }
  counterfactual_data <- dplyr::group_by_at(counterfactual_data, c(group_by,"counterfactual"))
  #to find median/quantiles
  if(!normalMedian){
    #else we randomly combine replicates and find their median/quantile over that
    noReplicates <- length(unique(counterfactual_data$replicate))
    uniqueCountries <- sort(unique(counterfactual_data$iso3c))
    noCountries <- length(uniqueCountries)
    #the median/quantiles of that
    counterfactual_data <- counterfactual_data %>% #remove previous groupings:
      dplyr::mutate(countryNumber =
                      sapply(.data$iso3c,
                             function(x){
                               which(uniqueCountries == x)
                             },
                             USE.NAMES = F)
      )
    #generate the random draws
    #data set to hold results
    #use group meta data to get output length
    meta_data <- dplyr::group_data(counterfactual_data)
    sample_length <- nrow(meta_data)
    sample_names <- c(group_by, "counterfactual",
                      "deaths", "infections",
                      "baseline_deaths", "baseline_infections",
                      "averted_deaths", "averted_infections"
    )
    samples <- counterfactual_data[1,] %>%
      select(dplyr::all_of(sample_names))

    samples[1:(quantileSamples*sample_length),] <- NA

    for(i in 1:quantileSamples){
      #get random draw
      replicates <- sample.int(noReplicates, size = noCountries, replace = T)

      sum_data <- suppressMessages(counterfactual_data %>% #reduce to replicates used:
        dplyr::filter(
          .data$replicate == replicates[.data$countryNumber]
        ) %>% #sum over the given groupings
        dplyr::summarise(
          dplyr::across(
            c(.data$deaths, .data$infections,
              .data$baseline_deaths, .data$baseline_infections,
              .data$averted_deaths, .data$averted_infections),
            ~sum(.x, na.rm = TRUE)
          )
        ))
      #add to samples
      samples[seq(min(sample_length,nrow(sum_data))) + sample_length*(i-1),] <- sum_data
    }
    #re-add grouping
    counterfactual_data <- samples %>% #remove missing data
      na.omit()
    remove(samples)
  }
  #find median/quantile
  suppressMessages(
  counterfactual_data %>%
    summarise(
      dplyr::across(
        .cols = dplyr::ends_with(c("deaths", "infections")),
        .fns = list(
          avg = ~median(.x, na.rm = TRUE),
          `025` = ~quantile(.x, probs = 0.025, na.rm = TRUE),
          `975` = ~quantile(.x, probs = 0.975, na.rm = TRUE)
        )
      )
    ) %>%
    dplyr::ungroup()
  )
}
