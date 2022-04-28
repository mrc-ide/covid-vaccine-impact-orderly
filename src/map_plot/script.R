df <- loadCounterfactualData("No Vaccines", group_by = "iso3c")

#load population
pop_df <- squire::population %>%
  group_by(iso3c) %>%
  summarise(population = sum(n))

#Merge
df <- left_join(
  df,
  pop_df,
  by = "iso3c"
) %>%
  mutate(
    across(.cols = contains("averted_deaths"),
           ~if_else(
             .x < 0,
             0,
             .x/population*10000
             ),
           .names = "{.col}_per_capita")
  )


#map function
vaccineMap <- function(data, var, lab=NULL, title=NULL,
                       n=7, zeroCutoff = TRUE, sig_digits = NULL, show_legend = TRUE){
  #load world sf data
  worldsf <- readRDS(
    "worldsf.rds"
  )
  #return codes not on map
  inMap <- unique(worldsf$iso3c)
  inData <- unique(data$iso3c)
  if(length(setdiff(inData, inMap)) != 0){
    message(paste0("The following ISO3 codes are not present on the map and
                   will not be plotted: ", paste(setdiff(inData, inMap),"",collapse=" ")))
  }
  #bin data
  #calculate quantiles
  breaks <- find_breaks(data[[var]], n, zeroCutoff, sig_digits)
  #Round to whole numbers for ease of use
  breaks[breaks < 10] <- signif(breaks[breaks < 10], 1)
  breaks[breaks >= 10] <- signif(breaks[breaks >= 10], 2)
  breaks <- unique(breaks)

  data <- dplyr::mutate(data,
                        varCat = cut(data[[var]],
                                     breaks = breaks,
                                     include.lowest=TRUE))
  #merge data with sf
  world_and_data <- dplyr::left_join(
      dplyr::select(worldsf, "iso3c"),
      dplyr::select(data, "varCat", iso3c),
    by = "iso3c"
  )
  #where to plot
    xlim <- c(-155,170)
    ylim <- c(-50,60)
    legendPos <-  c(0.15, 0.31)
  #plot
  ggplot2::ggplot(world_and_data) +
    ggplot2::geom_rect(ggplot2::aes(xmin = -180, xmax = 180, ymin = -75, ymax = 90), fill = "light blue", inherit.aes = FALSE) +
    ggplot2::geom_sf(ggplot2::aes_string(fill="varCat"), colour="black", size = 1/10) +
    ggplot2::theme_void() +
    ggplot2::scale_fill_brewer(palette = "OrRd", na.value = "grey")+#,  na.translate = F) +
    ggplot2::coord_sf(xlim = xlim, ylim = ylim) +
    ggplot2::labs(fill = lab, title = title) +
    ggplot2::theme(legend.position = legendPos,
                   legend.title = element_text( size=8), legend.text=element_text(size=8),
                   legend.key.size = unit(0.5, 'cm'))
}
# Function to calculate breaks
find_breaks <- function(variable, n, zeroCutoff = F, sig_digits = NULL){
  variable <- variable[!is.na(variable)]
  if(min(variable) > 0 | max(variable) < 0 | all(variable == 0)){
    #if variable doesn't cross 0
    zeroCutoff <- FALSE
  }
  if(zeroCutoff == "Even"){
    #then we set zero to the middle cutoff and create breaks on either side
    lowValues <- variable[variable < 0]
    lowbreaks <- find_breaks(lowValues, min(n, length(lowValues) - 1), zeroCutoff = F, sig_digits = sig_digits)
    highValues <- variable[variable > 0]
    highbreaks <- find_breaks(highValues, min(n, length(highValues) - 1), zeroCutoff = F, sig_digits = sig_digits)
    breaks <- c(lowbreaks, 0, highbreaks)
    return(breaks)
  }
  if(zeroCutoff == TRUE){
    breaks <- unique(quantile(variable,probs=seq.int(0,1, by=1/(n-1))))
    #ensure 0 is a cutoff
    aPos <- suppressWarnings(max(which(breaks < 0)))
    bPos <- suppressWarnings(min(which(breaks > 0)))
    if(is.infinite(bPos)){
      breaks[aPos + 1] <- 0
    } else if(is.infinite(aPos)){
      breaks <- c(0, breaks)
    } else{
      breaks <- c(
        breaks[1:aPos],
        0,
        breaks[bPos:length(breaks)]
      )
    }
  } else{
    breaks <- unique(quantile(variable,probs=seq.int(0,1, by=1/n)))
  }
  #round to whole numbers
  if(!is.null(sig_digits)){
    oldBreaks <- breaks
    breaks <- signif(oldBreaks, digits = sig_digits)
    breaks[1] <- floorPlus(oldBreaks[1], sig_digits)
    breaks[length(breaks)] <- ceilingPlus(oldBreaks[length(oldBreaks)], sig_digits)
  }
  return(unique(breaks))
}
#A function to do ceiling but to a specified number of digits
ceilingPlus <- function(values, digits){
  digitsInValues <- as.numeric(sub(".*e", "", base::format(values, scientific = TRUE)))
  baseValues <- as.numeric(sub("e.*", "", base::format(values, scientific = TRUE)))
  ceiling(baseValues*10^(digits - 1))/10^(digits - 1 -digitsInValues)
}
#A function to do floor but to a specified number of digits
floorPlus <- function(values, digits){
  digitsInValues <- as.numeric(sub(".*e", "", base::format(values, scientific = TRUE)))
  baseValues <- as.numeric(sub("e.*", "", base::format(values, scientific = TRUE)))
  floor(baseValues*10^(digits - 1))/10^(digits - 1 - digitsInValues)
}

#plot per capita
main_map <- vaccineMap(df, "averted_deaths_avg_per_capita", lab = "Deaths Averted\nPer 10k:")

saveRDS(main_map, "deaths_averted_map.Rds")
