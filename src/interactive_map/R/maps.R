#' Create a map of a given measure
#' @param data Data-frame containing the data
#' @param var Variable to be plotted
#' @param iso3 Iso3 variable to map to plot
#' @param region One of Europe, North America, Central America, South America,
#' Africa, Asia, South-East Asia
#' @param lab Label to give the variable
#' @param title Title for the plot
#' @param n Number of quantiles to split the variable into. Default = 7
#' @param zeroCutoff If true 0 will be the border of one of the quantiles.
#' Default = TRUE
#' @param sig_digits If not NULL then bins are rounded(?) to this number of
#' significant figures. Useful for neatening plots etc. Default = NULL
#' @param show_legend TRUE/FALSE determines if the map will have a legend.
#' Default = TRUE
vaccineMap <- function(data, var, iso3 = "iso3c", region = NULL, lab=NULL, title=NULL,
                       n=7, zeroCutoff = TRUE, sig_digits = NULL, show_legend = TRUE){
  #load world sf data
  worldsf <- readRDS(
    file.path(here::here(), "analysis/data/derived/worldsf.rds")
  )
  #return codes not on map
  inMap <- unique(worldsf$iso3c)
  inData <- unique(data[[iso3]])
  if(length(setdiff(inData, inMap)) != 0){
    message(paste0("The following ISO3 codes are not present on the map and
                   will not be plotted: ", paste(setdiff(inData, inMap),"",collapse=" ")))
  }
  #bin data
  #calculate quantiles
  breaks <- find_breaks(data[[var]], n, zeroCutoff, sig_digits)

  data <- dplyr::mutate(data,
                        varCat = cut(data[[var]],
                                    breaks = breaks,
                                    include.lowest=TRUE))
  #merge data with sf
  world_and_data <- dplyr::left_join(
    dplyr::rename(
      dplyr::select(worldsf, "iso3c"),
      iso3 = "iso3c"),
    dplyr::rename(
      dplyr::select(data, "varCat", iso3),
      iso3 = iso3),
    by = "iso3"
  )
  #which region
  if(is.null(region)){
    xlim <- c(-160,170)
    ylim <- c(-50,70)
    legendPos <-  c(0.1, 0.25)
  }
  else if(region == "Europe"){
    xlim = c(-30, 40)
    ylim = c(35, 65)
    legendPos <- c(0.1, 0.5)
  }
  else if(region == "Africa"){
    xlim = c(-25, 52)
    ylim = c(-35, 35)
    legendPos <- c(0.2, 0.3)
  }
  else if(region == "South America"){
    xlim = c(-85, -35)
    ylim = c(-55, 15)
    legendPos <- c(0.8, 0.15)
  }
  else if(region == "North America"){
    xlim = c(-130, -60)
    ylim = c(10, 60)
    legendPos <- c(0.1, 0.15)
  }
  else if(region == "Central America"){
    xlim = c(-91, -60)
    ylim = c(7.5, 25)
    legendPos <- c(0.8, 0.8)
  }
  else if(region == "Asia"){
    xlim = c(35, 140)
    ylim = c(5, 50)
    legendPos <- c(0.9, 0.2)
  }
  else if(region == "South-East Asia"){
    xlim = c(100, 175)
    ylim = c(-45, 20)
    legendPos <- c(0.8, 0.8)
  }
  else if(region == "SEAR"){
    xlim = c(70, 145)
    ylim = c(-10, 32)
    legendPos <- c(0.9, 0.2)
  }
  else if(region == "EUR"){
    xlim = c(-22, 70)
    ylim = c(32, 64)
    legendPos <- c(0.1, 0.5)
  }
  else if(region == "EMR"){
    xlim = c(-8, 72)
    ylim = c(6, 37)
    legendPos <- c(0.1, 0.5)
  }
  else if(region == "WPR"){
    xlim = c(75, 176)
    ylim = c(-43, 48)
    legendPos <- c(0.2, 0.25)
  }
  if(!show_legend){
    legendPos <- "none"
  }
  #plot
  ggplot2::ggplot(world_and_data) +
    ggplot2::geom_rect(ggplot2::aes(xmin = -180, xmax = 180, ymin = -75, ymax = 90), fill = "light blue", inherit.aes = FALSE) +
    ggplot2::geom_sf(ggplot2::aes_string(fill="varCat"), colour="black", size = 1/10) +
    ggplot2::theme_void() +
    ggplot2::scale_fill_brewer(palette = "OrRd", na.value = "Grey") +
    ggplot2::coord_sf(xlim = xlim, ylim = ylim) +
    ggplot2::labs(fill = lab, title = title) +
    ggplot2::theme(legend.position = legendPos)
}
#' Function to calculate breaks
#' @noRd
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
#' A function to do ceiling but to a specified number of digits
#' @noRd
ceilingPlus <- function(values, digits){
  digitsInValues <- as.numeric(sub(".*e", "", base::format(values, scientific = TRUE)))
  baseValues <- as.numeric(sub("e.*", "", base::format(values, scientific = TRUE)))
  ceiling(baseValues*10^(digits - 1))/10^(digits - 1 -digitsInValues)
}
#' A function to do floor but to a specified number of digits
#' @noRd
floorPlus <- function(values, digits){
  digitsInValues <- as.numeric(sub(".*e", "", base::format(values, scientific = TRUE)))
  baseValues <- as.numeric(sub("e.*", "", base::format(values, scientific = TRUE)))
  floor(baseValues*10^(digits - 1))/10^(digits - 1 - digitsInValues)
}
