##functions to simpilfy shiny code
plotMap <- function(map, data, pal, name, show_legend = TRUE, legend.title = NULL){
  if(is.null(legend.title)){
    legend.title <- name
  }
  if(all(is.character(pal))){
    thisMap <- map %>% addPolygons(
      fillColor = pal,
      weight = 1,
      opacity = 1,
      color = "white",
      fillOpacity = 0.7,
      highlight = highlightOptions(
        weight = 5,
        color = "#666",
        fillOpacity = 0.7,
        bringToFront = TRUE),
      label = paste0(data[["Country"]], ": ", signif(data[[name]], digits = digits + 1)),
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto"),
      group = name)
    if(show_legend){
      thisMap %>%
        addLegend(colors = pal, labels = signif(na.omit(data[[name]])[1], digits = digits), opacity = 0.7,
                  position = "bottomright",
                  title = legend.title,
                  group = name)
    } else{
      thisMap
    }
  } else{
    thisMap <- map %>% addPolygons(
      fillColor = ~pal(data[[name]]),
      weight = 1,
      opacity = 1,
      color = "white",
      fillOpacity = 0.7,
      highlight = highlightOptions(
        weight = 5,
        color = "#666",
        fillOpacity = 0.7,
        bringToFront = TRUE),
      label = paste0(data[["Country"]], ": ", signif(data[[name]], digits = digits + 1)),
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto"),
      group = name
    )
    if(show_legend){
      thisMap %>%
        addLegend(pal = pal, values = ~name, opacity = 0.7,
                  position = "bottomright",
                  title = legend.title,
                  group = name)
    } else{
      thisMap
    }
  }
}

getColBin <- function(data, name, bins){
  colLow <- "#267ae9"
  col0 <- "#ffffb2"
  colHigh <- "#bd0026"
  if(length(bins) == 1){ # if all the same value
    col0
  } else if(max(data[[name]], na.rm = T) <0){
    colour <- colorRamp(c(colLow, col0))
    colorBin(colour, domain = data[[name]], bins = bins)
  } else if(min(data[[name]], na.rm = T) > 0){
    colour <- colorRamp(c(col0, colHigh))
    colorBin(colour, domain = data[[name]], bins = bins)
  } else{
    colour_raw <- colorRamp(c(colLow, col0, colHigh))
    colFunc <- colorBin(colorRamp(c("#000000", "#FFFFFF")), domain = data[[name]], bins = bins)
    zeroValue <- col2rgb(colFunc(min(abs(data[[name]]), na.rm = T)))[1]/255
    colour <- function(x){
      newX <- rep(NA, length(x))
      newX[x < zeroValue] <- x[x < zeroValue]/(2*zeroValue)
      newX[x > zeroValue] <- (x[x > zeroValue] - zeroValue)/(2*(1 - zeroValue)) + 0.5
      newX[x == zeroValue] <- 0.5
      colour_raw(newX)
    }
    colorBin(colour, domain = data[[name]], bins = bins)
  }
}

plot_layered_map <- function(data, counterfactual, measure, adjust){
  data_counterfactual <- data[data$counterfactual == counterfactual,]
  base_map <- leaflet(data_counterfactual) %>%
    addProviderTiles("CartoDB.VoyagerNoLabels") %>%
    setView(50,50, zoom = 4) %>%
    setMaxBounds(lng1 = -154.498897,
                 lat1 = -45.328459,
                 lng2 = 177.260162,
                 lat2 = 69.234484)
  #derive variable names
  if(counterfactual == "No Vaccines"){
    comparison <- paste0(measure, " Averted")
  } else {
    comparison <- paste0("Additional ", measure, " Averted")
  }
  baseline <- paste0("Baseline ", measure)
  simulated <- paste0("Counterfactual ", measure)
  vaccines <- paste0("Vaccinated People (1 or 2 doses) (Counterfactual)")
  if(adjust != ""){
    comparison <- paste0(comparison, " ", adjust)
    baseline <- paste0(baseline, " ", adjust)
    simulated <- paste0(simulated, " ", adjust)
    vaccines <- paste0(vaccines, " ", adjust)
  }
  #map 1, comparison
  breaks_1 <- find_breaks(data_counterfactual[[comparison]],
                          zeroCutoff = T, n = n, sig_digits = digits)
  pal_1 <- getColBin(data_counterfactual, comparison, breaks_1)
  map <- plotMap(base_map, data_counterfactual, pal_1, comparison)
  #map 2, simulated
  breaks_23 <- find_breaks(c(data_counterfactual[[baseline]],
                             data_counterfactual[[simulated]]),
                           zeroCutoff = T, n = n, sig_digits = digits)
  pal_2 <- getColBin(data_counterfactual, simulated, breaks_23)
  map <- plotMap(map, data_counterfactual, pal_2, simulated, legend.title = paste0("Baseline/Counterfactual ", measure, " ", adjust))
  #map 3, baseline
  pal_3 <- getColBin(data_counterfactual, baseline, breaks_23)
  map <- plotMap(map, data_counterfactual, pal_3, baseline, show_legend = FALSE)
  #map 4, vaccines
  breaks_4 <- find_breaks(data_counterfactual[[vaccines]],
                          zeroCutoff = T, n = n, sig_digits = digits)
  pal_4 <- getColBin(data_counterfactual, vaccines, breaks_4)
  map <- plotMap(map, data_counterfactual, pal_4, vaccines)

  map %>%
    addLayersControl(baseGroups = c(
      comparison,
      baseline,
      simulated,
      vaccines
    ),
    position = "bottomleft")
}
