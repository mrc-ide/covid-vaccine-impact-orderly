#' Simple function to summarise the OWID data set by iso3c with no warning messages
#' @param date database to use
#' @param variable the variable to summarise
summarise_variable <- function(data, variable){
  data %>%
    group_by(.data$iso3c) %>%
    filter(!is.na(.data[[variable]])) %>%
    summarise(
      !! variable := max(.data[[variable]])
    )
}
