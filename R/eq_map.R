#' Mapping the earthquakes on a geographical map
#'
#' @description The 'eq_map' function maps the earthquakes on a geograpical map.
#'
#' @details The 'eq_map' function maps the earthquakes (as circles) on a geograpical map, given the longitude and latitude of
#'          the earthquake events. The magnitude of the earthquakes is shown by the radius of the circles. When hovering over
#'          an earthquake events a pop-up shows some information about the earthquake.
#'
#' @importFrom leaflet leaflet addTiles addCircleMarkers
#'
#' @param data - A file name
#' @param annot_col - The parameter that is annotated in a pop-up.
#'
#' @return A geographical map with earthquakes (circles) of which the size indicates the magnitude of the earthquakes.
#'
#' #@example
#' #\dontrun{
#' #x <- readr::read_delim("data/earthquakes.tsv.gz", delim = "\t") %>%
#' #  eq_clean_data() %>%
#' #  dplyr::filter(COUNTRY == "Mexico" & lubridate::year(DATE) >= 2000) %>%
#' #  eq_map(annot_col = "DATE")
#' #x
#' #}
#'
#' @export
eq_map <- function(data, annot_col){
  temp <- leaflet::leaflet() %>%
    leaflet::addTiles() %>%
    leaflet::addCircleMarkers(lng = data$LONGITUDE,
                              lat = data$LATITUDE,
                              radius = data$EQ_PRIMARY,
                              weight = 1,
                              popup = data[[annot_col]]
    )
  temp
}
