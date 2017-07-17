#' Labelling the pop-up of the geographical map with earthquakes
#'
#' @description Information about the earthquakes like locations, magnitude and the number of deaths are given in a pop-up.
#'
#' @details The 'eq_create_label' function indicates 3 information about the earthquakes in the pop-up, when hovering over an earthquake.
#'          A line about the locations of the earthquake, a line about the magnitude of the earthquakes and a line about the number of
#'          deaths.
#'
#' @note error - An error will be given when there are no column named as "locations", "EQ_PRIMARY" and "DEATHS".
#'
#' @param data - file name
#'
#' @return A pop-up with information about the location, magnitude and the number of deaths, when hovering over an earthquake.
#'
#' #@example
#' #\dontrun{
#' #x <- readr::read_delim("data/earthquakes.tsv.gz", delim = "\t") %>%
#' #  eq_clean_data() %>%
#' #  dplyr::filter(COUNTRY == "Mexico" & lubridate::year(DATE) >= 2000) %>%
#' #  dplyr::mutate(popup_text = eq_create_label(.)) %>%
#' #  eq_map(annot_col = "popupText")
#' #x
#' #}
#'
#' @export
eq_create_label <- function(data){
  data <- data
  data$loc <- ifelse(is.na(data$locations), "", paste0("<strong>Location:</strong> ", data$locations))
  data$mag <- ifelse(is.na(data$EQ_PRIMARY), "", paste0("<br><strong>Magnitude:</strong> ", data$EQ_PRIMARY))
  data$deaths <- ifelse(is.na(data$DEATHS), "", paste0("<br><strong>Total deahts::</strong> ", data$DEATHS))
  data$popupText <- paste0(data$loc, data$mag, data$deaths)

}
