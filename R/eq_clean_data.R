#' Cleaning a NOAA (earthquake) file
#'
#' @description The 'eq_clean_data' function returns a cleaned NOAA data frame
#'
#' @details The eq_clean_data function correct the data type of the columns YEAR, MONTH, DAY, LATITUDE, LONGITUDE, DEATHS and EQ_PRIMARY.
#'          The columns YEAR, MONTH and DAY are gathered to form a date (an as.Date object). The missing dates are filtered out. The
#'          magnitude of the earthquakes are rounded up to 2, 4, 6, 8 and 10. The locations in the column LOCATION_NAME is extracted and
#'          adjusted to title case. The column COUNTRY is alsp adjusted to title case with the eq_location_clean fucntion.
#'
#' @note error - When the column names do not include YEAR, MONTH, DAY, LATITUDE, LONGITUDE, DEATHS, EQ_PRIMARY, LOCATION_NAME
#'               and COUNTRY, an error will be given.
#'
#' @importFrom dplyr mutate_each mutate filter
#' @importFrom tidyr replace_na unite
#' @importFrom stringi stri_trans_totitle
#'
#' @param data - A delimited file that is read in by the 'read_delim' function
#'
#' @inheritParams eq_location_clean
#'
#' @return A cleaned NOAA data frame
#'
#' #@example
#' #\dontrun{
#' #file = "inst/extdata/earthquakes.tsv.gz"
#' #file <- readr::read_delim(file, delim = "\t")
#' #file_cleaned <- eq_clean_data(file)
#' #}
#'
#' @export
eq_clean_data <- function(data){
  data <- data %>%
    dplyr::mutate_each(funs(as.character), c(YEAR, MONTH, DAY)) %>%
    tidyr::replace_na(list(MONTH = 0, DAY = 0)) %>%
    tidyr::unite(DATE, DAY, MONTH, YEAR, sep = "-") %>%
    dplyr::mutate(DATE = as.Date(DATE, format = "%d-%m-%Y")) %>%
    dplyr::filter(!is.na(DATE)) %>%
    dplyr::mutate_each(funs(as.numeric), c(LATITUDE, LONGITUDE, DEATHS, EQ_PRIMARY))
  data$richterScaleValue <- ifelse(data$EQ_PRIMARY <= 2 & data$EQ_PRIMARY > 0, 2,
                                   ifelse(data$EQ_PRIMARY > 2 & data$EQ_PRIMARY <= 4, 4,
                                          ifelse(data$EQ_PRIMARY > 4 & data$EQ_PRIMARY <= 6, 6,
                                                 ifelse(data$EQ_PRIMARY > 6 & data$EQ_PRIMARY <= 8, 8,
                                                        ifelse(data$EQ_PRIMARY > 8 & data$EQ_PRIMARY <= 10, 10,
                                                               data$EQ_PRIMARY)))))
  data$locations <- substr(data$LOCATION_NAME, regexpr(": ", data$LOCATION_NAME), nchar(data$LOCATION_NAME))
  data$locations <- sub(": ", "", data$locations)
  data$locations <- stringi::stri_trans_totitle(data$locations)
  data <- data.frame(data)
  countryCol <- which(names(data) %in% "COUNTRY")
  eq_location_clean(data, countryCol)
}
