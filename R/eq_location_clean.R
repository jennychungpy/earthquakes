#' Cleaning country names
#'
#' @description The 'eq_location_clean(data, countryCol)' function will change the given column into title case.
#'
#' @details The column with the country names of the given data file will be changed into title case.
#'
#' @note error - An error is given when colName is not given.
#' @note error - An error is given when colName is not a character
#'
#' @param data - A delimited file that is read in by the 'read_delim' function
#' @param colName - The 'colName' parameter is a character and represents the column with the country names
#'
#' @return A data frame in the global environment
#'
#' #@example
#' #\dontrun{
#' #file = "data/earthquakes.tsv.gz"
#' #file <- read_delim(file, delim = "\t")
#' #eq_location_clean(file, COUNTRY)
#' #}
#'
#' @export
eq_location_clean <- function(data, colName){
  data[, colName] <- paste0(substr(data[, colName], 1, 1), tolower(substr(data[, colName], 2, nchar(data[, colName]))))
  assign("data", data, envir=.GlobalEnv)
}
