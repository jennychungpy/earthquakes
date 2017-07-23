
# files
######
test_that("file exists in directory",{
  expect_true(file.exists(file.path(system.file("extdata", package="NOAAearthquake"), "earthquakes.tsv.gz")))
})


# testing the eq_clean_data function
######
context("eq_clean_data")
test_that("Column YEAR does not exist", {
  file_csv <- system.file("extdata", "test.csv", package = "NOAAearthquake", mustWork = FALSE)
  expect_error(eq_clean_data(file_csv))
})


test_that("The file contains all necessary columns", {
  filePath <- file.path(system.file("extdata", package = "NOAAearthquake"), "earthquakes.tsv.gz")
  data <- readr::read_delim(filePath, delim = "\t")
  output <- eq_clean_data(data)
  expect_that(output, is_a('data.frame'))
})


# testing the eq_location_clean function
######
context("eq_location_clean")
test_that("The selected column do not contain character", {
  filePath <- file.path(system.file("extdata", package = "NOAAearthquake"), "earthquakes.tsv.gz")
  data <- readr::read_delim(filePath, delim = "\t")
  col <- which(names(data) %in% "EQ_PRIMARY")
  expect_error(eq_location_clean(file, col))
})

test_that("The selected column contains character", {
  filePath <- file.path(system.file("extdata", package = "NOAAearthquake"), "earthquakes.tsv.gz")
  data <- readr::read_delim(filePath, delim = "\t")
  col <- which(names(data) %in% "COUNTRY")
  output <- eq_location_clean(data, col)
  expect_that(output, is_a('data.frame'))
})


# testing the geom_timeline function
######
context("geom_timeline")
test_that("A required parameter is not given", {
  filePath <- file.path(system.file("extdata", package = "NOAAearthquake"), "earthquakes.tsv.gz")
  data <- readr::read_delim(filePath, delim = "\t")
  data <- eq_clean_data(data) %>%
    filter(COUNTRY == c("China", "Usa", "Japan"),
           DATE >= "1999-01-01",
           DATE <= "2012-12-31")
  expect_error(ggplot(data, aes(x = DATE, size = richterScaleValue, fill = DEATHS)) +
    geom_timeline() +
    theme(legend.position = "bottom"))
})

test_that("The selected column contains character", {
  filePath <- file.path(system.file("extdata", package = "NOAAearthquake"), "earthquakes.tsv.gz")
  data <- readr::read_delim(filePath, delim = "\t")
  data <- eq_clean_data(data) %>%
    filter(COUNTRY == c("China", "Usa", "Japan"),
           DATE >= "1999-01-01",
           DATE <= "2012-12-31")
  output <- ggplot2::ggplot(data, aes(x = DATE, y = COUNTRY, size = richterScaleValue, fill = DEATHS)) +
    geom_timeline() +
    ggplot2::theme(legend.position = "bottom")
  expect_that(output, is_a('ggplot'))
})


# testing the geom_timeline_label function
######
context("geom_timeline_label")
test_that("A required parameter is not given", {
  filePath <- file.path(system.file("extdata", package = "NOAAearthquake"), "earthquakes.tsv.gz")
  data <- readr::read_delim(filePath, delim = "\t")
  data <- eq_clean_data(data) %>%
    filter(COUNTRY == c("China", "Usa", "Japan"),
           DATE >= "1999-01-01",
           DATE <= "2012-12-31")
  expect_error(ggplot(data, aes(x = DATE, y = COUNTRY, size = richterScaleValue, fill = DEATHS)) +
                 geom_timeline() +
                 theme(legend.position = "bottom") +
                 geom_timeline_label(aes(x=DATE, label = locations, n_max=10)) +
                 labs(x = "Date", y = "Country", fill = "# deaths", size = "Richter scale value")
               )

})

test_that("The selected column contains character", {
  filePath <- file.path(system.file("extdata", package = "NOAAearthquake"), "earthquakes.tsv.gz")
  data <- readr::read_delim(filePath, delim = "\t")
  data <- eq_clean_data(data) %>%
    filter(COUNTRY == c("China", "Usa", "Japan"),
           DATE >= "1999-01-01",
           DATE <= "2012-12-31")
  output <- ggplot2::ggplot(data, aes(x = DATE, y = COUNTRY, size = richterScaleValue, fill = DEATHS)) +
    geom_timeline() +
    ggplot2::theme(legend.position = "bottom") +
    geom_timeline_label(aes(x=DATE, label = locations, n_maxVar = COUNTRY, n_max=10)) +
    ggplot2::labs(x = "Date", y = "Country", fill = "# deaths", size = "Richter scale value")
  expect_that(output, is_a('ggplot'))
})


# testing the eq_map function
######
context("eq_map")
test_that("The file does not exist", {
  filePath <- file.path(system.file("extdata", package = "NOAAearthquake"), "earthquakes.tsv.gz")
  data <- readr::read_delim(filePath, delim = "\t")
  expect_error(data %>%
                 eq_clean_data() %>%
                 dplyr::filter(COUNTRY == "Mexico" & lubridate::year(DATE) >= 2000) %>%
                 eq_map(annot_col = "DATE"))
})

test_that("The column does not exist, resulting in no pop-up", {
  filePath <- file.path(system.file("extdata", package = "NOAAearthquake"), "earthquakes.tsv.gz")
  output <- eq_clean_data(readr::read_delim(filePath, delim = "\t")) %>%
    filter(COUNTRY == "Mexico",
           DATE  >= "2000-01-01") %>%
    eq_map(annot_col = "DATE")
  expect_that(output, is_a('leaflet'))
})


# testing the eq_clean_label function
######
context("eq_clean_label")
test_that("The file does not exist", {
  filePath <- file.path(system.file("extdata", package = "NOAAearthquake"), "earthquakes.tsv.gz")
  expect_error(eq_clean_data(readr::read_delim(filePath, delim = "\t")) %>%
                 dplyr::filter(COUNTRY == "Mexico",
                        DATE  >= "2000-01-01") %>%
                 dplyr:: mutate(popup_text = eq_create_label(.)) %>%
                 eq_map())
})

test_that("The column does not exist, resulting in no pop-up", {
  filePath <- file.path(system.file("extdata", package = "NOAAearthquake"), "earthquakes.tsv.gz")
  output <- readr::read_delim(filePath, delim = "\t") %>%
    eq_clean_data() %>%
    filter(COUNTRY == "Mexico",
           DATE  >= "2000-01-01") %>%
    mutate(popup_text = eq_create_label(.)) %>%
    eq_map(annot_col = "popup_text")
  expect_that(output, is_a('leaflet'))
})


