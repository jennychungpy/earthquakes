
# testing the eq_clean_data function
######
context("eq_clean_data")
test_that("Column YEAR does not exist", {
  file <- read_csv("inst/extdata/test.csv")
  expect_error(eq_clean_data(file))
})

test_that("The file contains all necessary columns", {
  file = "inst/extdata/earthquakes.tsv.gz"
  file <- read_delim(file, delim = "\t")
  output <- eq_clean_data(file)
  expect_that(output, is_a('data.frame'))
})


# testing the eq_location_clean function
######
context("eq_location_clean")
test_that("The selected column do not contain character", {
  file = "inst/extdata/earthquakes.tsv.gz"
  file <- read_delim(file, delim = "\t")
  expect_error(eq_location_clean(file, EQ_PRIMARY))
})

test_that("The selected column contains character", {
  file = "inst/extdata/earthquakes.tsv.gz"
  file <- read_delim(file, delim = "\t")
  output <- eq_location_clean(file, COUNTRY)
  expect_that(output, is_a('data.frame'))
})


# testing the geom_timeline function
######
context("geom_timeline")
test_that("A required parameter is not given", {
  file = "inst/extdata/earthquakes.tsv.gz"
  file <- read_delim(file, delim = "\t")
  data <- eq_clean_data(file) %>%
    filter(COUNTRY == c("China", "Usa", "Japan"),
           DATE >= "1999-01-01",
           DATE <= "2012-12-31")
  expect_error(ggplot(data, aes(x = DATE, size = richterScaleValue, fill = DEATHS)) +
    geom_timeline() +
    theme(legend.position = "bottom"))
})

test_that("The selected column contains character", {
  file = "inst/extdata/earthquakes.tsv.gz"
  file <- read_delim(file, delim = "\t")
  data <- eq_clean_data(file) %>%
    filter(COUNTRY == c("China", "Usa", "Japan"),
           DATE >= "1999-01-01",
           DATE <= "2012-12-31")
  output <- ggplot(data, aes(x = DATE, y = COUNTRY, size = richterScaleValue, fill = DEATHS)) +
    geom_timeline() +
    theme(legend.position = "bottom")
  expect_that(output, is_a('ggplot'))
})


# testing the geom_timeline_label function
######
context("geom_timeline_label")
test_that("A required parameter is not given", {
  file = "inst/extdata/earthquakes.tsv.gz"
  file <- read_delim(file, delim = "\t")
  data <- eq_clean_data(file) %>%
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
  file = "inst/extdata/earthquakes.tsv.gz"
  file <- read_delim(file, delim = "\t")
  data <- eq_clean_data(file) %>%
    filter(COUNTRY == c("China", "Usa", "Japan"),
           DATE >= "1999-01-01",
           DATE <= "2012-12-31")
  output <- ggplot(data, aes(x = DATE, y = COUNTRY, size = richterScaleValue, fill = DEATHS)) +
    geom_timeline() +
    theme(legend.position = "bottom") +
    geom_timeline_label(aes(x=DATE, label = locations, n_maxVar = COUNTRY, n_max=10)) +
    labs(x = "Date", y = "Country", fill = "# deaths", size = "Richter scale value")
  expect_that(output, is_a('ggplot'))
})


# testing the eq_map function
######
context("eq_map")
test_that("The file does not exist", {
  x <- readr::read_delim("inst/extdata/earthquakes2017.tsv.gz", delim = "\t") %>%
    eq_clean_data() %>%
    dplyr::filter(COUNTRY == "Mexico" & lubridate::year(DATE) >= 2000) %>%
    eq_map(annot_col = "DATE")
  expect_error(x)
})

test_that("The column does not exist, resulting in no pop-up", {
  output <- readr::read_delim("inst/extdata/earthquakes.tsv.gz", delim = "\t") %>%
    eq_clean_data() %>%
    dplyr::filter(COUNTRY == "Mexico" & lubridate::year(DATE) >= 2000) %>%
    eq_map(annot_col = "DATE")
  expect_that(output, is_a('leaflet'))
})


# testing the eq_clean_label function
######
context("eq_clean_label")
test_that("The file does not exist", {
  x <- readr::read_delim("inst/extdata/earthquakes.tsv.gz", delim = "\t") %>%
    dplyr::filter(COUNTRY == "Mexico" & lubridate::year(DATE) >= 2000) %>%
    dplyr::mutate(popup_text = eq_create_label(.)) %>%
    eq_map(annot_col = "DATE")
  expect_error(x)
})

test_that("The column does not exist, resulting in no pop-up", {
  output <- readr::read_delim("inst/extdata/earthquakes.tsv.gz", delim = "\t") %>%
    eq_clean_data() %>%
    dplyr::filter(COUNTRY == "Mexico" & lubridate::year(DATE) >= 2000) %>%
    dplyr::mutate(popup_text = eq_create_label(.)) %>%
    eq_map(annot_col = "popup_text")
  expect_that(output, is_a('leaflet'))
})


