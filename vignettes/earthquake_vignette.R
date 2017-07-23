## ----libraries-----------------------------------------------------------
library(readr)
library(dplyr)
library(magrittr)
library(tidyr)
library(stringi)
library(scales)
library(ggplot2)
library(grid)
library(leaflet)
library(NOAAearthquake)

## ----cleaningLocation_NOAAData, eval=TRUE, echo=TRUE---------------------
fileName <- "earthquakes.tsv.gz"
file <- readr::read_delim(fileName, delim = "\t")
countryCol <- which(names(file) %in% "COUNTRY")
locations_cleaned <- eq_location_clean(file, countryCol)
locations_cleaned

## ----cleaning_NOAAData, eval=TRUE, echo=TRUE-----------------------------
fileName <- "earthquakes.tsv.gz"
file <- readr::read_delim(fileName, delim = "\t")
file_cleaned <- eq_clean_data(file)
file_cleaned

## ----visualising_NOAAData, eval=FALSE, echo=TRUE, fig.height=5, fig.width=7----
#  library(magrittr)
#  fileName <- "earthquakes.tsv.gz"
#  file <- readr::read_delim(fileName, delim = "\t")
#  data <- eq_clean_data(file) %>%
#    dplyr::filter(COUNTRY == c("China", "Usa", "Japan"),
#           DATE >= "1999-01-01",
#           DATE <= "2012-12-31")
#  ggplot() +
#    geom_timeline(data, aes(x = DATE, y = COUNTRY, size = richterScaleValue, fill = DEATHS)) +
#    theme(legend.position = "bottom") +
#    geom_timeline_label(aes(x=DATE, label = locations, n_maxVar = EQ_PRIMARY, n_max=10)) +
#    labs(x = "Date", y = "Country", fill = "# deaths", size = "Richter scale value")

## ----mapping_NOAAData, eval=TRUE, echo=TRUE, fig.height=5, fig.width=7----
file <- "earthquakes.tsv.gz"
map <- readr::read_delim(file, delim = "\t") %>% 
  eq_clean_data() %>% 
  dplyr::filter(COUNTRY == "Mexico",
                DATE >= "2000-01-01") %>% 
  dplyr::mutate(popup_text = eq_create_label(.)) %>% 
  eq_map(annot_col = "popup_text")
map 

