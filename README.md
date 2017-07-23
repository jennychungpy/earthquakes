# NOAAearthquake
The 'NOAAearthquake' package is a package to clean, visualise and map NOAA earthquakes data, using the following libraries:
 - library(readr)
 - library(magrittr)
 - library(dplyr)
 - library(tidyr)
 - library(ggplot2)
 - library(leaflet)


## Cleaning data
Data are cleaned with the functions
 - eq_clean_data()

```{r cleaning_NOAAData}
file_cleaned <- eq_clean_data()
```

## Visualising data
The cleaned data are visualised as timelines. Each timeline represents a country with its earthquakes over time. The magnitutde and the number of deaths are shown in this visualisation.

```{r visualising_NOAAData, eval=FALSE, include=FALSE}
data <- eq_clean_data() %>%
  filter(COUNTRY == c("China", "Usa", "Japan"),
         DATE >= "1999-01-01",
         DATE <= "2012-12-31")
ggplot(data, aes(x = DATE, y = COUNTRY, size = richterScaleValue, fill = DEATHS)) +
  geom_timeline() +
  theme(legend.position = "bottom") +
  geom_timeline_label(aes(x=DATE, label = locations, n_maxVar = EQ_PRIMARY, n_max=10)) +
  labs(x = "Date", y = "Country", fill = "# deaths", size = "Richter scale value") 
```

## Mapping data
This package allows you also to map the earthquakes data on a geographical map. When hoverin over an earthquake, a pop-up appears and shows information about the location, magnitude and number of death of an earthquake.

```{r mapping_NOAAData, eval=FALSE, include=FALSE}
map <- data %>% 
  eq_clean_data() %>% 
  dplyr::filter(COUNTRY == "Mexico" & lubridate::year(DATE) >= 2000) %>% 
  dplyr::mutate(popup_text = eq_create_label(.)) %>% 
  eq_map(annot_col = "popupText")
map 
```
