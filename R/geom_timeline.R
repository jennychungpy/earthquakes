#' Visualising a timeline of the earthquakes
#'
#' @description The geom_timeline visualises earthquakes events over time and per country in magnitude and number of deaths.
#'
#' @details The geom_timeline function works together with ggplot2. This function visualises the earthquakes events over time (x aesthetics)
#'          and per country (y aesthetics) in magnitude (size parameter) and number of deaths (fill parameter.
#'
#' @note error - An error will appear when a parameter is not given.
#'
#' @importFrom ggplot2 ggproto aes draw_key_point layer
#' @importFrom dplyr arrange
#' @importFrom grid pointsGrob gpar polylineGrob gList
#' @importFrom scales alpha
#' @importFrom magrittr %>%
#'
#' @param data - File name
#' @param x - The parameter in the X-axis: the time (as.Date object)
#' @param y -  The parameter in the Y-axis: the country names (character)
#' @param size - The parameter that represents the magnitude of the earthquake, indicating the size of the circles (=earthquake event): Richter scale value (numeric)
#' @param fill - The parameter that represents the deaths resulted from the earthquake, colouring the circles (=earthquake event)
#'
#' @inheritParams ggplot2::geom_point
#'
#' @return A geom layer with timeline, each timeline shows a country with its earthquake events in circles, the magnitude of an earthquake (size of
#'         circles) and the number of deaths (the colouring of the circles).
#'
#' @examples
#' \dontrun{
#' ggplot() +
#'   geom_timeline(eq_clean_data(file) %>%
#'        filter(COUNTRY == c("China", "Usa", "Japan"),
#'              DATE >= "1999-01-01",
#'              DATE <= "2012-12-31"),
#'              aes(x = DATE, y = COUNTRY, size = richterScaleValue, fill = DEATHS)) +
#'   theme(legend.position = "bottom")
#' }
#'
#' @export
geom_timeline <- function(mapping = NULL,
                          data = NULL,
                          stat = 'identity',
                          position = 'identity',
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE,
                          ...) {
  ggplot2::layer(geom = Geom_Timeline,
                 mapping = mapping,
                 data = data,
                 stat = stat,
                 position = position,
                 show.legend = show.legend,
                 inherit.aes = inherit.aes,
                 params = list()
  )
}

Geom_Timeline <- ggplot2::ggproto("Geom_Timeline",
                                  ggplot2::Geom,
                                  required_aes = c("x"),
                                  default_aes = ggplot2::aes(col = "grey60", fill = "grey75", shape = 21, size = 2, alpha = 0.5, stroke = 0.5),
                                  draw_key = ggplot2::draw_key_point,

                                  draw_group = function(data, panel_scales, coord) {
                                    data <- data %>%
                                      dplyr::arrange(x)

                                    coords <- coord$transform(data, panel_scales)
                                    circles <- grid::pointsGrob(
                                      x = coords$x,
                                      y = coords$y,
                                      pch = coords$shape,
                                      size = unit(coords$size, "char"),
                                      default.units = "native",
                                      gp = grid::gpar(
                                        col = scales::alpha(coords$col),
                                        fill = scales::alpha(coords$fill, coords$alpha)
                                      )
                                    )
                                    countries <- as.data.frame(unique(coords$y))
                                    colnames(countries)[1] <- "y"
                                    countries <- countries %>%
                                      dplyr::arrange(y)
                                    country_lines <- grid::polylineGrob(
                                      x = unit(rep(c(0, 1), each = length(countries)), "npc"),
                                      y = unit(c(countries, countries), "npc"),
                                      gp = grid::gpar(col = "grey45",
                                                      lwd = .pt
                                      )
                                    )
                                    grid::gList(circles, country_lines)
                                  }
)



