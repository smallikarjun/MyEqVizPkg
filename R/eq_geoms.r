#' GeomTimeLine
#'
#' GeomTimeLine class construction
#'
#' @importFrom ggplot2 ggproto
#' @importFrom ggplot2 Geom
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 draw_key_point
#' @importFrom grid segmentsGrob
#' @importFrom grid gpar
#' @importFrom grid pointsGrob
#' @importFrom grid unit
#' @importFrom grid gTree
#' @importFrom grid gList
#'
#' @export
#'
GeomTimeLine <- ggplot2::ggproto('GeomTimeLine', ggplot2::Geom,
                                 required_aes = 'x',
                                 default_aes = ggplot2::aes(colour = 'gray',
                                                            fill = NA,
                                                            shape = 19,
                                                            size = 5,
                                                            stroke = 1,
                                                            alpha = 0.5,
                                                            y = 0.2),
                                 draw_key = ggplot2::draw_key_point,
                                 draw_panel = function(data, panel_scales, coord) {
                                     coords <- coord$transform(data, panel_scales)
                                     segment <- grid::segmentsGrob(
                                         x0 = coords$x[which.min(coords$x)],
                                         y0 = coords$y,
                                         x1 = coords$x[which.max(coords$x)],
                                         y1 = coords$y,
                                         gp = grid::gpar(col = coords$colour)
                                     )
                                     points <- grid::pointsGrob(
                                         x = grid::unit(coords$x, 'native'),
                                         y = grid::unit(coords$y, 'native'),
                                         pch = 19,
                                         size = grid::unit(coords$size, 'mm'),
                                         gp = grid::gpar(
                                             col = coords$colour,
                                             fill = coords$fill,
                                             alpha = coords$alpha
                                         )
                                     )
                                     grid::gTree(children = grid::gList(segment, points))
                                 }
)

#' Geom Timeline
#'
#' Plotting a time line of earthquakes. Each point represents an earthquake.
#' Optional aesthetics include color, size, and alpha (for transparency).
#' The x aesthetic is a date and an optional y aesthetic is a factor indicating some
#' stratification in which case multiple time lines will be plotted for each level
#' of the factor (e.g. country)
#'
#' @param mapping Set of aesthetic mappings.
#' @param data The data to be displayed in this layer.
#' @param stat The statistical transformation to use on the data for this layer, as a string.
#' @param position Position adjustment, either as a string, or the result of a call to
#'                 a position adjustment function.
#' @param na.rm If FALSE, the default, missing values are removed with a warning.
#'              If TRUE, missing values are silently removed.
#' @param show.legend Logical. Should this layer be included in the legends?
#' @param inherit.aes If FALSE, overrides the default aesthetics.
#' @param ... Other arguments passed on to layer.
#'
#' @return A ggplot2 layer
#'
#' @importFrom ggplot2 layer
#'
#' @export
#'
#' @examples
#' \dontrun{eq_data <- readr::read_delim('signif.txt', delim = '\t')}
#' \dontrun{
#'     eq_data %>%
#'         eq_clean_data("./data/signif.txt") %>%
#'         dplyr::filter(lubridate::year(DATE) > 2010 & COUNTRY %in% c('INDIA', 'USA')) %>%
#'         ggplot2::ggplot(ggplot2::aes(x = DATE, y = COUNTRY,
#'                                      colour = DEATHS, size = EQ_PRIMARY)) +
#'         geom_timeline(alpha = 0.5) +
#'         theme_timeline
#'
#' }
#'

geom_timeline <- function(mapping = NULL,
                          data = NULL,
                          na.rm = TRUE,
                          position = "identity",
                          stat = "identity",
                          show.legend = NA,
                          inherit.aes = TRUE, ...) {
  ggplot2::layer(
    geom = GeomTimeLine,
    mapping = mapping,
    data = data,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...))
}

#' Theme Timeline
#'
#' A new theme for geom_timeline
#'
#' @importFrom ggplot2 theme_classic
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 element_blank
#'
#' @export
#'
theme_timeline <- ggplot2::theme_classic() +
    ggplot2::theme(axis.line.y = ggplot2::element_blank(),
                   axis.ticks.y = ggplot2::element_blank(),
                   axis.title.y = ggplot2::element_blank()
    )


#' GeomTimeLineLabel
#'
#' GeomTimeLineLabel class constraction
#'
#' @importFrom ggplot2 ggproto
#' @importFrom ggplot2 Geom
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 draw_key_blank
#' @importFrom grid segmentsGrob
#' @importFrom grid gpar
#' @importFrom grid textGrob
#' @importFrom grid gTree
#' @importFrom grid gList
#' @importFrom dplyr group_by
#' @importFrom dplyr top_n
#' @importFrom dplyr ungroup
#'
#' @export
#'
GeomTimeLineLabel <- ggplot2::ggproto('GeomTimeLineLabel', ggplot2::Geom,
                                      required_aes = c('x', 'label'),
                                      default_aes = ggplot2::aes(y = 0.2,
                                                                 n_max = 3,
                                                                 size = 0,
                                                                 alpha = 0.5,
                                                                 colour = 'black'),
                                      draw_key = ggplot2::draw_key_blank,
                                      draw_panel = function(data, panel_scales, coord) {
                                          data %<>% dplyr::group_by(y) %>%
                                              dplyr::top_n(n = data$n_max[1], wt = size) %>%
                                              dplyr::ungroup()
                                          coords <- coord$transform(data, panel_scales)
                                          segment <- grid::segmentsGrob(
                                              x0 = coords$x,
                                              x1 = coords$x,
                                              y0 = coords$y,
                                              y1 = coords$y + 0.1,
                                              gp = grid::gpar(col = coords$colour,
                                                              alpha = coords$alpha)
                                          )
                                          text <- grid::textGrob(
                                              label = coords$label,
                                              x = coords$x,
                                              y = coords$y + 0.11,
                                              rot = 65,
                                              just = 'left',
                                              gp = grid::gpar(fontsize = 9)
                                          )
                                          grid::gTree(children = grid::gList(segment, text))
                                      }
)


#'
#' Geom Timelinelabel
#'
#' Add annotations to the earthquake data.
#' This geom adds a vertical line to each data point with a text annotation
#' (e.g. the location of the earthquake) attached to each line.
#' We can subset to n_max number of earthquakes, where we take the n_max largest (by magnitude)
#' earthquakes. Aesthetics are x, which is the date of the earthquake and label which takes
#' the column name from which annotations will be obtained.
#'
#' @inheritParams geom_timeline
#'
#' @return A ggplot2 layer
#'
#' @export
#'
#' @examples
#' \dontrun{eq_data <- readr::read_delim('signif.txt', delim = '\t')}
#' \dontrun{
#'     eq_data %>%
#'         eq_clean_data("./data/signif.txt") %>%
#'         dplyr::filter(lubridate::year(DATE) > 2010 & COUNTRY %in% c('INDIA', 'USA')) %>%
#'         eq_location_clean() %>%
#'         ggplot2::ggplot(ggplot2::aes(x = DATE, y = COUNTRY,
#'                                      colour = DEATHS, size = EQ_PRIMARY)) +
#'         geom_timeline(alpha = 0.5) +
#'         geom_timelinelabel(ggplot2::aes(label = LOCATION_NAME, n_max = 3)) +
#'         theme_timeline
#'
#' }
#'
geom_timelinelabel <- function(mapping = NULL,
							   data = NULL,
							   stat = "identity",
                               position = "identity",
							   na.rm = TRUE,
                               show.legend = NA,
							   inherit.aes = TRUE, ...) {
    ggplot2::layer(
        geom = GeomTimeLineLabel,
		mapping = mapping,
        data = data,
		stat = stat,
		position = position,
        show.legend = show.legend,
		inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, ...)
    )
}
