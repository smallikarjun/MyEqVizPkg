#' Visualize earthquakes data on leaflet Map.
#'
#' This function takes an argument data containing the filtered data frame with earthquakes
#' to visualize. The function maps the epicenters (LATITUDE/LONGITUDE) and annotates
#' each point with in pop up window containing annotation data stored in a column of
#' the data frame.
#'
#' @param data Filtered dataframe of NOAA earthquakes dataset
#' @param annot_col Column name from dataset used for the annotation in the pop-up
#'
#' @return leaflet MAP shown with a circle markers, and the radius of the circle marker is
#'          proportional to the earthquake's magnitude.
#' @examples
#' \dontrun{
#'  eq_map(eq_clean_data, annot_col = "DATE")
#'
#'  eq_data %>%
#'        dplyr::filter(COUNTRY %in% COUNTRIES & lubridate::year(DATE) >= 2000) %>%
#'        eq_map(annot_col = "DATE")
#' }
#'
#' @importFrom leaflet leaflet
#' @importFrom leaflet addTiles
#' @importFrom leaflet addCircleMarkers
#'
#' @export
eq_map <- function(data, annot_col) {
  leaflet::leaflet() %>%
    leaflet::addTiles() %>%
    leaflet::addCircleMarkers(data = data,
							  radius = data$EQ_PRIMARY ,
							  stroke = FALSE,
							  fillOpacity = 0.5,
							  lng = ~ LONGITUDE,
							  lat = ~ LATITUDE,
							  popup = data[[annot_col]]
    )
}


#' Creates an HTML label to use in Popup text for Location, Magnitude and Total number of Deaths.
#'
#' Creates an HTML label that can be used as the annotation text in the leaflet map.
#' This function put together a character string for each earthquake that will show
#' the cleaned location , the magnitude (EQ_PRIMARY), and
#' the total number of deaths (TOTAL_DEATHS),
#' with boldface labels for each ("Location", "Total deaths", and "Magnitude").
#' If an earthquake is missing values for any of these, both the label and the
#' value will be skipped for that element of the tag.
#'
#' @param data Filtered dataframe of NOAA earthquakes dataset
#'
#' @return Popup text in Html label for Location, Magnitude and Total number of Deaths
#'
#' @examples
#' \dontrun{
#'  eq_create_label(eq_clean_data)
#'
#'  eq_data %>%
#'        dplyr::filter(COUNTRY %in% COUNTRIES & lubridate::year(DATE) >= 2000) %>%
#'        dplyr::mutate(popup_text = eq_create_label(.)) %>%
#'        eq_map(annot_col = "popup_text")
#' }
#'
#' @export
eq_create_label <- function(data) {
  location_name <- ifelse(is.na(data$LOCATION_NAME), "",
      paste("<b>Location:</b>", data$LOCATION_NAME)
    )
  magnitude <- ifelse(is.na(data$EQ_PRIMARY), "",
      paste("<br/><b>Magnitude:</b>", data$EQ_PRIMARY)
    )
  deaths <- ifelse(is.na(data$TOTAL_DEATHS), "",
      paste("<br/><b>Total deaths:</b>", data$TOTAL_DEATHS)
    )
  paste(location_name, magnitude, deaths)
}
