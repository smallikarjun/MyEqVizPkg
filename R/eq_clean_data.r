#' @title Clean NOAA Significant Earthquake Database Dataset
#'
#' This function takes raw NOAA data frame and returns a clean data frame
#'  1) A date column created by uniting the year, month, day
#'     and converting it to the Date class
#'  2) LATITUDE and LONGITUDE columns converted to numeric class
#'
#' @param raw_data_file  raw NOAA dataframe .txt file location
#'
#' @return clean dataframe with Date, latitude, longitude conversion
#'
#' @examples
#' \dontrun{
#'  eq_clean_data("signif.txt")
#' }
#'
#' @importFrom readr read_delim
#' @importFrom tidyr unite
#' @importFrom tidyr drop_na
#' @importFrom dplyr mutate
#' @importFrom dplyr %>%
#' @importFrom lubridate ymd
#'
#' @export
eq_clean_data <- function(raw_data_file) {
  raw_data <- readr::read_delim(file = raw_data_file, delim = "\t")
  raw_data  %>%
    #Remove values less than 1000 year
    dplyr::filter_(~YEAR > 1000) %>%
    tidyr::drop_na_(c("YEAR", "MONTH", "DAY")) %>%
    tidyr::unite_("DATE", c("YEAR","MONTH","DAY"), remove = TRUE, sep = "-") %>%
    tidyr::drop_na_(c("DATE")) %>%
    dplyr::mutate_(DATE = ~as.Date(lubridate::ymd(DATE)),
                    LATITUDE  = ~as.numeric(LATITUDE) ,
                    LONGITUDE = ~as.numeric(LONGITUDE) ,
                    EQ_PRIMARY = ~as.double(EQ_PRIMARY) ,
                    TOTAL_DEATHS = ~as.numeric(TOTAL_DEATHS))
}


#' @title Clean LOCATION_NAME Column
#'
#' @description This function cleans the LOCATION_NAME column by stripping out
#' the country name (including the colon) and converts names to title case.
#'
#' @param eq_dataset_location A dataframe of NOAA significant earthquakes data
#'
#' @return A dataframe with a clean LOCATION_NAME Column
#'
#' @importFrom tools toTitleCase
#'
#' @examples
#' \dontrun{
#'  eq_loc_data <- eq_location_clean(eq_raw_data)
#' }
#'
#' @export

eq_location_clean <- function(eq_dataset_location) {
  eq_dataset_location$LOCATION_NAME <-
    tools::toTitleCase(tolower(trimws(
      gsub(".*:", "", eq_dataset_location$LOCATION_NAME)
    )))
}

# List relevant packages
#packages <- c('readr', 'dplyr', 'stringr', 'tidyr', 'magrittr', 'lubridate', 'ggmap', 'geosphere', 'ggplot2')

# Load packages
#lapply(packages, require, character.only = TRUE)

#eq_raw_data <- eq_clean_data('signif.txt')
#eq_loc_data <- eq_location_clean(eq_raw_data)
