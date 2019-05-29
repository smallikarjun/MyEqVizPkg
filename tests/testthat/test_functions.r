context("Test the basic functionality of the package\n\n")
setwd(system.file("extdata", package = "MyEqVizPkg"))
eq_data <- eq_clean_data("signif.txt")

test_that("Clean Data", {
  print("Testing Data Clean Function")
  expect_that(eq_data, is_a("data.frame"))
  expect_that(eq_data$DATE, is_a("Date"))
})

test_that("Location Name", {
  print("Testing Location Name Function")
  expect_that(eq_data, is_a("data.frame"))
  expect_that(eq_data$DATE, is_a("Date"))
  eq_data$LOCATION_NAME <- eq_data %>% eq_location_clean()
  expect_that(eq_data$LOCATION_NAME, is_a("character"))
})

test_that("eq_create_label(eq_data) creates label", {
  print("Testing Create Label Function")
  nodata <- data.frame(LOCATION_NAME= character(0), EQ_PRIMARY= character(0), TOTAL_DEATHS = integer(0))
  eq_loc_data <- rbind(nodata, data.frame(LOCATION_NAME = "Some Location", EQ_PRIMARY = "5.7", TOTAL_DEATHS = 5))
  expect_match(eq_create_label(eq_loc_data), "<b>Location:</b> Some Location <br/><b>Magnitude:</b> 5.7 <br/><b>Total deaths:</b> 5")
})

test_that("eq_clean_data returns numeric coordinates", {
  print("Latitude and Longitude are supposed to be numeric")
  expect_true(is.numeric(eq_data$LATITUDE))
  expect_true(is.numeric(eq_data$LONGITUDE))
})

test_that("eq_map returns leaflet object", {
  print("eq_map returns a leaflet object")
  map <- eq_data %>%
    dplyr::filter(COUNTRY == "USA" & lubridate::year(DATE) >= 2010) %>%
    eq_map(annot_col = "DATE")
  expect_true(any(class(map) == "leaflet"))
})

test_that("geom_timeline returns ggplot object", {
  print("geom_timeline returns ggplot object")
  g <- eq_data %>%
    dplyr::filter(COUNTRY %in% c("GREECE", "ITALY"), lubridate::year(DATE) > 2000) %>%
    ggplot2::ggplot(ggplot2::aes(x = DATE,
                                 y = COUNTRY,
                                 color = as.numeric(TOTAL_DEATHS),
                                 size = as.numeric(EQ_PRIMARY)
    )) +
    geom_timeline()
  expect_is(g, "ggplot")
})

test_that("geom_timelinelabel returns ggplot object", {
  print("geom_timelinelabel returns ggplot object")
  g <- eq_data %>%
    dplyr::filter(COUNTRY %in% c("GREECE", "ITALY"), lubridate::year(DATE) > 2000) %>%
    ggplot2::ggplot(ggplot2::aes(x = DATE,
                                 y = COUNTRY,
                                 color = as.numeric(TOTAL_DEATHS),
                                 size = as.numeric(EQ_PRIMARY)
    )) +
    geom_timelinelabel(aes(label = LOCATION_NAME))
  expect_is(g, "ggplot")
})
