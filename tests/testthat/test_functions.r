context("Test the basic functionality of the package\n\n")
setwd(system.file("extdata", package = "MyEqVizPkg"))

test_that("Clean Data", {
  print("Testing Data Clean Functon")
  eq_data <- eq_clean_data('signif.txt')
  expect_that(eq_data, is_a("data.frame"))
  expect_that(eq_data$DATE, is_a("Date"))
})

test_that("Location Name", {
  print("Testing Location Name Functon")
  eq_data <- eq_clean_data("signif.txt")
  expect_that(eq_data, is_a("data.frame"))
  expect_that(eq_data$DATE, is_a("Date"))

  eq_data$LOCATION_NAME <- eq_data %>% eq_location_clean()
  expect_that(eq_data$LOCATION_NAME, is_a("character"))
})

test_that("eq_create_label(eq_data) creates label", {
  nodata <- data.frame(LOCATION_NAME= character(0), EQ_PRIMARY= character(0), TOTAL_DEATHS = integer(0))
  eq_data <- rbind(nodata, data.frame(LOCATION_NAME = "Some Location", EQ_PRIMARY = "5.7", TOTAL_DEATHS = 5))
  expect_match(eq_create_label(eq_data), "<b>Location:</b> Some Location <br/><b>Magnitude:</b> 5.7 <br/><b>Total deaths:</b> 5")
})


