test_that("Population variance works", {
  MEDI = sample.data.environment$MEDI
  Datetime = sample.data.environment$Datetime
  expect_equal(trunc_decimal(interdaily_stability(MEDI, Datetime),2), 0.77)
})

test_that("Sample variance works", {
  MEDI = sample.data.environment$MEDI
  Datetime = sample.data.environment$Datetime
  expect_equal(trunc_decimal(interdaily_stability(MEDI, Datetime, use.samplevar = TRUE),2), 0.79)
})

test_that("IS for Gaussian noise near 2", {
  N = 24*1000
  MEDI = rnorm(N)
  Datetime = lubridate::as_datetime(lubridate::dhours(0:(N-1)), tz = "UTC")
  expect_equal(round(interdaily_stability(MEDI, Datetime),1), 0)
})

test_that("IS equals 1 for perfect IS", {
  MEDI = rep(c(rep(1, 6), rep(250, 4), rep(500, 5), rep(750, 4), rep(1, 5)),7)
  Datetime = lubridate::as_datetime(lubridate::dhours(0:(24*7-1)), tz = "UTC")
  expect_equal(interdaily_stability(MEDI, Datetime), 1)
})

test_that("Handling missing values works", {
  MEDI = rep(c(as.numeric(NA), rep(1, 5), rep(250, 4), rep(500, 5), rep(750, 4), rep(1, 5)),7)
  Datetime = lubridate::as_datetime(lubridate::dhours(0:(24*7-1)), tz = "UTC")
  expect_equal(suppressWarnings({interdaily_stability(MEDI, Datetime)}), as.numeric(NA))
  expect_equal(suppressWarnings({interdaily_stability(MEDI, Datetime, na.rm=TRUE)}), 1)
})

test_that("Return data frame works", {
  MEDI = sample.data.environment$MEDI
  Datetime = sample.data.environment$Datetime
  expect_equal(
    round(interdaily_stability(MEDI, Datetime, as.df = TRUE), 1), 
    tibble::tibble("interdaily_stability" = 0.8)
  )
})



