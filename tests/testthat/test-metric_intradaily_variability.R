test_that("Population variance works", {
  MEDI = sample.data.environment$MEDI
  Datetime = sample.data.environment$Datetime
  expect_equal(trunc_decimal(intradaily_variability(MEDI, Datetime),3), 0.166)
})

test_that("Sample variance works", {
  MEDI = sample.data.environment$MEDI
  Datetime = sample.data.environment$Datetime
  expect_equal(trunc_decimal(intradaily_variability(MEDI, Datetime, use.samplevar = TRUE),3), 0.165)
})

test_that("IV for Gaussian noise near 2", {
  N = 24*1000
  MEDI = rnorm(N)
  Datetime = lubridate::as_datetime(lubridate::dhours(0:(N-1)), tz = "UTC")
  expect_equal(round(intradaily_variability(MEDI, Datetime)), 2)
})

test_that("IV for Sine wave near 0", {
  MEDI = sin(seq(0,7*pi,length.out = 24*7))
  Datetime = lubridate::as_datetime(lubridate::dhours(0:(24*7-1)), tz = "UTC")
  expect_equal(round(intradaily_variability(MEDI, Datetime),1), 0)
})

test_that("Handling missing values works", {
  MEDI = rep(c(as.numeric(NA), rep(1, 5), rep(250, 4), rep(500, 5), rep(750, 4), rep(1, 5)),7)
  Datetime = lubridate::as_datetime(lubridate::dhours(0:(24*7-1)), tz = "UTC")
  expect_equal(suppressWarnings({intradaily_variability(MEDI, Datetime)}),as.numeric(NA))
  expect_equal(suppressWarnings({round(intradaily_variability(MEDI, Datetime, na.rm=TRUE),1)}), 0.4)
})

test_that("Return data frame works", {
  MEDI = sample.data.environment$MEDI
  Datetime = sample.data.environment$Datetime
  expect_equal(
    round(intradaily_variability(MEDI, Datetime, as.df = TRUE), 2), 
    tibble::tibble("intradaily_variability" = 0.17)
  )
})