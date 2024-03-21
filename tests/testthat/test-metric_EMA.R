test_that("Half life works", {
  light <- c(rep(10,90), rep(0,90))
  datetime <- lubridate::as_datetime(0) + lubridate::minutes(1:180)
  ema <- exponential_moving_average(light, datetime, decay = "90 mins")
  expect_equal(round(ema[90]/2, 1), round(ema[180], 1))
})

test_that("Exponential works", {
  light <- c(rep(10,90), rep(0,90))
  datetime <- lubridate::as_datetime(0) + lubridate::minutes(1:180)
  ema <- exponential_moving_average(light, datetime, decay = "90 mins")
  expect_equal(cor(log(ema[91:180]), 1:90), -1)
})

test_that("Handling missing values works", {
  light <- c(rep(10,90), rep(NA, 45), rep(0,45))
  datetime <- lubridate::as_datetime(0) + lubridate::minutes(1:180)
  expect_warning(ema <- exponential_moving_average(light, datetime, decay = "90 mins"),
                 "Light data contains missing values! They are replaced by 0.")
  expect_equal(cor(log(ema[91:180]), 1:90), -1)
  expect_equal(any(is.na(ema)), FALSE)
})

test_that("Input checks", {
  expect_error(exponential_moving_average("1",lubridate::as_datetime(0)), 
               "`Light.vector` must be numeric!")
  expect_error(exponential_moving_average(1, "20/01/2022 10:00:00"), 
               "`Time.vector` must be POSIXct, hms, duration, or difftime!")
  expect_error(exponential_moving_average(1, 12), 
               "`Time.vector` must be POSIXct, hms, duration, or difftime!")
  expect_error(exponential_moving_average(1, lubridate::as_datetime(0), decay = 1), 
               "`decay` must either be a duration or a string")
  expect_error(exponential_moving_average(1, lubridate::as_datetime(0), epoch = 1), 
               "`epoch` must either be a duration or a string")
})