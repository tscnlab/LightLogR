test_that("Calculation works", {
  MEDI <- c(0,100,99,0,0,101,100,0)
  datetime <- lubridate::as_datetime((1:8)*60)
  expect_equal(
    threshold_for_duration(MEDI, datetime, duration = lubridate::dminutes(3)), 100
  )
  expect_equal(
    threshold_for_duration(MEDI, datetime, lubridate::dminutes(7), "below"), 100
  )
})

test_that("Handling of missing values", {
  MEDI <- c(0,100,99,0,NA,101,100,0)
  datetime <- lubridate::as_datetime((1:8)*60)
  expect_equal(
    threshold_for_duration(MEDI, datetime, lubridate::dminutes(3)), as.double(NA)
  )
  expect_equal(
    threshold_for_duration(MEDI, datetime, lubridate::dminutes(3), na.rm = TRUE), 100
  )
})

test_that("Return data frame", {
  MEDI <- c(0,100,99,0,0,101,100,0)
  datetime <- lubridate::as_datetime((1:8)*60)
  expect_equal(
    threshold_for_duration(MEDI, datetime, lubridate::dminutes(3), as.df = TRUE), 
    tibble::tibble("threshold_above_for_3_minutes" = 100)
  )
  expect_equal(
    threshold_for_duration(MEDI, datetime, lubridate::dminutes(7), "below", as.df = TRUE), 
    tibble::tibble("threshold_below_for_7_minutes" = 100)
  )
})

test_that("Input checks", {
  expect_error(
    threshold_for_duration("100", lubridate::as_datetime(1), lubridate::dminutes(3)), 
    "`Light.vector` must be numeric!"
  )
  expect_error(
    threshold_for_duration(100, 1, lubridate::dminutes(3)), 
    "`Time.vector` must be POSIXct, hms, duration, or difftime!"
  )
  expect_error(
    threshold_for_duration(100, lubridate::as_datetime(1), 3), 
    "`duration` must either be duration or a string!"
  )
  expect_error(
    threshold_for_duration(100, lubridate::as_datetime(1), lubridate::dminutes(3), epoch = 60), 
    "`epoch` must either be a duration or a string"
  )
  expect_error(
    threshold_for_duration(100, lubridate::as_datetime(1), lubridate::dminutes(3), as.df = "TRUE"), 
    "`as.df` must be logical!"
  )
})

