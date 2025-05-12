test_that("Calculation works", {
  MEDI = c(0,100,99,0,0,101,100,0)
  datetime = lubridate::as_datetime((1:8)*60)
  
  expect_equal(
    duration_above_threshold(MEDI, datetime, threshold = 100), lubridate::dminutes(3)
  )
  expect_equal(
    duration_above_threshold(MEDI, datetime, threshold = 102), lubridate::dminutes(0)
  )
  expect_equal(
    duration_above_threshold(MEDI, datetime, "below", 100), lubridate::dminutes(7)
  )
  expect_equal(
    duration_above_threshold(MEDI, datetime, threshold = c(0,100)), lubridate::dminutes(5)
  )
})

test_that("Handling of missing values", {
  MEDI = c(0,100,99,0,NA,101,100,0)
  datetime = lubridate::as_datetime((1:8)*60)
  expect_equal(
    duration_above_threshold(MEDI, datetime, threshold = 100), lubridate::dminutes(NA)
  )
  expect_equal(
    duration_above_threshold(MEDI, datetime, threshold = 100, na.rm = TRUE), 
    lubridate::dminutes(3)
  )
})

test_that("Return data frame", {
  MEDI = c(0,100,99,0,0,101,100,0)
  datetime = lubridate::as_datetime((1:8)*60)
  expect_equal(
    duration_above_threshold(MEDI, datetime, threshold = 100, as.df = TRUE), 
    tibble::tibble("duration_above_100" = lubridate::dminutes(3))
  )
  expect_equal(
    duration_above_threshold(MEDI, datetime, comparison = "below", threshold = 100, as.df = TRUE), 
    tibble::tibble("duration_below_100" = lubridate::dminutes(7))
  )
})

test_that("Input checks", {
  expect_error(
    duration_above_threshold("100", lubridate::as_datetime(1), threshold = 100), 
    "`Light.vector` must be numeric!"
  )
  expect_error(
    duration_above_threshold(100, 1, threshold = 100), 
    "`Time.vector` must be POSIXct, hms, duration, or difftime!"
  )
  expect_error(
    duration_above_threshold(100, lubridate::as_datetime(1), threshold = "100"), 
    "`threshold` must be numeric!"
  )
  expect_error(
    duration_above_threshold(100, lubridate::as_datetime(1), threshold = c(1,2,3)), 
    "`threshold` must be either one or two values!"
  )
  expect_error(
    duration_above_threshold(100, lubridate::as_datetime(1), threshold = 100, epoch = 60), 
    "`epoch` must either be a duration or a string"
  )
  expect_error(
    duration_above_threshold(100, lubridate::as_datetime(1), threshold = 100, na.rm="FALSE"), 
    "`na.rm` must be logical!"
  )
  expect_error(
    duration_above_threshold(100, lubridate::as_datetime(1), threshold = 100, as.df="TRUE"), 
    "`as.df` must be logical!"
  )
})
