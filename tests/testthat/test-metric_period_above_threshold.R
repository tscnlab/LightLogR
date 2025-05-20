test_that("Calculation works", {
  MEDI = c(10,10,10,0,0,11,12,13,14)
  datetime = lubridate::as_datetime(lubridate::dminutes(1:9))
  
  expect_equal(
    period_above_threshold(MEDI, datetime, threshold = 10), lubridate::dminutes(4)
  )
  expect_equal(
    period_above_threshold(MEDI, datetime, "below", 10), lubridate::dminutes(5)
  )
  expect_equal(
    period_above_threshold(MEDI, datetime, threshold = c(11,13)), lubridate::dminutes(2)
  )
  expect_equal(
    period_above_threshold(MEDI, datetime, threshold = 10, loop = TRUE), lubridate::dminutes(7)
  )
})

test_that("Handling of missing values", {
  MEDI = c(0,10,10,0,0,NA,12,13,14)
  datetime = lubridate::as_datetime(lubridate::dminutes(1:9))
  expect_equal(
    period_above_threshold(MEDI, datetime, threshold = 10), lubridate::dminutes(NA)
  )
  expect_equal(
    period_above_threshold(MEDI, datetime, threshold = 10, na.rm = TRUE),
    lubridate::dminutes(3)
  )
  expect_equal(
    period_above_threshold(MEDI, datetime, "below", 10, na.replace = TRUE),
    lubridate::dminutes(5)
  )
})

test_that("Return data frame", {
  MEDI = c(0,10,10,0,0,11,12,13,14)
  datetime = lubridate::as_datetime(lubridate::dminutes(1:9))
  expect_equal(
    period_above_threshold(MEDI, datetime, threshold = 10, as.df = TRUE), 
    tibble::tibble("period_above_10" = lubridate::dminutes(4))
  )
  expect_equal(
    period_above_threshold(MEDI, datetime, "below", threshold = 10, as.df = TRUE), 
    tibble::tibble("period_below_10" = lubridate::dminutes(5))
  )
})