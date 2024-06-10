test_that("Calculation works", {
  MEDI = c(rep(1, 6), rep(250, 4), rep(500, 5), rep(750, 4), rep(1, 5))
  Datetime = lubridate::as_datetime(lubridate::dhours(0:23), tz = "UTC")
  expect_equal(
    timing_above_threshold(MEDI, Datetime, threshold = 250), 
    list(
      "mean" = lubridate::as_datetime(lubridate::dhours(12), tz = "UTC"),
      "first" = lubridate::as_datetime(lubridate::dhours(6), tz = "UTC"),
      "last" = lubridate::as_datetime(lubridate::dhours(18), tz = "UTC")
    )
  )
  expect_equal(
    timing_above_threshold(MEDI, Datetime, comparison = "below", threshold = 250), 
    list(
      "mean" = lubridate::as_datetime(lubridate::dhours(10), tz = "UTC"),
      "first" = lubridate::as_datetime(lubridate::dhours(0), tz = "UTC"),
      "last" = lubridate::as_datetime(lubridate::dhours(23), tz = "UTC")
    )
  )
  expect_equal(
    timing_above_threshold(MEDI, Datetime, threshold = c(250, 500)), 
    list(
      "mean" = lubridate::as_datetime(lubridate::dhours(10), tz = "UTC"),
      "first" = lubridate::as_datetime(lubridate::dhours(6), tz = "UTC"),
      "last" = lubridate::as_datetime(lubridate::dhours(14), tz = "UTC")
    )
  )
})

test_that("Works with different time representations", {
  MEDI = c(rep(1, 6), rep(250, 13), rep(1, 5))
  Datetime = lubridate::as_datetime(lubridate::dhours(0:23), tz = "UTC")
  HMS = hms::as_hms(Datetime)
  Duration = lubridate::as.duration(HMS)
  Difftime = Datetime-Datetime[1]
  expect_equal(
    timing_above_threshold(MEDI, HMS, threshold = 250), 
    list(
      "mean" = hms::hms(0,0,12),
      "first" = hms::hms(0,0,6),
      "last" = hms::hms(0,0,18)
    )
  )
  expect_equal(
    timing_above_threshold(MEDI, Duration, threshold = 250), 
    list(
      "mean" = lubridate::dhours(12),
      "first" = lubridate::dhours(6),
      "last" = lubridate::dhours(18)
    )
  )
  expect_equal(
    timing_above_threshold(MEDI, Difftime, threshold = 250), 
    list(
      "mean" = lubridate::as.difftime(lubridate::dhours(12)),
      "first" = lubridate::as.difftime(lubridate::dhours(6)),
      "last" = lubridate::as.difftime(lubridate::dhours(18))
    )
  )
})

test_that("Handling of missing values works", {
  MEDI = c(rep(1, 6), rep(250, 12), NA, rep(1, 5))
  Datetime = lubridate::as_datetime(lubridate::dhours(0:23), tz = "UTC")
  expect_equal(
    timing_above_threshold(MEDI, Datetime, threshold = 250), 
    list(
      "mean" = lubridate::as_datetime(lubridate::dhours(NA), tz = "UTC"),
      "first" = lubridate::as_datetime(lubridate::dhours(6), tz = "UTC"),
      "last" = lubridate::as_datetime(lubridate::dhours(NA), tz = "UTC")
    )
  )
  expect_equal(
    timing_above_threshold(MEDI, Datetime, threshold = 250, na.rm = TRUE), 
    list(
      "mean" = lubridate::as_datetime(lubridate::dhours(11.5), tz = "UTC"),
      "first" = lubridate::as_datetime(lubridate::dhours(6), tz = "UTC"),
      "last" = lubridate::as_datetime(lubridate::dhours(17), tz = "UTC")
    )
  )
})

test_that("Return data frame works", {
  MEDI = c(rep(1, 6), rep(250, 13), rep(1, 5))
  Datetime = lubridate::as_datetime(lubridate::dhours(0:23), tz = "UTC")
  expect_equal(
    timing_above_threshold(MEDI, Datetime, threshold = 250, as.df = TRUE), 
    tibble::tibble(
      "mean_timing_above_250" = lubridate::as_datetime(lubridate::dhours(12), tz = "UTC"),
      "first_timing_above_250" = lubridate::as_datetime(lubridate::dhours(6), tz = "UTC"),
      "last_timing_above_250" = lubridate::as_datetime(lubridate::dhours(18), tz = "UTC")
    )
  )
})
