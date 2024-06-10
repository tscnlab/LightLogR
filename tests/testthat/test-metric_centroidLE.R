test_that("Calculation works", {
  MEDI = c(rep(0, 6), rep(250, 12), rep(0, 6))
  Datetime = lubridate::as_datetime(lubridate::dhours(0:23), tz = "UTC")
  expect_equal(centroidLE(MEDI, Datetime), lubridate::as_datetime(lubridate::dhours(11.5), tz = "UTC"))
  expect_equal(centroidLE(MEDI, Datetime, bin.size = "2 hours"), lubridate::as_datetime(lubridate::dhours(11), tz = "UTC"))
})

test_that("Missing values", {
  MEDI = c(rep(0, 6), rep(250, 12), NA, NA, rep(0, 4))
  Datetime = lubridate::as_datetime(lubridate::dhours(0:23), tz = "UTC")
  expect_equal(centroidLE(MEDI, Datetime), lubridate::as_datetime(NA))
  expect_equal(centroidLE(MEDI, Datetime, na.rm = TRUE), lubridate::as_datetime(lubridate::dhours(11.5), tz = "UTC"))
})

test_that("Works with different date time units", {
  MEDI = c(rep(0, 6), rep(250, 12), rep(0, 6))
  Datetime = lubridate::as_datetime(lubridate::dhours(0:23), tz = "UTC")
  HMS = hms::as_hms(Datetime)
  Duration = lubridate::as.duration(HMS)
  Difftime = Datetime-Datetime[1]
  expect_equal(centroidLE(MEDI, HMS), hms::hms(0,30,11))
  expect_equal(centroidLE(MEDI, Duration), lubridate::dhours(11.5))
  expect_equal(centroidLE(MEDI, Difftime), lubridate::as.difftime(lubridate::dhours(11.5)))
})

test_that("Return data frame works", {
  MEDI = c(rep(0, 6), rep(250, 12), rep(0, 6))
  Datetime = lubridate::as_datetime(lubridate::dhours(0:23), tz = "UTC")
  expect_equal(
    centroidLE(MEDI, Datetime, as.df = TRUE),
    tibble::tibble(
      "centroidLE" = lubridate::as_datetime(lubridate::dhours(11.5), tz = "UTC"),
    )
  )
})