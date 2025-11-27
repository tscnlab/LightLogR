test_that("style_time formats numeric seconds", {
  seconds <- c(0, 3661, 43200)
  expect_equal(
    style_time(seconds),
    c("00:00", "01:01", "12:00")
  )
})

test_that("style_time handles time-like objects", {
  hms_times <- hms::as_hms(c(90, 3600))
  difftimes <- as.difftime(c(120, 7200), units = "secs")
  posix_times <- as.POSIXct("2024-01-01 03:45:30", tz = "UTC")

  expect_equal(style_time(hms_times), c("00:01", "01:00"))
  expect_equal(style_time(difftimes), c("00:02", "02:00"))
  expect_equal(style_time(posix_times), "03:45")
})

test_that("style_time errors on unsupported input", {
  expect_error(style_time("not a time"),
               "x needs to be numeric, hms, difftime, POSIXct, or POSIXt")
})
