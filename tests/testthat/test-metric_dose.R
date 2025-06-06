test_that("dose calculates correctly for regular time series", {
  light <- c(100, 100, 100, 100) # 100lx
  time_posixct <- lubridate::as_datetime(0) + lubridate::hours(0:3) # 4 points, 1h epoch
  # Expected: sum(100,100,100,100) * 1hour / 3600 = 400 * 3600s / 3600s = 400 lx*h (error in formula logic)
  # Correct: sum of (value * interval_duration_in_hours)
  # If epoch is 1h, then each of the 4 values represents a 1-hour interval.
  # So, sum(100*1, 100*1, 100*1, 100*1) -> this is if light vector are averages over intervals.
  # The function's current logic: sum(Value) * (epoch_seconds/3600)
  # Here, epoch = 1 hour = 3600s.
  # dose = (100+100+100+100) * (3600/3600) = 400 * 1 = 400 lx*h.
  expect_equal(dose(light, time_posixct, epoch = "1 hour"), 400)
  
  time_hms <- hms::as_hms(time_posixct)
  expect_equal(dose(light, time_hms, epoch = "1 hour"), 400)
  
  # If epoch is 30 mins, and 4 points, total duration represented is 4*30min = 2 hours.
  # dose = sum(light) * (1800/3600) = 400 * 0.5 = 200 lx*h
  expect_equal(dose(light, time_posixct, epoch = "30 mins", na.rm = TRUE), 200)
})

test_that("dose handles na.rm correctly", {
  light_na <- c(100, NA, 100, NA)
  time <- lubridate::as_datetime(0) + lubridate::hours(0:3)
  
  # na.rm = FALSE (default)
  # sum(100, NA, 100, NA) = NA. NA * epoch_in_hours = NA.
  expect_true(is.na(dose(light_na, time, epoch = "1 hour", na.rm = FALSE)))
  
  # na.rm = TRUE
  # sum(100, 100, na.rm=T) = 200.
  # The NAs are in `Value`. `gap_handler` does not change these original NAs.
  # `sum(Value, na.rm=T)` means `sum(c(100,NA,100,NA), na.rm=T)` -> 200
  # dose = 200 * (3600/3600) = 200 lx*h.
  expect_equal(dose(light_na, time, epoch = "1 hour", na.rm = TRUE), 200)
})

test_that("dose handles irregular time series by aggregation", {
  light_irreg <- c(100, 150, 200) # Values at 0h, 0.5h, 1.5h
  time_irreg <- lubridate::as_datetime(0) + lubridate::minutes(c(0, 30, 90))
  # Epoch = "1 hour".
  # aggregate_Datetime to 1 hour:
  #   Interval 0h-1h: contains 100 (at 0h), 150 (at 0.5h). Mean = 125. Timestamped at 0h.
  #   Interval 1h-2h: contains 200 (at 1.5h). Mean = 200. Timestamped at 1h.
  # Aggregated data: Value = c(125, 200), Datetime = c(0h, 1h). This data is regular.
  # dose = sum(100, 150, 200) * (1hour_in_sec/3600) = 450 * 1 = 450 lx*h.
  expect_equal(dose(light_irreg, time_irreg, na.rm = TRUE, epoch = "1 hour"), 450)
  
  # With na.rm = TRUE during aggregation and final sum
  light_irreg_na <- c(100, NA, 200)
  # aggregate_Datetime:
  #   Interval 0h-1h: 100 (at 0h), NA (at 0.5h). Mean(100,NA,na.rm=T)=100. Timestamp 0h.
  #   Interval 1h-2h: 200 (at 1.5h). Mean = 200. Timestamp 1h.
  # Aggregated: Value=c(100,200).
  # dose = sum(100,200) * 1 = 300.
  expect_equal(dose(light_irreg_na, time_irreg, epoch = "1 hour", na.rm = TRUE), 300)
})

test_that("dose handles gaps in time series by filling with NA", {
  light_gap <- c(100, 200) # Values at 0h, 2h (gap at 1h)
  time_gap <- lubridate::as_datetime(0) + lubridate::hours(c(0, 2))
  # Epoch = "1 hour"
  # Data is regular but gappy. gap_handler fills:
  #   Value = c(100, NA, 200), Datetime = c(0h, 1h, 2h)
  # na.rm = FALSE: sum(100, NA, 200) = NA. Dose = NA.
  expect_true(is.na(dose(light_gap, time_gap, epoch = "1 hour", na.rm = FALSE)))
  
  # na.rm = TRUE: sum(100, NA, 200, na.rm=T) = 300. Dose = 300 * 1 = 300.
  expect_equal(dose(light_gap, time_gap, epoch = "1 hour", na.rm = TRUE), 300)
})

test_that("dose returns data.frame when as.df = TRUE", {
  light <- c(50, 50)
  time <- lubridate::as_datetime(0) + lubridate::hours(0:1)
  result_df <- dose(light, time, epoch = "1 hour", as.df = TRUE)
  expect_s3_class(result_df, "data.frame")
  expect_named(result_df, "dose")
  expect_equal(result_df$dose, 100) # sum(50,50) * 1 = 100
})

test_that("dose input validation works", {
  expect_error(dose("a", Sys.time()), "`Light.vector` must be numeric!")
  expect_error(dose(1, "b"), "`Time.vector` must be POSIXct, hms, duration, or difftime!")
  expect_error(dose(c(1,2), Sys.time()), "`Light.vector` and `Time.vector` must be same length!")
  expect_error(dose(1, Sys.time(), epoch = 123), "`epoch` must either be a duration or a string")
  expect_error(dose(1, Sys.time(), na.rm = "TRUE"), "`na.rm` must be logical!")
  expect_error(dose(1, Sys.time(), as.df = "FALSE"), "`as.df` must be logical!")
})

test_that("dose with dominant_epoch detection", {
  light_de <- c(10, 20, 30)
  # Time diffs: 10min, 10min. Dominant epoch is 10min.
  time_de <- lubridate::as_datetime(0) + lubridate::minutes(c(0, 10, 20))
  # dose = sum(10,20,30) * (10min_in_sec / 3600) = 60 * (600/3600) = 60 * (1/6) = 10.
  expect_equal(dose(light_de, time_de, epoch = "dominant.epoch"), 10)
  
  # If time vector is hms, dominant_epoch needs conversion.
  time_de_hms <- hms::as_hms(time_de)
  # dominant_epoch will use difftime on POSIXct representation.
  expect_equal(dose(light_de, time_de_hms, epoch = "dominant.epoch"), 10)
})
