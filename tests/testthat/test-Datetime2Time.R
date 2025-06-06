test_that("Datetime2Time converts a single POSIXct column to hms", {
  test_data <- tibble::tibble(
    ID = 1:3,
    Timestamp = lubridate::as_datetime("2023-01-01 10:30:00") + lubridate::hours(0:2),
    Value = c(10, 20, 30)
  )
  result <- Datetime2Time(test_data)
  expect_s3_class(result$Timestamp, "hms")
  expect_equal(hms::as_hms(result$Timestamp[1]), hms::as_hms("10:30:00"))
  expect_equal(result$ID, test_data$ID) # Other columns unchanged
})

test_that("Datetime2Time converts all POSIXct columns by default", {
  test_data <- tibble::tibble(
    EventTime = lubridate::as_datetime("2023-01-01 08:00:00"),
    MeasurementTime = lubridate::as_datetime("2023-01-01 14:15:30"),
    NonTime = "A"
  )
  result <- Datetime2Time(test_data)
  expect_s3_class(result$EventTime, "hms")
  expect_s3_class(result$MeasurementTime, "hms")
  expect_equal(hms::as_hms(result$EventTime), hms::as_hms("08:00:00"))
  expect_equal(hms::as_hms(result$MeasurementTime), hms::as_hms("14:15:30"))
  expect_equal(result$NonTime, "A")
})

test_that("Datetime2Time converts only specified POSIXct columns", {
  test_data <- tibble::tibble(
    StartTime = lubridate::as_datetime("2023-01-01 09:00:00"),
    EndTime = lubridate::as_datetime("2023-01-01 17:30:00"),
    OtherData = 123
  )
  result <- Datetime2Time(test_data, cols = EndTime)
  expect_s3_class(result$StartTime, "POSIXct") # Unchanged
  expect_s3_class(result$EndTime, "hms")     # Changed
  expect_equal(hms::as_hms(result$EndTime), hms::as_hms("17:30:00"))
})

test_that("Datetime2Time handles data with no POSIXct columns gracefully", {
  test_data <- tibble::tibble(
    ID = 1:2,
    Text = c("Hello", "World")
  )
  expect_message(result <- Datetime2Time(test_data), "No columns were affected")
  expect_identical(result, test_data)
})

test_that("Datetime2Time handles specified non-existent column with dplyr::any_of", {
  test_data <- tibble::tibble(
    Timestamp = lubridate::as_datetime("2023-01-01 12:00:00")
  )
  expect_message(result <- Datetime2Time(test_data, cols = dplyr::any_of("NonExistentColumn")), "No columns were affected")
  expect_identical(result, test_data) # Timestamp column should remain POSIXct
  expect_s3_class(result$Timestamp, "POSIXct")
})

test_that("Datetime2Time throws error for non-data.frame input", {
  expect_error(Datetime2Time(list(a = 1)), "dataset neets to be a data.frame")
  expect_error(Datetime2Time(c(1, 2, 3)), "dataset neets to be a data.frame")
})
